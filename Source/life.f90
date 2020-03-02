!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
module life

  use comms
  use io,    only : current_params,stdout,dp,pi,current_lifetable,io_present,io_errors
  use trace, only : trace_exit,trace_entry
  implicit none

  real(dp),dimension(:),allocatable,private  :: baby_prob


  type  human
     logical        :: is_female  !lool for sex
     integer        :: age        !defines human age
     integer        :: no_people=0   !Number of people
     integer        :: no_dead
  end type human

  !public :: life_do_life
  !public :: life_random
  !public :: life_allocate_babies

contains

  subroutine life_do_life(age_group,num_born,num_die,diff,teen_babies)
    !==============================================================================!
    !                           L I F E _ D O _ L I F E                            !
    !==============================================================================!
    ! Subroutine for the processing all the life actions of each human.            !
    ! Calculates if each human is going to die in a given year based on their age  !
    ! and then determines if they can have a baby.                                 !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           age_group,         intent :: inout                                 !
    !           num_born,          intent :: inout                                 !
    !           num_die,           intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  03/02/2020                                            !
    !==============================================================================!
    implicit none
    !Input age group, not an array!
    type(human), intent(inout)   :: age_group
    integer,     intent(inout)   :: num_born
    integer,     intent(inout),optional :: teen_babies
    real(dp),    intent(in)      :: diff
    !Some internal variables
    integer                      :: person !The loop variable for the people
    real(dp)                     :: rand
    integer,     intent(inout)   :: num_die !This can remain in the subroutine

    integer                      :: age_counter
    real(dp),allocatable         :: age_range(:)
    integer                      :: group_babies

    ! set group babies to 0
    group_babies=0


    ! Allocate the baby prob arrays 

    if (.not.allocated(baby_prob)) then
       allocate(baby_prob(0:100))
       allocate(age_range(0:100))
       age_range=(/(real(age_counter,dp),age_counter=0,100)/)
       call life_gaussian(age_range,real(current_params%child_age,dp),&
            & current_params%child_sd,current_params%child_norm,baby_prob)
    end if

    !print*,baby_prob(age_group%age)
    do person=0,age_group%no_people-1
       !Check to see if this person dies, need life table
       ! first thing, if no people, cycle
       if (age_group%no_people.lt.1)exit


       call life_random_number(rand)


       if (rand.lt.current_lifetable%life_data(age_group%age))then
          !This person is dead
          num_die=num_die+1

          cycle ! This person cant have a baby.. sucks
       end if

       ! Now lets check if they can have a baby
       if (age_group%is_female)then
          call life_random_number(rand)

          if (rand.lt.baby_prob(age_group%age)*life_modulate_prob(diff))then
             ! This person had a baby, Yay!!
             group_babies=group_babies+1
          end if
       end if
    end do

    !print*,"DIE= ",num_die

    !count up the teenage babies
    if (present(teen_babies))then
       if (age_group%age.lt.18)then
          teen_babies=teen_babies+group_babies
       end if
    end if
    ! Now I've checked the number of born and died, change this age groups num
    if (num_die.lt.age_group%no_people)then
       age_group%no_people=age_group%no_people-num_die
       !else
       !age_group%no_people=0
    end if
    !num_die=0
    num_born=num_born+group_babies

    ! Now we want to increment the age of the group

    if (age_group%age.lt.size(current_lifetable%life_data)-1)then
       age_group%age=age_group%age+1
    else
       age_group%age=0
    end if

    ! Set the number of dead people for this age group
    age_group%no_dead=num_die

    return
  end subroutine life_do_life

  subroutine life_init_pop(species_array)
    !==============================================================================!
    !                          L I F E _ I N I T _ P O P                           !
    !==============================================================================!
    ! Subroutine for initialising the human population arrays, gives 50% of the    !
    ! initial population to each of the men and women arrays.                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           species_array,     intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  03/02/2020                                            !
    !==============================================================================!
    implicit none
    type(human),dimension(0:100),intent(inout)  :: species_array

    ! Now for some internal variables
    real(dp)              :: rand
    integer               :: i,index
    integer               :: pop_buff
    integer               :: total=0

    call trace_entry("life_init_pop")

    total=current_params%init_pop /2

    ! calculate the numbers of people on each process

    pop_buff=total/nprocs
    if (on_root_node) pop_buff=pop_buff+mod(total,nprocs)



    do i=1,pop_buff
       call life_random_number(rand)
       index=int(0_dp*rand)
       species_array(index)%no_people =species_array(index)%no_people+1
    end do
    call trace_exit("life_init_pop")
    return
  end subroutine life_init_pop




  subroutine life_allocate_babies(men,women,children,year)
    !==============================================================================!
    !                   L I F E _ A L L O C A T E _ B A B I E S                    !
    !==============================================================================!
    ! Subroutine for deciding how to share out the babies born each year.          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           men,               intent :: in                                    !
    !           women,             intent :: in                                    !
    !           children,          intent :: inout                                 !
    !           year,              intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  03/02/2020                                            !
    !==============================================================================!
    implicit none
    type(human),intent(inout)    :: men(0:100)
    type(human),intent(inout)    :: women(0:100)
    integer    ,intent(inout)    :: children
    integer                   :: year

    real(dp)                  :: rand,prob
    integer                   :: i
    integer                   :: index



    call trace_entry("life_allocate_babies")
    if (year.lt.100) then
       index=100-year
    else
       index=100-mod(year,101)
    end if
    prob=0.5_dp
    !print*,"CHILDREN: ",children


    men(index)%no_people=0
    women(index)%no_people=0


    do i=1,children

       call life_random_number(rand)
       if (rand.gt.prob)then
          !print*,"boy child"
          women(index)%no_people=women(index)%no_people+1
       else
          !print*,"girl child"
          men(index)%no_people=men(index)%no_people+1

       end if

    end do
    call trace_exit("life_allocate_babies")

    return
  end subroutine life_allocate_babies


  subroutine life_random()
    !==============================================================================!
    !                            L I F E _ R A N D O M                             !
    !==============================================================================!
    ! Subroutine for initialising the random seed for the random number calls.     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  03/02/2020                                            !
    !==============================================================================!
    implicit none
    integer, allocatable :: seed(:)
    integer ::  n, un, istat,the_seed
    call trace_entry("life_random")


    if (on_root_node)then 
       if (.not.io_present("random_seed"))then
          call random_seed(size = n)
          allocate(seed(n))
          open(newunit=un, file="/dev/urandom", access="stream", &
               form="unformatted", action="read", status="old", iostat=istat)
          read(un) seed
          close(un)
          the_seed=abs(seed(1))
          seed=the_seed
       else
          call random_seed(size = n)
          allocate(seed(n))
          the_seed=current_params%random_seed
          seed=the_seed
       end if
    end if
    call comms_bcast(the_seed,1)
    seed=the_seed+37*rank
    current_params%random_seed=seed(1)

    call random_seed(put=seed)
    call trace_exit("life_random")
    return
  end subroutine life_random

  subroutine life_random_number(rand)
    implicit none
    real(dp),intent(inout)  ::rand 
    !call trace_entry("life_random_number")
    call random_number(rand)
    
    !call trace_exit("life_random_number")
    return
  end subroutine life_random_number

  
  subroutine life_count_pop(men,women,men_count,women_count)
    !==============================================================================!
    !                         L I F E _ C O U N T _ P O P                          !
    !==============================================================================!
    ! Subroutine for getting the current population count split into men and       !
    ! women.                                                                       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           men,               intent :: in                                    !
    !           women,             intent :: in                                    !
    !           men_count,         intent :: inout                                 !
    !           women_count,       intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  03/02/2020                                            !
    !==============================================================================!
    implicit none
    type(human),dimension(:),intent(in)    :: men
    type(human),dimension(:),intent(in)    :: women
    integer                 ,intent(inout) :: men_count
    integer                 ,intent(inout) :: women_count

    integer   ::  age
    !call trace_entry("life_count_pop")
    ! Zero the countszxz`
    men_count=0
    women_count=0

    do age=1,size(men)
       men_count=men_count+men(age)%no_people
       women_count=women_count+women(age)%no_people
    end do
    !call trace_exit("life_trace_pop")
    return
  end subroutine life_count_pop


  subroutine life_gaussian(x,mean,stdev,norm,prob)
    !==============================================================================!
    !                          L I F E _ G A U S S I A N                           !
    !==============================================================================!
    ! Low level subroutine for calculating a Gaussian distribution based on        !
    ! input parameters.                                                            !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           x,                 intent :: in                                    !
    !           mean,              intent :: in                                    !
    !           stdev,             intent :: in                                    !
    !           norm,              intent :: in                                    !
    !           prob,              intent :: out                                   !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  03/02/2020                                            !
    !==============================================================================!
    implicit none

    real(dp),intent(in)   ,dimension(:)    :: x
    real(dp),intent(in)                    :: mean,stdev,norm
    real(dp),intent(out),dimension(:)      :: prob

    integer  :: age

    call trace_entry("life_gaussian")
    do age=0,size(prob)
       prob(age)=norm/(stdev*sqrt(2_dp*pi))*&
            & exp(-0.5_dp*((mean-x(age))/stdev)**2)
    end do
    call trace_exit("life_gaussian")
    return
  end subroutine  life_gaussian



  subroutine life_average_age(men,women,average)
    !==============================================================================!
    !                       L I F E _ A V E R A G E _ A G E                        !
    !==============================================================================!
    ! Subroutine for calculating the average age of the entire population in any   !
    ! given year                                                                   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           men,               intent :: in                                    !
    !           women,             intent :: in                                    !
    !           average,           intent :: out                                   !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  03/02/2020                                            !
    !==============================================================================!
    implicit none
    type(human),intent(in), dimension(:) :: men, women
    real(dp)   ,intent(out)              :: average

    integer :: i
    integer :: no_men,no_women


    average=0_dp

    do i=1,100

       average=average+real(men(i)%no_people,dp)*real(men(i)%age,dp)+&
            & real(women(i)%no_people,dp)*real(women(i)%age,dp)
    end do

    call life_count_pop(men,women,no_men,no_women)

    !print*,real(no_men+no_women,dp)
    average=average/real(no_men+no_women,dp)

    return
  end subroutine life_average_age


  subroutine life_redistribute(people_array)
    !==============================================================================!
    !                      L I F E _ R E D I S T R I B U T E                       !
    !==============================================================================!
    ! Subroutine for redistributing the population across the processes so as to   !
    ! prevent idal cores if the local populatiom happens to die                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           people_array,      intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  23/02/2020                                            !
    !==============================================================================!
    implicit none
    type(human),intent(inout),dimension(:)  :: people_array

    integer,allocatable,dimension(:)        :: total_age_array, total_age_array_buff

    integer :: age
    call trace_entry("life_redistribute")
    allocate(total_age_array_buff(1:size(people_array)))
    allocate(total_age_array(1:size(people_array)))

    total_age_array=0
    total_age_array_buff=0



    ! Stop all of the things until they are all here, we dont want them to be running off
    call comms_barrier()

    ! Populate the array with the number of people at each age on each process
    do age=1,size(people_array)
       total_age_array(age)=people_array(age)%no_people
    end do


    ! Now we want a comms reduce of that array
    call comms_reduce(total_age_array,total_age_array_buff,size(total_age_array),"MPI_SUM")
    ! Now we need to send it all back
    call comms_bcast(total_age_array_buff,size(total_age_array))

    do age=1,size(people_array)
       people_array(age)%no_people = total_age_array_buff(age)/nprocs

       ! Now we handle the remainder
       if (on_root_node) people_array(age)%no_people = &
            & people_array(age)%no_people &
            & + mod(total_age_array_buff(age),nprocs)
    end do

    call trace_exit("life_redistribute")
    return
  end subroutine life_redistribute

  function life_modulate_prob(diff) result(modulation)
    !==============================================================================!
    !                     L I F E _ M O D U L A T E _ P R O B                      !
    !==============================================================================!
    ! Function which produces a Fermi-Dirac like distrubution which reduces the    !
    ! birth probability when there are more or less men than women.                !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           diff,              intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           modulation                                                         !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  23/02/2020                                            !
    !==============================================================================!
    implicit none
    real(dp),intent(in) :: diff
    real(dp)            :: modulation

    modulation=1_dp/(exp((0.25_dp-diff)/0.04)+1_dp)

  end function life_modulate_prob


  function life_demographics(men,women,age_low,age_up) result(percentage)
    implicit none
    type(human),intent(in)    :: men(0:100),women(0:100)

    integer    ,intent(in)    :: age_low,age_up
    real(dp)                  :: percentage
    
    integer                   :: pop,pop_men,pop_women
    integer                   :: count
    integer                   :: pop_buff
    
    call trace_entry("life_demographics")
    pop_buff=0
    
    do count=0,100
       if (men(count)%age.ge.age_low.and.men(count)%age.le.age_up)then
          call life_count_pop(men,women,pop_men,pop_women)
          pop=pop_men+pop_women
          pop_buff=pop_buff+men(count)%no_people+women(count)%no_people
       end if
    end do

    percentage=real(pop_buff,dp)/real(pop,dp)*100_dp
    call trace_exit("life_demographics")
    
  end function  life_demographics




end module life

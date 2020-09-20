!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
program mc_pop
  use io
  use trace, only : trace_entry,trace_exit,trace_finalise,trace_init
  use comms
  use life
  implicit none 
  logical                                :: warnings=.false.
  logical                                :: warnings_buff=.false.
  type(human),dimension(:),allocatable   :: population_men
  type(human),dimension(:),allocatable   :: population_women
  integer                                :: stat_pop
  integer                                :: i,j,k            ! Some boring counters
  integer                                :: babies=0,deaths=0,prev_babies=0
  integer                                :: year
  logical                                :: survived                 ! Flag for if people survived
  ! All the population counters
  integer                                :: popcount_men=0
  integer                                :: popcount_women=0
  integer                                :: popcount_total=0
  real(dp)                               :: pop_diff=1_dp
  real(dp)                               :: average_age=0 
  integer                                :: teen_girls=0
  integer                                :: teen_babies=0
  integer                                :: infants=0
  integer                                :: infant_deaths
  integer                                :: tot_deaths
  integer                                :: tot_births

  ! Lets build some arrays that are going to store all the stuff i need
  integer,dimension(:),allocatable       :: total_count,       total_buff  ! Array with total population
  real(dp),   dimension(:),allocatable   :: average_age_array, age_buff       ! Array to store average age each year
  real(dp),   dimension(:),allocatable   :: teen_preg_array,   teen_buff         ! Array to store teen prgnancy rate
  real(dp),   dimension(:),allocatable   :: birth_rate_array,  br_buff
  real(dp),   dimension(:),allocatable   :: infant_mortality,  infant_buff
  real(dp),   dimension(:),allocatable   :: life_expect,       expect_buff

  integer ::seed(0:0)
  ! The variable declaration for the results
  type(results)                          :: current_results

  ! Generic things for auxilliary calculations
  real(dp)                               :: efficiency
  real(dp)                               :: start_time,end_time,current_time,time   ! Some timing stuff
  real(dp)      :: random_test

  integer :: test_pop ! population counter for mpi buffers

  ! Set up all the things I'll need for the calculation Trace, IO, MPI
  call trace_init()          ! first things first, set up  communications
  call trace_entry("mc_pop") ! call trace for main calculation
  call comms_init()

  ! Set up the timers
  if(on_root_node)start_time=comms_wtime()
  call comms_bcast(start_time,1) !Broadcast the start time to thats its universal

!!$  open(2309,file="demographics.pop",form="UNFORMATTED")
!!$  read(2309)i
!!$  read(2309)i,j,k,warnings
!!$  print*,i,j,k,warnings


  call io_initialise()



  ! Set up the random numbers, these should be on each process

  call life_random()
  ! Random number testing code
  !open(newunit=year,form="FORMATTED",status="unknown",file="random.pop")
  !do i=1,current_params%calc_len
  !   call life_random_number(random_test)
  !   write(year,*)random_test
  !end do



  ! Here we write all the parameters to the main output file
  call io_write_params()

  ! before the calculations we can check if a dry run is required
  if (current_params%dry_run) call io_dryrun()



  ! Allocate some arrays
  allocate(total_count(0:current_params%calc_len))
  if(on_root_node)   allocate(total_buff(0:current_params%calc_len))

  allocate(average_age_array(0:current_params%calc_len))
  if(on_root_node)   allocate(age_buff(0:current_params%calc_len))

  allocate(teen_preg_array(0:current_params%calc_len))
  if(on_root_node)   allocate(teen_buff(0:current_params%calc_len))

  allocate(birth_rate_array(0:current_params%calc_len))
  if(on_root_node)   allocate(br_buff(0:current_params%calc_len))

  allocate(infant_mortality(0:current_params%calc_len))
  if(on_root_node)   allocate(infant_buff(0:current_params%calc_len))

  allocate(life_expect(0:current_params%calc_len))
  if(on_root_node)   allocate(expect_buff(0:current_params%calc_len))


  ! Here start initialising the population!!
  allocate(population_men(0:size(current_lifetable_m%life_data)-1),stat=stat_pop)
  if (stat_pop.ne.0) call io_errors("Error in allocate: Population allocate failed")
  allocate(population_women(0:size(current_lifetable_m%life_data)-1),stat=stat_pop)
  if (stat_pop.ne.0) call io_errors("Error in allocate: Population allocate failed")





  do i=0,size(population_men)-1
     population_men(i)%is_female=.false.
     population_men(i)%age=i
     population_men(i)%no_people=0
     population_women(i)%is_female=.true.
     population_women(i)%age=i
     population_women(i)%no_people=0

  end do


  ! Initialise the people arrays
  call life_init_pop(population_men)
  call life_init_pop(population_women)

  !if(rank.eq.1)print*,"RANK=",rank,population_men%no_people
  !stop
  call life_count_pop(population_men,population_women,popcount_men,popcount_women)



  call trace_entry("pop_main")
  ! Write the start of the table that tracks the calculations
13 format("|",26x,"Starting Calculation...",26x,"|",1x,"<--YEAR")! Start format 
14 format("+",A75,"+",1x,"<--YEAR")                               ! Header format
15 format("|",7x,A10,15x,A10,15x,A10,8x,"|",1x,"<--YEAR")         ! Titles format
16 format("|",13x,i4,15x,i10,15x,f8.2,1x,"s",8x,"|",1x,"<--YEAR") ! Numbers format


  ! Before calculation, write table header
  if (on_root_node)then
     write(stdout,*)
     write(stdout,14) repeat("=",75)
     write(stdout,13)
     write(stdout,14) repeat("=",75)
     write(stdout,15) "Year","Population","Time"
     write(stdout,14) repeat("=",75)
  end if


  do year=0,current_params%calc_len


!!$     print*,sum(population_men%no_people+population_women%no_people),&
!!$          -1*sum(population_men%no_dead+population_women%no_dead),&
!!$          sum(population_women%no_born)
!!$     if(year.eq.0)print*,population_women%no_people
     ! zero the useful variables 
     babies=0
     teen_girls=0
     teen_babies=0
     infants=0
     infant_deaths=0
     tot_deaths=0
     !print*,popcount_total,year
     ! Loop for each age group
     do i=0,100
        call life_do_life(population_men(i),pop_diff)

        ! Debugging totals
        tot_deaths=tot_deaths+deaths

        if (population_men(i)%age.lt.5) infant_deaths=infant_deaths+population_men(i)%no_dead
        call life_do_life(population_women(i),pop_diff,teen_babies)

        ! Debugging totals
        !tot_deaths=tot_deaths+deaths


        if (population_men(i)%age.lt.5)then
           infant_deaths=infant_deaths+population_women(i)%no_dead
        end if

        call life_count_pop(population_men,population_women,popcount_men,popcount_women)

        ! calculate the teenage pregnancy rate
        if (population_women(i)%age.lt.18) then
           teen_girls=teen_girls+population_women(i)%no_people
        end if
        ! calculate infant mortality rate
        if(population_men(i)%age.lt.6) then           
           infants=infants+population_men(i)%no_people+&
                & population_women(i)%no_people
        end if
        !if(i.lt.10)        print*,population_women(i)%age,population_men(i)%no_people,population_men(i)%no_born,population_men(i)%no_dead
     end do
     babies=sum(population_women%no_born)

     !print*,babies-tot_deaths,year



     !*********************************************************************************
     call life_average_age(population_men,population_women,average_age)
     call life_count_pop(population_men,population_women,popcount_men,popcount_women)
     popcount_total=0
     popcount_total=popcount_men+popcount_women
     pop_diff=1_dp-(abs(popcount_men-popcount_women)/real(popcount_total,dp))



     if (popcount_total.eq.0) then
        warnings=.true.
     end if

     !write total poulation to the array
     total_count(year)=popcount_total
     average_age_array(year)=average_age
     birth_rate_array(year)=1000_dp*real(babies,dp)/real(popcount_total,dp)

     ! Life expectancy
     if(sum(population_women%no_dead+population_men%no_dead).eq.0)then 
        life_expect(year)=0_dp
     else
        life_expect(year)=sum(population_women%age*population_women%no_dead + &
             & population_men%age*population_men%no_dead) / &
             & sum(population_women%no_dead+population_men%no_dead)
     end if




     ! Calculate the infant mortality rate that year
     if (infants.gt.0)then        
        infant_mortality(year)=1000_dp*real(infant_deaths,dp)/real(infants,dp)
     else
        infant_mortality(year)=0_dp
     end if

     if (teen_girls.gt.0)then
        teen_preg_array(year)=1000_dp*real(teen_babies,dp)/real(teen_girls,dp)
     else
        teen_preg_array(year)=0_dp
     end if

!!$     population_men%no_people=population_men%no_people*nprocs
!!$     population_women%no_people=population_women%no_people*nprocs
!!$     do k=0,100
!!$        write(demo_unit) 0,population_men(k)%age,population_men(k)%no_people,population_men(k)%is_female
!!$        write(demo_unit) 0,population_women(k)%age,population_women(k)%no_people,population_women(k)%is_female
!!$     end do
!!$     population_men%no_people=population_men%no_people/nprocs
!!$     population_women%no_people=population_women%no_people/nprocs

     !**************************************************************************************


     !Now that we have calculated all of the properties of the people, we need to age them
     do k=0,100
        if (population_men(k)%age.lt.size(current_lifetable_m%life_data)-1)then
           population_men(k)%age=population_men(k)%age+1
           population_women(k)%age=population_women(k)%age+1
        else
           population_men(k)%age=0
           population_women(k)%age=0
        end if
     end do

     ! allocate all of the babies to year 0 from the previous year

     call life_allocate_babies(population_men,population_women,babies,year)



     ! now we redisbribute the data if required
     if (mod(year,current_params%redistrib_freq).eq.0 &
          & .or.year.eq.current_params%calc_len) then

        if (year.gt.0) then 

           call life_redistribute(population_men)
           call life_redistribute(population_women)

           call life_count_pop(population_men,population_women,popcount_men,popcount_women)
           popcount_total=popcount_men+popcount_women
           call comms_reduce(popcount_total,test_pop,1,"MPI_SUM")
           call comms_bcast(test_pop,1)


           if (test_pop.eq.0)then
              survived=.FALSE.
           else
              survived=.TRUE.
           end if

        end if

        !calculate the time
        current_time=comms_wtime()
        call comms_reduce(current_time,time,1,"MPI_MAX")
        call life_count_pop(population_men,population_women,popcount_men,popcount_women)
        popcount_total=popcount_men+popcount_women
        call comms_reduce(popcount_total,test_pop,1,"MPI_SUM")
        if (on_root_node)then
           time=time-start_time

           ! Now we write...
           write(stdout,16) year,test_pop,time

           ! The demographics file
           if (current_params%write_demo)then 
              population_men%no_people=population_men%no_people*nprocs
              population_women%no_people=population_women%no_people*nprocs
              do k=0,100
                 write(demo_unit) year,population_men(k)%age,population_men(k)%no_people,population_men(k)%is_female
                 write(demo_unit) year,population_women(k)%age,population_women(k)%no_people,population_women(k)%is_female
              end do
              population_men%no_people=population_men%no_people/nprocs
              population_women%no_people=population_women%no_people/nprocs
           end if
        end if

     end if
     ! Flush the cashe every year
     call io_flush(stdout)
  end do

  ! print the final line of the table
  if (on_root_node) write(stdout,14) repeat("=",75)

  call trace_exit("pop_main")

  ! Do some comms to sum up the arrays


  call comms_reduce(total_count,total_buff,size(total_count),"MPI_SUM")
  call comms_reduce(teen_preg_array,teen_buff,size(teen_preg_array),"MPI_SUM")
  call comms_reduce(birth_rate_array,br_buff,size(birth_rate_array),"MPI_SUM")
  call comms_reduce(average_age_array,age_buff,size(average_age_array),"MPI_SUM")
  call comms_reduce(infant_mortality,infant_buff,size(infant_mortality),"MPI_SUM")
  call comms_reduce(warnings,warnings_buff,1,"MPI_LOR")
  call comms_reduce(life_expect,expect_buff,size(life_expect),"MPI_SUM")


  ! Print out that the people are dead and theres no point doing anything


  if (on_root_node) then
     call io_survival(survived)



     ! Handle the averaging
     teen_buff=teen_buff/real(nprocs,dp)
     age_buff=age_buff/real(nprocs,dp)
     br_buff=br_buff/real(nprocs,dp)
     infant_buff=infant_buff/real(nprocs,dp)
     expect_buff=expect_buff/real(nprocs,dp)

     ! Write out to the correct files

     write(stdout,*) ""
     if (current_params%write_population) then 
        open(4,file="population.pop",status="unknown",form="unformatted")
        write(stdout,*)"Writing population data to 'population.pop'"
        write(4) total_buff
        close(4)
     end if
     if (current_params%write_ave_age ) then
        open(5,file="average_age.pop",status="unknown",form="unformatted")
        write(stdout,*)"Writing average age data to 'average_age.pop'"
        write(5) age_buff
        close(5)
     end if
     if (current_params%write_birth_rate)then
        open(6,file="birth_rate.pop",status="unknown",form="unformatted")
        write(stdout,*)"Writing birth rate data to 'birth_rate.pop'"
        write(6) br_buff
        close(6)
     end if





     ! Lets do some result writing to the file, how exciting

     current_results%ave_age         =real(sum(age_buff)/size(age_buff),dp)
     current_results%infant_mort     =real(sum(infant_buff)/size(infant_buff),dp)
     current_results%teen_preg       =real(sum(teen_buff)/size(teen_buff),dp)
     current_results%birth_rate      =real(sum(br_buff)/size(br_buff),dp)
     current_results%lt_5            =life_demographics(population_men,population_women,0,5)
     current_results%i10_20          =life_demographics(population_men,population_women,10,19)
     current_results%i20_30          =life_demographics(population_men,population_women,20,29)
     current_results%i30_40          =life_demographics(population_men,population_women,30,39)
     current_results%i40_50          =life_demographics(population_men,population_women,40,49)
     current_results%i50_65          =life_demographics(population_men,population_women,50,64)
     current_results%i65_plus        =life_demographics(population_men,population_women,65,100)
     current_results%i85_plus        =life_demographics(population_men,population_women,85,100)
     current_results%life_expectancy =real(sum(expect_buff)/size(expect_buff),dp)
     current_results%men_pc          =100_dp*(real(popcount_men,dp)/real(popcount_total,dp))


     ! calculate the demographics 


     ! write out the properties calculated throughout the calculation
     call io_write_results(current_results,survived)


     write(stdout,1001) 
     if (warnings_buff)then
        write(stdout,'(1x,"|",8x,A,4x,"|")') "Population went to zero on one or more processes, consider increasing"
        write(stdout,'(1x,"|",8x,A,44x,"|")') "the redistribution frequency."
     else 
        write(stdout,'(1x,"|",8x,A,35x,"|")') "Calculation completed without warnings"
     end if
     write(stdout,*)"+"//repeat("-",81)//"+"
  end if

1001 FORMAT(1x,"|",5x,"Calculation Report:",57x"|")




  call trace_exit("mc_pop")


  current_time=comms_wtime()
  call trace_finalise(current_params%debuging,rank)
  call comms_reduce(global_time,time,1,"MPI_MAX")


  if (on_root_node)then
     !time=time-start_time
     efficiency=(1_dp-comms_time/time)*100_dp
     write(stdout,'(1x,"|",5x,"Total time: ",f10.2,1x,"s",52x,"|")')time
     if (nprocs.gt.1) write(stdout,'(1x,"|",5x,"Efficiency: ",f10.2,1x,"%",52x,"|")')Efficiency
     write(stdout,*)"+"//repeat("-",81)//"+"
  end if
  call comms_finalise() ! last things last, take down the comms.


  close(stdout)



end program mc_pop

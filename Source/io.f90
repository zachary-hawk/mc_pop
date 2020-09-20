module io

  !Impose strong typing
  use trace, only : trace_entry, trace_exit,trace_stack,trace_finalise
  use comms, only : rank,nprocs,on_root_node,comms_abort,max_version_length,comms_arch,COMMS_VERSION &
       & ,COMMS_LIBRARY_VERSION,COMMS_FINALISE,comms_barrier
  use iso_fortran_env, only :real64,compiler_version
  implicit none


  logical,           public                :: no_param,read_params=.true.
  character(len=128),public                :: version  = "MC_POP v.1.0, Z. Hawkhead"
  character(len=128),public                :: info     = "Parallel code for Monte Carlo Population simulation"
  integer,           public,parameter      :: stdout = 984 
  integer,           public,parameter      :: dp = real64
  integer                                  :: demo_unit
  real,              public,parameter      :: pi=3.1415926535
  logical,           public                :: file_exists
  character(100),dimension(:),allocatable  :: present_array

  character(100),dimension(:),allocatable  :: keys_array
  character(100),dimension(:),allocatable  :: keys_description
  character(100),dimension(:),allocatable  :: keys_default
  character(100),dimension(:),allocatable  :: keys_allowed
  character(100),dimension(:),allocatable  :: keys_type

  real(dp),    dimension(0:100),public      :: demo_init_men
  real(dp),    dimension(0:100),public      :: demo_init_women

  integer                                  :: max_params=1

  type  parameters
     ! Begin: parameters
     
     !Calculation parameters
     integer          :: init_pop          = 10000
     real(dp)         :: child_age         = 23.0
     integer          :: calc_len          = 100
     integer          :: life_table_year   = 2017
     !Child prob params
     real(dp)         :: child_sd          = 5.0_dp
     real(dp)         :: child_norm        = 2.2_dp!0.4_dp


     !Some extra functionality
     logical          :: dry_run           = .false.
     logical          :: debuging          = .false.
     integer          :: redistrib_freq    = 10

     logical          :: init_demo         = .false.
     integer          :: random_seed

     !I/O parameters
     logical          :: write_population  = .true.
     logical          :: write_ave_age     = .false.
     logical          :: write_birth_rate  = .false.
     logical          :: write_demo        = .false.
     ! Disease parameters
     real(dp)         :: disease_spread    = 0.1
     real(dp)         :: disease_mort      = 0.02
     real(dp)         :: disease_init      = 0.01
     real(dp)         :: disease_crit      = 0.5
     logical          :: disease           = .false.
     ! End: parameters
  end type parameters

  ! Begin: keys
  character(len=30),parameter,public :: key_init_pop         = "initial_population"
  character(len=30),parameter,public :: key_mean_child_age   = "birth_age"
  character(len=30),parameter,public :: key_calc_len         = "duration"
  character(len=30),parameter,public :: key_life_table_year  = "life_table_year"

  character(len=30),parameter,public :: key_child_norm       = "birth_rate"
  character(len=30),parameter,public :: key_child_sd         = "birth_std"
  character(len=30),parameter,public :: key_debug            = "debug"
  character(len=30),parameter,public :: key_redistrib_freq   = "redistrib_freq"

  character(len=30),parameter,public :: key_write_pop        = "write_pop"
  character(len=30),parameter,public :: key_write_br         = "write_birth_rate"
  character(len=30),parameter,public :: key_write_age        = "write_ave_age"
  character(len=30),parameter,public :: key_write_demo       = "write_demographics"
  character(len=30),parameter,public :: key_init_demo        = "init_demographics"
  
  character(len=30),parameter,public :: key_random_seed      = "random_seed"

  character(len=30),parameter,public :: key_disease_spread   = "disease_spread"
  character(len=30),parameter,public :: key_disease_mort     = "disease_mortality"
  character(len=30),parameter,public :: key_disease_init     = "disease_init_pop"
  character(len=30),parameter,public :: key_disease_crit     = "disease_critical_mass"
  character(len=30),parameter,public :: key_disease          = "disease"
  ! End: keys

  type life_table
     real(dp),dimension(0:100)  :: life_data
     integer                    :: year
     character(100)             :: web         ="https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/NVSR/"
  end type life_table


  type results
     real(dp)  :: birth_rate
     real(dp)  :: teen_preg
     real(dp)  :: ave_age
     real(dp)  :: infant_mort
     real(dp)  :: lt_5=0_dp
     real(dp)  :: i10_20
     real(dp)  :: i20_30
     real(dp)  :: i30_40
     real(dp)  :: i40_50
     real(dp)  :: i50_65
     real(dp)  :: i65_plus
     real(dp)  :: i85_plus
     real(dp)  :: life_expectancy
     real(dp)  :: men_pc
  end type results


  type(parameters),public,save             :: current_params

  type(life_table),public,save             :: current_lifetable_m
  type(life_table),public,save             :: current_lifetable_f
  


  ! No. Parameter keys
  integer,parameter                        :: max_keys = 19


  !-------------------------------------------------------!
  !              P U B L I C  R O U T I N E S             !
  !-------------------------------------------------------!
  public :: io_initialise
  public :: io_errors

contains



  subroutine io_initialise()
    !==============================================================================!
    !                          I O _ I N I T I A L I S E                           !
    !==============================================================================!
    ! Subroutine for initialising all input/output for the parallel code MC_POP    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  19/01/2020                                            !
    !==============================================================================!
    ! Should be called in top level file
    implicit none

    integer :: i ! counters
    character(10) :: line
    integer :: stat
    integer :: demo_length ,year, age, age_pop
    logical :: demo_file


    ! Some junk variables


    call trace_entry("io_initialise")
    call io_cl_parser() ! Read the commandline arguments



    ! Get the length of the parameters file
    inquire(file="params.pop",exist=file_exists)

    if (file_exists)then
       open(unit=1,file="params.pop",iostat=stat,status="OLD",access="stream",form="formatted")
       do while (stat.eq.0)
          read(1,'(A60)',iostat=stat) line
          max_params=max_params+1
       end do
       close(1)
    end if
    ! Allocate space for the params array
    allocate(present_array(0:max_params))

    ! Fist things first, try to read paramteters
    if (read_params) call io_read_param(current_params)


    ! This is where intialise life tables, there will be a datatype in this file
    call io_read_life(current_lifetable_m,.false.)
    call io_read_life(current_lifetable_f,.true.)



    ! Make some sensible defaults
    if (.not.io_present(key_redistrib_freq))then
       if (current_params%calc_len.gt.10) then 
          current_params%redistrib_freq=current_params%calc_len/10
       else
          current_params%redistrib_freq=1
       end if
    end if

    ! Set Disease
    if (io_present(key_disease_init)&
         & .or.io_present(key_disease_mort)&
         & .or.io_present(key_disease_spread)&
         & .or.io_present(key_disease_crit)) current_params%disease=.true.

    ! Check for stupidity 
    if (current_params%calc_len.lt.0) &
         & call io_errors("Error in I/O: "//trim(key_calc_len)//" must be positive")
    if (current_params%init_pop.lt.0) &
         & call io_errors("Error in I/O: "//trim(key_init_pop)//" must be positive")
!!$    if (current_params%calc_len.lt.current_params%redistrib_freq)&
!!$         & call io_errors("Error in I/O: "//trim(key_redistrib_freq)//" must be less than or equal to "//trim(key_calc_len)) 


    ! Check there are enough people for the number of cores
    if (current_params%init_pop.gt.0 .and.&
         & current_params%init_pop.lt.nprocs) &
         & call io_errors("Error in io_initialise: Too few people, cannot distribute accross requested proccesses")
    ! Print warning if numbers of people too low on each process
    if(current_params%init_pop/nprocs.lt.10)&
         & write(*,*) "Warning: Number of people per proccess is low, consider increasing"

    ! Open up the main file for the output
    open(stdout,file="out.pop",RECL=8192,form="FORMATTED",access="append")



    if (current_params%init_demo)then
       inquire(file="demographics.pop",exist=demo_file)

       if (demo_file)then
          open(100,file="demographics.pop",status='old',form="UNFORMATTED")


          read(100,iostat=stat) demo_length



          if(stat.ne.0) call io_errors("I/O Error: read error in demographics.pop")
          do while(stat.eq.0)
             read(100,iostat=stat) year, age,age_pop
             if(stat.ne.0)exit
             demo_init_men(age)=real(age_pop,dp)
             read(100,iostat=stat)year,age,age_pop
             demo_init_women(age)=real(age_pop,dp)
          end do
          demo_init_men=demo_init_men/sum(demo_init_men)
          demo_init_women=demo_init_women/sum(demo_init_women)
          close(100)
       else
          call io_errors("I/O Error: No file 'demographics.pop'")
       end if
       
    end if
    if (current_params%write_demo)then 
       if (.not.current_params%dry_run)then 
          ! Open the check file
          open(newunit=demo_unit,file="demographics.pop",form="UNFORMATTED",status="unknown")
          ! write the number of years we've got
          write(demo_unit) 1+current_params%calc_len/current_params%redistrib_freq
       end if
    end if
    call io_header()
    call trace_exit("io_initialise")


    return
  end subroutine io_initialise


  subroutine io_read_param(dummy_params)
    !==============================================================================!
    !                          I O _ R E A D _ P A R A M                           !
    !==============================================================================!
    ! Subroutine for reading parameters from the file "param.pop" to the           !
    ! parameter type current_params                                                !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           dummy_params,      intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  19/01/2020                                            !
    !==============================================================================!
    implicit none
    !The inout stuff
    type(parameters),intent(inout)  :: dummy_params


    !The boring stuff to make the whole shebang work
    integer           :: stat
    integer           :: read_stat=0
    integer           :: i,j,k           !counter

    character(len=60) :: line        ! charcter string into which each line is read, overwritten in loop
    character(len=30) :: key         ! the keyword used
    character(len=30) :: param       ! the value of the param
    logical           :: comment     ! boolean for comment line, will skip
    real(dp)          :: real_dump   ! a dump for handling scientific 
    call trace_entry("io_read_param")

    !Open the parameter file
    if (file_exists) then
       open(unit=1,file="params.pop",iostat=stat,status="OLD",access="stream",form="formatted")


       if (stat.ne.0) call io_errors("Error in I/O: Open file 'params.pop'")
       ! now we can do the reading
       do i=0,max_params
          !first thing, read new line into 'line' variable
          read(1,'(A60)',iostat=read_stat) line
          !if (read_stat.ne.0) call io_errors("Error in I/O: Error in read params")



          !Read everying into a thing

          call io_freeform_read(line,key,param,comment)

          if (comment) cycle ! skip if comment

          !Do some trimming
          key=adjustl(trim(io_case(key)))
          param=adjustl(trim(io_case(param)))

          ! Begin: case_read
          select case(key)
          case(key_init_pop)
             read(param,*,iostat=stat) real_dump
             dummy_params%init_pop=int(real_dump)
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_mean_child_age) 
             read(param,*,iostat=stat) dummy_params%child_age
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_calc_len ) 
             read(param,*,iostat=stat) real_dump
             dummy_params%calc_len=int(real_dump)
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_life_table_year) 
             read(param,*,iostat=stat) dummy_params%life_table_year
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_child_norm)
             read(param,*,iostat=stat) dummy_params%child_norm
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_child_sd)
             read(param,*,iostat=stat) dummy_params%child_sd
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_debug)
             read(param,*,iostat=stat) dummy_params%debuging
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_write_pop)
             read(param,*,iostat=stat) dummy_params%write_population
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_write_br)
             read(param,*,iostat=stat) dummy_params%write_birth_rate
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_write_age)
             read(param,*,iostat=stat) dummy_params%write_ave_age
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_redistrib_freq)
             read(param,*,iostat=stat) dummy_params%redistrib_freq
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_random_seed)
             read(param,*,iostat=stat) dummy_params%random_seed
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_disease_spread)
             read(param,*,iostat=stat) dummy_params%disease_spread
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_disease_init)
             read(param,*,iostat=stat) dummy_params%disease_init
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_disease_mort)
             read(param,*,iostat=stat) dummy_params%disease_mort
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_disease_crit)
             read(param,*,iostat=stat) dummy_params%disease_crit
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_disease)
             read(param,*,iostat=stat) dummy_params%disease
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_write_demo)
             read(param,*,iostat=stat) dummy_params%write_demo
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_init_demo)
             if (trim(param).eq."default")then
                dummy_params%init_demo = .false.
             else if(trim(param).eq."continuation")then
                dummy_params%init_demo = .true.
             else
                call io_errors("Error in I/O: Error parsing value: "//param)
             end if
             present_array(i)=key
             ! End: case_read
          case default
             call io_errors("Error in I/O: Error parsing keyword: "//key)
          end select

       end do


    else
       no_param=.true.
       call trace_exit("io_read_param")
       return
    end if

    ! Check for duplicates

    do i=0,max_params
       do j=0,max_params
          if (i.eq.j)cycle
          if (present_array(i).eq.present_array(j))then
             !call io_errors("Error in I/O: Duplicate parameter found: "//present_array(i))
          end if
       end do
    end do

    call trace_exit("io_read_param")
    return
  end subroutine io_read_param


  subroutine io_read_life(dummy_life,female)
    !==============================================================================!
    !                           I O _ R E A D _ L I F E                            !
    !==============================================================================!
    ! Subroutine for reading life files as dictated by the year specified in the   !
    ! parameters file                                                              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           dummy_life,        intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  19/01/2020                                            !
    !==============================================================================!
    implicit none
    type(life_table), intent(inout) :: dummy_life
    logical                         :: female
    integer                         :: wstat,stat,counter=0
    character(30)                   :: name,first,second,life_str
    real(dp)                        :: prob
    logical                         :: file_exists
    integer                         :: life_file

    call trace_entry("io_read_life")
#ifdef life_dir
#define life_str life_dir
#endif 


    if(female)then

       write(name,'("f_life_table_",I0,".csv")',iostat=stat) current_params%life_table_year
       if (stat.ne.0) call io_errors("Error in I/O: Internal variable write error")
       name=trim(name)
    else
       write(name,'("m_life_table_",I0,".csv")',iostat=stat) current_params%life_table_year
       if (stat.ne.0) call io_errors("Error in I/O: Internal variable write error")
       name=trim(name)
    end if
    !check if the User has used a correct file, if not, cause error

    inquire(file=trim(life_str)//"/life_tables/"//name,exist=file_exists)

    if (.not.file_exists) then

       call io_errors("Error in I/O: No life table found: "//name)
    end if

    open(newunit=life_file,file=trim(life_str)//"/life_tables/"//name,status='old')

    do while (trim(first) .ne. "0-1")
       read(life_file,*)first,second
    end do
    counter=0
    do while (stat.eq.0)
       read(second,*,iostat=wstat) prob

       if (wstat.ne.0) call io_errors("Error in I/O: Internal variable write error")
       dummy_life%life_data(counter)=prob
       read(life_file,*,iostat=stat) first,second

       counter=counter+1
    end do

    dummy_life%year=current_params%life_table_year
    close(life_file)
    call trace_exit("io_read_life")
    return
  end subroutine io_read_life



  subroutine io_freeform_read(line_unparsed,key,val,com)
    !==============================================================================!
    !                       I O _ F R E E F O R M _ R E A D                        !
    !==============================================================================!
    ! Subroutine for parsing keys and params from general line with delimiter      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           line_unparsed,     intent :: in                                    !
    !           key,               intent :: out                                   !
    !           val,               intent :: out                                   !
    !           com,               intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  19/01/2020                                            !
    !==============================================================================!
    !subroutine that parses the lines from param.pop into the key and the the value
    implicit none
    character(*),intent(in)           :: line_unparsed
    character(*),intent(out)          :: key
    character(*),intent(out)          :: val
    logical,     intent(inout)        :: com

    integer                           :: j


    if (line_unparsed(1:1).eq."!" .or. line_unparsed(1:1).eq."#") then
       com=.true.
       return
    else
       com=.false.
    end if

    do j=1,len_trim(line_unparsed)

       if (line_unparsed(j:j).eq.':' .or. line_unparsed(j:j).eq."=")then
          key=line_unparsed(1:j-1)
          val=line_unparsed(j+1:len_trim(line_unparsed))

          exit
       else if (j.eq.len_trim(line_unparsed))then
          call io_errors("Error in I/O: Error parsing line:  "//trim(line_unparsed))
       end if
    end do



    return
  end subroutine io_freeform_read


  subroutine io_errors(message)
    !==============================================================================!
    !                              I O _ E R R O R S                               !
    !==============================================================================!
    ! Subroutine handling all errors writing to the errors file                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           message,           intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  19/01/2020                                            !
    !==============================================================================!
    implicit none
    character(*)       :: message

    ! internal variable for rank processing
    character(len=20)  :: file_name

    write(file_name,'("err.",I0.4,".pop")') rank

    open(2,file=trim(file_name),RECL=8192)
    write(*,*)"Error: called io_abort"
    write(2,*) message

    call trace_stack(2,rank)
    stop
    return
  end subroutine io_errors

  function io_case( string , upper) result (new)
    !==============================================================================!
    !                                I O _ C A S E                                 !
    !==============================================================================!
    ! Low level subroutine for modifying the case of user given arguments to       !
    ! lowercase                                                                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           strin                                                              !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  19/01/2020                                            !
    !==============================================================================!
    character(len=*)           :: string 

    character(len=len(string)) :: new 

    integer                    :: i 
    integer                    :: k 
    integer                    :: length
    logical, optional          :: upper

    length = len(string)
    new    = string
    do i = 1,len(string)
       k = iachar(string(i:i))
       if ( k >= iachar('A') .and. k <= iachar('Z') ) then
          k = k + iachar('a') - iachar('A')
          new(i:i) = achar(k)
       end if
    end do

    if (present(upper))then
       if (upper) then
          do i = 1,len(string)
             k = iachar(string(i:i))
             if ( k >= iachar('a') .and. k <= iachar('z') ) then
                k = k + iachar('A') - iachar('a')
                new(i:i) = achar(k)
             end if
          end do
       end if
    end if
  end function io_case


  subroutine io_cl_parser()
    !==============================================================================!
    !                           I O _ C L _ P A R S E R                            !
    !==============================================================================!
    ! Subroutine for the handling of commandline arguments.                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  03/02/2020                                            !
    !==============================================================================!
    implicit none
    integer          ::   nargs     !Number of args
    integer          ::   arg_index !The index
    character(50)    ::   name      !The name of the argument

    logical          ::   search = .false.
    logical          ::   help   = .false.
    nargs=command_argument_count()

    if (nargs.gt.0)then
       do arg_index=1,nargs

          call get_command_argument(arg_index,name)
          select case(adjustl(trim(name)))
          case("-h","--help")
             write(*,*) trim(version)
             write(*,*) trim(info)
             read_params=.false.
             call io_list_params(.false.)
             help=.true.
             if (arg_index.eq.nargs) call io_help
          case("-s","--search")
             print*,"In seach cl parser" 
             write(*,*) trim(version)
             write(*,*) trim(info)
             write(*,*)
             read_params=.false.
             call io_list_params(.false.)
             search=.true.
             if (arg_index.eq.nargs) call io_help

          case("-v")
             write(*,*) trim(version)
             write(*,*) trim(info)
             read_params=.false.
             stop
          case("-d","--dryrun")
             current_params%dry_run=.true.
          case("-l","--list")
             write(*,*) trim(version)
             write(*,*) trim(info)
             read_params=.false.
             call io_list_params(.true.)
             stop
          case default
             if (help)then
                call io_help(name)
                help=.false.
             elseif(search)then 
                call io_search(io_case(name))
                search=.false.
                stop
             else
                write(*,*) "Unknown argument: ",adjustl(name)
                write(*,*) trim(version)
                write(*,*) trim(info)
                call io_help()
             end if
             stop
          end select
       end do
    end if
    return
  end subroutine io_cl_parser

  subroutine io_help(string)
    !==============================================================================!
    !                                I O _ H E L P                                 !
    !==============================================================================!
    ! Subroutine for printing the help information to the terminal.                !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  03/02/2020                                            !
    !==============================================================================!
    implicit none
    character(*),optional   :: string
    integer                 :: i !counter
    logical                 :: found=.false.
    if (present(string))then

       do i=1,max_keys
          if (trim(keys_array(i)).eq.io_case(trim(string))) then
             found=.true.
             write(*,*)
             write(*,12) repeat("*",70-len(keys_array(i))/2),&
                  & trim(io_case(keys_array(i),upper=.true.)),repeat("*",70-len(keys_array(i))/2)
             write(*,*) trim(keys_description(i))
             write(*,*)
             write(*,*) "Allowed Values: ",trim(keys_allowed(i))
             write(*,*) "Default:        ",trim(keys_default(i))
             write(*,*)
12           format(1x,a,1x,a,1x,a)

             exit
          end if
       end do
       if (.not.found)then
          write(*,*)
          write(*,*) "************** NO MATCHING PARAMETERS **************"
          write(*,*)
       end if
    else 
30     format(4x,A,T40,A)
       write(*,30) "-v","Print version information."
       write(*,30) "-h,--help   <keyword>","Get help and commandline options."
       write(*,30) "-s,--search <keyword>", "Search list of available parameters"
       write(*,30) "-l,--list","Get list of parameters avilable for the user."
       write(*,30) "-d,--dryrun","Run calculation to check input files"
    end if
    return
  end subroutine io_help


  subroutine io_search(string)
    !==============================================================================!
    !                              I O _ S E A R C H                               !
    !==============================================================================!
    ! Subroutine for using the command to search for availible variables that      !
    ! the user can change in a calculation                                         !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           string,            intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  29/02/2020                                            !
    !==============================================================================!
    implicit none
    character(*)     :: string
    logical          :: found
    integer          :: i,scan_res

    do i=1,max_keys
       scan_res=index(trim(keys_array(i)),trim(string))

       if (scan_res.gt.0)then
          found=.true.
100       format(1x,A,T35,A)
          write(*,100)io_case(keys_array(i),.true.),keys_description(i)
       end if
    end do
    if (.not.found)then
       write(*,*)
       write(*,*) "************** NO MATCHING PARAMETERS **************"
       write(*,*)
    end if

    return
  end subroutine io_search

  subroutine io_list_params(print_flag)
    !==============================================================================!
    !                         I O _ L I S T _ P A R A M S                          !
    !==============================================================================!
    ! Subroutine used to print the total list of variables to the terminal and     !
    ! also to allocate all of the global arrays which contain the variable data.   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           print_flag,        intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  29/02/2020                                            !
    !==============================================================================!
    implicit none
    logical  :: print_flag

    character(15) :: junk
    integer       :: i ! loops 
    ! Allocate all the arrays for the parameters
    allocate(keys_array(1:max_keys))
    allocate(keys_default(1:max_keys))
    allocate(keys_description(1:max_keys))
    allocate(keys_allowed(1:max_keys))




    ! assign the keys
    ! Begin: assign_keys
    keys_array(1)=trim(key_calc_len)
    keys_array(2)=trim(key_init_pop)
    keys_array(3)=trim(key_mean_child_age)
    keys_array(4)=trim(key_child_sd)
    keys_array(5)=trim(key_child_norm)
    keys_array(6)=trim(key_write_pop)
    keys_array(7)=trim(key_write_br)
    keys_array(8)=trim(key_write_age)
    keys_array(9)=trim(key_redistrib_freq)
    keys_array(10)=trim(key_random_seed)
    keys_array(11)=trim(key_debug)
    keys_array(12)=trim(key_life_table_year)
    keys_array(13)=trim(key_disease_spread)
    keys_array(14)=trim(key_disease_init)
    keys_array(15)=trim(key_disease_mort)
    keys_array(16)=trim(key_disease_crit)
    keys_array(17)=trim(key_disease)
    keys_array(18)=trim(key_init_demo)
    keys_array(19)=trim(key_write_demo)
    ! End: assign_keys

    ! Begin: assign_default
    write(junk,*)current_params%calc_len 
    keys_default(1)=trim(adjustl(junk))
    write(junk,*)current_params%init_pop 
    keys_default(2)=trim(adjustl(junk))
    write(junk,'(f5.2)')current_params%child_age 
    keys_default(3)=trim(adjustl(junk))
    write(junk,'(f4.2)')current_params%child_sd  
    keys_default(4)=trim(adjustl(junk))
    write(junk,'(f4.2)')current_params%child_norm 
    keys_default(5)=trim(adjustl(junk))
    write(junk,*)current_params%write_population 
    keys_default(6)=trim(adjustl(junk))
    write(junk,*)current_params%write_birth_rate 
    keys_default(7)=trim(adjustl(junk))
    write(junk,*)current_params%write_ave_age 
    keys_default(8)=trim(adjustl(junk))
    junk="CALC_LEN/10" ! Speical case 
    keys_default(9)=trim(adjustl(junk))
    junk="RANDOMISED"  ! Special case
    keys_default(10)=trim(adjustl(junk))
    write(junk,*)current_params%debuging 
    keys_default(11)=trim(adjustl(junk))
    write(junk,*)current_params%life_table_year
    keys_default(12)=trim(adjustl(junk))
    write(junk,'(f4.2)')current_params%disease_spread
    keys_default(13)=trim(adjustl(junk))
    write(junk,'(f4.2)')current_params%disease_init
    keys_default(14)=trim(adjustl(junk))
    write(junk,'(f4.2)')current_params%disease_mort
    keys_default(15)=trim(adjustl(junk))
    write(junk,'(f4.2)')current_params%disease_crit
    keys_default(16)=trim(adjustl(junk))
    write(junk,*)current_params%disease
    keys_default(17)=trim(adjustl(junk))
    write(junk,*)current_params%init_demo
    keys_default(18)=trim(adjustl(junk))
    write(junk,*)current_params%write_demo
    keys_default(19)=trim(adjustl(junk))
    ! End: assign_default

    ! Begin: assign_description
    keys_description(1)="Length of the calculation in years"
    keys_description(2)="Initial total population, combined men and women"
    keys_description(3)="Mean age for a woman to have a child, modelled as a Gaussian"
    keys_description(4)="Standard deviation for the birth probability"
    keys_description(5)="Number of children a woman will have on average in her lifetime"
    keys_description(6)="Write the population data to a file 'population.pop'"
    keys_description(7)="Write the birth rate data to file 'birth_rate.pop'"
    keys_description(8)="Write the avergae age data to file 'ave_age.pop'"
    keys_description(9)="Interval for parallel redistribution of population data and reporting, in years"
    keys_description(10)="Provide a random seed for reproducability"
    keys_description(11)="Toggle code profilling"
    keys_description(12)="Select year for life US life table"
    keys_description(13)="Maximum probablility of the catching the disease, varies as a function of population"
    keys_description(14)="Initial number of people with the disease"
    keys_description(15)="Increase in mortality rate for a person with the disease"
    keys_description(16)="Deseased population fraction when contagion is highest"
    keys_description(17)="Toggle for running a calculation with disease"
    keys_description(18)="Initialise the population from the final demographics of a previous calculation"
    keys_description(19)="Write demographic data to file 'demographics.pop'"
    ! End: assign_description
    
    ! Begin: assign_allowed
    keys_allowed(1)= "(any integer) > 0"
    keys_allowed(2)= "(any integer) > 0"
    keys_allowed(3)= "(any integer) > 0"
    keys_allowed(4)= "(any integer) > 0"
    keys_allowed(5)= "(any real) > 0"
    keys_allowed(6)= "Boolean"
    keys_allowed(7)= "Boolean"
    keys_allowed(8)= "Boolean"
    keys_allowed(9)= "(any integer) > 0"
    keys_allowed(10)= "(any integer)"
    keys_allowed(11)= "Boolean"
    keys_allowed(12)= "2011 < (any integer) < 2018"
    keys_allowed(13)= "0.0 < (any real) < 1.0"
    keys_allowed(14)= "0.0 < (any real) < 1.0"
    keys_allowed(15)= "(any real) > 0.0"
    keys_allowed(16)= "0.0 < (any real) < 1.0"
    keys_allowed(17)= "Boolean"
    keys_allowed(18)= "DEFAULT,CONTINUATION"
    keys_allowed(19)= "Boolean"
    ! End: assign_allowed
    
    ! do the loop for printing stuff

    if (print_flag)then 
100    format(1x,A,T35,A)
       write(*,*)
       do i=1,max_keys
          write(*,100) io_case(trim(keys_array(i)),.true.),trim(keys_description(i))

       end do
    end if


    return
  end subroutine io_list_params


  subroutine io_header()
    !==============================================================================!
    !                          I O _  H E A D E R                                  !
    !==============================================================================!
    ! Subroutine used to write the io_header of the main Mandelbrot output file    !
    ! "out.mand".                                                                  !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    implicit none
    integer::file
    integer :: maj_mpi,min_mpi,min_char
    character(len=max_version_length) :: mpi_c_version
    character(len=3) :: MPI_version_num
    character(len=100):: compile_version,cpuinfo
    character(len=5) :: opt
    
    call trace_entry("io_header")



    if (comms_arch.eq."MPI")then
       call COMMS_LIBRARY_VERSION(mpi_c_version)
       call COMMS_VERSION(min_mpi,maj_mpi)

       write(mpi_version_num,97)min_mpi,maj_mpi
97     format(i1,"."i1)
       min_char=scan(mpi_c_version,".")
       !print*, mpi_c_version,mpi_version_num
    end if

#ifdef __INTEL_COMPILER
#define compiler "Intel Compiler"

#endif
#ifdef __GFORTRAN__
#define compiler "GNU Fortran"
    !#define compile_version __VERSION__
#endif


    
#define opt opt_strat

    

    compile_version=compiler_version()
    if (compiler.eq."Intel Compiler")then
       compile_version=compiler_version()

       compile_version=trim(compile_version(87:97))
    end if


    write(stdout,*) "+==================================================================================+"
    write(stdout,*) '|        888b     d888  .d8888b.          8888888b.   .d88888b.  8888888b.         |'
    write(stdout,*) '|        8888b   d8888 d88P  Y88b         888   Y88b d88P" "Y88b 888   Y88b        |'
    write(stdout,*) '|        88888b.d88888 888    888         888    888 888     888 888    888        |'
    write(stdout,*) '|        888Y88888P888 888                888   d88P 888     888 888   d88P        |'
    write(stdout,*) '|        888 Y888P 888 888                8888888P"  888     888 8888888P"         |'
    write(stdout,*) '|        888  Y8P  888 888    888         888        888     888 888               |'
    write(stdout,*) '|        888   "   888 Y88b  d88P         888        Y88b. .d88P 888               |'
    write(stdout,*) '|        888       888  "Y8888P" 88888888 888         "Y88888P"  888               |'
    write(stdout,*) '+----------------------------------------------------------------------------------+'
    write(stdout,*) "|       ",trim(version),"                                                  |"
    write(stdout,*) "+==================================================================================+"
    write(stdout,*)
    write(stdout,*) "Compiled with ",compiler," ",Trim(compile_version), " on ", __DATE__, " at ",__TIME__
    write(stdout,*) "Communications architechture: ",comms_arch
    if (comms_arch.eq."MPI")then
       write(stdout,*) "MPI Version: ",mpi_c_version(1:min_char+1)
    end if
    write(stdout,*) "Optimisation Strategy: ",opt
    write(stdout,*)
    call trace_exit("io_header")
  end subroutine io_header


  subroutine io_dryrun()
    !==============================================================================!
    !                              I O _ D R Y R U N                               !
    !==============================================================================!
    ! Subroutine for handlind the dryrun command which allows for parameter        !
    ! checking                                                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  23/02/2020                                            !
    !==============================================================================!
    implicit none
    call trace_entry("io_dryrun")
    if (on_root_node)then
       write(stdout,*) " " 
       write(stdout,'(23x,A)') "****************************************"
       write(stdout,'(23x,A)') "*                                      *"
       write(stdout,'(23x,A)') "*         Dryrun complete....          *"
       write(stdout,'(23x,A)') "*          No errors found             *"
       write(stdout,'(23x,A)') "*                                      *"
       write(stdout,'(23x,A)') "****************************************"
    end if

    call trace_exit("io_dryrun")
    call trace_exit("mc_pop")
    call COMMS_FINALISE()
    call trace_finalise(current_params%debuging,rank)
    stop

  end subroutine io_dryrun



  subroutine io_write_params()
    !==============================================================================!
    !                        I O _ W R I T E _ P A R A M S                         !
    !==============================================================================!
    ! Subroutine that writes the input parameters to the stdout out.pop            !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  23/02/2020                                            !
    !==============================================================================!
    implicit none
    character(50)   :: sec_title
    integer         :: width=84,length
    ! Stuff for getting run time
    character(len=3),dimension(12)  :: months
    integer                         :: d_t(8)    
    character*10                    :: b(3)


    call trace_entry("io_write_params")

    call date_and_time(b(1), b(2), b(3), d_t)
    months=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

    write(stdout,*)"+"//repeat("-",(width-15)/2)//" RUN STARTED "//repeat("-",(width-15)/2)//"+"
    write(stdout,1000) d_t(5),d_t(6),d_t(7),trim(months(d_t(2))),d_t(3),d_t(1)
    write(stdout,*)"+"//repeat("-",width-3)//"+"
    write(stdout,*) " "
1000 FORMAT(1x,"|",30x,i2.2,":",i2.2,":",i2.2,",",1x,A,1x,i2.2,1x,i4,30x,"|")
10  format(1x,A,T44,':',5x,I12,1x,A)    !integer
11  format(1x,A,T44,":",5x,f12.2,1x,A)  !real
12  format(1x,A,T44,":",5x,L12,1x,A)    !logical
13  format(1x,A,T44,":",5x,A12,1x,A)    !character
14  format(1x,A,T44,":",5x,ES12.2,1x,A)  !Science 




    ! Do some printing about the parameters file

    if (file_exists)then
       write(stdout,*)"Parameters file found, using custom parameters"
    else
       write(stdout,*)"Parameters file not found, using defaults"
    end if

    sec_title="Calculation Parameters"
    length=len(trim(sec_title))
    write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)
    write(stdout,14) "Initial Population", real(current_params%init_pop)
    write(stdout,14)"Calculation Length",real(current_params%calc_len),"years"
    if (current_params%init_demo)then

       write(stdout,13)"Demographics Initialisation","Previous Run"
    else
       write(stdout,13)"Demographics Initialisation","Default"
    end if
    sec_title="Birth Parameters"
    length=len(trim(sec_title))
    write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)

    write(stdout,11)"Average Age",current_params%child_age
    write(stdout,11)"Standard Deviation",current_params%child_sd,"years"
    write(stdout,11)"Children per Woman",current_params%child_norm 

    sec_title="Death Parameters"
    length=len(trim(sec_title))
    write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)

    write(stdout,10)"Life Table year",current_params%life_table_year


    sec_title="Disease Parameters"
    length=len(trim(sec_title))
    write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)

    write(stdout,12)"Disease Present",current_params%disease
    if (current_params%disease)then
       write(stdout,11)"Contagiousness",current_params%disease_spread*100_dp,"%"
       write(stdout,11)"Initial Infected Population",current_params%disease_init*100_dp,"%"
       write(stdout,11)"Disease Severity",current_params%disease_mort*100_dp,"%"
       write(stdout,11)"Disease Critical Mass",current_params%Disease_crit*100_dp,"% population diseased"
    end if


    sec_title="I/O Parameters"
    length=len(trim(sec_title))
    write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)

    write(stdout,12)"Write Population Data",current_params%write_population
    write(stdout,12)"Write Age Data" ,current_params%write_ave_age
    write(stdout,12)"Write Birth Rate Data",current_params%write_birth_rate
    write(stdout,12)"Write Demographics Data",current_params%write_demo

    sec_title="General Parameters"
    length=len(trim(sec_title))
    write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)
    write(stdout,10) "Redistribition Frequency",current_params%redistrib_freq, "years"
    write(stdout,12) "Profilling",current_params%debuging
    write(stdout,10) "Random Seed",current_params%random_seed
    if(comms_arch.eq."MPI")then
       sec_title="Parallelisation Parameters"
       length=len(trim(sec_title))
       write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)
       write(stdout,10)"Number of Cores",nprocs
    end if


    write(stdout,*) repeat("-",width-1)

    call trace_exit("io_write_params")
    return
  end subroutine io_write_params




  subroutine io_write_results(res,survived)
    !==============================================================================!
    !                       I O _ W R I T E _ R E S U L T S                        !
    !==============================================================================!
    ! Subroutine used to write out the results calculated in the calculation       !
    ! which are stored in the derived data type 'results'.                         !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           res,               intent :: in                                    !
    !           survived,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  29/02/2020                                            !
    !==============================================================================!

    implicit none
    type(results)          :: res
    logical               :: survived

    character(50)   :: sec_title
    integer         :: width=84,length
    ! Stuff for getting run time
    character(len=3),dimension(12)  :: months
    integer                         :: d_t(8)    
    character*10                    :: b(3)
    call trace_entry("io_write_results")

10  format(1x,A,T44,':',5x,I9,1x,A)    !integer
11  format(1x,A,T44,":",5x,f9.2,1x,A)  !real
12  format(1x,A,T44,":",5x,L9,1x,A)    !logical

    if(survived)then 
       sec_title="Population Properties"
       length=len(trim(sec_title))
       write(stdout,*) ""
       write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-1)

       write(stdout,11) "Average Age",res%ave_age,"years"
       write(stdout,11) "Average Life Expectancy",res%life_expectancy,"years"
       write(stdout,11) "Average Teen Pregnacy Rate",res%teen_preg,"per 1000"
       write(stdout,11) "Average Infant Mortality Rate",res%infant_mort,"per 1000"
       write(stdout,11) "Average Birth Rate",res%birth_rate,"per 1000"
       write(stdout,11) "Percentage Men",res%men_pc,"%"
       write(stdout,11) "Percentage Women",100_dp-res%men_pc,"%"
       sec_title="Demographics"
       length=len(trim(sec_title))
       write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)


       write(stdout,11) "Age <5",res%lt_5,"%"
       write(stdout,11) "Age 10-19",res%i10_20,"%"
       write(stdout,11)	"Age 20-29",res%i20_30,"%"
       write(stdout,11)	"Age 30-39",res%i30_40,"%"
       write(stdout,11)	"Age 40-49",res%i40_50,"%"
       write(stdout,11)	"Age 50-64",res%i50_65,"%"
       write(stdout,11)	"Age 65+",res%i65_plus,"%"
       write(stdout,11)	"Age 85+",res%i85_plus,"%"
       write(stdout,*) repeat("-",width-1)
    end if
    write(stdout,*)


    call date_and_time(b(1), b(2), b(3), d_t)
    months=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

    write(stdout,*)"+"//repeat("-",(width-15)/2)//" RUN FINISHED "//repeat("-",(width-17)/2)//"+"
    write(stdout,1000) d_t(5),d_t(6),d_t(7),trim(months(d_t(2))),d_t(3),d_t(1)
    write(stdout,*)"+"//repeat("-",width-3)//"+"

1000 FORMAT(1x,"|",30x,i2.2,":",i2.2,":",i2.2,",",1x,A,1x,i2.2,1x,i4,30x,"|")

    call trace_exit("io_write_results")
    return 
  end subroutine io_write_results


  subroutine io_survival(survival)
    !==============================================================================!
    !                            I O _ S U R V I V A L                             !
    !==============================================================================!
    ! Subroutine for writing to the stdout out.pop, reporting the survival of      !
    ! the species.                                                                 !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           survival,          intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  23/02/2020                                            !
    !==============================================================================!
    implicit none
    logical,intent(inout) :: survival

    character(len=100)    :: text
    integer               :: width=84,span=50
    call trace_entry("io_survival")
45  format(17x,A)
    write(stdout,*)
    if(survival) then 
       write(stdout,45) "************************************************"
       write(stdout,45) "*                                              *"
       write(stdout,45) "*            Species Survived!! :)             *"
       write(stdout,45) "*                                              *"
       write(stdout,45) "************************************************"

    else
       write(stdout,45) "************************************************"
       write(stdout,45) "*                                              *"
       write(stdout,45) "*          Species Went Extinct!! :(           *"
       write(stdout,45) "*                                              *"
       write(stdout,45) "************************************************"

    end if
    call trace_exit("io_survival")
    return

  end subroutine io_survival

  function io_present(key) result(is_present)
    !==============================================================================!
    !                             I O _ P R E S E N T                              !
    !==============================================================================!
    ! Function used to determine if there is a keyword present in the input file   !
    !                                                                              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           key,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           is_present                                                         !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  23/02/2020                                            !
    !==============================================================================!
    implicit none
    logical      :: is_present
    character(*) :: key
    call trace_entry("io_present")
    if (any(present_array.eq.key))then
       is_present=.TRUE.
    else
       is_present=.FALSE.
    end if
    call trace_exit("io_present")
  end function io_present

  subroutine io_flush(unit)
    !==============================================================================!
    !                               I O _ F L U S H                                !
    !==============================================================================!
    ! Subroutine wrapper for the intrinsic function that forces the system to      !
    ! clear the cache so that I/O can be written to file. Mainly used with the     !
    ! GNU fortran compiler, programs compiled with that compiler tend to hold on   !
    ! to the cache much longer.                                                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           unit,              intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  29/02/2020                                            !
    !==============================================================================!
    implicit none
    integer  :: unit
    !call trace_entry("io_flush")
    call flush(unit)
    !call trace_exit("io_flush")
    return 
  end subroutine io_flush





end module io

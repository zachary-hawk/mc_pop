!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
module io

  !Impose strong typing
  use trace, only : trace_entry, trace_exit,trace_stack,trace_finalise
  use comms, only : rank,nprocs,on_root_node,comms_abort,max_version_length,comms_arch,COMMS_VERSION &
       & ,COMMS_LIBRARY_VERSION,COMMS_FINALISE,comms_barrier
  use iso_fortran_env, only :real64,compiler_version
  implicit none


  logical,           public                :: no_param
  character(len=128),public                :: version  = "MC_POP v.1.0, Z. Hawkhead"
  character(len=128),public                :: info     = "Parallel code for Monte Carlo Population simulation"
  integer,           public,parameter      :: stdout = 984 
  integer,           public,parameter      :: dp = real64
  real,              public,parameter      :: pi=3.1415926535
  integer,           private,parameter     :: life_file = 189
  logical,           public                :: file_exists
  character(100),dimension(:),allocatable  :: present_array

  integer                                  :: max_params=1

  type  parameters
     !Calculation parameters
     integer          :: init_pop          = 100
     integer          :: child_age         = 23
     integer          :: calc_len          = 100
     integer          :: life_table_year   = 2017
     !Child prob params
     real(dp)         :: child_sd          = 5.0_dp
     real(dp)         :: child_norm        = 2.8_dp!0.4_dp


     !Some extra functionality
     logical          :: dry_run           = .false.
     logical          :: debug             = .false.
     integer          :: redistrib_freq    = 10

     !I/O parameters
     logical          :: write_population  = .true.
     logical          :: write_ave_age     = .false.
     logical          :: write_birth_rate  = .false.


  end type parameters


  character(len=30),parameter,public :: key_init_pop         = "initial_population"
  character(len=30),parameter,public :: key_mean_child_age   = "child_age"
  character(len=30),parameter,public :: key_calc_len         = "duration"
  character(len=30),parameter,public :: key_life_table_year  = "life_table_year"

  character(len=30),parameter,public :: key_child_norm       = "child_per_woman"
  character(len=30),parameter,public :: key_child_sd         = "birth_std"
  character(len=30),parameter,public :: key_debug            = "debug"
  character(len=30),parameter,public :: key_redistrib_freq   = "redistrib_freq"

  character(len=30),parameter,public :: key_write_pop        = "write_pop"
  character(len=30),parameter,public :: key_write_br         = "write_birth_rate"
  character(len=30),parameter,public :: key_write_age        = "write_ave_age"

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
  end type results


  type(parameters),public,save             :: current_params

  type(life_table),public,save             :: current_lifetable



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
    call io_read_param(current_params)


    ! This is where intialise life tables, there will be a datatype in this file
    call io_read_life(current_lifetable)


    ! Make some sensible defaults
    if (.not.io_present(key_redistrib_freq))then
       current_params%redistrib_freq=current_params%calc_len/10
    end if


    ! Check there are enough people for the number of cores
    if (current_params%init_pop.gt.0 .and.&
         & current_params%init_pop.lt.nprocs) &
         & call io_errors("Error in io_initialise: Too few people, cannot distribute accross requested proccesses")
    ! Print warning if numbers of people too low on each process
    if(current_params%init_pop/nprocs.lt.10)&
         & write(*,*) "Warning: Number of people per proccess is low, consider increasing"

    ! Open up the main file for the output
    open(stdout,file="out.pop",RECL=8192,form="FORMATTED")
    !do some stuff about dry runs and commandline parsers here 

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

          select case(key)
          case(key_init_pop)
             read(param,*,iostat=stat) dummy_params%init_pop
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_mean_child_age) 
             read(param,*,iostat=stat) dummy_params%child_age
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_calc_len ) 
             read(param,*,iostat=stat) dummy_params%calc_len
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
             read(param,*,iostat=stat) dummy_params%debug
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


  subroutine io_read_life(dummy_life)
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
    integer                         :: wstat,stat,counter=0
    character(20)                   :: name,first,second,life_str
    real(dp)                        :: prob
    logical                         :: file_exists

    call trace_entry("io_read_life")
#ifdef life_dir
#define life_str life_dir
#endif 

    write(name,'("life_table_",I0,".csv")',iostat=stat) current_params%life_table_year
    if (stat.ne.0) call io_errors("Error in I/O: Internal variable write error")
    name=trim(name)

    !check if the User has used a correct file, if not, cause error

    inquire(file=trim(life_str)//"/life_tables/"//name,exist=file_exists)

    if (.not.file_exists) then

       call io_errors("Error in I/O: No life table found: "//name)
    end if

    open(unit=life_file,file=trim(life_str)//"/life_tables/"//name)

    do while (trim(first) .ne. "0-1")
       read(life_file,*)first,second
    end do

    do while (stat.eq.0)
       read(second,'(f7.6)',iostat=wstat) prob

       if (wstat.ne.0) call io_errors("Error in I/O: Internal variable write error")
       dummy_life%life_data(counter)=prob
       read(life_file,*,iostat=stat) first,second

       counter=counter+1
    end do

    dummy_life%year=current_params%life_table_year

    call trace_exit("io_read_life")

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

  function io_case( string ) result (new)
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
    integer::length

    length = len(string)
    new    = string
    do i = 1,len(string)
       k = iachar(string(i:i))
       if ( k >= iachar('A') .and. k <= iachar('Z') ) then
          k = k + iachar('a') - iachar('A')
          new(i:i) = achar(k)
       end if
    end do

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
    character(10)    ::   name      !The name of the argument

    nargs=command_argument_count()

    if (nargs.gt.0)then
       do arg_index=1,nargs
          call get_command_argument(arg_index,name)
          select case(adjustl(trim(name)))
          case('-h','--help')
             write(*,*) trim(version)
             write(*,*) trim(info)
             call io_help()
             stop
          case("-v")
             write(*,*) trim(version)
             write(*,*) trim(info)
             stop
          case("-d","--dryrun")
             current_params%dry_run=.true.
          case("-l","--list")
             write(*,*) trim(version)
             write(*,*) trim(info)

             call io_list_params()
             stop
          case default
             write(*,*) "Unknown argument: ",adjustl(name)
             write(*,*) trim(version)
             write(*,*) trim(info)
             call io_help()
             stop
          end select
       end do
    end if
    return
  end subroutine io_cl_parser

  subroutine io_help()
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
30  format(4x,A12,":",1x,A)
    write(*,30) adjustr("-v"),"Print version information."
    write(*,30) adjustr("-h, --help"),"Get help and commandline options."
    write(*,30) adjustr("-l,--list"),"Get list of options avilible for the user."
    return
  end subroutine io_help


  subroutine io_list_params()
    !==============================================================================!
    !                         I O _ L I S T _ P A R A M S                          !
    !==============================================================================!
    ! Subroutine for printing the list of availible parameters and their           !
    ! defaults.                                                                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  03/02/2020                                            !
    !==============================================================================!
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
#define version __VERSION__
#endif


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
    write(stdout,*) "Communications architechture: ",trim(comms_arch)
    if (comms_arch.eq."MPI")then
       write(stdout,*) "MPI Version: ",mpi_c_version(1:min_char+1)
    end if
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
    call trace_finalise(current_params%debug,rank)
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
10  format(1x,A,T44,':',5x,I9,1x,A)    !integer
11  format(1x,A,T44,":",5x,f9.2,1x,A)  !real
12  format(1x,A,T44,":",5x,L9,1x,A)    !logical





    ! Do some printing about the parameters file

    if (file_exists)then
       write(stdout,*)"Parameters file found, using custom parameters"
    else
       write(stdout,*)"Parameters file not found, using defaults"
    end if

    sec_title="Calculation Parameters"
    length=len(trim(sec_title))
    write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)

    write(stdout,10) "Initial Population", current_params%init_pop
    write(stdout,10)"Calculation Length",current_params%calc_len,"years"

    sec_title="Birth Parameters"
    length=len(trim(sec_title))
    write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)

    write(stdout,10)"Average Age",current_params%child_age
    write(stdout,11)"Standard Deviation",current_params%child_sd,"years"
    write(stdout,11)"Children per Woman",current_params%child_norm 

    sec_title="Death Parameters"
    length=len(trim(sec_title))
    write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)

    write(stdout,10)"Life Table year",current_params%life_table_year


    sec_title="I/O Parameters"
    length=len(trim(sec_title))
    write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)

    write(stdout,12)"Write Population Data",current_params%write_population
    write(stdout,12)"Write Age Data" ,current_params%write_ave_age
    write(stdout,12)"Write Birth Rate Data",current_params%write_birth_rate

    sec_title="General Parameters"
    length=len(trim(sec_title))
    write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)
    write(stdout,10) "Redistribition Frequency",current_params%redistrib_freq, "years"
    write(stdout,12) "Profilling",current_params%debug

    sec_title="Parallelisation Parameters"
    length=len(trim(sec_title))
    write(stdout,*)repeat("-",(width-length)/2-2)//"  "//trim(sec_title)//" "//repeat("-",(width-length)/2-2)
    write(stdout,10)"Number of Cores",nprocs



    write(stdout,*) repeat("-",width-1)

    call trace_exit("io_write_params")
    return
  end subroutine io_write_params




  subroutine io_write_results(res,survived)

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
       write(stdout,11) "Averge Teen Pregnacy Rate",res%teen_preg,"per 1000"
       write(stdout,11) "Average Infant Mortality Rate",res%infant_mort,"per 1000"
       write(stdout,11) "Average Birth Rate",res%birth_rate,"per 1000"

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


end module io

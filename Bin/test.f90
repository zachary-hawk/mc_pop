program test
  implicit none
  integer :: i
  character(len=20) :: first,second,name
  integer           :: year

  type a_type
     integer, dimension(0:10) :: data
  end type a_type

  type(a_type) :: thing

  thing%data(0)=10
  print*,thing

  
end program test

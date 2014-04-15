module fibo_rekursiv
  implicit none 

  private
  public :: fib

contains

  recursive function fib(z) result(zahl)
    integer :: z, zahl
    if ( z == 1 .or. z == 2 ) then
       zahl = 1
    else
       zahl = fib(z-1) + fib(z-2)
   end if 
   return
  end function fib
end module fibo_rekursiv



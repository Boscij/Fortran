module fibo_it
!program fibo_it
  implicit none

  private
  public :: fibo_iterativ

contains
  
  function fibo_iterativ(n)
  integer :: a, fibo_iterativ, n, c, k
  a = 1
  fibo_iterativ = 1
  if ( n == 0 ) then
     fibo_iterativ = 0
  end if
  if ( n > 2 ) then
     do k = 1, n-2
        c = fibo_iterativ
        fibo_iterativ = a + fibo_iterativ
        a = c
     end do
  end if
end function fibo_iterativ
end module fibo_it
!end program fibo_it



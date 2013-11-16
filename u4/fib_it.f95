program fibo_iterativ
  implicit none 
  integer :: a, b, n, c, k
  a = 1
  b = 1
  read (*,*) n
  if ( n == 0 ) then
     write (*,*) 'Fibonacci-Zahl', 1
  else if ( n == 1 .or. n == 2 ) then
     write (*,*) 'Fibonacci-Zahl', b
  end if
  if ( n > 1 ) then
     do k = 1, n-2
        c = b
        b = a + b
        a = c
     end do
     write (*,*) 'Fibonacci-Zahl', b
  end if
end program fibo_iterativ

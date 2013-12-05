program fibonacci
  use fibo_it
  implicit none
  integer :: zahl, t1, t2, clock_rate, clock_max
  real :: z1, z2
  zahl = 0
  do while (zahl > -1)

     write (*,*) 'Welcher Term der Fibonaccifolge soll ausgegeben werd&
          &en'
     read (*,*) zahl

     call system_clock ( t1, clock_rate, clock_max )
     call cpu_time (z1)
     write (*,*) 'Fibonacci_Zahl mittels iteration:',&
          & fibo_iterativ(zahl)
     call system_clock ( t2, clock_rate, clock_max )
     call cpu_time (z2)
     write (*,*) 'Zeit die für die Rechnung', real (t2 -t1 ) / real&
          & (clock_rate), 'sekunden'
     write (*,*) 'Zeit die für die Rechnung', real (z2 -z1 ) / real&
          & (clock_rate), 'cpu'

     call cpu_time (z1)
     call system_clock ( t1, clock_rate, clock_max )
     write (*,*) 'Fibonacci-Zahl mittels rekursion:',&
          & fibo_rekursive(zahl)
     call system_clock ( t2, clock_rate, clock_max )
     call cpu_time (z2)
     write (*,*) 'Zeit die für die Rechnung', real (t2 -t1 ) / real&
          & (clock_rate), 'sekunden'
     write (*,*) 'Zeit die für die Rechnung', real (z2 -z1 ) / real&
          & (clock_rate), 'cpu'

  end do
  
contains

  recursive function fibo_rekursive(z) result(res)
    integer :: z, res
    if ( z == 1 .or. z == 2 ) then
       res = 1
    else if (z == 0) then
       res = 0
    else
       res = fibo_rekursive(z-1) + fibo_rekursive(z-2)
   end if
   return
 end function fibo_rekursive
end program fibonacci




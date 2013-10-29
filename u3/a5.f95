program zyklus
  implicit none 
  real(kind=8) :: x, x_min, x_max, summe, x_mitt
  integer :: n, i = 2

  do
     write (*,*) 'bitte Anzahl der einzulesenden Zahlen(natuerliche Zahlen ohne 0) eingeben'
     read (*,*) n

     if ( n >= 1 ) exit
  end do

  write (*,*) 'x_1 eingeben'
  read (*,*) x

  x_min = x
  x_max = x
  summe = x
  x_mitt = x

  do while (n > 1)
     
     write (*,*) 'x_',i,' eingeben'
     read (*,*) x

     summe = summe + x
     if ( x < x_min ) x_min = x
     if ( x > x_max ) x_max = x
     x_mitt = summe / i
     i = i + 1
     n = n - 1
     
  end do
  write (*,*) 'x_min:',x_min,'x_max:',x_max,'Summe:',summe,'Mittelwert:',x_mitt
    
end program zyklus


program prime
implicit none

  integer :: n , flag , k , p , f

  write (*,*) 'Eingabe der ganzen Zahl, bis zu welcher geprueft werden soll'
  read (*,*) n

  p = 1


  do while (p /= n)
    p = p + 1
    k = 1
    f = 1
    do while (k /= 0 .and. (f+1) /= p)
      f = f + 1
      k = mod(p,f)
    end do
    if ( k /= 0 ) then
    write (*,*) p , 'ist eine Primzahl'
    end if 
  end do
     
end program prime



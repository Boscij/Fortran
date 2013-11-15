program mirp
  implicit none
  integer, parameter :: full = selected_int_kind(9)
  integer (kind=full) :: ober, unter, zaehl1, zaehl2, og, f, flag, flag2
  integer :: a, k, AZ, z, g, neu 		!int fuer umkehrung
  do
     write (*,*) 'Bitte untere und obere Grenze nacheinander in ganzen&
          & Zahlen im Bereich [2, 10^9] eingeben'
     read (*,*) unter
     read (*,*) ober
     if ( (unter > 1) .and. (unter <= ober) .and. (ober <= 10**9)) exit
  end do
  if (unter == 2) then
     write (*,*) unter, 'ist Primzahl'
     if ( ober > 2 ) then
        write (*,*) unter +1, 'ist Primzahl'
     end if
     unter = 5
  end if
  if (unter == 3) then
     write (*,*) 'ist eine Primzahl'
     unter = 5
  end if
  if (mod(unter,2) == 0) unter = unter +1
  do zaehl1 = unter,ober,2
     f = int(ceiling(sqrt(real(zaehl1))))
     flag = 0
     flag2 = 0
     do zaehl2 = 3,f,2
        if ( mod(zaehl1,zaehl2) == 0 ) then
           flag = 1
        end if
     end do
     if ( flag == 0 ) then      ! Umkehrfkt
        neu = 0
        a = zaehl1
        do k = 0,10**9		! Ziffernanzahl
           if ( a / 10**k == 0 ) then
              AZ = k
              exit
           end if
        end do
        do z = 1,AZ
           g = mod(a,10)
           a = a/10
           neu = neu + g*10**(AZ-z)
        end do
        f = int(ceiling(sqrt(real(neu))))
        do zaehl2 = 3,f,2
           if ( mod(neu,2) == 0) flag = 3
           if ( mod(neu,zaehl2) == 0 ) then
              flag = 3
           end if
        end do
        if (neu == zaehl1) flag2 = 1
     end if
     if ((flag == 3) .or. ((flag == 0) .and. (flag2 == 1))) then
        write (*,*) zaehl1, 'ist Primzahl'
     else if (flag == 0) then
        write (*,*) zaehl1, 'ist Mirpzahl'
     end if
  end do
end program mirp

!10^6   Primzahl 1000003
!       Mirpzahl 1000033

!10^7,  Primzahl 10000019
!	Mirpzahl 10000169

!10^8   Primzahl 100000037
!	Mirpzahl 100000007

!10^9   Primzahl 1000000037
! 	Mirpzahl 1000000007

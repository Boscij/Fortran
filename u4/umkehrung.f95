program umkehrung
  implicit none
  integer :: zahl, Nee
  write (*,*) 'Zahl eingeben'
  read (*,*) zahl
Nee = neu(zahl)
write (*,*) Nee
contains

  function neu(a)
  integer :: a, k, AZ, z, g, neu
  neu = 0

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
end function neu

end program umkehrung

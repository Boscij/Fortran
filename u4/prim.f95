module primz
  implicit none 
!  integer :: Zahl
!  write (*,*) 'Zahl > 3 eingeben'
!  read (*,*) Zahl
!write (*,*) prim(Zahl)
private
public :: prim, umkehrzahl

contains

    function prim(z)
    integer :: z, f, zaehl2
    logical :: prim
    prim = .not.prim
    if (mod(z,2) == 0) prim = (1 == 0)
        f = int(ceiling(sqrt(real(z))))
        do zaehl2 = 3,f,2
           if ( mod(z,zaehl2) == 0 ) then
              prim = 1 == 0
           end if
        end do
  end function prim

...
end module primz

use primz

f95 prim.f95 a8.f95 -o a8

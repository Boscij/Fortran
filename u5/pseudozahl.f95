module pseudo_z
  implicit none

  private :: zufall
  public :: zufall_init, wuerfel
  integer, parameter :: pr = Kind(0.0D0)
  real(kind=pr) :: zahl
  
contains
  subroutine zufall_init
    do
       write (*,*) 'Bitte Kern der Zufallszahl eingeben'
       read (*,*) zahl
       if (zahl >= 0 .and. zahl < 1) exit
    end do
  end subroutine zufall_init

  function zufall()
    real(kind=pr) :: zufall
    real(kind=pr) :: a, b
    a=12054.67293087436
    b=0.3183287916948132
    zufall=abs(a*zahl+b - int(a*zahl+b))
    zahl=zufall
  end function zufall

  function wuerfel(I_min,I_max)
    integer :: I_min,I_max
    integer :: wuerfel
    wuerfel = int(floor(I_min+(I_max-I_min + 1)*zufall()))
  end function wuerfel
  
end module pseudo_z




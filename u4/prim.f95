program primz
  implicit none 
  integer :: Zahl
  write (*,*) 'Zahl > 3 eingeben'
  read (*,*) Zahl
write (*,*) prim(Zahl)

contains

    function prim(z)
    integer :: z, f, zaehl2
    logical :: prim
    prim = (1 == 1)
    if (mod(z,2) == 0) prim = (1 == 0)
        f = int(ceiling(sqrt(real(z))))
        do zaehl2 = 3,f,2
           if ( mod(z,zaehl2) == 0 ) then
              prim = (1 == 0)
           end if
        end do
  end function prim
end program primz

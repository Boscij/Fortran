program a12
  use pseudo_z
  implicit none
  integer :: z,i,seitenzahl,ex,zaehler,k,j
  real :: mittelwert
  real, dimension(4):: m
  call zufall_init
  write (*,*) 'Wie oft soll das Experiment wiederholt werden'
  read (*,*) k
  do j=1,k
  do
     seitenzahl=wuerfel(1,20)
    if (seitenzahl.ne.1) exit
  end do
  do ex=1,4
     zaehler=0
     do i=1,10**(2+ex)
        z=wuerfel(1,seitenzahl)
        zaehler=zaehler+z
     end do
     mittelwert=zaehler/10.0**(2+ex)
     write (*,*) mittelwert
     m(ex)=nint(mittelwert)
  end do
  write (*,*) 'Seitenzahl',seitenzahl,'M1',nint(m(1)),'M2',nint(m(2)),'M3',nint(m(3)),'M4',nint(m(4))
end do
end program a12

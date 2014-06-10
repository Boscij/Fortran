program damenproblem
  use schach
  implicit none
  integer      :: n,i,j,loesungen
  integer, dimension(:,:), allocatable :: fie
  integer, dimension(:), allocatable   :: vec
  do
     write(*,*) "Dimension eingeben"
     read(*,*) n
  if (n <= 0) exit
  allocate(vec(n))
  allocate(fie(n,n))
  fie = 0
  vec = 0
  loesungen = 0
 call next(1,vec,fie,loesungen)
 write(*,*) loesungen
 deallocate(vec)
 deallocate(fie)
end do

 
end program damenproblem

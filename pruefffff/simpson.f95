program int_simpson
  use simpson_m
  implicit none
  integer :: n_0,k,i
  real(kind=pr) :: a,b,epsilon,epsilon2,sim,ff,fg
  sim=0
  write (*,*) "sachen eingeben"
  ! read (*,*) a,b,n_0,k,epsilon
  a=1
  b=4
  n_0=1
  k=1
  epsilon=0.01
! ff=simpson(n_0,a,b)
  write (*,*) ff,fg
  do i=0,10**4
!     epsilon2=abs(sim-simpson(n_0+i*k,a,b))/(abs(simpson(n_0+i*k,a,b)))
     epsilon2=abs(sim-simpson(standard_f,n_0+i*k,a,b))/(abs(simpson(standard_f,n_0+i*k,a,b)))
     write (*,*) "ganze Zeug", epsilon2, sim
     if (epsilon2<epsilon) exit
!     sim=simpson(n_0+i*k,a,b)
     sim=simpson(standard_f,n_0+i*k,a,b)
  end do
  write (*,*) "loesung",sim
end program int_simpson




     
     


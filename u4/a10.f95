program name
  implicit none 
  integer :: N=2
  real :: A, B, C
  A = 1.0
  B = 2.0
  C = 5.0
  write (*,*) -N**N**(N+1)/C/C <= -C*B .neqv. B > C-C/B .and. .not. B&
       &==C-A-B .or. 2*A<B
  write (*,*) B > C-C/B .and. .not. B&
       &==C-A-B .or. 2*A<B

end program name

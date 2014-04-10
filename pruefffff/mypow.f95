PROGRAM aufg1
IMPLICIT NONE
REAL :: x = 2.0
INTEGER :: n = 3;
write(*,*) mypow(x,n)
write(*,*) mypow(x,-n)
write(*,*) mypow(0.0, 0)
write(*,*) mypow(1.0+1/50,50)
write(*,*) mypow(1+1.0/50,50)
write(*,*) mypow(TINY(x),2)
write(*,*) mypow(245.876,HUGE(n)+n)
write (*,*) epsilon(x)
write(*,*) mypow(1+EPSILON(x)/2.3567,38)
write (*,*) 

write(*,*) mypow(-SQRT(HUGE(x)),3)
write(*,*) mypow(1/TINY(x),2) + mypow(-HUGE(x),3)
 
CONTAINS
REAL FUNCTION mypow(x, y)
REAL, INTENT(IN) :: x
INTEGER, INTENT(IN) :: y
INTEGER i
mypow = 1.0
DO i=1,y
mypow = mypow*x
END DO
END FUNCTION mypow
END PROGRAM aufg1

module simpson_m	
  implicit none
  private 
  public :: pr, simpson, standard_f

  integer,parameter :: pr=Kind(0.0D0)

contains
  ! function f (x)
  !     real(kind=pr) :: x
  !     real(kind=pr) :: f
  !     f=x
  !   f=(x**9)*exp(-(x**2))+(sin(3*x))**7-2*x*(sqrt(abs(x)))
  ! end function f
  function standard_f (x)
      real(kind=pr) :: x
      real(kind=pr) :: standard_f
      standard_f=x
    standard_f=(x**9)*exp(-(x**2))+(sin(3*x))**7-2*x*(sqrt(abs(x)))
  end function standard_f

  function simpson (f,n,a,b)
    integer ::n,k
    real(kind=pr) :: a,b
    real(kind=pr) :: h,x1,simpson,sum1,sum2
  
    interface
       function f (x)
         import :: pr
         
         real(kind=pr) :: x
         real(kind=pr) :: f
       end function f
    end interface
    
    h=(b-a)/(2*n*1.0)
    sum1=0
    
    do k=1,n-1
       x1=a+2*k*h
       sum1=sum1+f(x1)
    end do
    sum2=0
    do k=1,n
       x1=a+(2*k-1)*h
       sum2=sum2 + f(x1)
    end do
    simpson = (h/3)*(f(a)+f(b)+2*sum1+4*sum2)
  end function simpson
end module simpson_m



    
    
    

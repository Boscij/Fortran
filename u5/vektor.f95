module vektor
  implicit none
  
  private
  public :: L,KREUZ,F,OB
  
contains
  function L (vector)
    real(kind=8), dimension(3) :: vector
    real :: L
    L=sqrt(vector(1)**2+vector(2)**2+vector(3)**2)
  end function L

  function KREUZ (a,b)
    real(kind=8), dimension(3) :: a,b,KREUZ
    KREUZ(1) = a(2)*b(3)-a(3)*b(2)
    KREUZ(2) = a(3)*b(1)-a(1)*b(3)
    KREUZ(3) = a(1)*b(2)-a(2)*b(1)
    !KREUZ=(/ ..., ..., ... /)
  end function KREUZ

  function F (a,b)
    real(kind=8), dimension(3) :: a,b
    real :: F
    F = L(KREUZ(a,b))
  end function F

  function OB (a,b,c)
    real(kind=8), dimension(3) :: a,b,c
    real :: OB
    OB = 2*(F(a,b)+F(b,c)+F(c,a))
  end function OB
end module vektor

  

    


    
  

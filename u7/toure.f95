module tourede
  implicit none

  private
  public :: Fahrzeit, Fahrzeit_Null, operator(+), operator(<)

  type Fahrzeit
 !    private
     integer :: h,m
     real :: s
  end type Fahrzeit
!eigentlich noch ein typ aus Fahrzeit und character
  type(Fahrzeit), parameter :: Fahrzeit_Null=Fahrzeit(0,0,0.0)
  
  interface operator ( + )
     module procedure plus
  end interface operator ( + )
 
  interface operator ( < )
     module procedure vergleich
  end interface operator ( < )
  
contains
  function plus (L,R)
    type (Fahrzeit), intent(in) :: L,R
    type (Fahrzeit) :: plus
    plus = Fahrzeit_Null

    if ((L%s+R%s) >= 60) then
       plus%s=L%s+R%s - 60.0
       plus%m=1
    else
       plus%s=L%s+R%s
    end if
    
    if ((L%m+R%m+plus%m) >= 60) then
       plus%m=L%m+R%m - 60 + plus%m
       plus%h=1
    else
       plus%m=L%m+R%m + plus%m
    end if
    plus%h = L%h+R%h+plus%h
  end function plus
  function vergleich (L,R)
    type (Fahrzeit), intent(in) :: L,R
    logical :: vergleich
    if ((L%h < R%h) .or. ((L%h == R%h) .and. (L%m < R%m)) .or. ((L%h &
         &== R%h) .and. (L%m == R%m) .and. (L%s < R%s))) then
       vergleich = .true.
    else
       vergleich = .false.
    end if
  end function vergleich
end module tourede

       

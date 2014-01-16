module triloge
  implicit none

  private
  public ::  trilog, operator(.and.), operator(.or.),&
       & operator(.not.), operator(==), TEXT, false,&
       & maybe, true

  type trilog
     integer :: KONST
  end type trilog
  

  type(trilog), parameter :: false = trilog(-1), maybe = trilog(0),&
       & true = trilog (1)
    
  interface operator ( .and. )  !binaer
     module procedure UND
  end interface operator ( .and. )

  interface operator ( .or. )   !binaer
     module procedure ODER
  end interface operator ( .or. )
  
  interface operator ( .not. )   !unear
     module procedure NICHT
  end interface operator ( .not. )

  interface operator ( == )   !bi
     module procedure GLEICH
  end interface operator ( == )
  
  interface TEXT   !un
     module procedure umwandler
  end interface TEXT
  
contains
  
  function UND (L, R)
    type(trilog), intent(in) :: L, R
    type(trilog) :: UND
    if ((L%KONST == 1) .and. (R%KONST == 1)) &
         & then
       UND = trilog(1)
    else if ((L%KONST == -1) .or. (R%KONST == -1)) then
       UND = trilog(-1)
    else
       UND = trilog(0)
    end if
  end function UND
  

  function ODER (L, R)
    type(trilog), intent(in) :: L, R
    type(trilog) :: ODER
    if ((L%KONST == -1) .and. (R%KONST == -1)) &
         & then
       ODER = trilog(-1)
    else if ((L%KONST == 1) .or. (R%KONST == 1)) then
       ODER = trilog(1)
    else
       ODER = trilog(0)
    end if
  end function ODER

  function NICHT (tri)
    type(trilog), intent(in) :: tri
    type(trilog) :: NICHT
    if (tri%KONST == -1) then
       NICHT = trilog(1)
    else if (tri%KONST == 1) then
       NICHT = trilog(-1)
    else
       NICHT = trilog(0)
    end if
  end function NICHT
  
  function GLEICH (L,R)
    type(trilog), intent(in) :: L,R
    logical :: GLEICH
    if (L%KONST == R%KONST) then
       GLEICH = .true.
    else 
       GLEICH = .false.
    end if
  end function GLEICH
  
  function umwandler (tri)
    type(trilog), intent(in) :: tri
    character :: umwandler
    select case (tri%KONST)
    case (-1)
       umwandler = 'F'
    case (1)
       umwandler = 'T'
    case (0)
       umwandler = '?'
    end select
  end function umwandler

end module triloge


     

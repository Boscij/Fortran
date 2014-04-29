MODULE u18_module
  IMPLICIT NONE
  ! Eigener Datentyp als Listenelement
  TYPE :: Node
     CHARACTER(10)  :: Name
     INTEGER        :: Alter
     TYPE(Node), POINTER :: Next
  END TYPE Node
  
  ! Listenpointer
  TYPE :: CYCLE
     TYPE(Node), POINTER  :: Top
!hier koennte man durch das typenkonstrukt noch eine zusaetzliche Variable einfuehren. Z.B. die Laenge der Liste, welche immer um eins erhoehte wird wenn ein neuer Eintrag allociert wird. So spart man viel Rechenzeit bei der Berechnung der Listenlaenge.
  END TYPE CYCLE

  ! INTERFACE init_cycle
  !    MODULE PROCEDURE init_cycle
  ! END INTERFACE init_cycle
  
  ! INTERFACE put_cycle
  !    MODULE PROCEDURE put_cycle
  ! END INTERFACE put_cycle

  ! INTERFACE last_one
  !    MODULE PROCEDURE last_one
  ! END INTERFACE last_one

  ! INTERFACE del_next
  !    MODULE PROCEDURE del_next
  ! END INTERFACE del_next
  
  ! INTERFACE build_cycle
  !    MODULE PROCEDURE build_cycle
  ! END INTERFACE build_cycle

CONTAINS
  
  ! Initialisierung der Liste
  SUBROUTINE init_cycle(c)
    TYPE(CYCLE), INTENT (out) :: c
! Dadurch wird nicht mehr auf Speicher gezeigt
    NULLIFY(c%top)
  END SUBROUTINE init_cycle
  
  ! Ueberpruefen ob nur noch ein Eintrag in der Liste
  FUNCTION last_one(c)
    TYPE(CYCLE), INTENT (in) :: c
    LOGICAL :: last_one
    last_one=.FALSE.
    IF(ASSOCIATED(c%top,c%top%next)) last_one=.TRUE.
  END FUNCTION last_one

 subroutine build_cycle (fname,ptr)
    implicit none
    character(*) :: fname
    TYPE(CYCLE) :: tempp, ptr
    INTEGER :: talter,ios
    CHARACTER(10) :: tname
    
    ! Einlesen der Datei
    OPEN(34,file=fname,iostat=ios,status='old')
    IF(ios/= 0)THEN !FEHLER
       STOP
    END IF

    CALL init_cycle(ptr)
    ALLOCATE (Ptr%top)
    TemPp%top => Ptr%top
    READ (34,*,iostat=ios) TemPp%top%Name, TemPp%top%Alter
    DO
       ALLOCATE (TemPp%top%Next)      ! Neuen Knoten erstellen und verlinken
       READ (34,*,iostat=ios) tname, talter
       IF(ios /= 0) EXIT
       TemPp%top => TemPp%top%Next    ! Mit neuem Knoten arbeiten
       Tempp%top%Name=tname
       Tempp%top%Alter=talter
    END DO
    DEALLOCATE (Tempp%top%Next)
    Tempp%top%Next => ptr%top ! Letzter Eintrag Zeigt auf den Kopf
  END subroutine build_cycle

  ! Ausgaberoutine
  SUBROUTINE put_cycle(ptr)
    TYPE(node),POINTER :: ptr, neext
    neext => ptr%next
    WRITE (*,*) ptr%name
    DO WHILE(.NOT.(ASSOCIATED(ptr%next,neext%next)))
       WRITE (*,fmt='(A10)',advance='no') neext%name
       neext => neext%next
    END DO
  END SUBROUTINE put_cycle

  SUBROUTINE del_next(c)
    TYPE(CYCLE) :: c, temp
    temp%top => c%top%next%next
    DEALLOCATE(c%top%next)
    c%top%next => temp%top
    c%top => temp%top
  END SUBROUTINE del_next
END MODULE u18_module

 ! The structure is defined as follows:
program test_lliste
  
    TYPE :: Node
        Character(6)        :: Animal
        TYPE(Node), POINTER :: Next
    END TYPE Node
 
    TYPE(Node), POINTER  :: Head, TemP    !! Initially empty list

! !and the list can be created by ALLOCATE space, and filling it in, assigning links with pointer assignment, from right to left:

!     ALLOCATE (Head)
!     Head%Animal = "Mouse"
!     NULLIFY (Head%Next)        !! no next item
 
!     ALLOCATE (TemP)
!     TemP%Animal = "Cat"
!     TemP%Next   => Head        !! Link Cat to Mouse
!     Head        => TemP        !! Make Cat the new head
 
!     ALLOCATE (TemP)            !! Can now reuse TemP
!     TemP%Animal = "Dog"
!     TemP%Next   => Head
!     Head        => TemP        !! Dog is now at Head
!     NULLIFY (TemP)                !! No longer need service of TemP

! !We can traverse the list, printing the entries, as follows:

!     TemP => Head
!     DO WHILE (ASSOCIATED(TemP))
!         PRINT *, TemP%Animal
!         TemP => TemP%Next
!     END DO

!It would also be possible, and perhaps more intuitive, to build the list from left to right:

  print *,"Enter animals, one per line, blank line when done"
    ALLOCATE (Head)
    TemP => Head
    READ '(A)', TemP%Animal
    DO WHILE (TemP%Animal /= ' ')        !!until blank line entered    
        ALLOCATE (TemP%Next)             !!create new node and link to it  
        TemP => TemP%Next                !! work with new node
        READ '(A)', TemP%Animal
    END DO
    NULLIFY (Temp%Next)                  !! Final node points nowhere
    

DO WHILE (ASSOCIATED(Head))
        write(*,*) Head%Animal
        Head => Head%Next
    END DO    
end program test_lliste

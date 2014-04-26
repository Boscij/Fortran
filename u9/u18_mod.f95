module u18_module
  
  implicit none
  
    TYPE :: Node
       Character(10)        :: Name
       integer        :: Alter
       TYPE(Node), POINTER :: Next
    END TYPE Node

    TYPE(Node), POINTER  :: Head, TemP

    interface put_cycle
       MODULE PROCEDURE put_cycle
    end interface put_cycle


  contains
     subroutine put_cycle(ptr)
       type(node),pointer :: ptr, neext
       neext => ptr%next
       write (*,*) ptr%name
       do while(.not.(associated(ptr%next,neext%next)))
          write (*,fmt='(A10)',advance='no') neext%name
          neext => neext%next
       end do
     end subroutine put_cycle
     
end module u18_module

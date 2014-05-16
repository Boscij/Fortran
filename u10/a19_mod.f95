module a19_module
  implicit none
  type :: node
     integer :: anzahl
     type(node), pointer :: next
  end type node

  type :: queue
     integer :: laenge
     type(node), pointer :: head
     type(node), pointer :: tail
  end type queue

contains
  subroutine init(q)
    type(queue), intent (out):: q
    nullify(q%head)
    nullify(q%tail)
    q%laenge=0
  end subroutine init

  function empty(q)
    type(queue) :: q
    logical :: empty
    empty=.false.
    if(q%laenge == 0) empty=.true.
  end function empty

  subroutine enqueue(q,l)
    type(queue) :: q
    integer     :: l
    select case (q%laenge)
    case (0)
       allocate(q%head)
       q%tail => q%head
    case default
       allocate(q%tail%next)
       q%tail => q%tail%next
    end select
    q%tail%anzahl = l
    q%laenge=q%laenge + 1
  end subroutine enqueue

  subroutine dequeue(q)
    type(queue) :: q
    type(node), pointer :: temp    
    select case (q%laenge)
    case (0)
       write (*,*) "Liste ist leer"
    case default
       temp => q%head%next
       deallocate(q%head)
       q%head => temp
       q%laenge = q%laenge -1
    end select
  end subroutine dequeue

  subroutine put(q)
    type(queue) :: q
    type(node), pointer :: temp
    integer :: i
    temp => q%head
    if (empty(q)) then
       write (*,'(A30)',advance='no') "Die Warteschlange ist leer"
    else
       do
          write (*,'(I3,A1)',advance='no') temp%anzahl,' '
          if (associated(temp,q%tail)) exit
          temp => temp%next
       enddo
    end if
    write(*,*)
  end subroutine put
  
    
end module a19_module




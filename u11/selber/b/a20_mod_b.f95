module mod_sort
  implicit none

  type filecomp
     integer :: unitt, component
  end type filecomp

  interface operator (<=)
     module procedure compare
  end interface operator (<=)

contains

  logical function compare(first, second)
    type(filecomp), intent(in) :: first, second
    compare = first%component <= second%component
  end function compare

  subroutine change(a,b)
    type(filecomp) :: a, b, temp
    temp = a
    a = b
    b = temp
  end subroutine change

  recursive subroutine downheap(field,new1,i)
    type(filecomp), dimension(:) :: field
    integer                      :: i, l, r, new1, temp

    l = 2 * i
    r = 2 * i + 1

    if (l <= new1 .and. field(l)%component > field(i)%component) then
       temp = l
    else
       temp = i
    end if

    if (r <= new1 .and. field(r)%component > field(temp)%component)then
       temp = r
    end if

    if(temp /= i)then
       call change(field(i),field(temp))
       call downheap(field,new1,temp)
    end if
  end subroutine downheap
  

  subroutine buildheap(field)

    type(filecomp), dimension(:) :: field
    integer                      :: i
    do i=size(field)/2,1,-1
       call downheap(field, size(field), i)
    end do
  end subroutine buildheap

  subroutine heap_sort(field)
    type(filecomp), dimension(:) :: field
    integer :: lenght, j, i
    call buildheap(field)
    lenght = size(field)

    do i = lenght,2,-1
       call change(field(1), field(lenght))
       lenght = lenght - 1
       call downheap(field, lenght,1)
    end do
  end subroutine heap_sort
  
end module mod_sort




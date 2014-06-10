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

  subroutine ins_sort(field)
    type(filecomp), dimension(:) :: field
    integer                     :: i, j
    type(filecomp)              :: temp
    do i = 2, size(field)
       j = i
       do
          if ((j <= 1) .or. field(j-1) <= field(j)) exit
          temp       = field(j)
          field(j)   = field(j-1)
          field(j-1) = temp
          
          j = j - 1
       end do
    end do
  end subroutine ins_sort

  subroutine insert(field,filecomp1)
    type(filecomp), dimension(:) :: field
    type(filecomp), intent(in)   :: filecomp1
    integer                      :: l, r, middle

    l = 1
    r = size(field) + 1
    do
       middle = (l+r) / 2  ! round down
       if (middle == 1) then
          field(middle) = filecomp1
          exit
       else if (middle == size(field)) then
          field(1:middle-1) = field(2:middle)
          field(middle)     = filecomp1
          exit
       else if ((field(middle) <= filecomp1) .and. &
            &(filecomp1 <= field(middle+1))) then
          field(1:middle-1) = field(2:middle)
          field(middle)     = filecomp1
          exit
       else if (filecomp1 <= field(middle)) then
          r = middle
       else if (field(middle) <= filecomp1) then
          l = middle
       end if
    end do
  end subroutine insert
end module mod_sort




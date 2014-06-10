program sortieren
  use mod_sort
  implicit none
  type(filecomp), dimension(5)              :: field1,tt
  integer                                   :: n, int1, int2, i, j, ios
  integer                                   :: new_read
  character(12)                             :: filename
  logical                                   :: logic1
  type(filecomp)                            :: new1

  
  field1%component=(/2,7,19,3,200/)
  write (*,*) field1%component
  tt%component=field1%component
  call buildheap(tt)
  write (*,*) tt%component
  call heap_sort(tt)
  write (*,*) tt%component
    call heap_sort(field1)
  write (*,*) field1%component
end program sortieren







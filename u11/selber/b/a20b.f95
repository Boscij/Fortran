program sortieren
  use mod_sort
  implicit none
  type(filecomp), dimension(:), allocatable :: field1
  integer                                   :: n, int1, int2, i, j, ios
  integer                                   :: new_read
  character(12)                             :: filename
  logical                                   :: logic1
  type(filecomp)                            :: new1

 ! write(*,*) " Anzahl Dateien? "
  !read(*,*) n
n= 3
  allocate(field1(n))
  int1 = iachar("0")
  int2 = iachar("1")
  filename="erfass01.dat"
  do i=21,20+n
!write(*,*) filename
     open(unit=i,file=filename,status="old",action="read",iostat=ios)
     if (ios /= 0) then
        write(*,*) " Fehler beim oeffnen der Datei"
        stop
     end if

     int2= int2+1
     if (int2 == iachar("1")+9) then
        int2 = iachar("0")
        int1=int1 + 1
     end if
     filename = "erfass"//achar(int1)//achar(int2)//".dat"
  end do
  open(unit=20,file="ziel.dat",status="old",action="write",iostat=ios)
  if (ios /= 0) then
     write(*,*) "Ausgabedatei Fehler"
     stop
  end if

  do j=1,n
     read(j+20,*)new_read
     field1(j)%unitt = j+20
     field1(j)%component = new_read
  end do
    call heap_sort(field1)
!  write(*,*)field1%component
  j = 1
  do 
     write(20,*)field1(j)%component
     read (field1(j)%unitt,*,iostat=ios)new_read
!     write(*,*) field1%component
!     write(*,*) j, new_read, ios,field1(j)%unitt
     if(ios/=0) then
        j = j + 1
     else
        new1%unitt=field1(j)%unitt
        new1%component=new_read
        field1(j)=new1
     end if
     call heap_sort(field1(1:n))
     if (j>n) exit
  end do

  int1=0
  do i=1,n+1
     close(20+i-1)
  end do
  open(unit=20,file="ziel.dat",status="old",action="read",iostat=ios)
     if (ios /= 0) then
        write(*,*) "Fehler beim lesen der Datei"
        Stop
     end if
     
  do
     read (20,*,iostat=ios) int2
     write(*,*) int2
     if (int1>int2) then
        write(*,*) "Fehler in Ziel.dat"
        stop
     end if
     int1=int2
     if (ios == -1) then
        write(*,*) "Datei zu Ende, keine Fehler"
        Stop
     end if
     if (ios /= 0) then
        write(*,*) "Fehler beim lesen der Datei"
        Stop
     end if
  end do
  close(20)
  
end program sortieren







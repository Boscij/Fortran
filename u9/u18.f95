program spiel
  use u18_module
  implicit none
  integer :: ios, ii ,talter,kk, zaehler=1
  character(10) :: tname

  OPEN(34,file='kreim.dat',iostat=ios,status='old')
  IF(ios/= 0)THEN !FEHLER
     STOP
  END IF
  ALLOCATE (Head)
  TemP => Head
      READ (34,*,iostat=ios) TemP%Name, TemP%Alter
    DO 
        ALLOCATE (TemP%Next)             !!create new node and link to it  
        READ (34,*,iostat=ios) tname, talter
        if(ios /= 0) exit
        TemP => TemP%Next                !! work with new node
        Temp%Name=tname
        Temp%Alter=talter
     END DO
     deallocate (Temp%Next)
     Temp%Next => head                  !! Final node points to head
     !Abzählen
     write (*,*) "Folgende Mitspieler sind im Kreis"
     call put_cycle(head)
write (*,*) 
     do
     do ii=1,20
        head => head%next
     end do
     do ii=1,head%alter-1
        head => head%next
     end do
     Temp => head%next%next
     write (*,*) head%next%name, "ist nach Runde", zaehler, "geflogen"
     write (*,*) 

     deallocate(head%next)
     !     deallocate(head)
     head%next => temp
     head => temp
     if (associated(head,head%next)) exit
     write (*,*) "Es sind noch folgende Mitspieler im Kreis"

     write (*,*) 
     call put_cycle(head)
     write (*,*) 

     zaehler=zaehler + 1
  end do
write (*,*) "Gewinner/in ist ",head%name 
deallocate(head)
    close(34)

end program spiel

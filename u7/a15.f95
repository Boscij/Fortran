program touredefrance
  use tourede
  implicit none
  
  integer :: Teilnehmer,ios,ios2,H,M,Anzahl=0,i,flag
  real :: S
  character(len=30),allocatable,dimension(:) :: Liste_Name
  type(Fahrzeit),allocatable,dimension (:) :: Liste_Zeit
  type(Fahrzeit) :: aktuell
  character(len=32) :: fname,Name
  
  write (*,*) 'Teilnehmerzahl eingeben'
  read (*,*) Teilnehmer
  fname='tourdefrance.dat'
  open (unit=700,file=fname,status='old',action='read',iostat=ios)
  if (ios /= 0) goto 999
  allocate(Liste_Name(Teilnehmer))
  allocate(Liste_Zeit(Teilnehmer))
  do
     read (unit=700, FMT=*,iostat=ios2) Name, H,M,S
     if (ios2 /= 0) exit
     flag = 0
     aktuell=Fahrzeit(H,M,S)
     inner : do i=1,Anzahl
        if (Liste_Name(i) == Name) then
           Liste_Zeit(i)=Liste_Zeit(i)+aktuell
           flag = 1
           exit inner
        end if
     end do inner
     if (flag == 0) then
        Anzahl=Anzahl+1
        if (Anzahl > Teilnehmer) goto 1000
        Liste_Name(Anzahl)=Name
        Liste_Zeit(Anzahl)=aktuell
     end if
     
  end do
  ! do i=1,Anzahl
  ! Liste_Zeit(i)%s=nint(Liste_Zeit(i)%s*10.0)/1.0     
  ! end do
  ! mit 10 multipliziert wird das Ergebnis exakt

  write (*,*) 'Ergebnisse der Fahrt: Name Stunden Minuten Sekunden'
  do i=1,Anzahl
     write (*,*) Liste_Name(i), Liste_Zeit(i)
  end do
  write (*,*) Anzahl, 'Teilnehmer sind mitgefahren'

  deallocate(Liste_Name)
  deallocate(Liste_Zeit)
  close(700)
stop
999 write (*,*) 'Datei nicht gefunden'
1000 write (*,*) 'Anzahl der Fahrer überschreiten eingegebene maximale Anzahl von Teilnehmern'

end program touredefrance

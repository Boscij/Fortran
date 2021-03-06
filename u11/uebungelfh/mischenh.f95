program mischenh
  use mod_mischenh
  implicit none
  type(filecomp),dimension(:),allocatable::arbeitsfeld,arbeitsfeld2
  integer::n,dateinummer,fehler,zahl1,zahl2,i,inhaltskomp
  character(len=12)::dateiname
  logical::vergleich
  type(filecomp)::neu

  write(*,*)"wieviel dateien sollen eingelesen werden?"
  read(*,*)n

  allocate(arbeitsfeld(n))  
 
  zahl1=iachar("0")              !erste stelle
  zahl2=iachar("1")              !zweite stelle
  dateiname="erfass"//achar(zahl1)//achar(zahl2)//".dat"      !dateiname erste datei
  do dateinummer=21,20+n                                      !einleseschleife
    open(unit=dateinummer,file=dateiname,status="old",action="read",iostat=fehler)
    if (fehler/=0) then
      write(*,*)"erfass.dat verursacht einen Fehler"
      STOP
    end if
    zahl2=zahl2+1                         !in ascii-tabelle einen schritt weiter
    if (zahl2==iachar("1")+9)then         !bei "10" wirds kritisch
      zahl2=iachar("0")                   !zweite stelle auf "0"
      zahl1=zahl1+1                       !erste stelle, also zehnerstelle eins hoeher  
    end if
    dateiname="erfass"//achar(zahl1)//achar(zahl2)//".dat" 
  end do 
  open(unit=20,file="ziel.dat",status="old",action="write",iostat=fehler) !oeffnen der zieldatei
    if (fehler/=0) then
      write(*,*)"ziel.dat verursacht einen Fehler"
      STOP
    end if
  do i=1,n
    read(i+20,*)inhaltskomp
    arbeitsfeld(i)%unitn=i+20
    arbeitsfeld(i)%inhalt=inhaltskomp
  end do
  call sortiereheap(arbeitsfeld)    !erstmalige sortierung
!  write(*,)
  do i=1,n
    write(*,*)arbeitsfeld(i)
  end do

  i=1                          !indexuntergrenze
  do
   write(20,*)arbeitsfeld(i)%inhalt                  !erster eintrag feld wird in ziel.dat geschrieben
     read(arbeitsfeld(i)%unitn,*,iostat=fehler)inhaltskomp           !einlesen naechster eintrag
     if (fehler/=0)then
     i=i+1
     else                               !es sind noch daten vorhanden
       neu%unitn=arbeitsfeld(i)%unitn
       neu%inhalt=inhaltskomp
       arbeitsfeld(i)=neu
    end if 
    call sortiereheap(arbeitsfeld(i:n))                   !neue werte werden an korrekte stelle eingetragen
    if (i>n)exit
  end do

   !do i=1,n
    !write(*,*)arbeitsfeld(i)
  !end do
end program mischenh

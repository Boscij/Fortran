program a19
  use a19_module
  implicit none
!  type(queue) :: test
!  logical     :: tttt
  integer     :: Kassen, i,mi,j
  real        :: Wahrscheinlichkeit, h
  type(queue), dimension(:), allocatable :: Feld

  call random_seed() !damit sich die Zufallszahlen bei jedem Programmaufruf aendern
  
  write (*,*) "Bitte Anzahl der geöffneten Kassen angeben und Ankunftswahrscheinlichkeit"
  read (*,*) Kassen
  write (*,*) "Bitte Wahrscheinlichkeit im Bereich 0.0 und 1.0 angeben"
  
  do
     read (*,*) Wahrscheinlichkeit
     if ((Wahrscheinlichkeit <= 1.0) .and. (Wahrscheinlichkeit >= 0.0)) &
          & exit
  end do
!  Wahrscheinlichkeit = real(Wahrscheinlichkeit)
!  write (*,*) Wahrscheinlichkeit, Kassen

  allocate(Feld(Kassen))
  
  do i=1,Kassen
     call init(Feld(i))
  end do

  do j=1,12*7*60!*60
     call random_number(h)
     if (h .lt. Wahrscheinlichkeit) then
        call random_number(h)
        h=int(20+h*280)
        !      write(*,*) h
        mi=1
        do i=1,Kassen
           if(Feld(i)%laenge < Feld(mi)%laenge) then
              mi=i
           end if
        end do
        call enqueue(Feld(mi),int(h))
        call ausgabe(Feld(i))
     end if
     do i=1,Kassen
        if(Feld(i)%laenge .ne. 0) then
           Feld(i)%head%anzahl=Feld(i)%head%anzahl-1
           if (Feld(i)%head%anzahl == 0) then
              call dequeue(Feld(i))
              write (*,*) "Kasse",i,"fertig"
              call ausgabe(Feld)
           end if
        end if
     end do
  end do
  
contains
  
  subroutine ausgabe(F)
    type(queue), dimension(Kassen)  :: F
    integer :: g
    write (*,*) "Zeit: ",j," Sekunden"
    do g=1,Kassen
       write (*,'(A6,I2,A3)',advance='no') "Kasse:",g,"   "
       call put(Feld(g))
    end do
    write (*,*)
    write (*,*) 


  end subroutine ausgabe
  
    


end program a19

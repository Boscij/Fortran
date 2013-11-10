program ratespiel
  integer :: r, l, k, Zahl, Anzahl
  character(len=1) :: a, b
outer : do
  Anzahl = 1
  do     
     write (*,*) 'Bitte untere Grenze(ganze Zahl) eingeben, bestaetigen, obere Grenze eingeben(ganze Zahl) und bestaetigen'
     read (*,*) l
     read (*,*) r
     if ( l < r ) exit
  end do
 middle : do
  Zahl = int(floor(( r + l ) / 2.0))
  write (*,*) Zahl, 'zahl'
  inner : do
        write (*,*) 'Ist die gedachte Zahl groesser, kleiner oder gleich? Bitte mit folgenden Symbolen antworten (>,<,=)?'
        read (*,*) a
        select case (a)
           case ('=')
              exit middle
           case ('<')
              if ( r == l .or. Zahl == l) then
                 write (*,*) 'Fehleingabe'
                 exit outer
              end if
              r = Zahl - 1
              exit inner
           case ('>')
              if ( r == l .or. Zahl == r) then
                 write (*,*) 'Fehleingabe'
                 exit outer
              end if
              l = Zahl + 1
              exit inner
        end select
     end do inner
     Anzahl = Anzahl + 1
  end do middle 
write (*,*) 'Es wurde(n) ',Anzahl,'Versuch(e) zum "erraten" der Zahl benoetigt'
write (*,*) 'Soll das Spiel wiederholt werden?(ja oder nein)'
read (*,*) b
if ( b /= 'j') exit
end do outer
end program ratespiel

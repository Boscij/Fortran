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
 inner : do
  Zahl = int(floor(( r + l ) / 2.0))
  write (*,*) Zahl, 'zahl'
     do
        write (*,*) 'Ist die gedachte Zahl groesser, kleiner oder gleich? Bitte mit folgenden Symbolen antworten (>,<,=)?'
        read (*,*) a
        if ( a == '=' ) then
           k = 0
           exit inner
        else
           if ( a == '<' ) then
              if ( r == l .or. Zahl == l) then
                 write (*,*) 'Fehleingabe'
                 exit inner
              end if
              r = Zahl - 1
              exit
           else
              if ( a == '>' ) then
                 if ( r == l .or. Zahl == r) then
                    write (*,*) 'Fehleingabe'
                    exit inner
                 end if
                 l = Zahl + 1
                 exit
              end if
           end if
        end if
     end do
     Anzahl = Anzahl + 1
  end do inner
write (*,*) 'Es wurden ',Anzahl,'Versuche zum durchgefuehrt'
write (*,*) 'Soll das Spiel wiederholt werden?(ja oder nein)'
read (*,*) b
if ( b /= 'j') exit
end do outer
end program ratespiel

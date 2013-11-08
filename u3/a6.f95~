program ratespiel
  integer :: r, l, k, Zahl, Anzahl
  character(len=1) :: a, b
outer : do
  Anzahl = 0
  do     
     write (*,*) 'Bitte untere Grenze(ganze Zahl) eingeben, bestaetigen, obere Grenze eingeben(ganze Zahl) und bestaetigen'
     read (*,*) l
     read (*,*) r
     if ( l < r ) exit
  end do

  Zahl = int(floor(( r + l ) / 2.0))
  write (*,*) Zahl, 'zahl'
  do
     do
        write (*,*) 'Ist die gedachte Zahl groesser, kleiner oder gleich? Bitte mit folgenden Symbolen antworten (>,<,=)?'
        read (*,*) a
        if ( a == '=' ) then
           k = 0
        else
           if ( a == '<' ) then
              k = -1
           else
              if ( a == '>' ) then
                 k = 1
              else
                 k = 2
              end if

           end if
        end if
        if ( k /= 2 ) exit
     end do
     Anzahl = Anzahl + 1
     if ( k /= 0 ) then
        if ( (r - l) == 2 ) then
           if ( k == 1 ) then
              Zahl = Zahl + 1
           else
              write (*,*) 'Eingaben widersprechen sich, Spiel wird beendet'
              exit outer
           end if
        else
           if ( k == 1) then
              l = Zahl
              Zahl = int(floor(( r + Zahl ) / 2.0))
           else
              r = Zahl
              Zahl = int(floor(( l + Zahl ) / 2.0))
           end if
        end if
        if ( Zahl > r) then
           write (*,*) 'Eingaben widersprechen sich, obere Grenze ueberschritten, Spiel wird beendet'
           exit outer
        end if
        if ( Zahl < l) then
           write (*,*) 'Eingaben widersprechen sich, untere Grenze unterschritten, Spiel wird beendet'
           exit outer
        end if
        write (*,*) Zahl, 'Zahl'
     end if
     if ( k == 0) exit
  end do
write (*,*) 'Es wurden ',Anzahl,'Versuche zum "erraten" der Zahl benoetigt'
write (*,*) 'Soll das Spiel wiederholt werden?(ja oder nein)'
read (*,*) b
if ( b == 'n') exit
end do outer
end program ratespiel


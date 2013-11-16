program ratespiel
  integer :: r, l, k, Zahl, Anzahl, flag, pruef, flag2
  character(len=1) :: a, b
flag = 0
flag2 = 0
outer : do
  Anzahl = 0
  do     
     write (*,*) 'Bitte untere Grenze(ganze Zahl) eingeben, bestaetigen, obere Grenze eingeben(ganze Zahl) und bestaetigen'
     read (*,*) l
     read (*,*) r
     if ( l < r ) exit
  end do

  Zahl = int(floor(( r + l ) / 2.0))

  if ( (r == Zahl) .or. (l == Zahl)  ) then
     write (*,*) 'Eingaben widersprechen sich, Spiel wird beendet'
     exit outer
  end if
  write (*,*) Zahl, 'zahl'
 inner : do
pruef = Zahl
     do
        write (*,*) 'Ist die gedachte Zahl groesser, kleiner oder gleich? Bitte mit folgenden Symbolen antworten (>,<,=)?'
        read (*,*) a
        if ( a == '=' ) then
           k = 0
        else
           if ( a == '<' ) then
              k = -1
              r = Zahl - 1
           else
              if ( a == '>' ) then
                 k = 1
                 l = Zahl + 1
              else
                 k = 2
              end if
           end if
        end if
        if ( k /= 2 ) exit
     end do

     Anzahl = Anzahl + 1
     if ( k /= 0 ) then
<<<<<<< HEAD
        if ( (r - l) == 1 ) then
=======
        ! if ( (r - l) == 2 ) then
        !    if ( k == 1 ) then
        !       if ( Zahl == r .or. flag == -1 ) then
        !          write (*,*) 'Eingaben widersprechen sich, Spiel wird beendet'
        !          exit outer
        !       end if
        !       flag = 1
        !       Zahl = Zahl + 1
        !    else
        !       if ( Zahl == l .or. flag == 1 ) then
        !          write (*,*) 'Eingaben widersprechen sich, Spiel wird beendet'
        !          exit outer
        !       else
        !          Zahl = Zahl - 1
        !       end if
        !       flag = -1
        !    end if
        ! else
        !    if ( k == 1) then
        !       l = Zahl
        !       Zahl = int(floor(( r + Zahl ) / 2.0))
        !    else
        !       r = Zahl
        !       Zahl = int(floor(( l + Zahl ) / 2.0))
        !    end if


end if

        if ( (r - l) == 1 ) then! write (*,*) 'test'

>>>>>>> 3cb9572e6672b9678d97fa76f643b133c41f437a
           if ( k == 1 ) then
              if ( Zahl == r .or. flag == 1) then
                 write (*,*) 'Eingaben widersprechen sich, Spiel wird beendet'
                 exit outer
              end if
              flag = 1
              Zahl = Zahl + 1
!              exit inner
           else
              if ( Zahl == l .or. flag == 1) then
                 write (*,*) 'Eingaben widersprechen sich, Spiel wird beendet'
                 exit outer
              end if
              Zahl = Zahl - 1
!              exit inner
              flag = 1
           end if
        else
           if ( k == 1) then
!write (*,*) 'test2',r,Zahl

              l = Zahl + 1
              Zahl = int(floor(( r + Zahl + 1 ) / 2.0))
           else
!write (*,*) 'test3',Zahl

              r = Zahl - 1
              Zahl = int(floor(( l + Zahl - 1) / 2.0))
           end if
        end if
<<<<<<< HEAD
        if ( Zahl > r) then
           write (*,*) 'Eingaben widersprechen sich, obere Grenze ueberschritten, Spiel wird beendet'
           exit outer
        end if
        if ( Zahl < l .or. ( pruef == Zahl) .or. (Zahl == r)) then
           write (*,*) 'Eingaben widersprechen sich, untere Grenze unterschritten, Spiel wird beendet'
           exit outer
        end if
=======
        ! if ( Zahl > r) then
        !    write (*,*) 'Eingaben widersprechen sich, obere Grenze ueberschritten, Spiel wird beendet'
        !    exit outer
        ! end if
        ! if ( Zahl < l .or. ( pruef == Zahl)) then
        !    write (*,*) 'Eingaben widersprechen sich, untere Grenze unterschritten, Spiel wird beendet'
        !    exit outer
        ! end if

     if ( pruef == Zahl ) then
        write (*,*) 'Eingaben widersprechen sich, Spiel wird beendet!'
        exit outer
     end if
write (*,*) 'r',r,'l',l

>>>>>>> 3cb9572e6672b9678d97fa76f643b133c41f437a
        write (*,*) Zahl, 'Zahl'
     end if
     if ( k == 0) exit
! write (*,*) pruef,Zahl
  end do inner
write (*,*) 'Es wurden ',Anzahl,'Versuche zum "erraten" der Zahl benoetigt'
write (*,*) 'Soll das Spiel wiederholt werden?(ja oder nein)'
read (*,*) b
if ( b /= 'j') exit
end do outer
end program ratespiel


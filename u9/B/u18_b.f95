PROGRAM spiel
  USE u18_module
  IMPLICIT NONE
  INTEGER :: ii , zaehler=1 
  type(cycle) :: headd

  ! Einlesen der Daten und erstellen der Liste
   call build_cycle('kreim.dat',headd)

  ! Ausgabe bis zum Ende des Spiels
  WRITE (*,*) "Folgende Mitspieler sind im Kreis"
  ! Ausgabe aller beteiligten Spieler ueber subroutine put_cycle
  CALL put_cycle(headd%top)
  WRITE (*,*)
  DO
     ! Springen der 21 Silben
     DO ii=1,20
        headd%top => headd%top%next
     END DO
     ! Springen des jeweiligen Alters
     DO ii=1,headd%top%alter-1
        headd%top=> headd%top%next
     END DO

     WRITE (*,*) headd%top%next%name, "ist nach Runde", zaehler, "geflogen"
     WRITE (*,*)
     ! Löschen des jeweiligen Spielers über Subroutinge del_next
     CALL del_next(headd)

     ! Entscheiden ob Schleifenende ueber Function last_one
     IF (last_one(headd)) EXIT
     WRITE (*,*) "Es sind noch folgende Mitspieler im Kreis"

     WRITE (*,*) 
     CALL put_cycle(headd%top)
     WRITE (*,*) 

     zaehler=zaehler + 1
  END DO
  WRITE (*,*) "Gewinner/in ist ",headd%top%name

  ! Textdatei schließen
  close(34)
DEALLOCATE(headd%top)


END PROGRAM spiel

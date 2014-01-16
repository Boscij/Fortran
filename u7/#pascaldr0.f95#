    !!  Informatik 1, Prof. Dr. Wolfgang Walter, Mathematik, TU Dresden  !!

    PROGRAM  pascaldr               !!  Pascalsches Dreieck (Version 0)  !!
      IMPLICIT NONE

      INTEGER, PARAMETER          :: lines = 4 ! Anzahl Zeilen (Konst.) !
      INTEGER                     :: i, j, OLD, NEW
      INTEGER, DIMENSION( 0:lines):: P = 1    ! Initialis. aller Elemente !
      CHARACTER(LEN=2)            :: stri1, stri2       ! interne Dateien !

      OPEN ( UNIT=35, FILE='pascaldr.out' )

      DO i= 0, lines
        OLD= 1                 ! in der ersten j-Iteration wird P(0)= 1   !
                               ! gesetzt (unnoetigerweise + immer wieder) !
        DO j= 1, i-1
          NEW= P(j) + P(j-1)   ! neue Summe zunaechst temporaer speichern !
          P(j-1)= OLD          ! Summe aus vorhergehender Iteration im    !
                               ! vorhergehenden Feldelement abspeichern - !
                               ! P(j-1) wird jetzt nicht mehr gebraucht;  !
          OLD= NEW             ! neue Summe NEW fuer naechste Iteration   !
                               ! in temporaere Variable OLD umspeichern.  !
        END DO
                               ! Summe aus letzter Iteration abspeichern; !
        P(i-1)= OLD            ! fuer 1. Iteration (i=0) ist P(-1) noetig !
                               
        WRITE( UNIT=stri1, FMT='(I2.2)' )  i+1   ! Anzahl Zahlen in Zeile !
        WRITE( UNIT=stri2, FMT='(I2.2)' )  3  ! Tabulatorpos. !
!        WRITE( UNIT=stri2, FMT='(I2.2)' )  1+(lines-i)*3  ! Tabulatorpos. !
        WRITE( UNIT=*, FMT='(T'//stri2//','//stri1//'I6)' )  P(0:i)
!        WRITE( UNIT=*, FMT='(T'//stri2//','//stri1//'I2.2)' )  P(0:i)
  ! im Format:  absolute Tabposition, Wiederholfaktor I-Formatdeskriptor  !
      END DO

      CLOSE ( UNIT=35 )

    END PROGRAM pascaldr


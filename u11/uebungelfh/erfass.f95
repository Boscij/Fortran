PROGRAM erfass
  IMPLICIT NONE

  INTEGER, PARAMETER :: n= 99, min_anz= 30, max_anz= 10000, &
                               min_add=  0, max_add= 100
  INTEGER            :: i, j, anz, summe
  CHARACTER(LEN=2)   :: str
  REAL               :: z

  CALL RANDOM_SEED()   ! optional

  DO i= 1, n

    WRITE(UNIT=str, FMT="(I2.2)") i
    OPEN (UNIT=100+i, FILE="erfass"//TRIM(ADJUSTL(str))     &
         &//".dat", ACTION="WRITE", STATUS="REPLACE") 

    CALL RANDOM_NUMBER(z)
    anz= INT(z*REAL(max_anz-min_anz)) + min_anz

    summe= 0
    DO j= 1, anz
      CALL RANDOM_NUMBER(z)
      summe= summe + INT(z*REAL(max_add-min_add)) + min_add
      WRITE(UNIT=100+i, FMT=*) summe
    END DO

    CLOSE(100+i)

  END DO

END PROGRAM erfass


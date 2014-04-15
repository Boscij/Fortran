PROGRAM EX1
  implicit none
      INTEGER PGOPEN, I
      REAL XS(9), YS(9), XR(101), YR(101)

! Compute numbers to be plotted.

      DO 10 I=1,101
          XR(I) = 0.1*(I-1)
          YR(I) = XR(I)**2*EXP(-XR(I))
 10   CONTINUE
      DO 20 I=1,9
          XS(I) = I
          YS(I) = XS(I)**2*EXP(-XS(I))
 20   CONTINUE

! Open graphics device.

                IF (PGOPEN('?') .LT. 1) STOP
!                 (PGOPEN('xwindow')

! Define coordinate range of graph (0 < x < 10, 0 < y < 0.65),
! and draw axes.

                CALL PGENV(0., 10., 0., 10.0,  0,  0)
                call pgline(2, (/0.0, 10.0/),(/0.0, 0.0/))

! Label the axes (note use of \u and \d for raising exponent).

      CALL PGLAB('x', 'y', 'PGPLOT Graph: y = x\u2\dexp(-x)')

! Plot the line graph.

      CALL PGLINE(101, XR, YR)

! Plot symbols at selected points.

      CALL PGPT(9, XS, YS, 18)

! Close the graphics device.

      CALL PGCLOS

      END

program a17
  use mod_a17
  implicit none
  integer :: i,j, a, z,s,spalte, zaehler, istat,pgopen, spieler
  integer, dimension(:,:), allocatable :: Matr, vec
!   10   Green + Cyan           270, 0.50, 1.00   0.00, 1.00, 0.50
! 11   Blue + Cyan            330, 0.50, 1.00   0.00, 0.50, 1.00
! 12   Blue + Magenta          30, 0.50, 1.00   0.50, 0.00, 1.00
! 13   Red + Magenta           90, 0.50, 1.00   1.00, 0.00, 0.50
! 14   Dark Gray                0, 0.33, 0.00   0.33, 0.33, 0.33
! 15   Light Gray   	      0, 0.66, 0.00   0.66, 0.66, 0.66
  outer : do
     do
        write (*,*) ' Zeilen und Spalten des Spielfeldes bitte hier eingeben'
        read (*,*)  z,s
        if ((z < 0) .or. (s < 0)) exit outer
        if ((z > 3) .or. (s > 3)) exit
     end do
 ISTAT = PGOPEN('/xwindow')
     IF (ISTAT .LE. 0 ) STOP
      call PGENV (0, real(s), 0, real(z), 1, -1)
      call PGSCI (11)
      do i=0,z
         call pgline(2, (/0.0, real(s)/),(/real(i), real(i)/))
      end do
      do i=0,s
         call pgline(2, (/real(i),real(i)/),(/0.0, real(z)/))
      end do
     allocate(Matr(z,s))
     allocate(vec(s,1))
     ! vec = 0
     ! Matr = 0
     do i = 1,s
        vec(i,1)=0
        do j = 1,z
           Matr(i,j)=0
        end do
     end do
!     call aus(Matr)
     spieler = -1
     do zaehler = 1, z*s


        do 
           write (*,*) 'Spalte eigeben'
           read (*,*) spalte

!            write (*,*) vec(:,:),'genau davor'

           if ((vec(spalte,1) < z) .and. (spalte > 0) .and. (spalte <= s)) exit
           write (*,*) 'Spalte schon voll'
        end do
        vec(spalte,1)=vec(spalte,1) + 1
        Matr(vec(spalte,1),spalte) = spieler
         if(spieler == 1) then
            call PGSCI (10)
         else
            call pgsci (13)
         end if
        call PGCIRC (real(spalte)-0.5,real(vec(spalte,1)-0.5), 0.4)
!        call aus(Matr)
        if (Gewinn(Matr) /= 0) exit
        spieler = -spieler
     end do
     deallocate(Matr)
     deallocate(vec)
  end do outer
end program a17

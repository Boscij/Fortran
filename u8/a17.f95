program a17
  use mod_a17
  implicit none
  integer :: i,j, a, z,s,spalte, zaehler
  integer, dimension(:,:), allocatable :: Matr, vec

  outer : do
     do
        write (*,*) ' Zeilen und Spalten des Spielfeldes bitte hier eingeben'
        read (*,*)  z,s
        if ((z < 0) .or. (s < 0)) exit outer
        if ((z > 3) .or. (s > 3)) exit
     end do
     allocate(Matr(z,s))
     allocate(vec(s,1))
     do i = 1,s
        vec(i,s)=0
        do j = 1,z
           Matr(i,j)=0
        end do
     end do
     call aus(Matr)
     do zaehler = 1, 10**3
        do 
           write (*,*) 'Spalte eigeben'
           read (*,*) spalte
           if ((vec(spalte,1) < z) .and. (spalte > 0) .and. (spalte <= s)) exit
           write (*,*) 'Spalte schon voll'
        end do
        vec(spalte,1)=vec(spalte,1) + 1
        Matr(vec(spalte,1),spalte) = (-1)**zaehler
        call aus(Matr)
        if (Gewinn(Matr) /= 0) exit
     end do
     deallocate(Matr)
     deallocate(vec)
  end do outer
end program a17

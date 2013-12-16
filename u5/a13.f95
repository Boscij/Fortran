program parallelepiped
  use vektor
  implicit none
  real(kind=8), dimension(3) :: a,b,c
  character(len=1) :: Antwort
  do
     write (*,*) 'Vektor eins eingeben'
     read (*,*) a(1),a(2),a(3)
     write (*,*) 'Vektor zwei eingeben'
     read (*,*) b(1),b(2),b(3)
     write (*,*) 'Vektor drei eingeben'
     read (*,*) c(1),c(2),c(3)

     write (*,*) L(a),L(b),L(c)
     write (*,*) KREUZ(b,c)
     write (*,*) F(b,c)
     write (*,*) OB(a,b,c)
     write (*,*) 'Noch einmal ? (y/n)'
     read (*,*) Antwort
     if (Antwort == 'n') exit
  end do
end program parallelepiped

  

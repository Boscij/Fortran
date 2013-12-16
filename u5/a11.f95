program linse
  use ivalmod
  implicit none
  real(kind=pr) :: f_0,b_0,d_f,d_b,g_0,d_g
  type(interval) :: f,b,g_1,g_2
  write (*,*) 'f_0 ='
  f_0=20
!  read (*,*) f_0
  write (*,*) 'b_0 ='
  b_0=25
!  read (*,*) b_0
  write (*,*) 'd_f ='
  d_f=0.1
!  read (*,*) d_f
  write (*,*) 'd_b'
  d_b=0.5
!  read (*,*) d_b
  f=ival(f_0-d_f,f_0+d_f)
  b=ival(b_0-d_b,b_0+d_b)
  write (*,*) 'Intervall f'
  call put(f)
  write (*,*) 'Intervall b'
  call put(b)
  g_0 = 1/((1/f_0)-(1/b_0))
  d_g = d_f/(((1-f_0/b_0)**2))+d_b/((b_0/f_0-1)**2)
  g_1 = ival(g_0-d_g,g_0+d_g)
  write (*,*) 'Intervall g_1'
  call put(g_1)
  g_2 = ival_one/((ival_one/f)-(ival_one/b))
  write (*,*) 'Intervall g_2'
  call put(g_2)
end program linse

program magische_quadrate
  implicit none 
  integer :: m,S,n,i,j,zahler=1,summe,test
  integer,allocatable,dimension(:,:) :: A
  character(LEN=2)            :: stri1, stri2
out: do 
  do
     write (*,*) 'Dimension eingeben'
     read (*,*) n
     if (n < 0) exit out
     if (modulo(n,2) /= 0 .and. n /= 1) exit
  end do
  m=(n+1)/2
  allocate(A(n,n))
  do i=1,n
     do j=1,n
        A(i,j)=0 !A=0
     end do
  end do
  i=1
  j=m
  A(1,m)=zahler
  outer: do
     
     if (((i-1)>0) .and. ((j-1)>0) .and. (A(i-1,j-1) == 0)) then
        zahler=zahler+1
        A(i-1,j-1)=zahler
        i=i-1
        j=j-1
     else
        inner: do
           if (zahler > n**2) exit outer
           select case (i == 1)
           case (.true.)
              select case (j == 1)
              case (.true.)
                 i=2
                 j=1
              case default
                 i=n
                 j=j-1
              end select
           case default
              select case (j == 1)
              case (.true.)
                 i=i-1
                 j=n
              case default
                 i=i+1
              end select
           end select
           if (A(i,j) == 0) then
              zahler = zahler + 1
              A(i,j) = zahler
              exit inner
           end if
        end do inner
     end if
  end do outer
  ! i,j bei 1 oder 0 beginnen? Wenn nicht anders bei allocate oder bei deklaration wenn statisches feld definiert 1
  do j=1,2
     write (*,*)
  end do

  do i=1,n
     write( UNIT=stri1, FMT='(I2.2)' )  n   ! Anzahl Zahlen in Zeile !
    ! write( UNIT=stri2, FMT='(I2.2)' )  3  ! Tabulatorpos. !
    ! write( UNIT=*, FMT='(T'//stri2//','//stri1//'I6)' )  A(i,1:n)
write(*,'(T3,'//stri1//'I6)') A(i,1:n)
     do j=1,2
        write (*,*)
     end do
  end do

  write (*,*) 'Zeilensummen'

  do i=1,n
     summe = 0
     do j=1,n
        summe=summe+A(i,j) !summe=sum(i,:)
     end do
            write (*,*) i
     
     write (*,*) summe
     if ( i == 1 ) then !vor die schleife
        test = summe
     else
        if ( test == summe ) then
           write (*,*) 'Bis hier hin sind alle Summen gleich'
        end if
     end if
  end do


  write (*,*) 'Spaltensummen'

    do i=1,n
     summe = 0
     do j=1,n
        summe=summe+A(j,i)
     end do
     write (*,*) summe
     if ( test == summe ) then
        write (*,*) 'Bis hier hin sind alle Summen gleich'
     end if
     
  end do
  
  summe = 0
  do j=1,n
     summe=summe+A(j,j)
  end do
  write (*,*) 'Diagonale rechts unten nach links oben',summe
  if ( test == summe ) then
     write (*,*) 'Bis hier hin sind alle Summen gleich'
  end if
  summe = 0
  do j=1,n
     summe=summe+A(n+1-j,j)
  end do
  write (*,*) 'Diagonale links unten nach rechts oben',summe
  if ( test == summe ) then
     write (*,*) 'Bis hier hin sind alle Summen gleich'
  end if
  write (*,*) 'n(n^2+1)/2 =',n*(n**2+1)/2

  deallocate(A)
end do out

end program magische_quadrate
  

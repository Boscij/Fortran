module mod_a17
  implicit none

  private
  public :: aus, Gewinn
  interface aus
     module procedure ausgabe
  end interface aus

    interface Gewinn
     module procedure Gewinn_Nr
  end interface Gewinn
contains
  
  subroutine ausgabe (matrix)
    integer, dimension (:,:), intent(in) :: matrix
    character(len = 50) :: text, cc,ss
    integer :: zeilen, spalten, i, j, k,t
    zeilen = size(matrix,1)
    spalten = size(matrix,2)

    do i=zeilen,1,-1
       text = ''
       do j=1,spalten
          select case(matrix(i,j))
          case (-1)
             cc = 'O'
          case (0)
             cc = '_'
          case (1)
             cc = 'X'
          end select
          text = trim(text)//' '//cc
       end do
       write (*,*) text
    end do
    write (*,'(A2)',advance='no') '  ' 
    do i = 1,2*spalten-1
       write (*,fmt='(A1)',advance='no') '='
    end do
    write (*,*) 
    write (*,'(A2)',advance='no') '  ' 
    do i = 1,spalten
       write (*,fmt='(I1A1)',advance='no') i,' '
    end do
    write (*,*) 
  end subroutine ausgabe
  
  function Gewinn_Nr (matrix)
    integer, dimension (:,:), intent(in) :: matrix
    integer :: zeilen, spalten, i, Gewinn_Nr,j , k, u
    Gewinn_Nr = 0
    zeilen = size(matrix,1)
    spalten = size(matrix,2)
 !   write (*,*) zeilen
    ! ueberpruefen der Zeilen
    if (spalten >= 4) then
       do i = 1,zeilen
          do j = 1, spalten-3
!write (*,*) sum(matrix(i,j:j+3))
             if (abs(sum(matrix(i,j:j+3))) == 4) then
                Gewinn_Nr = matrix(i,j)
             end if
          end do
       end do
    end if
    ! Ueberpruefen der Spalten
        if (zeilen >= 4) then
       do i = 1,spalten
          do j = 1, zeilen-3
    !         write (*,*) sum(matrix(j:j+3,i))
             if (abs(sum(matrix(j:j+3,i))) == 4) then
                Gewinn_Nr = matrix(j,i)
             end if
          end do
       end do
    end if
    ! Diagonalen links unten rechts oben
    k=0
    if (zeilen >= 4 .and. spalten >= 4) then
       do i = 1,spalten-3
          do j = 1, zeilen-3
             k = 0 	! ABS(SUM((/ (matrix(j-1+u, i-1+u), u = 0, 4) /)))
             do u = 1,4
                k = k + matrix(j-1+u,i-1+u)
   !             write(*,*) k, 'h'
             end do
  !           write (*,*) k
             if (abs(k) == 4) then
                Gewinn_Nr = matrix(j,i)
             end if
          end do
       end do
    end if

    ! Diagonalen rechts unten links oben
    k=0
    if (zeilen >= 4 .and. spalten >= 4) then
       do i = 4,spalten
          do j = 1, zeilen-3
             k = 0
             do u = 1,4
                k = k + matrix(j-1+u,i+1-u)
 !               write(*,*) k, 'h'
             end do
!             write (*,*) k
             if (abs(k) == 4) then
                Gewinn_Nr = matrix(j,i)
             end if
          end do
       end do
    end if

    if (Gewinn_Nr /= 0) then
       select case(Gewinn_Nr)
          case (-1)
             write (*,*) 'Der Gewinner ist: O'
          case (1)
             write (*,*) 'Der Gewinner ist: X'                          
          end select
       end if
  end function Gewinn_Nr
end module mod_a17

  



  


module schach
  implicit none
  
contains
  
  integer function test(vector,field)
    integer,dimension(:) :: vector
    integer,dimension(:,:) :: field
    integer                :: m,n,j,i
    
 ! leere Feld mit Einsen an den Stellen füllen, 
 ! welche von Damen bedroht werden
 ! vector(3) = 5
 ! vector(8) = 8
  
    test = 0
    do i = 1,size(vector)
       if (vector(i) /= 0) then
          do j = 1,size(vector)
             ! horizontal, vertikal
             if(j /= i) then
                if(field(vector(i),j) == -1) test = 1
             end if

             if (j /= vector(i))then
                if(field(j,i) == -1) test = 1
             end if
          end do

          m = i
          n = vector(i)
          do
             m = m + 1
             n = n + 1
             !                  write(*,*) n,m,k
             if ((m > size(vector)) .or. (n > size(vector))) exit
             ! rechts unten
             if(field(n,m) == -1) test = 1
          end do
          m = i
          n = vector(i)
          do
             m = m - 1
             n = n - 1
             ! links oben
             if ((m == 0) .or. (n == 0)) exit
             if(field(n,m) == -1) test = 1
          end do
          m = i
          n = vector(i)
          do
             m = m - 1
             n = n + 1
             ! rechts oben
             if ((m == 0 ) .or. (n > size(vector))) exit
             if(field(n,m) == -1) test = 1
          end do
          m = i
          n = vector(i)
          do
             m = m + 1
             n = n - 1
             ! links unten
             if ((m > size(vector)) .or. (n == 0)) exit
             if(field(n,m) == -1) test = 1
          end do
       end if
    end do
  end function test
  
  recursive subroutine next(k,vector,field,loesungen) ! k ist die
    !  Spalte in der verschoben werden soll.
    integer    :: k, i, j, z, dimension1, flag, loesungen
    integer, dimension(:)   :: vector
    integer, dimension(:,:) :: field

    dimension1=size(vector)
    
    do

       if (vector(k) /= 0) then
          field(vector(k),k) = 0
       end if
       if (vector(k) /= dimension1) then
          vector(k) = vector(k) + 1
          field(vector(k),k) = -1
       else
          if((vector(1)==dimension1).and.(k == 1)) then
!             write(*,*) "loesungen:",loesungen
             exit
          end if
          field(vector(k),k) = 0
          vector(k) = 0
          call next(k-1,vector,field,loesungen)
          exit
       end if

10     format(8(2X,I2))

       ! write(*,*)
       ! write(*,*) flag, k, vector
       ! write(*,10) ((field(i,j),j=1,dimension1),i=1,dimension1)
       ! write(*,*)
       flag = test(vector,field)
       if (flag == 0) then
          if (k /= dimension1)then
             call next(k+1,vector,field,loesungen)
             exit
          else
             
             ! write(*,*) flag, k, vector! Loesungen ausgeben format anpassen
             ! write(*,*)
             ! write(*,10) ((field(i,j),j=1,dimension1),i=1,dimension1)
             ! write(*,*)
             loesungen = loesungen + 1
!             write(*,*) "Loesung",loesungen
             call next(k,vector,field,loesungen)
             exit
          end if
          
       else if ((flag /= 0) .and. (vector(k) /= dimension1)) then
          call next(k,vector,field,loesungen)
          exit
       else if (flag /= 0) then

          if((vector(1)==dimension1).and.(k == 1)) then
!             write(*,*) "loesungen:",loesungen
             exit
          end if
      
          field(vector(k),k) = 0    
          vector(k) = 0
          call next(k-1,vector,field,loesungen)
          exit
       end if
    end do
  end subroutine next
     
end module schach


program matr_mul
  use matr
  implicit none
  real, dimension(:,:), allocatable :: A, B, M1, C1,C2,C3,C4
  integer              :: k, n, i, j, clock_rate,clock_max,t1,t2,flag,ios
  real                 :: time1,time2,test


  do
     write(*,*)"Matrix einlesen(1) oder generieren(0)"
     read(*,*) k
     if (k == 0) then
        write(*,*) "Bitte Potenz eingeben"
        read(*,*) k
        n = 2**k
        allocate(A(n,n))
        allocate(B(n,n))
        allocate(C1(n,n))
        allocate(C2(n,n))
        allocate(C3(n,n))
        allocate(C4(n,n))
  
        A =  reshape ((/ ((40.0/( i +j -1.0) , i =1 ,n) , j =1, n) /), (/n,n/))
        B =  reshape ((/ ((40.0/( i +j -1.0) , i =1 ,n) , j =1, n) /), (/n,n/))
         exit
     else if(k == 1) then
        open(20,file="matrix.dat",status="old",action="read",iostat=ios)
        if(ios/=0) then
           write(*,*) "Fehler beim oeffnen von matrix.dat"
           stop
        end if
         
        n=4
        allocate(A(n,n))
        allocate(B(n,n))
        allocate(C1(n,n))
        allocate(C2(n,n))
        allocate(C3(n,n))
        allocate(C4(n,n))
        do i=1,4
           do j=1,4
              read(20,*) A(i,j)
              B(i,j)=A(i,j)
           end do
        end do
10      format (4(2X,F6.1))
        write(0,10) ((A(i,j),i=1,4),j=1,4)
        exit
     end if
  end do


  
  


  
  call cpu_time(time1)
  C1= matmul(A,B)
  call cpu_time(time2)
  write(*,*) "Matmul_zeit: ", time2-time1

  ! call cpu_time(time1)
  ! C4= strassen_matmul_2(A,B)
  ! call cpu_time(time2)
  ! write(*,*) "strassen_2_zeit: ", time2-time1
  
  call cpu_time(time1)
  C2= strassen_matmul(A,B)
  call cpu_time(time2)
  write(*,*) "Strassen_zeit: ", time2-time1
  
  call cpu_time(time1)  
  C3= herkoemmlich(A,B)
  call cpu_time(time2)
  write(*,*) "Herkoemmlich_zeit: ", time2-time1
  
  call system_clock ( t1, clock_rate, clock_max )
  call cpu_time(time1)
  C4= strassen_matmul_2(A,B)
  call cpu_time(time2)
  call system_clock ( t2, clock_rate, clock_max )

  write(*,*) "strassen_2_zeit: ","cpu: ", time2-time1, "sekunden",& 
       & real(t2-t1)/real(clock_rate)

  write(*,*) "Test ob Ergebnis gleich(nur bis zur ersten Nachkommastelle)"

  do i=1,n
     do j=1,n
        call round(C1(i,j))
        call round(C2(i,j))
        call round(C3(i,j))
     end do
  end do
  
   flag=0
  do i=1,n
     do j=1,n
       if((C1(i,j)-C2(i,j))/=0 .or. (C2(i,j)-C3(i,j))/=0 .or. &
            & (C1(i,j)-C3(i,j))/=0) then
          write(*,*) "Ungleich"
          flag=1
       end if
     end do
  end do
  if(flag == 0) write(*,*) "Matrizen liefern gleiches Ergebnis"


  ! write(0,10) ((C1(i,j),i=1,4),j=1,4)
  ! write(*,*)
  ! write(0,10) ((C2(i,j),i=1,4),j=1,4)
  ! write(*,*)
  ! write(0,10) ((C3(i,j),i=1,4),j=1,4)
  ! write(*,*)
  ! write(0,10) ((C4(i,j),i=1,4),j=1,4)
  
    

  deallocate(A)
  deallocate(B)
  deallocate(C1)
  deallocate(C2)
  deallocate(C3)
  deallocate(C4)
contains
  subroutine round(R)
    real :: R
    R = real(int(R*10))/10.0
  end subroutine round
  
! Wie besser runden?  

  
end program matr_mul









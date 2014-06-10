module matr
  implicit none

contains
  recursive function strassen_matmul(A,B) result(C)
    real, dimension(:,:)  :: A, B
    real, dimension(:,:), allocatable  :: A11,A12,A21,A22, B11,B12,B21,B22, &
         & M1,M2,M3,M4,M5,M6,M7,C
    integer               :: n 
    n = size(A,1)        
    allocate(C(n,n))
    allocate(M1(n/2,n/2))
    allocate(M2(n/2,n/2))
    allocate(M3(n/2,n/2))
    allocate(M4(n/2,n/2))
    allocate(M5(n/2,n/2))
    allocate(M6(n/2,n/2))
    allocate(M7(n/2,n/2))
    
    allocate(A11(n/2,n/2))
    allocate(A12(n/2,n/2))
    allocate(A21(n/2,n/2))
    allocate(A22(n/2,n/2))

    allocate(B11(n/2,n/2))
    allocate(B12(n/2,n/2))
    allocate(B21(n/2,n/2))
    allocate(B22(n/2,n/2))


    if (n /= 2) then
       A11=A(1:n/2,1:n/2)
       A12=A(n/2+1:n,1:n/2)
       A21=A(1:n/2,n/2+1:n)
       A22=A(n/2+1:n,n/2+1:n)

       B11=B(1:n/2,1:n/2)
       B12=B(n/2+1:n,1:n/2)
       B21=B(1:n/2,n/2+1:n)
       B22=B(n/2+1:n,n/2+1:n)
       
       M1 = strassen_matmul(A12-A22,B21+B22)
       M2 = strassen_matmul(A11+A22,B11+B22)
       M3 = strassen_matmul(A11-A21,B11+B12)
       M4 = strassen_matmul(A11+A12,B22)
       M5 = strassen_matmul(A11,B12-B22)
       M6 = strassen_matmul(A22,B21-B11)
       M7 = strassen_matmul(A21+A22,B11)

    else
       M1 = (A(1,2)-A(2,2))*(B(2,1)+B(2,2))
       M2 = (A(1,1)+A(2,2))*(B(1,1)+B(2,2))
       M3 = (A(1,1)-A(2,1))*(B(1,1)+B(1,2))
       M4 = (A(1,1)+A(1,2))*(B(2,2))
       M5 = (A(1,1))*(B(1,2)-B(2,2))
       M6 = (A(2,2))*(B(2,1)-B(1,1))
       M7 = (A(2,1)+A(2,2))*(B(1,1))
    end if

    C(1:n/2,1:n/2) = M1+M2-M4+M6
    C(1:n/2,n/2+1:n) = M4 + M5
    C(n/2+1:n,1:n/2) = M6 + M7
    C(n/2+1:n,n/2+1:n) = M2-M3+M5-M7

!    deallocate(C)
    deallocate(M1)
    deallocate(M2)
    deallocate(M3)
    deallocate(M4)
    deallocate(M5)
    deallocate(M6)
    deallocate(M7)

    deallocate(A11)
    deallocate(A12)
    deallocate(A21)
    deallocate(A22)

    deallocate(B11)
    deallocate(B12)
    deallocate(B21)
    deallocate(B22)
    

    
  end function strassen_matmul

  recursive function strassen_matmul_2(A,B) result(C)
    real, dimension(:,:)  :: A, B
    real, dimension(:,:), allocatable  :: M1,M2,M3,M4,M5,M6,M7,C
    integer               :: n 
    n = size(A,1)        
    allocate(C(n,n))
    allocate(M1(n/2,n/2))
    allocate(M2(n/2,n/2))
    allocate(M3(n/2,n/2))
    allocate(M4(n/2,n/2))
    allocate(M5(n/2,n/2))
    allocate(M6(n/2,n/2))
    allocate(M7(n/2,n/2))

    if (n /= 2) then
       
       M1 = strassen_matmul(A(n/2+1:n,1:n/2)-A(n/2+1:n,n/2+1:n), & 
            & B(1:n/2,n/2+1:n)+B(n/2+1:n,n/2+1:n))
       
       M2 = strassen_matmul(A(1:n/2,1:n/2)+A(n/2+1:n,n/2+1:n),& 
            & B(1:n/2,1:n/2)+B(n/2+1:n,n/2+1:n))
       M3 = strassen_matmul(A(1:n/2,1:n/2)-A(1:n/2,n/2+1:n),&
            & B(1:n/2,1:n/2)+B(n/2+1:n,1:n/2))
       M4 = strassen_matmul(A(1:n/2,1:n/2)+A(n/2+1:n,1:n/2),&
            & B(n/2+1:n,n/2+1:n))
       M5 = strassen_matmul(A(1:n/2,1:n/2),B(n/2+1:n,1:n/2)-&
            & B(n/2+1:n,n/2+1:n))
       M6 = strassen_matmul(A(n/2+1:n,n/2+1:n),B(1:n/2,n/2+1:n)- &
            & B(1:n/2,1:n/2))
       M7 = strassen_matmul(A(1:n/2,n/2+1:n)+A(n/2+1:n,n/2+1:n),& 
            & B(1:n/2,1:n/2))

    else
       M1 = (A(1,2)-A(2,2))*(B(2,1)+B(2,2))
       M2 = (A(1,1)+A(2,2))*(B(1,1)+B(2,2))
       M3 = (A(1,1)-A(2,1))*(B(1,1)+B(1,2))
       M4 = (A(1,1)+A(1,2))*(B(2,2))
       M5 = (A(1,1))*(B(1,2)-B(2,2))
       M6 = (A(2,2))*(B(2,1)-B(1,1))
       M7 = (A(2,1)+A(2,2))*(B(1,1))
    end if

    C(1:n/2,1:n/2) = M1+M2-M4+M6
    C(1:n/2,n/2+1:n) = M4 + M5
    C(n/2+1:n,1:n/2) = M6 + M7
    C(n/2+1:n,n/2+1:n) = M2-M3+M5-M7

    !   deallocate(C)
    ! wann und wie dieses C deallocieren ??
    deallocate(M1)
    deallocate(M2)
    deallocate(M3)
    deallocate(M4)
    deallocate(M5)
    deallocate(M6)
    deallocate(M7)

  end function strassen_matmul_2



  
  function herkoemmlich(A,B)
    real, dimension(:,:), allocatable :: herkoemmlich
    real, dimension(:,:),intent(in)   :: A, B
    integer                           :: n, i, j, k
    n = size(A,1)
    allocate(herkoemmlich(n,n))

    do i = 1, n       
       do j = 1, n
          herkoemmlich(i,j) = 0
          do k = 1, n
             herkoemmlich(i,j)= herkoemmlich(i,j) + A(i,k)*B(k,j)
          end do
       end do
    end do
  end function herkoemmlich

  
    
  
end module matr











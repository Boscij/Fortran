program a14
  use triloge
  implicit none 
  type(trilog),dimension(3):: A
  integer :: i,k,l,z
 A(1) = false
 A(2) = maybe
 A(3) = true


  do i=1,3
     do k=1,3
        write (*,*) text(A(i)), ' und ', text(A(k)), ' ergibt ',&
             & text(A(i).and.A(k))
     end do
  end do
  write (*,*)
  do i=1,3
     do k=1,3
        write (*,*) text(A(i)), ' oder ', text(A(k)), ' ergibt ',&
             & text(A(i).or.A(k))
     end do
  end do
  write (*,*)
  do k=1,3
     write (*,*) ' nicht ', text(A(k)), ' ergibt ',&
          & text(.not.A(k))
  end do
  
write (*,*) 
z=0
  do i=1,3
     do k=1,3
        do l=1,3
           write (*,*) ((A(i).and.(A(k).or.A(l)))&
                &==((A(i).and.A(k)).or.(A(i).and.A(l))))
           write (*,*)  ((A(i).or.(A(k).and.A(l)))&
                &==((A(i).or.A(k)).and.(A(i).or.A(l))))
        !   z=z+1
        !   write (*,*) 'Durchlauf', z
        end do
        write (*,*) ((.not.(A(i).and.A(k))) ==&
             & (.not.(A(i)).or.(.not.(A(k)))))
        
        write (*,*) ((.not.(A(i).or.A(k))) ==&
             & (.not.(A(i)).and.(.not.(A(k)))))
     end do
  end do
end program a14

  PROGRAM  PtrTrial      ! POINTERs: association status and operations !
    IMPLICIT NONE
    INTEGER, POINTER :: p, q, r
    INTEGER, TARGET  :: i, k


    NULLIFY (p, q)
    WRITE (*,*) " Is p ASSOCIATED? ", ASSOCIATED(p)
    WRITE (*,*) " Is q ASSOCIATED? ", ASSOCIATED(q)
    WRITE (*,*) " Is r ASSOCIATED? ", ASSOCIATED(r) ! ARBITRARY result !
    r => p      ! equivalent to NULLIFY(r) since p is NULL 
    WRITE (*,*) " Is r ASSOCIATED? ", ASSOCIATED(r) ! reliable result  !
    ALLOCATE (p)
    p = 5
!!! q = p       ! ILLEGAL value assignment to nonexistent target object 
    q => p      ! p and q now have same target 
    q = p*q-3   ! new value for this target, automatic dereferencing 
    WRITE (*,*) " Value of p = ", p
    WRITE (*,*) " Value of q = ", q
    DEALLOCATE (q)                   ! creates DANGLING POINTER p 
    WRITE (*,*) " Value of p = ", p  ! ILLEGAL access via dangling ptr p
    p = 17                           ! ILLEGAL access via dangling ptr p
    WRITE (*,*) " Value of p = ", p  ! ILLEGAL access via dangling ptr p
    r => p                           ! makes r a DANGLING POINTER, too
    WRITE (*,*) " Value of r = ", r  ! ILLEGAL access via dangling ptr r
    WRITE (*,*) " Is p ASSOCIATED? ", ASSOCIATED(p)
    WRITE (*,*) " Is q ASSOCIATED? ", ASSOCIATED(q)
    WRITE (*,*) " Is r ASSOCIATED? ", ASSOCIATED(r)


    ALLOCATE (r)
    r = 13
    p => i
    q => k
    WRITE (*,*) " Is p ASSOCIATED with i? ", ASSOCIATED(p,i)
    WRITE (*,*) " Is p ASSOCIATED with k? ", ASSOCIATED(p,k)
    WRITE (*,*) " Is q ASSOCIATED with i? ", ASSOCIATED(q,i)
    WRITE (*,*) " Is q ASSOCIATED with k? ", ASSOCIATED(q,k)
    i = 100
    q = 200
    p = p + q + r
    k = i + k + r
    WRITE (*,*) " Value of p = ", p
    WRITE (*,*) " Value of i = ", i
    WRITE (*,*) " Value of q = ", q
    WRITE (*,*) " Value of k = ", k
!!! DEALLOCATE (p, q)           ! ILLEGAL attempt to deallocate normal !
                                ! named (non-heap) variables i and k   !
    DEALLOCATE (r)
  END PROGRAM  PtrTrial


PROGRAM simple_matrix
IMPLICIT NONE
INTEGER:: a(7,7),i,j

DO i=1,7
  a(i,i)=i+i
  DO j=i+1,7
    a(i,j)=i+j
    a(j,i)=a(i,j)
  END DO
END DO

WRITE(*,*) MINLOC(a)
WRITE(*,*) MAXLOC(a)

! DO i=1,7
!   DO j=1,7
!     a(i,j)=i+j
!   END DO
! END DO

! DO i=1,7
!   DO j=1,7
!     IF(i.eq.j)THEN
!       a(i,j)=1
!     ELSE
!       a(i,j)=0
!     END IF
!   END DO
! END DO


DO i=1,7
  WRITE(*,*) (a(i,j),j=1,7)
END DO




END PROGRAM simple_matrix

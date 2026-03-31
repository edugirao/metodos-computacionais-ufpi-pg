PROGRAM read_example
IMPLICIT NONE
INTEGER:: i
REAL(KIND=8):: x,y
OPEN(UNIT=1,FILE='data.dat')
DO i=1,5
  READ(1,*) x,y
  WRITE(*,*) 'f(',x,')=',y
END DO
CLOSE(UNIT=1)
END PROGRAM read_example

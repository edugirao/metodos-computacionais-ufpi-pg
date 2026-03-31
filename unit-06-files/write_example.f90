PROGRAM write_example
IMPLICIT NONE
INTEGER:: i
REAL(KIND=8):: x,y
OPEN(UNIT=1,FILE='data2.dat')
DO i=1,5
  x=2.0*i
  y=x*x
  WRITE(1,*) x,y
END DO
CLOSE(UNIT=1)
END PROGRAM write_example

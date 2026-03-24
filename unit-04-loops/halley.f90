PROGRAM halley
IMPLICIT NONE
INTEGER:: i,year

year=1986
DO i=1,10
  year=year+76
  WRITE(*,*) 'Aparição em ',year
END DO


END PROGRAM halley

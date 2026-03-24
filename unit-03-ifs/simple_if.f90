PROGRAM simple_if
IMPLICIT NONE
INTEGER:: a,b,c


WRITE(*,*) 'Entre com dois valores inteiros:'
READ(*,*) a,b
c=0
IF(b.gt.a)THEN
  c=a+b
ELSE IF(a.gt.b)THEN
  c=a-b
ELSE
  c=a*b
END IF
WRITE(*,*) 'c=',c

END PROGRAM simple_if

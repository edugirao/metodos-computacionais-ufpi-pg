PROGRAM q5_1
IMPLICIT NONE
INTEGER:: a

WRITE(*,*) 'Digite um número inteiro:'
READ(*,*) a
IF(a.gt.0)THEN
  WRITE(*,*) 'O número é positivo.'
ELSE IF(a.lt.0)THEN
  WRITE(*,*) 'O número é negativo.'
ELSE !IF(a.eq.0)THEN
  WRITE(*,*) 'O número é nulo.'
END IF

END PROGRAM q5_1

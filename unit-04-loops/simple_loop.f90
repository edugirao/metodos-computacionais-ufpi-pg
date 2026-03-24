PROGRAM simple_loop
IMPLICIT NONE
INTEGER:: i,quadrado

DO i=10,1,-1
  quadrado=i*i
  WRITE(*,*) 'O quadrado de',i,'é igual a',quadrado,'.'
END DO

END PROGRAM simple_loop

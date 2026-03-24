PROGRAM reaction
IMPLICIT NONE
REAL(KIND=8):: temp,t,q,k,yield
! Input temperature
WRITE(*,*) 'Enter the reaction temperature:'
READ(*,*) temp
! q value
q=2000.0D0/(temp+273.16D0)
! k value
k=DEXP(-q)
! Loop no tempo
t=0.0D0
DO 
  ! Rendimento
  yield=1.0D0-DEXP(-k*t)
  IF(yield.gt.0.95D0) EXIT
  WRITE(*,*) 'O Rendimento após',t/60.0D0,'é de',yield*100.0D0,'%' 
  t=t+60.0D0
  
END DO



END PROGRAM reaction

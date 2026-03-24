PROGRAM q5_7
IMPLICIT NONE
REAL(KIND=8):: income,tax

WRITE(*,*) 'Enter you income:'
READ(*,*) income

IF(income.le.5.0D3)THEN
  ! Tipo a
  tax=0.0D0
ELSE IF(income.gt.2.0D4)THEN
  ! Tipo c
  tax=15000.0D0*0.25D0+(income-2.0D4)*0.32D0
ELSE
  ! Tipo b
  tax=(income-5.0D3)*0.25D0
END IF
WRITE(*,*) 'Tax to be paid=',tax

END PROGRAM q5_7

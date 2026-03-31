PROGRAM formatos
IMPLICIT NONE
INTEGER:: i,j
REAL(KIND=8):: x,y

i=53
j=52
WRITE(*,'(I5,I5)') i,j
WRITE(*,'(I5,I3)') i,j
WRITE(*,'(I5,I2)') i,j
WRITE(*,'(2I5)') i,j
i=53
j=5748542
WRITE(*,'(2I5)') i,j
i=53
j=52
WRITE(*,'(I2,1X,I2)') i,j
i=53
j=520
WRITE(*,'(I0,1X,I0)') i,j
x=4.52D0
WRITE(*,'(F8.3)') x
x=4000.52D0
WRITE(*,'(F8.3)') x
x=14000.52D0
WRITE(*,'(F8.3)') x
x=-4000.52D0
WRITE(*,'(F8.3)') x
x=-4000.52D0
WRITE(*,'(F0.3)') x

x=4.52D0
WRITE(*,'(E8.3)') x
x=4.52D0
WRITE(*,'(E11.3)') x
x=4.52D0
WRITE(*,'(E11.3)') x

END PROGRAM formatos

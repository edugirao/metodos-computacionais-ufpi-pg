PROGRAM first_derivative
IMPLICIT NONE
INTEGER:: n,i
REAL(KIND=8):: a,b,h,f,dx
REAL(KIND=8),ALLOCATABLE:: x(:),y(:),dya(:),dyn(:)

WRITE(*,*) 'Irei calcular a derivada de'
WRITE(*,*) 'f(x)=DSIN(DEXP(x))'
WRITE(*,*) 'Em um intervalo de valores de x'
WRITE(*,*) 'desde a até b com n pontos.'
WRITE(*,*) 'a='
READ(*,*) a
WRITE(*,*) 'b='
READ(*,*) b
WRITE(*,*) 'n='
READ(*,*) n
WRITE(*,*) 'Entre com o valor de h'
WRITE(*,*) 'para o método da diferença central:'
READ(*,*) h

! Calcular os pontos x do grid
ALLOCATE(x(n))
dx=(b-a)/DFLOAT(n-1)
DO i=1,n
  x(i)=a+(i-1)*dx
END DO

! Calculando o valor da função no grid
ALLOCATE(y(n))
DO i=1,n
  y(i)=f(x(i))
END DO

! Calculando a derivada analítica
ALLOCATE(dya(n))
DO i=1,n
  dya(i)=(DEXP(x(i)))*DCOS(DEXP(x(i)))
END DO

! Calculando a derivada numérica
ALLOCATE(dyn(n))
DO i=1,n
  dyn(i)=(f(x(i)+h)-f(x(i)-h))/(2.0D0*h)
END DO

! Output para a função
OPEN(UNIT=1,FILE='function.dat')
DO i=1,n
  WRITE(1,*) x(i),y(i)
END DO
WRITE(1,*)
CLOSE(UNIT=1)

! Output para a derivada analítica
OPEN(UNIT=2,FILE='derivative-1a.dat')
DO i=1,n
  WRITE(2,*) x(i),dya(i)
END DO
WRITE(2,*)
CLOSE(UNIT=2)

! Output para a derivada numérica
OPEN(UNIT=3,FILE='derivative-1n.dat')
DO i=1,n
  WRITE(3,*) x(i),dyn(i)
END DO
WRITE(3,*)
CLOSE(UNIT=3)


DO i=1,n
  print*,x(i),100.0*ABS(dya(i)-dyn(i))/ABS(dya(i))
END DO






END PROGRAM first_derivative

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION f(x)
IMPLICIT NONE
REAL(KIND=8):: x,f
f=DSIN(DEXP(x))
END FUNCTION f

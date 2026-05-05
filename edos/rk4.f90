PROGRAM rk4
IMPLICIT NONE
INTEGER:: i,n
REAL(KIND=8):: a,b,h,f,k1,k2,k3,k4
REAL(KIND=8),ALLOCATABLE:: x(:),y(:),ya(:)
! Equação
! dy/dx=1-x+4*y=f(x,y)
! y(0)=1

! Grid
a=0.0D0
b=1.0D0
n=10001
h=(b-a)/DFLOAT(n-1)
ALLOCATE(x(n))
DO i=1,n
  x(i)=a+DFLOAT(i-1)*h
END DO

! Condição inicial
ALLOCATE(y(n))
y(1)=1.0D0

! Integração por RK2
DO i=1,n-1
  k1=h*f(x(i),y(i))
  k2=h*f(x(i)+0.5D0*h,y(i)+0.5D0*k1)
  k3=h*f(x(i)+0.5D0*h,y(i)+0.5D0*k2)
  k4=h*f(x(i)+h,y(i)+k3)
  y(i+1)=y(i)+(k1+k2+k2+k3+k3+k4)/6.0D0
END DO

OPEN(UNIT=1,FILE='rk4.dat')
DO i=1,n
  WRITE(1,*) x(i),y(i)
END DO
WRITE(1,*)
CLOSE(UNIT=1)

OPEN(UNIT=1,FILE='analytical.dat')
ALLOCATE(ya(n))
DO i=1,n
  ya(i)=0.25D0*x(i)-3.0D0/16.0D0+(19.0D0/16.0D0)*DEXP(4.0D0*x(i))
  WRITE(1,*) x(i),ya(i)
END DO
WRITE(1,*)
CLOSE(UNIT=1)


OPEN(UNIT=1,FILE='rk4-error.dat')
DO i=2,n
  WRITE(1,*) x(i),100*ABS(y(i)-ya(i))/ABS(ya(i))
END DO
WRITE(1,*)
CLOSE(UNIT=1)

END PROGRAM rk4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION f(x,y)
IMPLICIT NONE
REAL(KIND=8):: f,x,y
f=1.0D0-x+4.0D0*y
END FUNCTION f

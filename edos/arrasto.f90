PROGRAM arrasto
IMPLICIT NONE
INTEGER:: i,n
REAL(KIND=8):: m,b,v0,tau,h,ta,tb,f,k1,k2,k3,k4
REAL(KIND=8),ALLOCATABLE:: t(:),v(:)

! Parâmetros iniciais
WRITE(*,*) 'Entre com a massa do corpo em kg:'
READ(*,*) m
WRITE(*,*) 'Entre com a constante b em kg/s:'
READ(*,*) b
WRITE(*,*) 'Entre com a velocidade inicial em m/s:'
READ(*,*) v0

! Escala de tempo
tau=m/b

! Intervalo de tempo
ta=0.0D0
tb=5.0D0*tau

! Grid no tempo
n=10001
h=(tb-ta)/DFLOAT(n-1)
ALLOCATE(t(n))
DO i=1,n
  t(i)=ta+DFLOAT(i-1)*h
END DO

! Condição inicial
ALLOCATE(v(n))
v(1)=v0

! Integração por rk4
DO i=1,n-1
  k1=h*f(t(i),v(i),b,m)
  k2=h*f(t(i)+0.5D0*h,v(i)+0.5D0*k1,b,m)
  k3=h*f(t(i)+0.5D0*h,v(i)+0.5D0*k2,b,m)
  k4=h*f(t(i)+h,v(i)+k4,b,m)
  v(i+1)=v(i)+(k1+k2+k2+k3+k3+k4)/6.0D0
END DO

OPEN(UNIT=1,FILE='arrasto-rk4.dat')
OPEN(UNIT=2,FILE='arrasto-analytical.dat')
DO i=1,n
  WRITE(1,*) t(i),v(i)
  WRITE(2,*) t(i),v0*DEXP(-t(i)/tau)
END DO
WRITE(1,*)
WRITE(2,*)
CLOSE(UNIT=1)
CLOSE(UNIT=2)
END PROGRAM arrasto

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION f(t,v,b,m)
IMPLICIT NONE
REAL(KIND=8):: f,t,v,b,m
f=-b*v/m
END FUNCTION f

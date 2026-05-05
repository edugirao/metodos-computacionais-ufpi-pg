PROGRAM arrasto3
IMPLICIT NONE
INTEGER:: i,n
REAL(KIND=8):: m,b,v0,tau,h,ta,tb,fv,fy,l
REAL(KIND=8):: k1v,k2v,k3v,k4v,k1y,k2y,k3y,k4y
REAL(KIND=8),ALLOCATABLE:: t(:),v(:),y(:)

! Parâmetros iniciais
WRITE(*,*) 'Entre com a massa do corpo em kg:'
READ(*,*) m
WRITE(*,*) 'Entre com a constante b em kg/s:'
READ(*,*) b
WRITE(*,*) 'Entre com a altura inicial em relação ao solo (em m):'
READ(*,*) l

! Escala de tempo
tau=m/b

! Intervalo de tempo
ta=0.0D0
tb=40.0D0*tau

! Grid no tempo
n=10001
h=(tb-ta)/DFLOAT(n-1)
ALLOCATE(t(n))
DO i=1,n
  t(i)=ta+DFLOAT(i-1)*h
END DO

! Condições iniciais
ALLOCATE(v(n),y(n))
v(1)=0.0D0
y(1)=0.0D0

! Integração por rk4
DO i=1,n-1
  k1y=h*fy(t(i),y(i),v(i))
  k1v=h*fv(t(i),y(i),v(i),b,m)
  k2y=h*fy(t(i)+0.5D0*h,y(i)+0.5D0*k1y,v(i)+0.5D0*k1v)
  k2v=h*fv(t(i)+0.5D0*h,y(i)+0.5D0*k1y,v(i)+0.5D0*k1v,b,m)
  k3y=h*fy(t(i)+0.5D0*h,y(i)+0.5D0*k2y,v(i)+0.5D0*k2v)
  k3v=h*fv(t(i)+0.5D0*h,y(i)+0.5D0*k2y,v(i)+0.5D0*k2v,b,m)
  k4y=h*fy(t(i)+h,y(i)+k3y,v(i)+k3v)
  k4v=h*fv(t(i)+h,y(i)+k3y,v(i)+k3v,b,m)
  y(i+1)=y(i)+(k1y+k2y+k2y+k3y+k3y+k4y)/6.0D0
  v(i+1)=v(i)+(k1v+k2v+k2v+k3v+k3v+k4v)/6.0D0
  IF(y(i+1).gt.l) y(i+1)=l
END DO

OPEN(UNIT=1,FILE='arrasto3-v.dat')
OPEN(UNIT=2,FILE='arrasto3-y.dat')
DO i=1,n
  WRITE(1,*) t(i),v(i)
  WRITE(2,*) t(i),y(i)
END DO
WRITE(1,*)
WRITE(2,*)
CLOSE(UNIT=1)
CLOSE(UNIT=2)
END PROGRAM arrasto3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fv(t,y,v,b,m)
IMPLICIT NONE
REAL(KIND=8):: fv,t,v,b,m,y
fv=10.0D0-b*v/m
END FUNCTION fv

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fy(t,y,v)
IMPLICIT NONE
REAL(KIND=8):: fy,t,v,y
fy=v
END FUNCTION fy

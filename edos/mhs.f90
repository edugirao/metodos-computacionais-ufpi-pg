PROGRAM mhs
IMPLICIT NONE
INTEGER:: i,n
REAL(KIND=8):: m,k,w,tau,x0,v0,h,ta,tb,fv,fx,e0,ei
REAL(KIND=8):: k1v,k2v,k3v,k4v,k1x,k2x,k3x,k4x,error
REAL(KIND=8),ALLOCATABLE:: t(:),v(:),x(:)

! Parâmetros iniciais
m=1.0D0
WRITE(*,*) 'Massa do corpo em kg:',m
k=4.0D0*DACOS(-1.0D0)**2
WRITE(*,*) 'Constante da mola em N/m:',k
WRITE(*,*) 'Entre com a posição inicial em m:'
READ(*,*) x0
WRITE(*,*) 'Entre com a velocidade inicial em m/s:'
READ(*,*) v0

w=DSQRT(k/m)
tau=2.0D0*DACOS(-1.0D0)/w

! Intervalo de tempo
ta=0.0D0
tb=2.0D0*tau

! Grid no tempo
n=10001
h=(tb-ta)/DFLOAT(n-1)
ALLOCATE(t(n))
DO i=1,n
  t(i)=ta+DFLOAT(i-1)*h
END DO

! Condições iniciais
ALLOCATE(x(n),v(n))
x(1)=x0
v(1)=v0


! Integrando as equações por RK4
DO i=1,n-1
  k1x=h*fx(t(i),x(i),v(i))
  k1v=h*fv(t(i),x(i),v(i),w)
  k2x=h*fx(t(i)+0.5D0*h,x(i)+0.5D0*k1x,v(i)+0.5D0*k1v)
  k2v=h*fv(t(i)+0.5D0*h,x(i)+0.5D0*k1x,v(i)+0.5D0*k1v,w)
  k3x=h*fx(t(i)+0.5D0*h,x(i)+0.5D0*k2x,v(i)+0.5D0*k2v)
  k3v=h*fv(t(i)+0.5D0*h,x(i)+0.5D0*k2x,v(i)+0.5D0*k2v,w)
  k4x=h*fx(t(i)+h,x(i)+k3x,v(i)+k3v)
  k4v=h*fv(t(i)+h,x(i)+k3x,v(i)+k3v,w)
  x(i+1)=x(i)+(k1x+k2x+k2x+k3x+k3x+k4x)/6.0D0
  v(i+1)=v(i)+(k1v+k2v+k2v+k3v+k3v+k4v)/6.0D0
END DO



OPEN(UNIT=1,FILE='mhs-x.dat')
OPEN(UNIT=2,FILE='mhs-v.dat')
DO i=1,n
  WRITE(1,*) t(i),x(i)
  WRITE(2,*) t(i),v(i)
END DO
WRITE(1,*)
WRITE(2,*)
CLOSE(UNIT=1)
CLOSE(UNIT=2)


OPEN(UNIT=1,FILE='mhs-xa.dat')
OPEN(UNIT=2,FILE='mhs-va.dat')
DO i=1,n
  WRITE(1,*) t(i),x0*DCOS(w*t(i))+(v0/w)*DSIN(w*t(i))
  WRITE(2,*) t(i),-w*x0*DSIN(w*t(i))+v0*DCOS(w*t(i))
END DO
WRITE(1,*)
WRITE(2,*)
CLOSE(UNIT=1)
CLOSE(UNIT=2)

OPEN(UNIT=1,FILE='mhs-e-error.dat')
e0=0.5D0*m*v0**2+0.5D0*k*x0**2
DO i=2,n
  ei=0.5D0*m*v(i)**2+0.5D0*k*x(i)**2
  error=-LOG10(ABS((ei-e0)/e0))
  WRITE(1,*) t(i),error
END DO
WRITE(1,*)
CLOSE(UNIT=1)



END PROGRAM mhs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fv(t,x,v,w)
IMPLICIT NONE
REAL(KIND=8):: fv,t,x,v,w
fv=-w*w*x
END FUNCTION fv

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fx(t,x,v)
IMPLICIT NONE
REAL(KIND=8):: fx,t,v,x
fx=v
END FUNCTION fx

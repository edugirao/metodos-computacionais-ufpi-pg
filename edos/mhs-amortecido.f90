PROGRAM mhs_amortecido
IMPLICIT NONE
INTEGER:: i,n
REAL(KIND=8):: m,k,w,tau,x0,v0,h,ta,tb,fv,fx,e0,ei
REAL(KIND=8):: k1v,k2v,k3v,k4v,k1x,k2x,k3x,k4x,gamma0,b
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
WRITE(*,*) 'omega_0=',w
tau=2.0D0*DACOS(-1.0D0)/w
WRITE(*,*) 'Entre com gamma (em Hz):'
READ(*,*) gamma0
b=2.0D0*m*gamma0
WRITE(*,*) 'b=',b



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
  k1v=h*fv(t(i),x(i),v(i),w,gamma0)
  k2x=h*fx(t(i)+0.5D0*h,x(i)+0.5D0*k1x,v(i)+0.5D0*k1v)
  k2v=h*fv(t(i)+0.5D0*h,x(i)+0.5D0*k1x,v(i)+0.5D0*k1v,w,gamma0)
  k3x=h*fx(t(i)+0.5D0*h,x(i)+0.5D0*k2x,v(i)+0.5D0*k2v)
  k3v=h*fv(t(i)+0.5D0*h,x(i)+0.5D0*k2x,v(i)+0.5D0*k2v,w,gamma0)
  k4x=h*fx(t(i)+h,x(i)+k3x,v(i)+k3v)
  k4v=h*fv(t(i)+h,x(i)+k3x,v(i)+k3v,w,gamma0)
  x(i+1)=x(i)+(k1x+k2x+k2x+k3x+k3x+k4x)/6.0D0
  v(i+1)=v(i)+(k1v+k2v+k2v+k3v+k3v+k4v)/6.0D0
END DO



OPEN(UNIT=1,FILE='mhs-amortecido-x.dat')
OPEN(UNIT=2,FILE='mhs-amortecido-v.dat')
DO i=1,n
  WRITE(1,*) t(i),x(i)
  WRITE(2,*) t(i),v(i)
END DO
WRITE(1,*)
WRITE(2,*)
CLOSE(UNIT=1)
CLOSE(UNIT=2)

END PROGRAM mhs_amortecido

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fv(t,x,v,w,gamma0)
IMPLICIT NONE
REAL(KIND=8):: fv,t,x,v,w,gamma0
fv=-2.0D0*gamma0*v-w*w*x
END FUNCTION fv

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fx(t,x,v)
IMPLICIT NONE
REAL(KIND=8):: fx,t,v,x
fx=v
END FUNCTION fx

PROGRAM heat
IMPLICIT NONE
INTEGER:: nx,nt,i,j
REAL(KIND=8):: C,K,rho,dx,dt,eta,L,T1,T2
REAL(KIND=8),ALLOCATABLE:: x(:),t(:),Temp(:,:)

! Parâmetros inicias (Iron)
C=0.108D0
K=0.163D0
rho=7.874

! Grid na posição
L=10.0D0
nx=201
dx=L/(nx-1) ! dx=0.01
ALLOCATE(x(nx))
DO i=1,nx
  x(i)=(i-1)*dx
END DO

! Grid no tempo
dt=0.001
nt=1000000
ALLOCATE(t(nt))
DO i=1,nt
  t(i)=(i-1)*dt
END DO

! eta
eta=K*dt/(C*rho*dx*dx)
WRITE(*,*) eta

! Reservatórios de calor
T1=0.0D0
T2=100.0D0

! Condições iniciais
ALLOCATE(Temp(nx,nt))
Temp(:,1)=30.0D0

! Condições de contorno
Temp(1,1)=T1
Temp(nx,1)=T2

! Evolução temporal
DO j=1,nt-1
  Temp(1,j+1)=T1
  Temp(nx,j+1)=T2
  DO i=2,nx-1
    Temp(i,j+1)=Temp(i,j)+eta*(Temp(i+1,j)+Temp(i-1,j)-2.0D0*Temp(i,j))
  END DO
END DO

OPEN(UNIT=1,FILE='heat.dat')
DO j=1,nt
  IF(MOD(j,50).ne.0) CYCLE
  DO i=1,nx
    WRITE(1,*) x(i),t(j),Temp(i,j)
  END DO
  WRITE(1,*)
END DO
CLOSE(UNIT=1)




END PROGRAM heat


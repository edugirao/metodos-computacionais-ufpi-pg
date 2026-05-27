PROGRAM wave
IMPLICIT NONE
INTEGER:: nx,nt,i,j
REAL(KIND=8):: L,c,cl,dx,dt,delta
REAL(KIND=8),ALLOCATABLE:: x(:),t(:),y(:,:),v(:)

! Comprimento da corda
L=10.0D0

! Velocidade de propagação
c=1.0D0

! Grid na posição
nx=1001
dx=L/(nx-1)
ALLOCATE(x(nx))
DO i=1,nx
  x(i)=(i-1)*dx
END DO

! Grid no tempo
dt=0.005
nt=10000
ALLOCATE(t(nt))
DO i=1,nt
  t(i)=(i-1)*dt
END DO

! c linha
cl=dx/dt


! Posição inicial da corda
ALLOCATE(y(nx,nt))
delta=0.1D0*L
y(:,1)=0.0D0


! Velocidade inicial da corda
ALLOCATE(v(nx))
v=0.0D0
DO i=1,nx
  IF(x(i).le.0.05*L) v(i)=c
END DO
DO i=1,nx
  IF(x(i).ge.0.95*L) v(i)=-c
END DO

! Primeiro dt
DO i=1,nx
  y(i,2)=y(i,1)+v(i)*dt
END DO
! y(:,2)=y(:,1)+v(:)*dt

! Propagação
DO j=2,nt-1
  y(1,j)=0.0D0
  y(nx,j)=0.0D0
  DO i=2,nx-1
    y(i,j+1)=2.0D0*y(i,j)-y(i,j-1)+(c**2/cl**2)*(y(i+1,j)+y(i-1,j)-2.0D0*y(i,j))
  END DO
END DO

OPEN(UNIT=1,FILE='wave.xyz')
DO i=1,nt
  IF(mod(i,10).ne.0) CYCLE
  WRITE(1,*) nx
  WRITE(1,*)
  DO j=1,nx
    WRITE(1,*) 'H',x(j),y(j,i),0.0D0
  END DO
END DO
CLOSE(UNIT=1)


END PROGRAM wave


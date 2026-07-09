PROGRAM dinamica1
IMPLICIT NONE

INTEGER:: N,nt,it,i
REAL(KIND=8):: L,T,dt,soma_k,c,m,drmax,drtest
REAL(KIND=8),ALLOCATABLE:: r1(:,:),r2(:,:),r3(:,:),v(:,:),f(:,:),dr(:,:)

! Parâmetros
L=10.0D0
N=10
T=40.0D0
dt=0.005/DSQRT(T)
nt=100000

! Velocidades iniciais
ALLOCATE(v(N,2))
CALL RANDOM_NUMBER(v)
v=v-0.5D0
! soma_k=0.0D0
! DO i=1,N
!   soma_k=soma_k+v(i,1)**2+v(i,2)**2
! END DO
soma_k=sum(v**2)
c=DSQRT(T/soma_k)
v=v*c

! Posições inicias
ALLOCATE(r1(N,2))
CALL RANDOM_NUMBER(r1)
r1=r1*L

! Abrindo o arquivo de saída
OPEN(UNIT=1,FILE='dinamica.xyz')
CALL write_config(N,r1)

! Primeiro passo de tempo
m=1.0D0
ALLOCATE(f(N,2))
CALL force_calc(N,r1,L,f)
r2=r1+v*dt+0.5D0*(f/m)*dt**2
CALL write_config(N,r2)

! Loop da dinâmica
ALLOCATE(dr(N,2))
DO it=1,nt
  ! Forças
  CALL force_calc(N,r2,L,f)
  ! Integração por Verlet
  r3=0.5D0*(f/m)*dt**2+2.0D0*r2-r1
  dr=r3-r2
  drmax=0.0D0
  DO i=1,N
    drtest=DSQRT(dr(i,1)**2+dr(i,2)**2)
    IF(drtest.gt.drmax) drmax=drtest
  END DO
  IF(drmax.gt.0.25*L) STOP 'Too long displacements.'
  ! Correções
  DO i=1,N
    ! Correção borda superior
    IF(r3(i,2).gt.L)THEN
      r3(i,2)=2*L-r3(i,2)
      r2(i,2)=2*L-r2(i,2)
    END IF
    ! Correção borda inferior
    IF(r3(i,2).lt.0.0D0)THEN
      r3(i,2)=-r3(i,2)
      r2(i,2)=-r2(i,2)
    END IF
    ! Correção borda dianteira
    IF(r3(i,1).gt.L)THEN
      r3(i,1)=2*L-r3(i,1)
      r2(i,1)=2*L-r2(i,1)
    END IF
    ! Correção borda traseira
    IF(r3(i,1).lt.0.0D0)THEN
      r3(i,1)=-r3(i,1)
      r2(i,1)=-r2(i,1)
    END IF
  END DO
  ! Escrevendo a configuração
  CALL write_config(N,r3)
  ! "Dança das cadeiras"
  r1=r2
  r2=r3
END DO

CLOSE(UNIT=1)

END PROGRAM dinamica1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE write_config(N,r)
IMPLICIT NONE
INTEGER:: i,N
REAL(KIND=8):: r(N,2) 
WRITE(1,*) N
WRITE(1,*)
DO i=1,N
  WRITE(1,*) 'H',r(i,1),r(i,2),0.0D0
END DO
END SUBROUTINE 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE force_calc(N,r,L,f)
IMPLICIT NONE
INTEGER:: i,N,j
REAL(KIND=8):: r(N,2),f(N,2),L,ri(2),rj(2),rij(2),dij,cutoff
cutoff=0.7*L
f=0.0D0
DO i=1,N
  ri=r(i,:)
  DO j=1,N
    IF(j.eq.i) CYCLE
    rj=r(j,:)
    rij=ri-rj
    dij=DSQRT(rij(1)**2+rij(2)**2)
    IF(dij.gt.cutoff)CYCLE
    f(i,:)=f(i,:)+30.0D0*(dij**(-14)-dij**(-8))*rij
  END DO
END DO

END SUBROUTINE 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

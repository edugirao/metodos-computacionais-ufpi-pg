PROGRAM dinamica0
IMPLICIT NONE

INTEGER:: N,nt,it,i
REAL(KIND=8):: L,T,dt,soma_k,c
REAL(KIND=8),ALLOCATABLE:: r(:,:),v(:,:)

! Parâmetros
L=10.0D0
N=10
T=50.0D0
dt=0.01/DSQRT(T)
nt=10000

! Posições inicias
ALLOCATE(r(N,2))
CALL RANDOM_NUMBER(r)
r=r*L

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

! Abrindo o arquivo de saída
OPEN(UNIT=1,FILE='dinamica.xyz')
CALL write_config(N,r)

! Loop da dinâmica
DO it=1,nt
  ! Integração
  r=r+v*dt
  ! Correções
  DO i=1,N
    ! Correção borda superior
    IF(r(i,2).gt.L)THEN
      r(i,2)=r(i,2)-L
    END IF
    ! Correção borda inferior
    IF(r(i,2).lt.0.0D0)THEN
      r(i,2)=r(i,2)+L
    END IF
    ! Correção borda dianteira
    IF(r(i,1).gt.L)THEN
      r(i,1)=r(i,1)-L
    END IF
    ! Correção borda traseira
    IF(r(i,1).lt.0.0D0)THEN
      r(i,1)=r(i,1)+L
    END IF
  END DO
  ! Escrevendo a configuração
  CALL write_config(N,r)
END DO

CLOSE(UNIT=1)





END PROGRAM dinamica0

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

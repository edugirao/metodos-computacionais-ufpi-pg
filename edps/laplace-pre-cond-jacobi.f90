PROGRAM laplace
IMPLICIT NONE
INTEGER:: n,i,j,contador,contador_parcial,l1,l2
REAL(KIND=8):: l,h,tol,dif,w,Rij,Ugs
REAL(KIND=8),ALLOCATABLE:: x(:),y(:),Uold(:,:),Unew(:,:)

! Tolerância
tol=1.0D-3


! Primeiro cálculo

!Grid
l=10.0D0
n=101

! Condições de contorno
ALLOCATE(Uold(n,n))
Uold(:,1)=100.0D0 ! 100V ! Aresta inferior
Uold(:,n)=0.0D0 ! 0V     ! Aresta superior
Uold(1,:)=0.0D0          ! Aresta esquerda
Uold(n,:)=0.0D0          ! Aresta direita

! Chute inicial
Uold(2:n-1,2:n-1)=0.0D0

! Copiando os valores de fronteira
ALLOCATE(Unew(n,n))
Unew(:,1)=Uold(:,1)
Unew(:,n)=Uold(:,n)
Unew(1,:)=Uold(1,:)
Unew(n,:)=Uold(n,:)

! Loop
w=1.9D0
contador=0
DO
  contador=contador+1
  ! Atualização via mixing
  DO i=2,n-1
    DO j=2,n-1
      Unew(i,j)=(Uold(i+1,j)+Uold(i-1,j)+Uold(i,j+1)+Uold(i,j-1))/4.0D0
    END DO
  END DO
  ! Checando convergência
  dif=MAXVAL(ABS(Unew-Uold))
  WRITE(*,*) contador,dif
  ! Teste
  IF(dif.le.tol)THEN
    ! Solução encontrada
    EXIT
  ELSE
    ! Atualização para o próximo passo
    Uold=Unew
  END IF  
END DO
contador_parcial=contador

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Segundo cálculo

! Unew -> solução para o grid pobre
! Redefinir Uold no novo grid

! Redefinir o Grid
n=1001
ALLOCATE(x(n),y(n))
h=l/(n-1)
DO i=1,n
  x(i)=(i-1)*l
END DO  
DO i=1,n
  y(i)=(i-1)*l
END DO  


! Realocando Uold
DEALLOCATE(Uold)
ALLOCATE(Uold(n,n))
! Rescalonando o miolo 100*10+1 -> 1001 
DO j=1,100
DO i=1,100
  DO l1=1,10
  DO l2=1,10
    Uold((i-1)*10+l1,(j-1)*10+l2)=Unew(i,j)
  END DO
  END DO
END DO
END DO
! Condições de contorno
Uold(:,1)=100.0D0 ! 100V ! Aresta inferior
Uold(:,n)=0.0D0 ! 0V     ! Aresta superior
Uold(1,:)=0.0D0          ! Aresta esquerda
Uold(n,:)=0.0D0          ! Aresta direita


! Realocando Unew
DEALLOCATE(Unew)
ALLOCATE(Unew(n,n))
! Copiando os valores de fronteira
Unew(:,1)=Uold(:,1)
Unew(:,n)=Uold(:,n)
Unew(1,:)=Uold(1,:)
Unew(n,:)=Uold(n,:)


! Loop
w=1.9D0
DO
  contador=contador+1
  ! Atualização via mixing
  DO i=2,n-1
    DO j=2,n-1
      Unew(i,j)=(Uold(i+1,j)+Uold(i-1,j)+Uold(i,j+1)+Uold(i,j-1))/4.0D0
    END DO
  END DO
  ! Checando convergência
  dif=MAXVAL(ABS(Unew-Uold))
  WRITE(*,*) contador,dif
  ! Teste
  IF(dif.le.tol)THEN
    ! Solução encontrada
    EXIT
  ELSE
    ! Atualização para o próximo passo
    Uold=Unew
  END IF  
END DO
WRITE(*,*) contador_parcial

! Output
OPEN(UNIT=1,FILE='potential.dat')
DO i=1,n
  DO j=1,n
    WRITE(1,*) x(i),y(j),Unew(i,j)
  END DO
  WRITE(1,*)
END DO
WRITE(1,*)
CLOSE(UNIT=1)








END PROGRAM laplace


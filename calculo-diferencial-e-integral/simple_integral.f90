PROGRAM simple_integral
IMPLICIT NONE
INTEGER:: n,i,j
REAL(KIND=8):: h,a,b,f,integral_a,integral_s
REAL(KIND=8):: integral_d,integral_e,integral_t
REAL(KIND=8),ALLOCATABLE:: x(:),y(:),w(:)

WRITE(*,*) 'Calcularei a integral de:'
WRITE(*,*) 'f(x)=(DEXP(x))*DCOS(DEXP(x))'
WRITE(*,*) 'Com n pontos no intervalo'
WRITE(*,*) 'de x desde a até b.'
WRITE(*,*) 'a='
READ(*,*) a
WRITE(*,*) 'b='
READ(*,*) b
WRITE(*,*) 'n='
READ(*,*) n

! Grid
h=(b-a)/DFLOAT(n-1)
ALLOCATE(x(n))
DO i=1,n
  x(i)=a+DFLOAT(i-1)*h
END DO

! Valores da função
ALLOCATE(y(n))
DO i=1,n
  y(i)=f(x(i))
END DO

!!!!!!!!!!!!!!!!!!!!!
! Esquerda
!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(w(n))
w=h
w(n)=0.0D0
! DO i=1,n-1
!   w(i)=h
! END DO
! w(n)=0.0D0
integral_e=0.0D0
DO i=1,n
  integral_e=integral_e+y(i)*w(i)
END DO


!!!!!!!!!!!!!!!!!!!!!
! Direita
!!!!!!!!!!!!!!!!!!!!!
w=h
w(1)=0.0D0
integral_d=0.0D0
DO i=1,n
  integral_d=integral_d+y(i)*w(i)
END DO


!!!!!!!!!!!!!!!!!!!!!
! Trapézio
!!!!!!!!!!!!!!!!!!!!!
w=h
w(1)=h*0.5D0
w(n)=h*0.5D0
integral_t=0.0D0
DO i=1,n
  integral_t=integral_t+y(i)*w(i)
END DO


!!!!!!!!!!!!!!!!!!!!!
! Simpson
!!!!!!!!!!!!!!!!!!!!!
! Estratégia 1
! w(1)=h/3.0D0
! w(n)=h/3.0D0
! DO i=2,n-1,2 ! Termos pares
!   w(i)=4.0D0*h/3.0D0
! END DO
! DO i=3,n-2,2 ! Termos ímpares
!   w(i)=2.0D0*h/3.0D0
! END DO
! Estratégia 2
! w(1)=h/3.0D0
! w(n)=h/3.0D0
! DO i=2,n-1
!   IF(MOD(i,2).eq.0)THEN
!     w(i)=4.0D0*h/3.0D0
!   ELSE ! IF(MOD(i,2).eq.1)THEN  
!     w(i)=2.0D0*h/3.0D0
!   END IF
! END DO
! Estratégia 3
! w=2.0D0*h/3.0D0
! w(1)=h/3.0D0 ! Consertando i=1
! w(n)=h/3.0D0 ! Consertando i=n
! DO i=2,n-1,2 ! Consertando i=pares
!   w(i)=4.0D0*h/3.0D0
! END DO
! Estratégia 4 (Edilson)
w(1)=h/3.0D0 
DO j=1,(n-1)/2
  w(2*j)=4.0D0*h/3.0D0
  w(2*j+1)=2.0D0*h/3.0D0
END DO
w(n)=h/3.0D0 
integral_s=0.0D0
DO i=1,n
  integral_s=integral_s+y(i)*w(i)
END DO


!!!!!!!!!!!!!!!!!!!!!
! Resultado analítico
!!!!!!!!!!!!!!!!!!!!!
integral_a=DSIN(DEXP(b))-DSIN(DEXP(a))

WRITE(*,*) 'Valor da integral'
WRITE(*,*) 'Direita===',integral_d
WRITE(*,*) 'Esquerda==',integral_e
WRITE(*,*) 'Trapézio==',integral_t
WRITE(*,*) 'Simpson===',integral_s
WRITE(*,*) 'Analítico=',integral_a

WRITE(*,*) 'Erros'
WRITE(*,*) 'Direita===',100.0D0*ABS(integral_d-integral_a)/integral_a
WRITE(*,*) 'Esquerda==',100.0D0*ABS(integral_e-integral_a)/integral_a
WRITE(*,*) 'Trapézio==',100.0D0*ABS(integral_t-integral_a)/integral_a
WRITE(*,*) 'Simpson===',100.0D0*ABS(integral_s-integral_a)/integral_a

END PROGRAM simple_integral

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION f(x)
IMPLICIT NONE
REAL(KIND=8):: x,f
f=(DEXP(x))*DCOS(DEXP(x))
END FUNCTION f

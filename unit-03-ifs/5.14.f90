PROGRAM q5_14
IMPLICIT NONE
INTEGER:: a,b,c
LOGICAL:: triangle,isosceles,equilat
! Dados de entrada
WRITE(*,*) 'Entre com 3 distâncias inteiras:'
READ(*,*) a,b,c

! Triangle
triangle=.true.
IF(a.ge.b+c)THEN
  triangle=.false.
ELSE IF(b.ge.a+c)THEN
  triangle=.false.
ELSE IF(c.ge.a+b)THEN
  triangle=.false.
END IF

! Isosceles
isosceles=.false.
IF(triangle)THEN
  IF((a.eq.b).AND.(a.ne.c)) isosceles=.true.
  ! ! IF((a.eq.b).AND.(a.ne.c))THEN
  ! !   isosceles=.true.
  ! ! END IF
  ! IF(a.eq.b)THEN
  !   IF(a.ne.c)THEN
  !     isosceles=.true.
  !   END IF
  ! END IF
  IF((a.eq.c).AND.(a.ne.b)) isosceles=.true.
  ! ! IF((a.eq.c).AND.(a.ne.b))THEN
  ! !   isosceles=.true.
  ! ! END IF
  ! IF(a.eq.c)THEN
  !   IF(a.ne.b)THEN
  !     isosceles=.true.
  !   END IF
  ! END IF
  IF((b.eq.c).AND.(b.ne.a)) isosceles=.true.
  ! ! IF((b.eq.c).AND.(b.ne.a))THEN
  ! !   isosceles=.true.
  ! ! END IF
  ! IF(b.eq.c)THEN
  !   IF(b.ne.a)THEN
  !     isosceles=.true.
  !   END IF
  ! END IF
END IF


! Equilat
equilat=.false.
IF(triangle)THEN
  IF((a.eq.b).AND.(a.eq.c)) equilat=.true.
END IF


! Mensagem
IF(triangle)THEN
  WRITE(*,*) 'As distâncias podem formar um triângulo.'
  IF(isosceles)THEN
    WRITE(*,*) 'E esse triângulo é isosceles.'
  ELSE IF(equilat)THEN
    WRITE(*,*) 'E esse triângulo é equilátero.'
  ELSE
    WRITE(*,*) 'E esse triângulo é escaleno.'
  END IF
ELSE
  WRITE(*,*) 'As distâncias não podem formar um triângulo.'
END IF





END PROGRAM q5_14

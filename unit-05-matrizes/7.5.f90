PROGRAM p7_5
IMPLICIT NONE
INTEGER:: i,j,winner(1),l
REAL(KIND=8):: nota(5,6),score(6)
CHARACTER*10:: name(6)
! Lendo os nomes dos candidatos
WRITE(*,*) 'Escreva os nomes dos seis competidores:'
DO i=1,6
  READ(*,*) name(i)
END DO


! Notas dos rounds
DO j=1,6 ! j indica o player
  WRITE(*,*) 'Insira as notas do candidato ',name(j)
  DO i=1,5
    READ(*,*) nota(i,j)
  END DO
END DO
! DO i=1,5 ! i indica o round
!   WRITE(*,*) 'Insira as notas do round',i,'para:'
!   DO j=1,6
!     WRITE(*,*) name(j)
!     READ(*,*) nota(i,j)
!   END DO
! END DO





! Calculando os scores simples
DO j=1,6
  ! Calculando o score simples
  score(j)=SUM(nota(:,j))
  ! Imprimido todos os resultados
!   WRITE(*,*) name(j),(nota(i,j),i=1,5),score(j)
END DO

! Calculando os pontos extras
DO i=1,5
  winner=MAXLOC(nota(i,:))
  l=winner(1)
  score(l)=score(l)+5.0D0
END DO

WRITE(*,*)'-------------'
! DO j=1,6
!   WRITE(*,*) name(j),(nota(i,j),i=1,5),score(j)
! END DO

winner=MAXLOC(score)
l=winner(1)
WRITE(*,*) 'O vencedor é: ',name(l),'com ',score(l),'pontos.'
score(l)=0.0
winner=MAXLOC(score)
l=winner(1)
WRITE(*,*) 'O segundo é: ',name(l),'com ',score(l),'pontos.'

END PROGRAM p7_5

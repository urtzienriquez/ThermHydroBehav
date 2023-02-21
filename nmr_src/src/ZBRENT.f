      FUNCTION ZBRENT(FUN,X1,X2,TOL)

C     NICHEMAPR: SOFTWARE FOR BIOPHYSICAL MECHANISTIC NICHE MODELLING

C     COPYRIGHT (C) 2018 MICHAEL R. KEARNEY AND WARREN P. PORTER

C     THIS PROGRAM IS FREE SOFTWARE: YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C     IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C     THE FREE SOFTWARE FOUNDATION, EITHER VERSION 3 OF THE LICENSE, OR (AT
C      YOUR OPTION) ANY LATER VERSION.

C     THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C     WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C     MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C     GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C     YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C     ALONG WITH THIS PROGRAM. IF NOT, SEE HTTP://WWW.GNU.ORG/LICENSES/.

C     ROOT FINING ROUTINE TO FIND TEMPERATURE THAT BALANCES STATIC HEAT BUDGET
C     MODIFIED FROM
C     RICHARD BRENT,
C     ALGORITHMS FOR MINIMIZATION WITHOUT DERIVATIVES,
C     DOVER, 2002,
C     ISBN: 0-486-41998-3,
C     LC: QA402.5.B74.

C     EPS IS MACHINE FLOATING POINT PRECISION

      DOUBLE PRECISION A,B,C,D,E,DIAGNOS,EPS,FA,FB,FC,FUN
      DOUBLE PRECISION P,Q,R,S,TOL,TOL1,X1,X2,XM,ZBRENT
      INTEGER ITER,ITMAX

      ITMAX=100
      EPS=3.E-8
      DIAGNOS = 0.
      A=X1
      B=X2
      E=0.D0
      C=0.D0
      FA=FUN(A)
      FB=FUN(B)
      IF((FB*FA.GT.0.) .AND. (DIAGNOS.GT.0.0)) THEN
C      WRITE (0,*)'ROOT MUST BE BRACKETED FOR ZBRENT.'
      ELSE
      ENDIF
      FC=FB
      DO 11 ITER=1,ITMAX
       IF(FB*FC.GT.0.) THEN
        C=A
        FC=FA
        D=B-A
        E=D
       ENDIF
       IF(ABS(FC).LT.ABS(FB))THEN
        A=B
        B=C
        C=A
        FA=FB
        FB=FC
        FC=FA
       ENDIF
       TOL1=2.*EPS*ABS(B)+0.5*TOL
       XM=.5*(C-B)
       IF(ABS(XM).LE.TOL1 .OR. FB.EQ.0.)THEN
        ZBRENT=B
        GO TO 20
       ENDIF
C      AN ADDITION, SINCE THIS SUBROUTINE SOMETIMES MISSES A SOLUTION
C      WARREN PORTER 2003
       IF(ABS(FB).LE.TOL)THEN
        IF(ITER.GT.1)THEN
         ZBRENT=B
         GO TO 20
        ENDIF
       ENDIF
       IF(ABS(E).GE.TOL1 .AND. ABS(FA).GT.ABS(FB)) THEN
        S=FB/FA
        IF(A.EQ.C) THEN
         P=2.*XM*S
         Q=1.-S
        ELSE
         Q=FA/FC
         R=FB/FC
         P=S*(2.*XM*Q*(Q-R)-(B-A)*(R-1.))
         Q=(Q-1.)*(R-1.)*(S-1.)
        ENDIF
        IF(P.GT.0.) Q=-Q
         P=ABS(P)
         IF(2.*P .LT. MIN(3.*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
          E=D
          D=P/Q
         ELSE
          D=XM
          E=D
         ENDIF
        ELSE
         D=XM
         E=D
        ENDIF
        A=B
        FA=FB
        IF(ABS(D) .GT. TOL1) THEN
         B=B+D
        ELSE
         B=B+SIGN(TOL1,XM)
        ENDIF
       FB=FUN(B)
11    CONTINUE
      IF (DIAGNOS .GT. 0.0) THEN
C      WRITE(0,*) 'ZBRENT EXCEEDING MAXIMUM ITERATIONS.'
      ENDIF
      ZBRENT=B
20    RETURN
      END
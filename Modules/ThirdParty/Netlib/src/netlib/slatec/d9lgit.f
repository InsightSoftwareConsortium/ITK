*DECK D9LGIT
      DOUBLE PRECISION FUNCTION D9LGIT (A, X, ALGAP1)
C***BEGIN PROLOGUE  D9LGIT
C***SUBSIDIARY
C***PURPOSE  Compute the logarithm of Tricomi's incomplete Gamma
C            function with Perron's continued fraction for large X and
C            A .GE. X.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      DOUBLE PRECISION (R9LGIT-S, D9LGIT-D)
C***KEYWORDS  FNLIB, INCOMPLETE GAMMA FUNCTION, LOGARITHM,
C             PERRON'S CONTINUED FRACTION, SPECIAL FUNCTIONS, TRICOMI
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Compute the log of Tricomi's incomplete gamma function with Perron's
C continued fraction for large X and for A .GE. X.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900720  Routine changed from user-callable to subsidiary.  (WRB)
C***END PROLOGUE  D9LGIT
      DOUBLE PRECISION A, X, ALGAP1, AX, A1X, EPS, FK, HSTAR, P, R, S,
     1  SQEPS, T, D1MACH
      LOGICAL FIRST
      SAVE EPS, SQEPS, FIRST
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  D9LGIT
      IF (FIRST) THEN
         EPS = 0.5D0*D1MACH(3)
         SQEPS = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LE. 0.D0 .OR. A .LT. X) CALL XERMSG ('SLATEC', 'D9LGIT',
     +   'X SHOULD BE GT 0.0 AND LE A', 2, 2)
C
      AX = A + X
      A1X = AX + 1.0D0
      R = 0.D0
      P = 1.D0
      S = P
      DO 20 K=1,200
        FK = K
        T = (A+FK)*X*(1.D0+R)
        R = T/((AX+FK)*(A1X+FK)-T)
        P = R*P
        S = S + P
        IF (ABS(P).LT.EPS*S) GO TO 30
 20   CONTINUE
      CALL XERMSG ('SLATEC', 'D9LGIT',
     +   'NO CONVERGENCE IN 200 TERMS OF CONTINUED FRACTION', 3, 2)
C
 30   HSTAR = 1.0D0 - X*S/A1X
      IF (HSTAR .LT. SQEPS) CALL XERMSG ('SLATEC', 'D9LGIT',
     +   'RESULT LESS THAN HALF PRECISION', 1, 1)
C
      D9LGIT = -X - ALGAP1 - LOG(HSTAR)
      RETURN
C
      END

*DECK DLBETA
      DOUBLE PRECISION FUNCTION DLBETA (A, B)
C***BEGIN PROLOGUE  DLBETA
C***PURPOSE  Compute the natural logarithm of the complete Beta
C            function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7B
C***TYPE      DOUBLE PRECISION (ALBETA-S, DLBETA-D, CLBETA-C)
C***KEYWORDS  FNLIB, LOGARITHM OF THE COMPLETE BETA FUNCTION,
C             SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DLBETA(A,B) calculates the double precision natural logarithm of
C the complete beta function for double precision arguments
C A and B.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D9LGMC, DGAMMA, DLNGAM, DLNREL, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900727  Added EXTERNAL statement.  (WRB)
C***END PROLOGUE  DLBETA
      DOUBLE PRECISION A, B, P, Q, CORR, SQ2PIL, D9LGMC, DGAMMA, DLNGAM,
     1  DLNREL
      EXTERNAL DGAMMA
      SAVE SQ2PIL
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
C***FIRST EXECUTABLE STATEMENT  DLBETA
      P = MIN (A, B)
      Q = MAX (A, B)
C
      IF (P .LE. 0.D0) CALL XERMSG ('SLATEC', 'DLBETA',
     +   'BOTH ARGUMENTS MUST BE GT ZERO', 1, 2)
C
      IF (P.GE.10.D0) GO TO 30
      IF (Q.GE.10.D0) GO TO 20
C
C P AND Q ARE SMALL.
C
      DLBETA = LOG (DGAMMA(P) * (DGAMMA(Q)/DGAMMA(P+Q)) )
      RETURN
C
C P IS SMALL, BUT Q IS BIG.
C
 20   CORR = D9LGMC(Q) - D9LGMC(P+Q)
      DLBETA = DLNGAM(P) + CORR + P - P*LOG(P+Q)
     1  + (Q-0.5D0)*DLNREL(-P/(P+Q))
      RETURN
C
C P AND Q ARE BIG.
C
 30   CORR = D9LGMC(P) + D9LGMC(Q) - D9LGMC(P+Q)
      DLBETA = -0.5D0*LOG(Q) + SQ2PIL + CORR + (P-0.5D0)*LOG(P/(P+Q))
     1  + Q*DLNREL(-P/(P+Q))
      RETURN
C
      END

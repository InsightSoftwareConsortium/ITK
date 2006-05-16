*DECK DGAMI
      DOUBLE PRECISION FUNCTION DGAMI (A, X)
C***BEGIN PROLOGUE  DGAMI
C***PURPOSE  Evaluate the incomplete Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      DOUBLE PRECISION (GAMI-S, DGAMI-D)
C***KEYWORDS  FNLIB, INCOMPLETE GAMMA FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Evaluate the incomplete gamma function defined by
C
C DGAMI = integral from T = 0 to X of EXP(-T) * T**(A-1.0) .
C
C DGAMI is evaluated for positive values of A and non-negative values
C of X.  A slight deterioration of 2 or 3 digits accuracy will occur
C when DGAMI is very large or very small, because logarithmic variables
C are used.  The function and both arguments are double precision.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DGAMIT, DLNGAM, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DGAMI
      DOUBLE PRECISION A, X, FACTOR, DLNGAM, DGAMIT
C***FIRST EXECUTABLE STATEMENT  DGAMI
      IF (A .LE. 0.D0) CALL XERMSG ('SLATEC', 'DGAMI',
     +   'A MUST BE GT ZERO', 1, 2)
      IF (X .LT. 0.D0) CALL XERMSG ('SLATEC', 'DGAMI',
     +   'X MUST BE GE ZERO', 2, 2)
C
      DGAMI = 0.D0
      IF (X.EQ.0.0D0) RETURN
C
C THE ONLY ERROR POSSIBLE IN THE EXPRESSION BELOW IS A FATAL OVERFLOW.
      FACTOR = EXP (DLNGAM(A) + A*LOG(X))
C
      DGAMI = FACTOR * DGAMIT (A, X)
C
      RETURN
      END

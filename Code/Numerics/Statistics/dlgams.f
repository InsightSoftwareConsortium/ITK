*DECK DLGAMS
      SUBROUTINE DLGAMS (X, DLGAM, SGNGAM)
C***BEGIN PROLOGUE  DLGAMS
C***PURPOSE  Compute the logarithm of the absolute value of the Gamma
C            function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A
C***TYPE      DOUBLE PRECISION (ALGAMS-S, DLGAMS-D)
C***KEYWORDS  ABSOLUTE VALUE OF THE LOGARITHM OF THE GAMMA FUNCTION,
C             FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DLGAMS(X,DLGAM,SGNGAM) calculates the double precision natural
C logarithm of the absolute value of the Gamma function for
C double precision argument X and stores the result in double
C precision argument DLGAM.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DLNGAM
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C***END PROLOGUE  DLGAMS
      DOUBLE PRECISION X, DLGAM, SGNGAM, DLNGAM
C***FIRST EXECUTABLE STATEMENT  DLGAMS
      DLGAM = DLNGAM(X)
      SGNGAM = 1.0D0
      IF (X.GT.0.D0) RETURN
C
      INT = MOD (-AINT(X), 2.0D0) + 0.1D0
      IF (INT.EQ.0) SGNGAM = -1.0D0
C
      RETURN
      END

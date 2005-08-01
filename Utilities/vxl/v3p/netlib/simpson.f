C     NUMERICAL METHODS: FORTRAN Programs, (c) John H. Mathews 1994
C     To accompany the text:
C     NUMERICAL METHODS for Mathematics, Science and Engineering, 2nd Ed, 1992
C     Prentice Hall, Englewood Cliffs, New Jersey, 07632, U.S.A.
C     This free software is complements of the author.
C
C     Algorithm 7.2 (Composite Simpson Rule). 
C     Section 7.2, Composite Trapezoidal and Simpson's Rule, Page 365
C

C     comment added by Kongbin Kang
C     F: integrand function
C     A: lower integration limit
C     B: higher integration limit
C     M: number of intervals. Notice, the subintervals used is 2M
C     Srule: output parameter to store simpson rule result
      
      SUBROUTINE SIMPRU(F,A,B,M,Srule)
      INTEGER K,M
      REAL A,B,H,Sum,SumEven,SumOdd,Srule,X
      EXTERNAL F
      H=(B-A)/(2*M)
      SumEven=0
      DO K=1,(M-1)
        X=A+H*2*K
        SumEven=SumEven+F(X)
      ENDDO
      SumOdd=0
      DO K=1,M
        X=A+H*(2*K-1)
        SumOdd=SumOdd+F(X)
      ENDDO
      Sum=H*(F(A)+F(B)+2*SumEven+4*SumOdd)/3
      Srule=Sum
      RETURN
      END

      SUBROUTINE XSIMPRU(F,A,B,M,Srule)
C     This subroutine uses labeled DO loop(s).
      INTEGER K,M
      REAL A,B,H,Sum,SumEven,SumOdd,Srule,X
      EXTERNAL F
      H=(B-A)/(2*M)
      SumEven=0
      DO 10 K=1,(M-1)
        X=A+H*2*K
        SumEven=SumEven+F(X)
10    CONTINUE
      SumOdd=0
      DO 20 K=1,M
        X=A+H*(2*K-1)
        SumOdd=SumOdd+F(X)
20    CONTINUE
      Sum=H*(F(A)+F(B)+2*SumEven+4*SumOdd)/3
      Srule=Sum
      RETURN
      END



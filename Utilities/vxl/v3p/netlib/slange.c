#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Table of constant values */
static integer c__1 = 1;

real slange_(const char *norm, const integer *m, const integer *n, real *a, const integer *lda, real *work)
{
    /* Local variables */
    static integer i, j;
    static real value;
    static real sum;

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*  Purpose                                                              */
/*  =======                                                              */
/*                                                                       */
/*  SLANGE  returns the value of the one norm, or the Frobenius norm, or */
/*  the infinity norm, or the element of largest absolute value of a     */
/*  real matrix A.                                                       */
/*                                                                       */
/*  Description                                                          */
/*  ===========                                                          */
/*                                                                       */
/*  SLANGE returns the value                                             */
/*                                                                       */
/*     SLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'                    */
/*              (                                                        */
/*              ( norm1(A),         NORM = '1', 'O' or 'o'               */
/*              (                                                        */
/*              ( normI(A),         NORM = 'I' or 'i'                    */
/*              (                                                        */
/*              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'          */
/*                                                                       */
/*  where norm1 denotes the one norm of a matrix (maximum column sum),   */
/*  normI denotes the infinity norm of a matrix (maximum row sum) and    */
/*  normF denotes the Frobenius norm of a matrix (square root of sum of  */
/*  squares).  Note that  max(abs(A(i,j)))  is not a matrix norm.        */
/*                                                                       */
/*  Arguments                                                            */
/*  =========                                                            */
/*                                                                       */
/*  NORM    (input) CHARACTER*1                                          */
/*          Specifies the value to be returned in SLANGE as described    */
/*          above.                                                       */
/*                                                                       */
/*  M       (input) INTEGER                                              */
/*          The number of rows of the matrix A.  M >= 0.  When M = 0,    */
/*          SLANGE is set to zero.                                       */
/*                                                                       */
/*  N       (input) INTEGER                                              */
/*          The number of columns of the matrix A.  N >= 0.  When N = 0, */
/*          SLANGE is set to zero.                                       */
/*                                                                       */
/*  A       (input) REAL array, dimension (LDA,N)                        */
/*          The m by n matrix A.                                         */
/*                                                                       */
/*  LDA     (input) INTEGER                                              */
/*          The leading dimension of the array A.  LDA >= max(M,1).      */
/*                                                                       */
/*  WORK    (workspace) REAL array, dimension (LWORK),                   */
/*          where LWORK >= M when NORM = 'I'; otherwise, WORK is not     */
/*          referenced.                                                  */
/*                                                                       */
/* ===================================================================== */

    value = 0.f;
    if (*m == 0 || *n == 0) {
        return value;
    } else if (lsame_(norm, "M")) {

/*        Find max(abs(A(i,j))). */

        for (j = 0; j < *n; ++j) {
            for (i = 0; i < *m; ++i) {
                if (value < abs(a[i + j * *lda]))
                    value = abs(a[i + j * *lda]);
            }
        }
    } else if (lsame_(norm, "O") || *norm == '1') {

/*        Find norm1(A). */

        for (j = 0; j < *n; ++j) {
            sum = 0.f;
            for (i = 0; i < *m; ++i) {
                sum += abs(a[i + j * *lda]);
            }
            if (value < sum)
                value = sum;
        }
    } else if (lsame_(norm, "I")) {

/*        Find normI(A). */

        for (i = 0; i < *m; ++i) {
            work[i] = 0.f;
        }
        for (j = 0; j < *n; ++j) {
            for (i = 0; i < *m; ++i) {
                work[i] += abs(a[i + j * *lda]);
            }
        }
        for (i = 0; i < *m; ++i) {
            if (value < work[i])
                value = work[i];
        }
    } else if (lsame_(norm, "F") || lsame_(norm, "E")) {

/*        Find normF(A). */

        sum = 1.f;
        for (j = 0; j < *n; ++j) {
            slassq_(m, &a[j * *lda], &c__1, &value, &sum);
        }
        value *= sqrtf(sum);
    }

    return value;

} /* slange_ */

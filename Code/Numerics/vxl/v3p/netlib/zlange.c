/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

doublereal zlange_(norm, m, n, a, lda, work, norm_len)
char *norm;
integer *m, *n;
doublecomplex *a;
integer *lda;
doublereal *work;
ftnlen norm_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    doublereal ret_val, d__1, d__2;

    /* Builtin functions */
    double z_abs(), sqrt();

    /* Local variables */
    static integer i, j;
    static doublereal scale;
    extern logical lsame_();
    static doublereal value;
    extern /* Subroutine */ int zlassq_();
    static doublereal sum;


/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLANGE  returns the value of the one norm,  or the Frobenius norm, or
*/
/*  the  infinity norm,  or the  element of  largest absolute value  of a
*/
/*  complex matrix A. */

/*  Description */
/*  =========== */

/*  ZLANGE returns the value */

/*     ZLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm' */
/*              ( */
/*              ( norm1(A),         NORM = '1', 'O' or 'o' */
/*              ( */
/*              ( normI(A),         NORM = 'I' or 'i' */
/*              ( */
/*              ( normF(A),         NORM = 'F', 'f', 'E' or 'e' */

/*  where  norm1  denotes the  one norm of a matrix (maximum column sum),
*/
/*  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
*/
/*  normF  denotes the  Frobenius norm of a matrix (square root of sum of
*/
/*  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm. */

/*  Arguments */
/*  ========= */

/*  NORM    (input) CHARACTER*1 */
/*          Specifies the value to be returned in ZLANGE as described */
/*          above. */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A.  M >= 0.  When M = 0, */
/*          ZLANGE is set to zero. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A.  N >= 0.  When N = 0,
*/
/*          ZLANGE is set to zero. */

/*  A       (input) COMPLEX*16 array, dimension (LDA,N) */
/*          The m by n matrix A. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(M,1). */

/*  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK), */
/*          where LWORK >= M when NORM = 'I'; otherwise, WORK is not */
/*          referenced. */

/* =====================================================================
*/

/*     .. Parameters .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */

    /* Parameter adjustments */
    --work;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (min(*m,*n) == 0) {
        value = 0.;
    } else if (lsame_(norm, "M", 1L, 1L)) {

/*        Find max(abs(A(i,j))). */

        value = 0.;
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
            i__2 = *m;
            for (i = 1; i <= i__2; ++i) {
/* Computing MAX */
                d__1 = value, d__2 = z_abs(&a[i + j * a_dim1]);
                value = max(d__1,d__2);
/* L10: */
            }
/* L20: */
        }
    } else if (lsame_(norm, "O", 1L, 1L) || *norm == '1') {

/*        Find norm1(A). */

        value = 0.;
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
            sum = 0.;
            i__2 = *m;
            for (i = 1; i <= i__2; ++i) {
                sum += z_abs(&a[i + j * a_dim1]);
/* L30: */
            }
            value = max(value,sum);
/* L40: */
        }
    } else if (lsame_(norm, "I", 1L, 1L)) {

/*        Find normI(A). */

        i__1 = *m;
        for (i = 1; i <= i__1; ++i) {
            work[i] = 0.;
/* L50: */
        }
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
            i__2 = *m;
            for (i = 1; i <= i__2; ++i) {
                work[i] += z_abs(&a[i + j * a_dim1]);
/* L60: */
            }
/* L70: */
        }
        value = 0.;
        i__1 = *m;
        for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
            d__1 = value, d__2 = work[i];
            value = max(d__1,d__2);
/* L80: */
        }
    } else if (lsame_(norm, "F", 1L, 1L) || lsame_(norm, "E", 1L, 1L)) {

/*        Find normF(A). */

        scale = 0.;
        sum = 1.;
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
            zlassq_(m, &a[j * a_dim1 + 1], &c__1, &scale, &sum);
/* L90: */
        }
        value = scale * sqrt(sum);
    }

    ret_val = value;
    return ret_val;

/*     End of ZLANGE */

} /* zlange_ */


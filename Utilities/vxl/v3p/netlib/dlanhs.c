#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Table of constant values */
static integer c__1 = 1;

doublereal dlanhs_(norm, n, a, lda, work)
const char *norm;
const integer *n;
doublereal *a;
const integer *lda;
doublereal *work;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i, j;
    static doublereal scale;
    static doublereal value;
    static doublereal sum;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*  Purpose                                                              */
/*  =======                                                              */
/*                                                                       */
/*  DLANHS  returns the value of the one norm,  or the Frobenius norm, or*/
/*  the  infinity norm,  or the  element of  largest absolute value  of a*/
/*  Hessenberg matrix A.                                                 */
/*                                                                       */
/*  Description                                                          */
/*  ===========                                                          */
/*                                                                       */
/*  DLANHS returns the value                                             */
/*                                                                       */
/*     DLANHS = ( max(abs(A(i,j))), NORM = 'M' or 'm'                    */
/*              (                                                        */
/*              ( norm1(A),         NORM = '1', 'O' or 'o'               */
/*              (                                                        */
/*              ( normI(A),         NORM = 'I' or 'i'                    */
/*              (                                                        */
/*              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'          */
/*                                                                       */
/*  where  norm1  denotes the  one norm of a matrix (maximum column sum),*/
/*  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and*/
/*  normF  denotes the  Frobenius norm of a matrix (square root of sum of*/
/*  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.       */
/*                                                                       */
/*  Arguments                                                            */
/*  =========                                                            */
/*                                                                       */
/*  NORM    (input) CHARACTER*1                                          */
/*          Specifies the value to be returned in DLANHS as described    */
/*          above.                                                       */
/*                                                                       */
/*  N       (input) INTEGER                                              */
/*          The order of the matrix A.  N >= 0.  When N = 0, DLANHS is   */
/*          set to zero.                                                 */
/*                                                                       */
/*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)            */
/*          The n by n upper Hessenberg matrix A; the part of A below the*/
/*          first sub-diagonal is not referenced.                        */
/*                                                                       */
/*  LDA     (input) INTEGER                                              */
/*          The leading dimension of the array A.  LDA >= max(N,1).      */
/*                                                                       */
/*  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK),       */
/*          where LWORK >= N when NORM = 'I'; otherwise, WORK is not     */
/*          referenced.                                                  */
/*                                                                       */
/* ===================================================================== */

    if (*n == 0) {
        value = 0.;
    } else if (lsame_(norm, "M")) {

/*        Find max(abs(A(i,j))). */

        value = 0.;
        for (j = 0; j < *n; ++j) {
            for (i = 0; i < min(*n,j + 2); ++i) {
                value = max(value, abs(a[i + j * *lda]));
            }
        }
    } else if (lsame_(norm, "O") || *(unsigned char *)norm == '1') {

/*        Find norm1(A). */

        value = 0.;
        for (j = 0; j < *n; ++j) {
            sum = 0.;
            for (i = 0; i < min(*n,j + 2); ++i) {
                sum += abs(a[i + j * *lda]);
            }
            value = max(value,sum);
        }
    } else if (lsame_(norm, "I")) {

/*        Find normI(A). */

        for (i = 0; i < *n; ++i) {
            work[i] = 0.;
        }
        for (j = 0; j < *n; ++j) {
            for (i = 0; i < min(*n,j + 2); ++i) {
                work[i] += abs(a[i + j * *lda]);
            }
        }
        value = 0.;
        for (i = 0; i < *n; ++i) {
            value = max(value, work[i]);
        }
    } else if (lsame_(norm, "F") || lsame_(norm, "E")) {

/*        Find normF(A). */

        scale = 0.;
        sum = 1.;
        for (j = 0; j < *n; ++j) {
            i__1 = min(*n,j + 2);
            dlassq_(&i__1, &a[j * *lda], &c__1, &scale, &sum);
        }
        value = scale * sqrt(sum);
    }

    return value;

} /* dlanhs_ */

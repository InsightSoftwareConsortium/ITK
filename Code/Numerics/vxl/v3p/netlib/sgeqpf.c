/* sgeqpf.f -- translated by f2c (version of 4 June 1993  1:43:59).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int sgeqpf_(integer *m, integer *n, real *a, integer *lda,
        integer *jpvt, real *tau, real *work, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real temp, temp2;
    extern doublereal snrm2_(integer *, real *, integer *);
    static integer i, j;
    extern /* Subroutine */ int slarf_(char *, integer *, integer *, real *,
            integer *, real *, real *, integer *, real *, ftnlen);
    static integer itemp;
    extern /* Subroutine */ int sswap_(integer *, real *, integer *, real *,
            integer *), sgeqr2_(integer *, integer *, real *, integer *, real
            *, real *, integer *);
    static integer ma;
    extern /* Subroutine */ int sorm2r_(char *, char *, integer *, integer *,
            integer *, real *, integer *, real *, real *, integer *, real *,
            integer *, ftnlen, ftnlen);
    static integer mn;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen), slarfg_(
            integer *, real *, real *, integer *, real *);
    extern integer isamax_(integer *, real *, integer *);
    static real aii;
    static integer pvt;


/*  -- LAPACK test routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     March 31, 1993 */

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */

/*  Purpose */
/*  ======= */

/*  SGEQPF computes a QR factorization with column pivoting of a */
/*  real M-by-N matrix A: A*P = Q*R. */

/*  Arguments */
/*  ========= */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A. M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A. N >= 0 */

/*  A       (input/output) REAL array, dimension (LDA,N) */
/*          On entry, the M-by-N matrix A. */
/*          On exit, the upper triangle of the array contains the */
/*          min(M,N)-by-N upper triangular matrix R; the elements */
/*          below the diagonal, together with the array TAU, */
/*          represent the orthogonal matrix Q as a product of */
/*          min(m,n) elementary reflectors. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. LDA >= max(1,M). */

/*  JPVT    (input/output) INTEGER array, dimension (N) */
/*          On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
*/
/*          to the front of A*P (a leading column); if JPVT(i) = 0, */
/*          the i-th column of A is a free column. */
/*          On exit, if JPVT(i) = k, then the i-th column of A*P */
/*          was the k-th column of A. */

/*  TAU     (output) REAL array, dimension (min(M,N)) */
/*          The scalar factors of the elementary reflectors. */

/*  WORK    (workspace) REAL array, dimension (3*N) */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value */

/*  Further Details */
/*  =============== */

/*  The matrix Q is represented as a product of elementary reflectors */

/*     Q = H(1) H(2) . . . H(n) */

/*  Each H(i) has the form */

/*     H = I - tau * v * v' */

/*  where tau is a real scalar, and v is a real vector with */
/*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i).
*/

/*  The matrix P is represented in jpvt as follows: If */
/*     jpvt(j) = i */
/*  then the jth column of P is the ith canonical unit vector. */

/*  =====================================================================
*/

/*     .. Parameters .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input arguments */

    /* Parameter adjustments */
    --work;
    --tau;
    --jpvt;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    *info = 0;
    if (*m < 0) {
        *info = -1;
    } else if (*n < 0) {
        *info = -2;
    } else if (*lda < max(1,*m)) {
        *info = -4;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("SGEQPF", &i__1, 6L);
        return 0;
    }

    mn = min(*m,*n);

/*     Move initial columns up front */

    itemp = 1;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        if (jpvt[i] != 0) {
            if (i != itemp) {
                sswap_(m, &a[i * a_dim1 + 1], &c__1, &a[itemp * a_dim1 + 1], &
                        c__1);
                jpvt[i] = jpvt[itemp];
                jpvt[itemp] = i;
            } else {
                jpvt[i] = i;
            }
            ++itemp;
        } else {
            jpvt[i] = i;
        }
/* L10: */
    }
    --itemp;

/*     Compute the QR factorization and update remaining columns */

    if (itemp > 0) {
        ma = min(itemp,*m);
        sgeqr2_(m, &ma, &a[a_offset], lda, &tau[1], &work[1], info);
        if (ma < *n) {
            i__1 = *n - ma;
            sorm2r_("Left", "Transpose", m, &i__1, &ma, &a[a_offset], lda, &
                    tau[1], &a[(ma + 1) * a_dim1 + 1], lda, &work[1], info,
                    4L, 9L);
        }
    }

    if (itemp < mn) {

/*        Initialize partial column norms. The first n elements of */
/*        work store the exact column norms. */

        i__1 = *n;
        for (i = itemp + 1; i <= i__1; ++i) {
            i__2 = *m - itemp;
            work[i] = snrm2_(&i__2, &a[itemp + 1 + i * a_dim1], &c__1);
            work[*n + i] = work[i];
/* L20: */
        }

/*        Compute factorization */

        i__1 = mn;
        for (i = itemp + 1; i <= i__1; ++i) {

/*           Determine ith pivot column and swap if necessary */

            i__2 = *n - i + 1;
            pvt = i - 1 + isamax_(&i__2, &work[i], &c__1);

            if (pvt != i) {
                sswap_(m, &a[pvt * a_dim1 + 1], &c__1, &a[i * a_dim1 + 1], &
                        c__1);
                itemp = jpvt[pvt];
                jpvt[pvt] = jpvt[i];
                jpvt[i] = itemp;
                work[pvt] = work[i];
                work[*n + pvt] = work[*n + i];
            }

/*           Generate elementary reflector H(i) */

            if (i < *m) {
                i__2 = *m - i + 1;
                slarfg_(&i__2, &a[i + i * a_dim1], &a[i + 1 + i * a_dim1], &
                        c__1, &tau[i]);
            } else {
                slarfg_(&c__1, &a[*m + *m * a_dim1], &a[*m + *m * a_dim1], &
                        c__1, &tau[*m]);
            }

            if (i < *n) {

/*              Apply H(i) to A(i:m,i+1:n) from the left */

                aii = a[i + i * a_dim1];
                a[i + i * a_dim1] = 1.f;
                i__2 = *m - i + 1;
                i__3 = *n - i;
                slarf_("LEFT", &i__2, &i__3, &a[i + i * a_dim1], &c__1, &tau[
                        i], &a[i + (i + 1) * a_dim1], lda, &work[(*n << 1) +
                        1], 4L);
                a[i + i * a_dim1] = aii;
            }

/*           Update partial column norms */

            i__2 = *n;
            for (j = i + 1; j <= i__2; ++j) {
                if (work[j] != 0.f) {
/* Computing 2nd power */
                    r__2 = (r__1 = a[i + j * a_dim1], abs(r__1)) / work[j];
                    temp = 1.f - r__2 * r__2;
                    temp = max(temp,0.f);
/* Computing 2nd power */
                    r__1 = work[j] / work[*n + j];
                    temp2 = temp * .05f * (r__1 * r__1) + 1.f;
                    if (temp2 == 1.f) {
                        if (*m - i > 0) {
                            i__3 = *m - i;
                            work[j] = snrm2_(&i__3, &a[i + 1 + j * a_dim1], &
                                    c__1);
                            work[*n + j] = work[j];
                        } else {
                            work[j] = 0.f;
                            work[*n + j] = 0.f;
                        }
                    } else {
                        work[j] *= sqrt(temp);
                    }
                }
/* L30: */
            }

/* L40: */
        }
    }
    return 0;

/*     End of SGEQPF */

} /* sgeqpf_ */


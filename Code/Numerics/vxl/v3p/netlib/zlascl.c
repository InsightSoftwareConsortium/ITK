/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int zlascl_(type, kl, ku, cfrom, cto, m, n, a, lda, info,
        type_len)
char *type;
integer *kl, *ku;
doublereal *cfrom, *cto;
integer *m, *n;
doublecomplex *a;
integer *lda, *info;
ftnlen type_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4, i__5;
    doublecomplex z__1;

    /* Local variables */
    static logical done;
    static doublereal ctoc;
    static integer i, j;
    extern logical lsame_();
    static integer itype, k1, k2, k3, k4;
    static doublereal cfrom1;
    extern doublereal dlamch_();
    static doublereal cfromc;
    extern /* Subroutine */ int xerbla_();
    static doublereal bignum, smlnum, mul, cto1;


/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLASCL multiplies the M by N complex matrix A by the real scalar */
/*  CTO/CFROM.  This is done without over/underflow as long as the final
*/
/*  result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
*/
/*  A may be full, upper triangular, lower triangular, upper Hessenberg,
*/
/*  or banded. */

/*  Arguments */
/*  ========= */

/*  TYPE    (input) CHARACTER*1 */
/*          TYPE indices the storage type of the input matrix. */
/*          = 'G':  A is a full matrix. */
/*          = 'L':  A is a lower triangular matrix. */
/*          = 'U':  A is an upper triangular matrix. */
/*          = 'H':  A is an upper Hessenberg matrix. */
/*          = 'B':  A is a symmetric band matrix with lower bandwidth KL
*/
/*                  and upper bandwidth KU and with the only the lower */
/*                  half stored. */
/*          = 'Q':  A is a symmetric band matrix with lower bandwidth KL
*/
/*                  and upper bandwidth KU and with the only the upper */
/*                  half stored. */
/*          = 'Z':  A is a band matrix with lower bandwidth KL and upper
*/
/*                  bandwidth KU. */

/*  KL      (input) INTEGER */
/*          The lower bandwidth of A.  Referenced only if TYPE = 'B', */
/*          'Q' or 'Z'. */

/*  KU      (input) INTEGER */
/*          The upper bandwidth of A.  Referenced only if TYPE = 'B', */
/*          'Q' or 'Z'. */

/*  CFROM   (input) DOUBLE PRECISION */
/*  CTO     (input) DOUBLE PRECISION */
/*          The matrix A is multiplied by CTO/CFROM. A(I,J) is computed */
/*          without over/underflow if the final result CTO*A(I,J)/CFROM */
/*          can be represented without over/underflow.  CFROM must be */
/*          nonzero. */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A.  M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A.  N >= 0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,M) */
/*          The matrix to be multiplied by CTO/CFROM.  See TYPE for the */
/*          storage type. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,M). */

/*  INFO    (output) INTEGER */
/*          0  - successful exit */
/*          <0 - if INFO = -i, the i-th argument had an illegal value. */

/*  =====================================================================
*/

/*     .. Parameters .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input arguments */

    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    *info = 0;

    if (lsame_(type, "G", 1L, 1L)) {
        itype = 0;
    } else if (lsame_(type, "L", 1L, 1L)) {
        itype = 1;
    } else if (lsame_(type, "U", 1L, 1L)) {
        itype = 2;
    } else if (lsame_(type, "H", 1L, 1L)) {
        itype = 3;
    } else if (lsame_(type, "B", 1L, 1L)) {
        itype = 4;
    } else if (lsame_(type, "Q", 1L, 1L)) {
        itype = 5;
    } else if (lsame_(type, "Z", 1L, 1L)) {
        itype = 6;
    } else {
        itype = -1;
    }

    if (itype == -1) {
        *info = -1;
    } else if (*cfrom == 0.) {
        *info = -4;
    } else if (*m < 0) {
        *info = -6;
    } else if (*n < 0 || itype == 4 && *n != *m || itype == 5 && *n != *m) {
        *info = -7;
    } else if (itype <= 3 && *lda < max(1,*m)) {
        *info = -9;
    } else if (itype >= 4) {
/* Computing MAX */
        i__1 = *m - 1;
        if (*kl < 0 || *kl > max(i__1,0)) {
            *info = -2;
        } else /* if(complicated condition) */ {
/* Computing MAX */
            i__1 = *n - 1;
            if (*ku < 0 || *ku > max(i__1,0) || (itype == 4 || itype == 5) &&
                    *kl != *ku) {
                *info = -3;
            } else if (itype == 4 && *lda < *kl + 1 || itype == 5 && *lda < *
                    ku + 1 || itype == 6 && *lda < (*kl << 1) + *ku + 1) {
                *info = -9;
            }
        }
    }

    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("ZLASCL", &i__1, 6L);
        return 0;
    }

/*     Quick return if possible */

    if (*n == 0 || *m == 0) {
        return 0;
    }

/*     Get machine parameters */

    smlnum = dlamch_("S", 1L);
    bignum = 1. / smlnum;

    cfromc = *cfrom;
    ctoc = *cto;

L10:
    cfrom1 = cfromc * smlnum;
    cto1 = ctoc / bignum;
    if (abs(cfrom1) > abs(ctoc) && ctoc != 0.) {
        mul = smlnum;
        done = FALSE_;
        cfromc = cfrom1;
    } else if (abs(cto1) > abs(cfromc)) {
        mul = bignum;
        done = FALSE_;
        ctoc = cto1;
    } else {
        mul = ctoc / cfromc;
        done = TRUE_;
    }

    if (itype == 0) {

/*        Full matrix */

        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
            i__2 = *m;
            for (i = 1; i <= i__2; ++i) {
                i__3 = i + j * a_dim1;
                i__4 = i + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/* L20: */
            }
/* L30: */
        }

    } else if (itype == 1) {

/*        Lower triangular matrix */

        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
            i__2 = *m;
            for (i = j; i <= i__2; ++i) {
                i__3 = i + j * a_dim1;
                i__4 = i + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/* L40: */
            }
/* L50: */
        }

    } else if (itype == 2) {

/*        Upper triangular matrix */

        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
            i__2 = min(j,*m);
            for (i = 1; i <= i__2; ++i) {
                i__3 = i + j * a_dim1;
                i__4 = i + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/* L60: */
            }
/* L70: */
        }

    } else if (itype == 3) {

/*        Upper Hessenberg matrix */

        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/* Computing MIN */
            i__3 = j + 1;
            i__2 = min(i__3,*m);
            for (i = 1; i <= i__2; ++i) {
                i__3 = i + j * a_dim1;
                i__4 = i + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/* L80: */
            }
/* L90: */
        }

    } else if (itype == 4) {

/*        Lower half of a symmetric band matrix */

        k3 = *kl + 1;
        k4 = *n + 1;
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/* Computing MIN */
            i__3 = k3, i__4 = k4 - j;
            i__2 = min(i__3,i__4);
            for (i = 1; i <= i__2; ++i) {
                i__3 = i + j * a_dim1;
                i__4 = i + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/* L100: */
            }
/* L110: */
        }

    } else if (itype == 5) {

/*        Upper half of a symmetric band matrix */

        k1 = *ku + 2;
        k3 = *ku + 1;
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/* Computing MAX */
            i__2 = k1 - j;
            i__3 = k3;
            for (i = max(i__2,1); i <= i__3; ++i) {
                i__2 = i + j * a_dim1;
                i__4 = i + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__2].r = z__1.r, a[i__2].i = z__1.i;
/* L120: */
            }
/* L130: */
        }

    } else if (itype == 6) {

/*        Band matrix */

        k1 = *kl + *ku + 2;
        k2 = *kl + 1;
        k3 = (*kl << 1) + *ku + 1;
        k4 = *kl + *ku + 1 + *m;
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/* Computing MAX */
            i__3 = k1 - j;
/* Computing MIN */
            i__4 = k3, i__5 = k4 - j;
            i__2 = min(i__4,i__5);
            for (i = max(i__3,k2); i <= i__2; ++i) {
                i__3 = i + j * a_dim1;
                i__4 = i + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/* L140: */
            }
/* L150: */
        }

    }

    if (! done) {
        goto L10;
    }

    return 0;

/*     End of ZLASCL */

} /* zlascl_ */


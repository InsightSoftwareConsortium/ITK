#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void dlascl_(type, kl, ku, cfrom, cto, m, n, a, lda, info)
const char *type;
const integer *kl, *ku;
doublereal *cfrom, *cto;
const integer *m, *n;
doublereal *a;
const integer *lda;
integer *info;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static logical done;
    static doublereal ctoc;
    static integer i, j;
    static integer itype;
    static doublereal cfrom1;
    static doublereal cfromc;
    static doublereal bignum, smlnum, mul, cto1;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLASCL multiplies the M by N real matrix A by the real scalar         */
/*  CTO/CFROM.  This is done without over/underflow as long as the final  */
/*  result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that  */
/*  A may be full, upper triangular, lower triangular, upper Hessenberg,  */
/*  or banded.                                                            */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  TYPE    (input) CHARACTER*1                                           */
/*          TYPE indices the storage type of the input matrix.            */
/*          = 'G':  A is a full matrix.                                   */
/*          = 'L':  A is a lower triangular matrix.                       */
/*          = 'U':  A is an upper triangular matrix.                      */
/*          = 'H':  A is an upper Hessenberg matrix.                      */
/*          = 'B':  A is a symmetric band matrix with lower bandwidth KL  */
/*                  and upper bandwidth KU and with the only the lower    */
/*                  half stored.                                          */
/*          = 'Q':  A is a symmetric band matrix with lower bandwidth KL  */
/*                  and upper bandwidth KU and with the only the upper    */
/*                  half stored.                                          */
/*          = 'Z':  A is a band matrix with lower bandwidth KL and upper  */
/*                  bandwidth KU.                                         */
/*                                                                        */
/*  KL      (input) INTEGER                                               */
/*          The lower bandwidth of A.  Referenced only if TYPE = 'B',     */
/*          'Q' or 'Z'.                                                   */
/*                                                                        */
/*  KU      (input) INTEGER                                               */
/*          The upper bandwidth of A.  Referenced only if TYPE = 'B',     */
/*          'Q' or 'Z'.                                                   */
/*                                                                        */
/*  CFROM   (input) DOUBLE PRECISION                                      */
/*  CTO     (input) DOUBLE PRECISION                                      */
/*          The matrix A is multiplied by CTO/CFROM. A(I,J) is computed   */
/*          without over/underflow if the final result CTO*A(I,J)/CFROM   */
/*          can be represented without over/underflow.  CFROM must be     */
/*          nonzero.                                                      */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          The number of rows of the matrix A.  M >= 0.                  */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrix A.  N >= 0.               */
/*                                                                        */
/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,M)      */
/*          The matrix to be multiplied by CTO/CFROM.  See TYPE for the   */
/*          storage type.                                                 */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max(1,M).       */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          0  - successful exit                                          */
/*          <0 - if INFO = -i, the i-th argument had an illegal value.    */
/*                                                                        */
/*  ===================================================================== */

/*     Test the input arguments */

    *info = 0;

    if (lsame_(type, "G")) {
        itype = 0;
    } else if (lsame_(type, "L")) {
        itype = 1;
    } else if (lsame_(type, "U")) {
        itype = 2;
    } else if (lsame_(type, "H")) {
        itype = 3;
    } else if (lsame_(type, "B")) {
        itype = 4;
    } else if (lsame_(type, "Q")) {
        itype = 5;
    } else if (lsame_(type, "Z")) {
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
    } else if (*n < 0 || ( ( itype == 4 || itype == 5 ) && *n != *m) ) {
        *info = -7;
    } else if (itype <= 3 && *lda < max(1,*m)) {
        *info = -9;
    } else if (itype >= 4) {
        if (*kl < 0 || *kl > max(*m - 1,0)) {
            *info = -2;
        } else /* if(complicated condition) */ {
            if (*ku < 0 || *ku > max(*n - 1,0) || ( (itype == 4 || itype == 5) && *kl != *ku) ) {
                *info = -3;
            } else if ( (itype == 4 && *lda < *kl + 1 ) ||
                        (itype == 5 && *lda < * ku + 1 ) ||
                        (itype == 6 && *lda < (*kl << 1) + *ku + 1) ) {
                *info = -9;
            }
        }
    }

    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("DLASCL", &i__1);
        return;
    }

/*     Quick return if possible */

    if (*n == 0 || *m == 0) {
        return;
    }

/*     Get machine parameters */

    smlnum = dlamch_("S");
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

        for (j = 0; j < *n; ++j) {
            for (i = 0; i < *m; ++i) {
                a[i + j * *lda] *= mul;
            }
        }

    } else if (itype == 1) {

/*        Lower triangular matrix */

        for (j = 0; j < *n; ++j) {
            for (i = j; i < *m; ++i) {
                a[i + j * *lda] *= mul;
            }
        }

    } else if (itype == 2) {

/*        Upper triangular matrix */

        for (j = 0; j < *n; ++j) {
            for (i = 0; i <= j && i < *m; ++i) {
                a[i + j * *lda] *= mul;
            }
        }

    } else if (itype == 3) {

/*        Upper Hessenberg matrix */

        for (j = 0; j < *n; ++j) {
            for (i = 0; i <= j+1 && i < *m; ++i) {
                a[i + j * *lda] *= mul;
            }
        }

    } else if (itype == 4) {

/*        Lower half of a symmetric band matrix */

        for (j = 0; j < *n; ++j) {
            for (i = 0; i < min(*kl+1,*n-j); ++i) {
                a[i + j * *lda] *= mul;
            }
        }

    } else if (itype == 5) {

/*        Upper half of a symmetric band matrix */

        for (j = 0; j < *n; ++j) {
            for (i = max(*ku-j,0); i <= *ku; ++i) {
                a[i + j * *lda] *= mul;
            }
        }

    } else if (itype == 6) {

/*        Band matrix */

        for (j = 0; j < *n; ++j) {
            for (i = max(*kl + *ku - j,*kl); i <= *kl*2 + *ku && i < *kl + *ku + *m - j; ++i) {
                a[i + j * *lda] *= mul;
            }
        }
    }

    if (! done) {
        goto L10;
    }

} /* dlascl_ */

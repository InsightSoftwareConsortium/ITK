#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__1 = 1;

/* Subroutine */ void dgecon_(norm, n, a, lda, anorm, rcond, work, iwork, info)
char *norm;
integer *n;
doublereal *a;
integer *lda;
doublereal *anorm, *rcond, *work;
integer *iwork, *info;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer kase, kase1;
    static doublereal scale;
    static doublereal sl;
    static integer ix;
    static doublereal su;
    static doublereal ainvnm;
    static logical onenrm;
    static char normin[1];
    static doublereal smlnum;


/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DGECON estimates the reciprocal of the condition number of a general  */
/*  real matrix A, in either the 1-norm or the infinity-norm, using       */
/*  the LU factorization computed by DGETRF.                              */
/*                                                                        */
/*  An estimate is obtained for norm(inv(A)), and the reciprocal of the   */
/*  condition number is computed as                                       */
/*     RCOND = 1 / ( norm(A) * norm(inv(A)) ).                            */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  NORM    (input) CHARACTER*1                                           */
/*          Specifies whether the 1-norm condition number or the          */
/*          infinity-norm condition number is required:                   */
/*          = '1' or 'O':  1-norm;                                        */
/*          = 'I':         Infinity-norm.                                 */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrix A.  N >= 0.                           */
/*                                                                        */
/*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)             */
/*          The factors L and U from the factorization A = P*L*U          */
/*          as computed by DGETRF.                                        */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max(1,N).       */
/*                                                                        */
/*  ANORM   (input) DOUBLE PRECISION                                      */
/*          If NORM = '1' or 'O', the 1-norm of the original matrix A.    */
/*          If NORM = 'I', the infinity-norm of the original matrix A.    */
/*                                                                        */
/*  RCOND   (output) DOUBLE PRECISION                                     */
/*          The reciprocal of the condition number of the matrix A,       */
/*          computed as RCOND = 1/(norm(A) * norm(inv(A))).               */
/*                                                                        */
/*  WORK    (workspace) DOUBLE PRECISION array, dimension (4*N)           */
/*                                                                        */
/*  IWORK   (workspace) INTEGER array, dimension (N)                      */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0:  successful exit                                         */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value    */
/*                                                                        */
/*  ===================================================================== */

/*     Test the input parameters. */

    *info = 0;
    onenrm = *(unsigned char *)norm == '1' || lsame_(norm, "O");
    if (! onenrm && ! lsame_(norm, "I")) {
        *info = -1;
    } else if (*n < 0) {
        *info = -2;
    } else if (*lda < max(1,*n)) {
        *info = -4;
    } else if (*anorm < 0.) {
        *info = -5;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("DGECON", &i__1);
        return;
    }

/*     Quick return if possible */

    *rcond = 0.;
    if (*n == 0) {
        *rcond = 1.;
        return;
    } else if (*anorm == 0.) {
        return;
    }

    smlnum = dlamch_("Safe minimum");

/*     Estimate the norm of inv(A). */

    ainvnm = 0.;
    *(unsigned char *)normin = 'N';
    if (onenrm) {
        kase1 = 1;
    } else {
        kase1 = 2;
    }
    kase = 0;
L10:
    dlacon_(n, &work[*n], work, iwork, &ainvnm, &kase);
    if (kase != 0) {
        if (kase == kase1) {

/*           Multiply by inv(L). */

            dlatrs_("Lower", "No transpose", "Unit", normin, n, a, lda, work, &sl, &work[*n << 1], info);

/*           Multiply by inv(U). */

            dlatrs_("Upper", "No transpose", "Non-unit", normin, n, a, lda, work, &su, &work[*n * 3], info);
        } else {

/*           Multiply by inv(U'). */

            dlatrs_("Upper", "Transpose", "Non-unit", normin, n, a, lda, work, &su, &work[*n * 3], info);

/*           Multiply by inv(L'). */

            dlatrs_("Lower", "Transpose", "Unit", normin, n, a, lda, work, &sl, &work[*n << 1], info);
        }

/*        Divide X by 1/(SL*SU) if doing so will not cause overflow. */

        scale = sl * su;
        *(unsigned char *)normin = 'Y';
        if (scale != 1.) {
            ix = idamax_(n, work, &c__1) - 1;
            if (scale < abs(work[ix]) * smlnum || scale == 0.) {
                return;
            }
            drscl_(n, &scale, work, &c__1);
        }
        goto L10;
    }

/*     Compute the estimate of the reciprocal condition number. */

    if (ainvnm != 0.) {
        *rcond = 1. / ainvnm / *anorm;
    }
} /* dgecon_ */

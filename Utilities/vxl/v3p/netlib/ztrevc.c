#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static integer c__1 = 1;
static doublecomplex c_b17 = {1.,0.};

/* Subroutine */ void ztrevc_(side, howmny, select, n, t, ldt, vl, ldvl, vr, ldvr, mm, m, work, rwork, info)
const char *side, *howmny;
logical *select;
const integer *n;
doublecomplex *t;
const integer *ldt;
doublecomplex *vl;
const integer *ldvl;
doublecomplex *vr;
const integer *ldvr;
const integer *mm;
integer *m;
doublecomplex *work;
doublereal *rwork;
integer *info;
{
    /* System generated locals */
    integer i__1, i__2;
    doublecomplex z__1;

    /* Local variables */
    static logical allv;
    static doublereal unfl, ovfl, smin;
    static logical over;
    static integer i, j, k;
    static doublereal scale;
    static doublereal remax;
    static logical leftv, bothv;
    static logical somev;
    static integer ii, ki;
    static integer is;
    static logical rightv;
    static doublereal smlnum;
    static doublereal ulp;

/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZTREVC computes some or all of the right and/or left eigenvectors of  */
/*  a complex upper triangular matrix T.                                  */
/*                                                                        */
/*  The right eigenvector x and the left eigenvector y of T corresponding */
/*  to an eigenvalue w are defined by:                                    */
/*                                                                        */
/*               T*x = w*x,     y'*T = w*y'                               */
/*                                                                        */
/*  where y' denotes the conjugate transpose of the vector y.             */
/*                                                                        */
/*  If all eigenvectors are requested, the routine may either return the  */
/*  matrices X and/or Y of right or left eigenvectors of T, or the        */
/*  products Q*X and/or Q*Y, where Q is an input unitary                  */
/*  matrix. If T was obtained from the Schur factorization of an          */
/*  original matrix A = Q*T*Q', then Q*X and Q*Y are the matrices of      */
/*  right or left eigenvectors of A.                                      */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  SIDE    (input) CHARACTER*1                                           */
/*          = 'R':  compute right eigenvectors only;                      */
/*          = 'L':  compute left eigenvectors only;                       */
/*          = 'B':  compute both right and left eigenvectors.             */
/*                                                                        */
/*  HOWMNY  (input) CHARACTER*1                                           */
/*          = 'A':  compute all right and/or left eigenvectors;           */
/*          = 'B':  compute all right and/or left eigenvectors,           */
/*                  and backtransform them using the input matrices       */
/*                  supplied in VR and/or VL;                             */
/*          = 'S':  compute selected right and/or left eigenvectors,      */
/*                  specified by the logical array SELECT.                */
/*                                                                        */
/*  SELECT  (input) LOGICAL array, dimension (N)                          */
/*          If HOWMNY = 'S', SELECT specifies the eigenvectors to be      */
/*          computed.                                                     */
/*          If HOWMNY = 'A' or 'B', SELECT is not referenced.             */
/*          To select the eigenvector corresponding to the j-th           */
/*          eigenvalue, SELECT(j) must be set to .TRUE..                  */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrix T. N >= 0.                            */
/*                                                                        */
/*  T       (input/output) COMPLEX*16 array, dimension (LDT,N)            */
/*          The upper triangular matrix T.  T is modified, but restored   */
/*          on exit.                                                      */
/*                                                                        */
/*  LDT     (input) INTEGER                                               */
/*          The leading dimension of the array T. LDT >= max(1,N).        */
/*                                                                        */
/*  VL      (input/output) COMPLEX*16 array, dimension (LDVL,MM)          */
/*          On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must      */
/*          contain an N-by-N matrix Q (usually the unitary matrix Q of   */
/*          Schur vectors returned by ZHSEQR).                            */
/*          On exit, if SIDE = 'L' or 'B', VL contains:                   */
/*          if HOWMNY = 'A', the matrix Y of left eigenvectors of T;      */
/*          if HOWMNY = 'B', the matrix Q*Y;                              */
/*          if HOWMNY = 'S', the left eigenvectors of T specified by      */
/*                           SELECT, stored consecutively in the columns  */
/*                           of VL, in the same order as their            */
/*                           eigenvalues.                                 */
/*          If SIDE = 'R', VL is not referenced.                          */
/*                                                                        */
/*  LDVL    (input) INTEGER                                               */
/*          The leading dimension of the array VL.  LDVL >= max(1,N) if   */
/*          SIDE = 'L' or 'B'; LDVL >= 1 otherwise.                       */
/*                                                                        */
/*  VR      (input/output) COMPLEX*16 array, dimension (LDVR,MM)          */
/*          On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must      */
/*          contain an N-by-N matrix Q (usually the unitary matrix Q of   */
/*          Schur vectors returned by ZHSEQR).                            */
/*          On exit, if SIDE = 'R' or 'B', VR contains:                   */
/*          if HOWMNY = 'A', the matrix X of right eigenvectors of T;     */
/*          if HOWMNY = 'B', the matrix Q*X;                              */
/*          if HOWMNY = 'S', the right eigenvectors of T specified by     */
/*                           SELECT, stored consecutively in the columns  */
/*                           of VR, in the same order as their            */
/*                           eigenvalues.                                 */
/*          If SIDE = 'L', VR is not referenced.                          */
/*                                                                        */
/*  LDVR    (input) INTEGER                                               */
/*          The leading dimension of the array VR.  LDVR >= max(1,N) if   */
/*           SIDE = 'R' or 'B'; LDVR >= 1 otherwise.                      */
/*                                                                        */
/*  MM      (input) INTEGER                                               */
/*          The number of columns in the arrays VL and/or VR. MM >= M.    */
/*                                                                        */
/*  M       (output) INTEGER                                              */
/*          The number of columns in the arrays VL and/or VR actually     */
/*          used to store the eigenvectors.  If HOWMNY = 'A' or 'B', M    */
/*          is set to N.  Each selected eigenvector occupies one          */
/*          column.                                                       */
/*                                                                        */
/*  WORK    (workspace) COMPLEX*16 array, dimension (2*N)                 */
/*                                                                        */
/*  RWORK   (workspace) DOUBLE PRECISION array, dimension (N)             */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0:  successful exit                                         */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value    */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  The algorithm used in this program is basically backward (forward)    */
/*  substitution, with scaling to make the the code robust against        */
/*  possible overflow.                                                    */
/*                                                                        */
/*  Each eigenvector is normalized so that the element of largest         */
/*  magnitude has magnitude 1; here the magnitude of a complex number     */
/*  (x,y) is taken to be |x| + |y|.                                       */
/*                                                                        */
/*  ===================================================================== */

    bothv = lsame_(side, "B");
    rightv = lsame_(side, "R") || bothv;
    leftv = lsame_(side, "L") || bothv;

    allv = lsame_(howmny, "A");
    over = lsame_(howmny, "B") || lsame_(howmny, "O");
    somev = lsame_(howmny, "S");

/*     Set M to the number of columns required to store the selected */
/*     eigenvectors. */

    if (somev) {
        *m = 0;
        for (j = 0; j < *n; ++j) {
            if (select[j]) {
                ++(*m);
            }
        }
    } else {
        *m = *n;
    }

    *info = 0;
    if (! rightv && ! leftv) {
        *info = -1;
    } else if (! allv && ! over && ! somev) {
        *info = -2;
    } else if (*n < 0) {
        *info = -4;
    } else if (*ldt < max(1,*n)) {
        *info = -6;
    } else if (*ldvl < 1 || (leftv && *ldvl < *n)) {
        *info = -8;
    } else if (*ldvr < 1 || (rightv && *ldvr < *n)) {
        *info = -10;
    } else if (*mm < *m) {
        *info = -11;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("ZTREVC", &i__1);
        return;
    }

/*     Quick return if possible. */

    if (*n == 0) {
        return;
    }

/*     Set the constants to control overflow. */

    unfl = dlamch_("Safe minimum");
    ovfl = 1. / unfl;
    dlabad_(&unfl, &ovfl);
    ulp = dlamch_("Precision");
    smlnum = unfl * (*n / ulp);

/*     Store the diagonal elements of T in working array WORK. */

    for (i = 0; i < *n; ++i) {
        i__1 = i + *n;
        i__2 = i + i * *ldt; /* index [i,i] */
        work[i__1].r = t[i__2].r, work[i__1].i = t[i__2].i;
    }

/*     Compute 1-norm of each column of strictly upper triangular */
/*     part of T to control overflow in triangular solver. */

    rwork[0] = 0.;
    for (j = 1; j < *n; ++j) {
        rwork[j] = dzasum_(&j, &t[j * *ldt], &c__1);
    }

    if (rightv) {

/*        Compute right eigenvectors. */

        is = *m - 1;
        for (ki = *n - 1; ki >= 0; --ki) {

            if (somev) {
                if (! select[ki]) {
                    continue; /* next ki */
                }
            }
            i__1 = ki + ki * *ldt; /* index [ki,ki] */
            smin = ulp * (abs(t[i__1].r) + abs(t[i__1].i));
            smin = max(smin, smlnum);

            work[0].r = 1., work[0].i = 0.;

/*           Form right-hand side. */

            for (k = 0; k < ki; ++k) {
                i__1 = k + ki * *ldt; /* index [k,ki] */
                work[k].r = -t[i__1].r, work[k].i = -t[i__1].i;
            }

/*           Solve the triangular system: */
/*              (T(1:KI-1,1:KI-1) - T(KI,KI))*X = SCALE*WORK. */

            for (k = 0; k < ki; ++k) {
                i__1 = k + k * *ldt; /* index [k,k] */
                i__2 = ki + ki * *ldt; /* index [ki,ki] */
                t[i__1].r -= t[i__2].r,
                t[i__1].i -= t[i__2].i;
                if (abs(t[i__1].r) + abs(t[i__1].i) < smin) {
                    t[i__1].r = smin, t[i__1].i = 0.;
                }
            }

            if (ki > 0) {
                zlatrs_("Upper", "No transpose", "Non-unit", "Y", &ki, t, ldt, work, &scale, rwork, info);
                work[ki].r = scale, work[ki].i = 0.;
            }

/*           Copy the vector x or Q*x to VR and normalize. */

            if (! over) {
                k = ki+1;
                zcopy_(&k, work, &c__1, &vr[is * *ldvr], &c__1);

                ii = izamax_(&k, &vr[is * *ldvr], &c__1);
                i__1 = ii-1 + is * *ldvr; /* index [ii-1,is] */
                remax = 1. / (abs(vr[i__1].r) + abs(vr[i__1].i));
                zdscal_(&k, &remax, &vr[is * *ldvr], &c__1);

                for (k = ki+1; k < *n; ++k) {
                    i__1 = k + is * *ldvr; /* index [k,is] */
                    vr[i__1].r = 0., vr[i__1].i = 0.;
                }
            } else {
                if (ki > 0) {
                    z__1.r = scale, z__1.i = 0.;
                    zgemv_("N", n, &ki, &c_b17, vr, ldvr, work, &c__1, &z__1, &vr[ki * *ldvr], &c__1);
                }

                ii = izamax_(n, &vr[ki * *ldvr], &c__1);
                i__1 = ii-1 + ki * *ldvr; /* index [ii-1,ki] */
                remax = 1. / (abs(vr[i__1].r) + abs(vr[i__1].i));
                zdscal_(n, &remax, &vr[ki * *ldvr], &c__1);
            }

/*           Set back the original diagonal elements of T. */

            for (k = 0; k < ki; ++k) {
                i__1 = k + k * *ldt; /* index [k,k] */
                i__2 = k + *n;
                t[i__1].r = work[i__2].r, t[i__1].i = work[i__2].i;
            }

            --is;
        }
    }

    if (leftv) {

/*        Compute left eigenvectors. */

        is = 0;
        for (ki = 0; ki < *n; ++ki) {
            if (somev) {
                if (! select[ki]) {
                    continue; /* next ki */
                }
            }
            i__1 = ki + ki * *ldt; /* index [ki,ki] */
            smin = ulp * (abs(t[i__1].r) + abs(t[i__1].i));
            smin = max(smin, smlnum);
            work[*n - 1].r = 1., work[*n - 1].i = 0.;

/*           Form right-hand side. */

            for (k = ki+1; k < *n; ++k) {
                work[k].r = -t[ki + k * *ldt].r, work[k].i = t[ki + k * *ldt].i;
            }

/*           Solve the triangular system: */
/*              (T(KI+1:N,KI+1:N) - T(KI,KI))'*X = SCALE*WORK. */

            for (k = ki+1; k < *n; ++k) {
                i__1 = k + k * *ldt; /* index [k,k] */
                i__2 = ki + ki * *ldt; /* index [ki,ki] */
                t[i__1].r -= t[i__2].r,
                t[i__1].i -= t[i__2].i;
                if (abs(t[i__1].r) + abs(t[i__1].i) < smin) {
                    t[i__1].r = smin, t[i__1].i = 0.;
                }
            }

            k = ki + 1;
            if (k < *n) {
                i__1 = *n - k;
                zlatrs_("Upper", "Conjugate transpose", "Non-unit", "Y",
                        &i__1, &t[k + k * *ldt], ldt, &work[k], &scale, rwork, info);
                work[ki].r = scale, work[ki].i = 0.;
            }

/*           Copy the vector x or Q*x to VL and normalize. */

            if (! over) {
                i__1 = *n - ki;
                zcopy_(&i__1, &work[ki], &c__1, &vl[ki + is * *ldvl], &c__1);
                ii = izamax_(&i__1, &vl[ki + is * *ldvl], &c__1) + ki;
                i__2 = ii-1 + is * *ldvl; /* index [ii-1,is] */
                remax = 1. / (abs(vl[i__2].r) + abs(vl[i__2].i));
                zdscal_(&i__1, &remax, &vl[ki + is * *ldvl], &c__1);

                for (k = 0; k < ki; ++k) {
                    i__1 = k + is * *ldvl; /* index [k,is] */
                    vl[i__1].r = 0., vl[i__1].i = 0.;
                }
            } else {
                k = ki + 1;
                if (k < *n) {
                    i__1 = *n - k;
                    z__1.r = scale, z__1.i = 0.;
                    zgemv_("N", n, &i__1, &c_b17, &vl[k * *ldvl],
                           ldvl, &work[k], &c__1, &z__1, &vl[ki * *ldvl], &c__1);
                }

                ii = izamax_(n, &vl[ki * *ldvl], &c__1);
                i__1 = ii-1 + ki * *ldvl; /* index [ii-1,ki] */
                remax = 1. / (abs(vl[i__1].r) + abs(vl[i__1].i));
                zdscal_(n, &remax, &vl[ki * *ldvl], &c__1);
            }

/*           Set back the original diagonal elements of T. */

            for (k = ki+1; k < *n; ++k) {
                i__1 = k + k * *ldt; /* index [k,k] */
                i__2 = k + *n;
                t[i__1].r = work[i__2].r, t[i__1].i = work[i__2].i;
            }
            ++is;
        }
    }
} /* ztrevc_ */

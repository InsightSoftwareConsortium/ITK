#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void zgebak_(job, side, n, ilo, ihi, scale, m, v, ldv, info)
const char *job, *side;
const integer *n;
integer *ilo, *ihi;
doublereal *scale;
const integer *m;
doublecomplex *v;
const integer *ldv;
integer *info;
{
    /* System generated locals */
    integer v_dim1, v_offset, i__1;

    /* Local variables */
    static integer i, k;
    static doublereal s;
    static logical leftv;
    static integer ii;
    static logical rightv;


/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZGEBAK forms the right or left eigenvectors of a complex general      */
/*  matrix by backward transformation on the computed eigenvectors of the */
/*  balanced matrix output by ZGEBAL.                                     */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  JOB     (input) CHARACTER*1                                           */
/*          Specifies the type of backward transformation required:       */
/*          = 'N', do nothing, return immediately;                        */
/*          = 'P', do backward transformation for permutation only;       */
/*          = 'S', do backward transformation for scaling only;           */
/*          = 'B', do backward transformations for both permutation and   */
/*                 scaling.                                               */
/*          JOB must be the same as the argument JOB supplied to ZGEBAL.  */
/*                                                                        */
/*  SIDE    (input) CHARACTER*1                                           */
/*          = 'R':  V contains right eigenvectors;                        */
/*          = 'L':  V contains left eigenvectors.                         */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of rows of the matrix V.  N >= 0.                  */
/*                                                                        */
/*  ILO     (input) INTEGER                                               */
/*  IHI     (input) INTEGER                                               */
/*          The integers ILO and IHI determined by ZGEBAL.                */
/*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.      */
/*                                                                        */
/*  SCALE   (input) DOUBLE PRECISION array, dimension (N)                 */
/*          Details of the permutation and scaling factors, as returned   */
/*          by ZGEBAL.                                                    */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          The number of columns of the matrix V.  M >= 0.               */
/*                                                                        */
/*  V       (input/output) COMPLEX*16 array, dimension (LDV,M)            */
/*          On entry, the matrix of right or left eigenvectors to be      */
/*          transformed, as returned by ZHSEIN or ZTREVC.                 */
/*          On exit, V is overwritten by the transformed eigenvectors.    */
/*                                                                        */
/*  LDV     (input) INTEGER                                               */
/*          The leading dimension of the array V. LDV >= max(1,N).        */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0:  successful exit                                         */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value.   */
/*                                                                        */
/*  ===================================================================== */

    /* Parameter adjustments */
    v_dim1 = *ldv;
    v_offset = v_dim1 + 1;
    v -= v_offset;
    --scale;

/*     Decode and Test the input parameters */

    rightv = lsame_(side, "R");
    leftv = lsame_(side, "L");

    *info = 0;
    if (! lsame_(job, "N") && ! lsame_(job, "P") && ! lsame_(job, "S") && ! lsame_(job, "B")) {
        *info = -1;
    } else if (! rightv && ! leftv) {
        *info = -2;
    } else if (*n < 0) {
        *info = -3;
    } else if (*ilo < 1 || *ilo > max(1,*n)) {
        *info = -4;
    } else if (*ihi < min(*ilo,*n) || *ihi > *n) {
        *info = -5;
    } else if (*m < 0) {
        *info = -7;
    } else if (*ldv < max(1,*n)) {
        *info = -9;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("ZGEBAK", &i__1);
        return;
    }

/*     Quick return if possible */

    if (*n == 0) {
        return;
    }
    if (*m == 0) {
        return;
    }
    if (lsame_(job, "N")) {
        return;
    }

    if (*ilo == *ihi) {
        goto L30;
    }

/*     Backward balance */

    if (lsame_(job, "S") || lsame_(job, "B")) {

        if (rightv) {
            i__1 = *ihi;
            for (i = *ilo; i <= i__1; ++i) {
                s = scale[i];
                zdscal_(m, &s, &v[i + v_dim1], ldv);
            }
        }

        if (leftv) {
            i__1 = *ihi;
            for (i = *ilo; i <= i__1; ++i) {
                s = 1. / scale[i];
                zdscal_(m, &s, &v[i + v_dim1], ldv);
            }
        }
    }

/*     Backward permutation */

/*     For  I = ILO-1 step -1 until 1, */
/*              IHI+1 step 1 until N do -- */

L30:
    if (lsame_(job, "P") || lsame_(job, "B")) {
        if (rightv) {
            i__1 = *n;
            for (ii = 1; ii <= i__1; ++ii) {
                i = ii;
                if (i >= *ilo && i <= *ihi) {
                    goto L40;
                }
                if (i < *ilo) {
                    i = *ilo - ii;
                }
                k = (integer) scale[i];
                if (k == i) {
                    goto L40;
                }
                zswap_(m, &v[i + v_dim1], ldv, &v[k + v_dim1], ldv);
L40:
                ;
            }
        }

        if (leftv) {
            i__1 = *n;
            for (ii = 1; ii <= i__1; ++ii) {
                i = ii;
                if (i >= *ilo && i <= *ihi) {
                    goto L50;
                }
                if (i < *ilo) {
                    i = *ilo - ii;
                }
                k = (integer) scale[i];
                if (k == i) {
                    goto L50;
                }
                zswap_(m, &v[i + v_dim1], ldv, &v[k + v_dim1], ldv);
L50:
                ;
            }
        }
    }
} /* zgebak_ */

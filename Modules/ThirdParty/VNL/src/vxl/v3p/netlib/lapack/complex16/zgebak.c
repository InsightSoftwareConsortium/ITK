/* lapack/complex16/zgebak.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "v3p_netlib.h"

/*<    >*/
/* Subroutine */ int zgebak_(char *job, char *side, integer *n, integer *ilo,
        integer *ihi, doublereal *scale, integer *m, doublecomplex *v,
        integer *ldv, integer *info, ftnlen job_len, ftnlen side_len)
{
    /* System generated locals */
    integer v_dim1, v_offset, i__1;

    /* Local variables */
    integer i__, k;
    doublereal s;
    integer ii;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    logical leftv;
    extern /* Subroutine */ int zswap_(integer *, doublecomplex *, integer *,
            doublecomplex *, integer *), xerbla_(char *, integer *, ftnlen),
            zdscal_(integer *, doublereal *, doublecomplex *, integer *);
    logical rightv;
    (void)job_len;
    (void)side_len;

/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          JOB, SIDE >*/
/*<       INTEGER            IHI, ILO, INFO, LDV, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   SCALE( * ) >*/
/*<       COMPLEX*16         V( LDV, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGEBAK forms the right or left eigenvectors of a complex general */
/*  matrix by backward transformation on the computed eigenvectors of the */
/*  balanced matrix output by ZGEBAL. */

/*  Arguments */
/*  ========= */

/*  JOB     (input) CHARACTER*1 */
/*          Specifies the type of backward transformation required: */
/*          = 'N', do nothing, return immediately; */
/*          = 'P', do backward transformation for permutation only; */
/*          = 'S', do backward transformation for scaling only; */
/*          = 'B', do backward transformations for both permutation and */
/*                 scaling. */
/*          JOB must be the same as the argument JOB supplied to ZGEBAL. */

/*  SIDE    (input) CHARACTER*1 */
/*          = 'R':  V contains right eigenvectors; */
/*          = 'L':  V contains left eigenvectors. */

/*  N       (input) INTEGER */
/*          The number of rows of the matrix V.  N >= 0. */

/*  ILO     (input) INTEGER */
/*  IHI     (input) INTEGER */
/*          The integers ILO and IHI determined by ZGEBAL. */
/*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0. */

/*  SCALE   (input) DOUBLE PRECISION array, dimension (N) */
/*          Details of the permutation and scaling factors, as returned */
/*          by ZGEBAL. */

/*  M       (input) INTEGER */
/*          The number of columns of the matrix V.  M >= 0. */

/*  V       (input/output) COMPLEX*16 array, dimension (LDV,M) */
/*          On entry, the matrix of right or left eigenvectors to be */
/*          transformed, as returned by ZHSEIN or ZTREVC. */
/*          On exit, V is overwritten by the transformed eigenvectors. */

/*  LDV     (input) INTEGER */
/*          The leading dimension of the array V. LDV >= max(1,N). */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE >*/
/*<       PARAMETER          ( ONE = 1.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            LEFTV, RIGHTV >*/
/*<       INTEGER            I, II, K >*/
/*<       DOUBLE PRECISION   S >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA, ZDSCAL, ZSWAP >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX, MIN >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Decode and Test the input parameters */

/*<       RIGHTV = LSAME( SIDE, 'R' ) >*/
    /* Parameter adjustments */
    --scale;
    v_dim1 = *ldv;
    v_offset = 1 + v_dim1;
    v -= v_offset;

    /* Function Body */
    rightv = lsame_(side, "R", (ftnlen)1, (ftnlen)1);
/*<       LEFTV = LSAME( SIDE, 'L' ) >*/
    leftv = lsame_(side, "L", (ftnlen)1, (ftnlen)1);

/*<       INFO = 0 >*/
    *info = 0;
/*<    >*/
    if (! lsame_(job, "N", (ftnlen)1, (ftnlen)1) && ! lsame_(job, "P", (
            ftnlen)1, (ftnlen)1) && ! lsame_(job, "S", (ftnlen)1, (ftnlen)1)
            && ! lsame_(job, "B", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( .NOT.RIGHTV .AND. .NOT.LEFTV ) THEN >*/
    } else if (! rightv && ! leftv) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( ILO.LT.1 .OR. ILO.GT.MAX( 1, N ) ) THEN >*/
    } else if (*ilo < 1 || *ilo > max(1,*n)) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( IHI.LT.MIN( ILO, N ) .OR. IHI.GT.N ) THEN >*/
    } else if (*ihi < min(*ilo,*n) || *ihi > *n) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( M.LT.0 ) THEN >*/
    } else if (*m < 0) {
/*<          INFO = -7 >*/
        *info = -7;
/*<       ELSE IF( LDV.LT.MAX( 1, N ) ) THEN >*/
    } else if (*ldv < max(1,*n)) {
/*<          INFO = -9 >*/
        *info = -9;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZGEBAK', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZGEBAK", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<    >*/
    if (*n == 0) {
        return 0;
    }
/*<    >*/
    if (*m == 0) {
        return 0;
    }
/*<    >*/
    if (lsame_(job, "N", (ftnlen)1, (ftnlen)1)) {
        return 0;
    }

/*<    >*/
    if (*ilo == *ihi) {
        goto L30;
    }

/*     Backward balance */

/*<       IF( LSAME( JOB, 'S' ) .OR. LSAME( JOB, 'B' ) ) THEN >*/
    if (lsame_(job, "S", (ftnlen)1, (ftnlen)1) || lsame_(job, "B", (ftnlen)1,
            (ftnlen)1)) {

/*<          IF( RIGHTV ) THEN >*/
        if (rightv) {
/*<             DO 10 I = ILO, IHI >*/
            i__1 = *ihi;
            for (i__ = *ilo; i__ <= i__1; ++i__) {
/*<                S = SCALE( I ) >*/
                s = scale[i__];
/*<                CALL ZDSCAL( M, S, V( I, 1 ), LDV ) >*/
                zdscal_(m, &s, &v[i__ + v_dim1], ldv);
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<          END IF >*/
        }

/*<          IF( LEFTV ) THEN >*/
        if (leftv) {
/*<             DO 20 I = ILO, IHI >*/
            i__1 = *ihi;
            for (i__ = *ilo; i__ <= i__1; ++i__) {
/*<                S = ONE / SCALE( I ) >*/
                s = 1. / scale[i__];
/*<                CALL ZDSCAL( M, S, V( I, 1 ), LDV ) >*/
                zdscal_(m, &s, &v[i__ + v_dim1], ldv);
/*<    20       CONTINUE >*/
/* L20: */
            }
/*<          END IF >*/
        }

/*<       END IF >*/
    }

/*     Backward permutation */

/*     For  I = ILO-1 step -1 until 1, */
/*              IHI+1 step 1 until N do -- */

/*<    30 CONTINUE >*/
L30:
/*<       IF( LSAME( JOB, 'P' ) .OR. LSAME( JOB, 'B' ) ) THEN >*/
    if (lsame_(job, "P", (ftnlen)1, (ftnlen)1) || lsame_(job, "B", (ftnlen)1,
            (ftnlen)1)) {
/*<          IF( RIGHTV ) THEN >*/
        if (rightv) {
/*<             DO 40 II = 1, N >*/
            i__1 = *n;
            for (ii = 1; ii <= i__1; ++ii) {
/*<                I = II >*/
                i__ = ii;
/*<    >*/
                if (i__ >= *ilo && i__ <= *ihi) {
                    goto L40;
                }
/*<    >*/
                if (i__ < *ilo) {
                    i__ = *ilo - ii;
                }
/*<                K = SCALE( I ) >*/
                k = (integer) scale[i__];
/*<    >*/
                if (k == i__) {
                    goto L40;
                }
/*<                CALL ZSWAP( M, V( I, 1 ), LDV, V( K, 1 ), LDV ) >*/
                zswap_(m, &v[i__ + v_dim1], ldv, &v[k + v_dim1], ldv);
/*<    40       CONTINUE >*/
L40:
                ;
            }
/*<          END IF >*/
        }

/*<          IF( LEFTV ) THEN >*/
        if (leftv) {
/*<             DO 50 II = 1, N >*/
            i__1 = *n;
            for (ii = 1; ii <= i__1; ++ii) {
/*<                I = II >*/
                i__ = ii;
/*<    >*/
                if (i__ >= *ilo && i__ <= *ihi) {
                    goto L50;
                }
/*<    >*/
                if (i__ < *ilo) {
                    i__ = *ilo - ii;
                }
/*<                K = SCALE( I ) >*/
                k = (integer) scale[i__];
/*<    >*/
                if (k == i__) {
                    goto L50;
                }
/*<                CALL ZSWAP( M, V( I, 1 ), LDV, V( K, 1 ), LDV ) >*/
                zswap_(m, &v[i__ + v_dim1], ldv, &v[k + v_dim1], ldv);
/*<    50       CONTINUE >*/
L50:
                ;
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of ZGEBAK */

/*<       END >*/
} /* zgebak_ */

#ifdef __cplusplus
        }
#endif

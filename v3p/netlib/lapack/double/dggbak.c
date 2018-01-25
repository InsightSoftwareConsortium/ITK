/* lapack/double/dggbak.f -- translated by f2c (version 20050501).
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
/* Subroutine */ int dggbak_(char *job, char *side, integer *n, integer *ilo,
        integer *ihi, doublereal *lscale, doublereal *rscale, integer *m,
        doublereal *v, integer *ldv, integer *info, ftnlen job_len, ftnlen
        side_len)
{
    /* System generated locals */
    integer v_dim1, v_offset, i__1;

    /* Local variables */
    integer i__, k;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *);
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int dswap_(integer *, doublereal *, integer *,
            doublereal *, integer *);
    logical leftv;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
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
/*<       DOUBLE PRECISION   LSCALE( * ), RSCALE( * ), V( LDV, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DGGBAK forms the right or left eigenvectors of a real generalized */
/*  eigenvalue problem A*x = lambda*B*x, by backward transformation on */
/*  the computed eigenvectors of the balanced pair of matrices output by */
/*  DGGBAL. */

/*  Arguments */
/*  ========= */

/*  JOB     (input) CHARACTER*1 */
/*          Specifies the type of backward transformation required: */
/*          = 'N':  do nothing, return immediately; */
/*          = 'P':  do backward transformation for permutation only; */
/*          = 'S':  do backward transformation for scaling only; */
/*          = 'B':  do backward transformations for both permutation and */
/*                  scaling. */
/*          JOB must be the same as the argument JOB supplied to DGGBAL. */

/*  SIDE    (input) CHARACTER*1 */
/*          = 'R':  V contains right eigenvectors; */
/*          = 'L':  V contains left eigenvectors. */

/*  N       (input) INTEGER */
/*          The number of rows of the matrix V.  N >= 0. */

/*  ILO     (input) INTEGER */
/*  IHI     (input) INTEGER */
/*          The integers ILO and IHI determined by DGGBAL. */
/*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0. */

/*  LSCALE  (input) DOUBLE PRECISION array, dimension (N) */
/*          Details of the permutations and/or scaling factors applied */
/*          to the left side of A and B, as returned by DGGBAL. */

/*  RSCALE  (input) DOUBLE PRECISION array, dimension (N) */
/*          Details of the permutations and/or scaling factors applied */
/*          to the right side of A and B, as returned by DGGBAL. */

/*  M       (input) INTEGER */
/*          The number of columns of the matrix V.  M >= 0. */

/*  V       (input/output) DOUBLE PRECISION array, dimension (LDV,M) */
/*          On entry, the matrix of right or left eigenvectors to be */
/*          transformed, as returned by DTGEVC. */
/*          On exit, V is overwritten by the transformed eigenvectors. */

/*  LDV     (input) INTEGER */
/*          The leading dimension of the matrix V. LDV >= max(1,N). */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit. */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */

/*  Further Details */
/*  =============== */

/*  See R.C. Ward, Balancing the generalized eigenvalue problem, */
/*                 SIAM J. Sci. Stat. Comp. 2 (1981), 141-152. */

/*  ===================================================================== */

/*     .. Local Scalars .. */
/*<       LOGICAL            LEFTV, RIGHTV >*/
/*<       INTEGER            I, K >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DSCAL, DSWAP, XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters */

/*<       RIGHTV = LSAME( SIDE, 'R' ) >*/
    /* Parameter adjustments */
    --lscale;
    --rscale;
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
/*<       ELSE IF( ILO.LT.1 ) THEN >*/
    } else if (*ilo < 1) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( IHI.LT.ILO .OR. IHI.GT.MAX( 1, N ) ) THEN >*/
    } else if (*ihi < *ilo || *ihi > max(1,*n)) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( M.LT.0 ) THEN >*/
    } else if (*m < 0) {
/*<          INFO = -6 >*/
        *info = -6;
/*<       ELSE IF( LDV.LT.MAX( 1, N ) ) THEN >*/
    } else if (*ldv < max(1,*n)) {
/*<          INFO = -10 >*/
        *info = -10;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DGGBAK', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DGGBAK", &i__1, (ftnlen)6);
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

/*        Backward transformation on right eigenvectors */

/*<          IF( RIGHTV ) THEN >*/
        if (rightv) {
/*<             DO 10 I = ILO, IHI >*/
            i__1 = *ihi;
            for (i__ = *ilo; i__ <= i__1; ++i__) {
/*<                CALL DSCAL( M, RSCALE( I ), V( I, 1 ), LDV ) >*/
                dscal_(m, &rscale[i__], &v[i__ + v_dim1], ldv);
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<          END IF >*/
        }

/*        Backward transformation on left eigenvectors */

/*<          IF( LEFTV ) THEN >*/
        if (leftv) {
/*<             DO 20 I = ILO, IHI >*/
            i__1 = *ihi;
            for (i__ = *ilo; i__ <= i__1; ++i__) {
/*<                CALL DSCAL( M, LSCALE( I ), V( I, 1 ), LDV ) >*/
                dscal_(m, &lscale[i__], &v[i__ + v_dim1], ldv);
/*<    20       CONTINUE >*/
/* L20: */
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*     Backward permutation */

/*<    30 CONTINUE >*/
L30:
/*<       IF( LSAME( JOB, 'P' ) .OR. LSAME( JOB, 'B' ) ) THEN >*/
    if (lsame_(job, "P", (ftnlen)1, (ftnlen)1) || lsame_(job, "B", (ftnlen)1,
            (ftnlen)1)) {

/*        Backward permutation on right eigenvectors */

/*<          IF( RIGHTV ) THEN >*/
        if (rightv) {
/*<    >*/
            if (*ilo == 1) {
                goto L50;
            }

/*<             DO 40 I = ILO - 1, 1, -1 >*/
            for (i__ = *ilo - 1; i__ >= 1; --i__) {
/*<                K = RSCALE( I ) >*/
                k = (integer) rscale[i__];
/*<    >*/
                if (k == i__) {
                    goto L40;
                }
/*<                CALL DSWAP( M, V( I, 1 ), LDV, V( K, 1 ), LDV ) >*/
                dswap_(m, &v[i__ + v_dim1], ldv, &v[k + v_dim1], ldv);
/*<    40       CONTINUE >*/
L40:
                ;
            }

/*<    50       CONTINUE >*/
L50:
/*<    >*/
            if (*ihi == *n) {
                goto L70;
            }
/*<             DO 60 I = IHI + 1, N >*/
            i__1 = *n;
            for (i__ = *ihi + 1; i__ <= i__1; ++i__) {
/*<                K = RSCALE( I ) >*/
                k = (integer) rscale[i__];
/*<    >*/
                if (k == i__) {
                    goto L60;
                }
/*<                CALL DSWAP( M, V( I, 1 ), LDV, V( K, 1 ), LDV ) >*/
                dswap_(m, &v[i__ + v_dim1], ldv, &v[k + v_dim1], ldv);
/*<    60       CONTINUE >*/
L60:
                ;
            }
/*<          END IF >*/
        }

/*        Backward permutation on left eigenvectors */

/*<    70    CONTINUE >*/
L70:
/*<          IF( LEFTV ) THEN >*/
        if (leftv) {
/*<    >*/
            if (*ilo == 1) {
                goto L90;
            }
/*<             DO 80 I = ILO - 1, 1, -1 >*/
            for (i__ = *ilo - 1; i__ >= 1; --i__) {
/*<                K = LSCALE( I ) >*/
                k = (integer) lscale[i__];
/*<    >*/
                if (k == i__) {
                    goto L80;
                }
/*<                CALL DSWAP( M, V( I, 1 ), LDV, V( K, 1 ), LDV ) >*/
                dswap_(m, &v[i__ + v_dim1], ldv, &v[k + v_dim1], ldv);
/*<    80       CONTINUE >*/
L80:
                ;
            }

/*<    90       CONTINUE >*/
L90:
/*<    >*/
            if (*ihi == *n) {
                goto L110;
            }
/*<             DO 100 I = IHI + 1, N >*/
            i__1 = *n;
            for (i__ = *ihi + 1; i__ <= i__1; ++i__) {
/*<                K = LSCALE( I ) >*/
                k = (integer) lscale[i__];
/*<    >*/
                if (k == i__) {
                    goto L100;
                }
/*<                CALL DSWAP( M, V( I, 1 ), LDV, V( K, 1 ), LDV ) >*/
                dswap_(m, &v[i__ + v_dim1], ldv, &v[k + v_dim1], ldv);
/*<   100       CONTINUE >*/
L100:
                ;
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<   110 CONTINUE >*/
L110:

/*<       RETURN >*/
    return 0;

/*     End of DGGBAK */

/*<       END >*/
} /* dggbak_ */

#ifdef __cplusplus
        }
#endif

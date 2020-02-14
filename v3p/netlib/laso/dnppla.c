/* laso/dnppla.f -- translated by f2c (version 20050501).
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

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;


/* ------------------------------------------------------------------ */

/*<    >*/
/* Subroutine */ int dnppla_(
        void (*op)(integer*,integer*,doublereal*,doublereal*),
        void (*iovect)(integer*,integer*,doublereal*,integer*,integer*),
        integer *n, integer *nperm,
        integer *nop, integer *nmval, doublereal *val, integer *nmvec,
        doublereal *vec, integer *nblock, doublereal *h__, doublereal *hv,
        doublereal *p, doublereal *q, doublereal *bound, doublereal *d__,
        doublereal *delta, logical *small, logical *raritz, doublereal *eps)
{
    /* System generated locals */
    integer val_dim1, val_offset, vec_dim1, vec_offset, h_dim1, h_offset,
            hv_dim1, hv_offset, p_dim1, p_offset, q_dim1, q_offset, i__1,
            i__2, i__3, i__4;
    doublereal d__1;

    /* Local variables */
    integer i__, j, k, l, m, jj, kk;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);
    doublereal hmin, hmax, temp;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *);
    doublereal dzero[1];
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *,
            integer *, doublereal *, integer *), dlaeig_(integer *, integer *,
             integer *, integer *, doublereal *, doublereal *, integer *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *), dlager_(
            integer *, integer *, integer *, doublereal *, doublereal *,
            doublereal *);


/*<       INTEGER N, NPERM, NOP, NMVAL, NMVEC, NBLOCK >*/
/*<       LOGICAL SMALL, RARITZ >*/
/*<    >*/
/*<       EXTERNAL OP, IOVECT >*/

/* THIS SUBROUTINE POST PROCESSES THE EIGENVECTORS.  BLOCK MATRIX */
/* VECTOR PRODUCTS ARE USED TO MINIMIZED THE NUMBER OF CALLS TO OP. */

/*<       INTEGER I, J, JJ, K, KK, L, M, MOD >*/
/*<       DOUBLE PRECISION HMIN, HMAX, TEMP, DDOT, DNRM2, DZERO(1) >*/
/*<       EXTERNAL DAXPY, DCOPY, DDOT, DLAGER, DLAEIG >*/

/* IF RARITZ IS .TRUE.  A FINAL RAYLEIGH-RITZ PROCEDURE IS APPLIED */
/* TO THE EIGENVECTORS. */

/*<       DZERO(1) = 0.0D0 >*/
    /* Parameter adjustments */
    q_dim1 = *n;
    q_offset = 1 + q_dim1;
    q -= q_offset;
    p_dim1 = *n;
    p_offset = 1 + p_dim1;
    p -= p_offset;
    hv_dim1 = *nperm;
    hv_offset = 1 + hv_dim1;
    hv -= hv_offset;
    h_dim1 = *nperm;
    h_offset = 1 + h_dim1;
    h__ -= h_offset;
    val_dim1 = *nmval;
    val_offset = 1 + val_dim1;
    val -= val_offset;
    vec_dim1 = *nmvec;
    vec_offset = 1 + vec_dim1;
    vec -= vec_offset;
    --bound;
    --d__;

    /* Function Body */
    dzero[0] = 0.;
/*<       IF (.NOT.RARITZ) GO TO 190 >*/
    if (! (*raritz)) {
        goto L190;
    }

/* ------------------------------------------------------------------ */

/* THIS CONSTRUCTS H=Q*AQ, WHERE THE COLUMNS OF Q ARE THE */
/* APPROXIMATE EIGENVECTORS.  TEMP = -1 IS USED WHEN SMALL IS */
/* FALSE TO AVOID HAVING TO RESORT THE EIGENVALUES AND EIGENVECTORS */
/* COMPUTED BY DLAEIG. */

/*<       CALL DCOPY(NPERM*NPERM, DZERO, 0, H, 1) >*/
    i__1 = *nperm * *nperm;
    dcopy_(&i__1, dzero, &c__0, &h__[h_offset], &c__1);
/*<       TEMP = -1.0D0 >*/
    temp = -1.;
/*<       IF (SMALL) TEMP = 1.0D0 >*/
    if (*small) {
        temp = 1.;
    }
/*<       M = MOD(NPERM,NBLOCK) >*/
    m = *nperm % *nblock;
/*<       IF (M.EQ.0) GO TO 40 >*/
    if (m == 0) {
        goto L40;
    }
/*<       DO 10 I=1,M >*/
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          CALL DCOPY(N, VEC(1,I), 1, P(1,I), 1) >*/
        dcopy_(n, &vec[i__ * vec_dim1 + 1], &c__1, &p[i__ * p_dim1 + 1], &
                c__1);
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       CALL IOVECT(N, M, P, M, 0) >*/
    (*iovect)(n, &m, &p[p_offset], &m, &c__0);
/*<       CALL OP(N, M, P, Q) >*/
    (*op)(n, &m, &p[p_offset], &q[q_offset]);
/*<       NOP = NOP + 1 >*/
    ++(*nop);
/*<       DO 30 I=1,M >*/
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          DO 20 J=I,NPERM >*/
        i__2 = *nperm;
        for (j = i__; j <= i__2; ++j) {
/*<             JJ = J - I + 1 >*/
            jj = j - i__ + 1;
/*<             H(JJ,I) = TEMP*DDOT(N,VEC(1,J),1,Q(1,I),1) >*/
            h__[jj + i__ * h_dim1] = temp * ddot_(n, &vec[j * vec_dim1 + 1], &
                    c__1, &q[i__ * q_dim1 + 1], &c__1);
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<    30 CONTINUE >*/
/* L30: */
    }
/*<       IF (NPERM.LT.NBLOCK) GO TO 90 >*/
    if (*nperm < *nblock) {
        goto L90;
    }
/*<    40 M = M + NBLOCK >*/
L40:
    m += *nblock;
/*<       DO 80 I=M,NPERM,NBLOCK >*/
    i__1 = *nperm;
    i__2 = *nblock;
    for (i__ = m; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<          DO 50 J=1,NBLOCK >*/
        i__3 = *nblock;
        for (j = 1; j <= i__3; ++j) {
/*<             L = I - NBLOCK + J >*/
            l = i__ - *nblock + j;
/*<             CALL DCOPY(N, VEC(1,L), 1, P(1,J), 1) >*/
            dcopy_(n, &vec[l * vec_dim1 + 1], &c__1, &p[j * p_dim1 + 1], &
                    c__1);
/*<    50    CONTINUE >*/
/* L50: */
        }
/*<          CALL IOVECT(N, NBLOCK, P, I, 0) >*/
        (*iovect)(n, nblock, &p[p_offset], &i__, &c__0);
/*<          CALL OP(N, NBLOCK, P, Q) >*/
        (*op)(n, nblock, &p[p_offset], &q[q_offset]);
/*<          NOP = NOP + 1 >*/
        ++(*nop);
/*<          DO 70 J=1,NBLOCK >*/
        i__3 = *nblock;
        for (j = 1; j <= i__3; ++j) {
/*<             L = I - NBLOCK + J >*/
            l = i__ - *nblock + j;
/*<             DO 60 K=L,NPERM >*/
            i__4 = *nperm;
            for (k = l; k <= i__4; ++k) {
/*<                KK = K - L + 1 >*/
                kk = k - l + 1;
/*<                H(KK,L) = TEMP*DDOT(N,VEC(1,K),1,Q(1,J),1) >*/
                h__[kk + l * h_dim1] = temp * ddot_(n, &vec[k * vec_dim1 + 1],
                         &c__1, &q[j * q_dim1 + 1], &c__1);
/*<    60       CONTINUE >*/
/* L60: */
            }
/*<    70      CONTINUE >*/
/* L70: */
        }
/*<    80 CONTINUE >*/
/* L80: */
    }

/* THIS COMPUTES THE SPECTRAL DECOMPOSITION OF H. */

/*<    90 HMIN = H(1,1) >*/
L90:
    hmin = h__[h_dim1 + 1];
/*<       HMAX = H(1,1) >*/
    hmax = h__[h_dim1 + 1];
/*<       CALL DLAGER(NPERM, NPERM, 1, H, HMIN, HMAX) >*/
    dlager_(nperm, nperm, &c__1, &h__[h_offset], &hmin, &hmax);
/*<    >*/
    dlaeig_(nperm, nperm, &c__1, nperm, &h__[h_offset], &val[val_offset],
            nperm, &hv[hv_offset], &bound[1], &p[p_offset], &d__[1], &q[
            q_offset], eps, &hmin, &hmax);

/* THIS COMPUTES THE RITZ VECTORS--THE COLUMNS OF */
/* Y = QS WHERE S IS THE MATRIX OF EIGENVECTORS OF H. */

/*<       DO 120 I=1,NPERM >*/
    i__2 = *nperm;
    for (i__ = 1; i__ <= i__2; ++i__) {
/*<          CALL DCOPY(N, DZERO, 0, VEC(1,I), 1) >*/
        dcopy_(n, dzero, &c__0, &vec[i__ * vec_dim1 + 1], &c__1);
/*<   120 CONTINUE >*/
/* L120: */
    }
/*<       M = MOD(NPERM,NBLOCK) >*/
    m = *nperm % *nblock;
/*<       IF (M.EQ.0) GO TO 150 >*/
    if (m == 0) {
        goto L150;
    }
/*<       CALL IOVECT(N, M, P, M, 1) >*/
    (*iovect)(n, &m, &p[p_offset], &m, &c__1);
/*<       DO 140 I=1,M >*/
    i__2 = m;
    for (i__ = 1; i__ <= i__2; ++i__) {
/*<          DO 130 J=1,NPERM >*/
        i__1 = *nperm;
        for (j = 1; j <= i__1; ++j) {
/*<             CALL DAXPY(N, HV(I,J), P(1,I), 1, VEC(1,J), 1) >*/
            daxpy_(n, &hv[i__ + j * hv_dim1], &p[i__ * p_dim1 + 1], &c__1, &
                    vec[j * vec_dim1 + 1], &c__1);
/*<   130    CONTINUE >*/
/* L130: */
        }
/*<   140 CONTINUE >*/
/* L140: */
    }
/*<       IF (NPERM.LT.NBLOCK) GO TO 190 >*/
    if (*nperm < *nblock) {
        goto L190;
    }
/*<   150 M = M + NBLOCK >*/
L150:
    m += *nblock;
/*<       DO 180 I=M,NPERM,NBLOCK >*/
    i__2 = *nperm;
    i__1 = *nblock;
    for (i__ = m; i__1 < 0 ? i__ >= i__2 : i__ <= i__2; i__ += i__1) {
/*<          CALL IOVECT(N, NBLOCK, P, I, 1) >*/
        (*iovect)(n, nblock, &p[p_offset], &i__, &c__1);
/*<          DO 170 J=1,NBLOCK >*/
        i__3 = *nblock;
        for (j = 1; j <= i__3; ++j) {
/*<             L = I - NBLOCK + J >*/
            l = i__ - *nblock + j;
/*<             DO 160 K=1,NPERM >*/
            i__4 = *nperm;
            for (k = 1; k <= i__4; ++k) {
/*<                CALL DAXPY(N, HV(L,K), P(1,J), 1, VEC(1,K), 1) >*/
                daxpy_(n, &hv[l + k * hv_dim1], &p[j * p_dim1 + 1], &c__1, &
                        vec[k * vec_dim1 + 1], &c__1);
/*<   160       CONTINUE >*/
/* L160: */
            }
/*<   170    CONTINUE >*/
/* L170: */
        }
/*<   180 CONTINUE >*/
/* L180: */
    }

/* ------------------------------------------------------------------ */

/* THIS SECTION COMPUTES THE RAYLEIGH QUOTIENTS (IN VAL(*,1)) */
/* AND RESIDUAL NORMS (IN VAL(*,2)) OF THE EIGENVECTORS. */

/*<   190 IF (.NOT.SMALL) DELTA = -DELTA >*/
L190:
    if (! (*small)) {
        *delta = -(*delta);
    }
/*<       M = MOD(NPERM,NBLOCK) >*/
    m = *nperm % *nblock;
/*<       IF (M.EQ.0) GO TO 220 >*/
    if (m == 0) {
        goto L220;
    }
/*<       DO 200 I=1,M >*/
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          CALL DCOPY(N, VEC(1,I), 1, P(1,I), 1) >*/
        dcopy_(n, &vec[i__ * vec_dim1 + 1], &c__1, &p[i__ * p_dim1 + 1], &
                c__1);
/*<   200 CONTINUE >*/
/* L200: */
    }
/*<       CALL OP(N, M, P, Q) >*/
    (*op)(n, &m, &p[p_offset], &q[q_offset]);
/*<       NOP = NOP + 1 >*/
    ++(*nop);
/*<       DO 210 I=1,M >*/
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          VAL(I,1) = DDOT(N,P(1,I),1,Q(1,I),1) >*/
        val[i__ + val_dim1] = ddot_(n, &p[i__ * p_dim1 + 1], &c__1, &q[i__ *
                q_dim1 + 1], &c__1);
/*<          CALL DAXPY(N, -VAL(I,1), P(1,I), 1, Q(1,I), 1) >*/
        d__1 = -val[i__ + val_dim1];
        daxpy_(n, &d__1, &p[i__ * p_dim1 + 1], &c__1, &q[i__ * q_dim1 + 1], &
                c__1);
/*<          VAL(I,2) = DNRM2(N,Q(1,I),1) >*/
        val[i__ + (val_dim1 << 1)] = dnrm2_(n, &q[i__ * q_dim1 + 1], &c__1);
/*<   210 CONTINUE >*/
/* L210: */
    }
/*<       IF (NPERM.LT.NBLOCK) GO TO 260 >*/
    if (*nperm < *nblock) {
        goto L260;
    }
/*<   220 M = M + 1 >*/
L220:
    ++m;
/*<       DO 250 I=M,NPERM,NBLOCK >*/
    i__1 = *nperm;
    i__2 = *nblock;
    for (i__ = m; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<          DO 230 J=1,NBLOCK >*/
        i__3 = *nblock;
        for (j = 1; j <= i__3; ++j) {
/*<             L = I - 1 + J >*/
            l = i__ - 1 + j;
/*<             CALL DCOPY(N, VEC(1,L), 1, P(1,J), 1) >*/
            dcopy_(n, &vec[l * vec_dim1 + 1], &c__1, &p[j * p_dim1 + 1], &
                    c__1);
/*<   230    CONTINUE >*/
/* L230: */
        }
/*<          CALL OP(N, NBLOCK, P, Q) >*/
        (*op)(n, nblock, &p[p_offset], &q[q_offset]);
/*<          NOP = NOP + 1 >*/
        ++(*nop);
/*<          DO 240 J=1,NBLOCK >*/
        i__3 = *nblock;
        for (j = 1; j <= i__3; ++j) {
/*<             L = I - 1 + J >*/
            l = i__ - 1 + j;
/*<             VAL(L,1) = DDOT(N,P(1,J),1,Q(1,J),1) >*/
            val[l + val_dim1] = ddot_(n, &p[j * p_dim1 + 1], &c__1, &q[j *
                    q_dim1 + 1], &c__1);
/*<             CALL DAXPY(N, -VAL(L,1), P(1,J), 1, Q(1,J), 1) >*/
            d__1 = -val[l + val_dim1];
            daxpy_(n, &d__1, &p[j * p_dim1 + 1], &c__1, &q[j * q_dim1 + 1], &
                    c__1);
/*<             VAL(L,2) = DNRM2(N,Q(1,J),1) >*/
            val[l + (val_dim1 << 1)] = dnrm2_(n, &q[j * q_dim1 + 1], &c__1);
/*<   240    CONTINUE >*/
/* L240: */
        }
/*<   250 CONTINUE >*/
/* L250: */
    }

/* THIS COMPUTES THE ACCURACY ESTIMATES.  FOR CONSISTENCY WITH DILASO */
/* A DO LOOP IS NOT USED. */

/*<   260 I = 0 >*/
L260:
    i__ = 0;
/*<   270 I = I + 1 >*/
L270:
    ++i__;
/*<       IF (I.GT.NPERM) RETURN >*/
    if (i__ > *nperm) {
        return 0;
    }
/*<       TEMP = DELTA - VAL(I,1) >*/
    temp = *delta - val[i__ + val_dim1];
/*<       IF (.NOT.SMALL) TEMP = -TEMP >*/
    if (! (*small)) {
        temp = -temp;
    }
/*<       VAL(I,4) = 0.0D0 >*/
    val[i__ + (val_dim1 << 2)] = 0.;
/*<       IF (TEMP.GT.0.0D0) VAL(I,4) = VAL(I,2)/TEMP >*/
    if (temp > 0.) {
        val[i__ + (val_dim1 << 2)] = val[i__ + (val_dim1 << 1)] / temp;
    }
/*<       VAL(I,3) = VAL(I,4)*VAL(I,2) >*/
    val[i__ + val_dim1 * 3] = val[i__ + (val_dim1 << 2)] * val[i__ + (
            val_dim1 << 1)];
/*<       GO TO 270 >*/
    goto L270;

/*<       END >*/
} /* dnppla_ */

#ifdef __cplusplus
        }
#endif

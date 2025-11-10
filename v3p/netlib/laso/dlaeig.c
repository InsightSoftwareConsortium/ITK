/* laso/dlaeig.f -- translated by f2c (version 20050501).
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
/* Subroutine */ int dlaeig_(integer *n, integer *nband, integer *nl, integer
        *nr, doublereal *a, doublereal *eigval, integer *lde, doublereal *
        eigvec, doublereal *bound, doublereal *atemp, doublereal *d__,
        doublereal *vtemp, doublereal *eps, doublereal *tmin, doublereal *
        tmax)
{
    /* System generated locals */
    integer a_dim1, a_offset, eigvec_dim1, eigvec_offset, i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__, m;
    doublereal atol;
    integer nval;
    doublereal artol;
    extern /* Subroutine */ int dlabcm_(integer *, integer *, integer *,
            integer *, doublereal *, doublereal *, integer *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *, doublereal *);


/*  THIS IS A SPECIALIZED VERSION OF THE SUBROUTINE BNDEIG TAILORED */
/*  SPECIFICALLY FOR USE BY THE LASO PACKAGE. */

/*<       INTEGER N, NBAND, NL, NR, LDE >*/
/*<    >*/

/*  LOCAL VARIABLES */

/*<       INTEGER I, M, NVAL >*/
/*<       DOUBLE PRECISION ARTOL, ATOL >*/

/*  FUNCTIONS CALLED */

/*<       DOUBLE PRECISION DMAX1 >*/

/*  SUBROUTINES CALLED */

/*     DLABCM, DLABFC, DLAGER, DCOPY */

/*  SET PARAMETERS */

/*<       ATOL = DBLE(FLOAT(N))*EPS*DMAX1(TMAX,-TMIN) >*/
    /* Parameter adjustments */
    a_dim1 = *nband;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --eigval;
    eigvec_dim1 = *lde;
    eigvec_offset = 1 + eigvec_dim1;
    eigvec -= eigvec_offset;
    bound -= 3;
    --atemp;
    --d__;
    --vtemp;

    /* Function Body */
/* Computing MAX */
    d__1 = *tmax, d__2 = -(*tmin);
    atol = (doublereal) ((real) (*n)) * *eps * max(d__1,d__2);
/*<       ARTOL = ATOL/DSQRT(EPS) >*/
    artol = atol / sqrt(*eps);
/*<       NVAL = NR - NL + 1 >*/
    nval = *nr - *nl + 1;

/*   CHECK FOR SPECIAL CASE OF N = 1 */

/*<       IF(N .NE. 1) GO TO 30 >*/
    if (*n != 1) {
        goto L30;
    }
/*<       EIGVAL(1) = A(1,1) >*/
    eigval[1] = a[a_dim1 + 1];
/*<       EIGVEC(1,1) = 1.0D0 >*/
    eigvec[eigvec_dim1 + 1] = 1.;
/*<       RETURN >*/
    return 0;

/*   SET UP INITIAL EIGENVALUE BOUNDS */

/*<    30 M = NVAL + 1 >*/
L30:
    m = nval + 1;
/*<       DO 50 I = 2, M >*/
    i__1 = m;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<          BOUND(1,I) = TMIN >*/
        bound[(i__ << 1) + 1] = *tmin;
/*<          BOUND(2,I) = TMAX >*/
        bound[(i__ << 1) + 2] = *tmax;
/*<    50 CONTINUE >*/
/* L50: */
    }
/*<       BOUND(2,1) = TMAX >*/
    bound[4] = *tmax;
/*<       BOUND(1,NVAL + 2) = TMIN >*/
    bound[((nval + 2) << 1) + 1] = *tmin;
/*<       IF(NL .EQ. 1) BOUND(2,1) = TMIN >*/
    if (*nl == 1) {
        bound[4] = *tmin;
    }
/*<       IF(NR .EQ. N) BOUND(1,NVAL + 2) = TMAX >*/
    if (*nr == *n) {
        bound[((nval + 2) << 1) + 1] = *tmax;
    }

/*<    >*/
/* L60: */
    dlabcm_(n, nband, nl, nr, &a[a_offset], &eigval[1], lde, &eigvec[
            eigvec_offset], &atol, &artol, &bound[3], &atemp[1], &d__[1], &
            vtemp[1]);
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* dlaeig_ */

#ifdef __cplusplus
        }
#endif

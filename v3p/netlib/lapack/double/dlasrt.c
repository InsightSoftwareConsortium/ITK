/* lapack/double/dlasrt.f -- translated by f2c (version 20090411).
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

/*<       SUBROUTINE DLASRT( ID, N, D, INFO ) >*/
/* Subroutine */ int dlasrt_(char *id, integer *n, doublereal *d__, integer *
        info, ftnlen id_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer i__, j;
    doublereal d1, d2, d3;
    integer dir;
    doublereal tmp;
    integer endd;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    integer stack[64]        /* was [2][32] */;
    doublereal dmnmx;
    integer start;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    integer stkpnt;


/*  -- LAPACK routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          ID >*/
/*<       INTEGER            INFO, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   D( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  Sort the numbers in D in increasing order (if ID = 'I') or */
/*  in decreasing order (if ID = 'D' ). */

/*  Use Quick Sort, reverting to Insertion sort on arrays of */
/*  size <= 20. Dimension of STACK limits N to about 2**32. */

/*  Arguments */
/*  ========= */

/*  ID      (input) CHARACTER*1 */
/*          = 'I': sort D in increasing order; */
/*          = 'D': sort D in decreasing order. */

/*  N       (input) INTEGER */
/*          The length of the array D. */

/*  D       (input/output) DOUBLE PRECISION array, dimension (N) */
/*          On entry, the array to be sorted. */
/*          On exit, D has been sorted into increasing order */
/*          (D(1) <= ... <= D(N) ) or into decreasing order */
/*          (D(1) >= ... >= D(N) ), depending on ID. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       INTEGER            SELECT >*/
/*<       PARAMETER          ( SELECT = 20 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            DIR, ENDD, I, J, START, STKPNT >*/
/*<       DOUBLE PRECISION   D1, D2, D3, DMNMX, TMP >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       INTEGER            STACK( 2, 32 ) >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters. */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    --d__;

    /* Function Body */
    *info = 0;
/*<       DIR = -1 >*/
    dir = -1;
/*<       IF( LSAME( ID, 'D' ) ) THEN >*/
    if (lsame_(id, "D", (ftnlen)1, (ftnlen)1)) {
/*<          DIR = 0 >*/
        dir = 0;
/*<       ELSE IF( LSAME( ID, 'I' ) ) THEN >*/
    } else if (lsame_(id, "I", (ftnlen)1, (ftnlen)1)) {
/*<          DIR = 1 >*/
        dir = 1;
/*<       END IF >*/
    }
/*<       IF( DIR.EQ.-1 ) THEN >*/
    if (dir == -1) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DLASRT', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DLASRT", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<        >*/
    if (*n <= 1) {
        return 0;
    }

/*<       STKPNT = 1 >*/
    stkpnt = 1;
/*<       STACK( 1, 1 ) = 1 >*/
    stack[0] = 1;
/*<       STACK( 2, 1 ) = N >*/
    stack[1] = *n;
/*<    10 CONTINUE >*/
L10:
/*<       START = STACK( 1, STKPNT ) >*/
    start = stack[(stkpnt << 1) - 2];
/*<       ENDD = STACK( 2, STKPNT ) >*/
    endd = stack[(stkpnt << 1) - 1];
/*<       STKPNT = STKPNT - 1 >*/
    --stkpnt;
/*<       IF( ENDD-START.LE.SELECT .AND. ENDD-START.GT.0 ) THEN >*/
    if (endd - start <= 20 && endd - start > 0) {

/*        Do Insertion sort on D( START:ENDD ) */

/*<          IF( DIR.EQ.0 ) THEN >*/
        if (dir == 0) {

/*           Sort into decreasing order */

/*<             DO 30 I = START + 1, ENDD >*/
            i__1 = endd;
            for (i__ = start + 1; i__ <= i__1; ++i__) {
/*<                DO 20 J = I, START + 1, -1 >*/
                i__2 = start + 1;
                for (j = i__; j >= i__2; --j) {
/*<                   IF( D( J ).GT.D( J-1 ) ) THEN >*/
                    if (d__[j] > d__[j - 1]) {
/*<                      DMNMX = D( J ) >*/
                        dmnmx = d__[j];
/*<                      D( J ) = D( J-1 ) >*/
                        d__[j] = d__[j - 1];
/*<                      D( J-1 ) = DMNMX >*/
                        d__[j - 1] = dmnmx;
/*<                   ELSE >*/
                    } else {
/*<                      GO TO 30 >*/
                        goto L30;
/*<                   END IF >*/
                    }
/*<    20          CONTINUE >*/
/* L20: */
                }
/*<    30       CONTINUE >*/
L30:
                ;
            }

/*<          ELSE >*/
        } else {

/*           Sort into increasing order */

/*<             DO 50 I = START + 1, ENDD >*/
            i__1 = endd;
            for (i__ = start + 1; i__ <= i__1; ++i__) {
/*<                DO 40 J = I, START + 1, -1 >*/
                i__2 = start + 1;
                for (j = i__; j >= i__2; --j) {
/*<                   IF( D( J ).LT.D( J-1 ) ) THEN >*/
                    if (d__[j] < d__[j - 1]) {
/*<                      DMNMX = D( J ) >*/
                        dmnmx = d__[j];
/*<                      D( J ) = D( J-1 ) >*/
                        d__[j] = d__[j - 1];
/*<                      D( J-1 ) = DMNMX >*/
                        d__[j - 1] = dmnmx;
/*<                   ELSE >*/
                    } else {
/*<                      GO TO 50 >*/
                        goto L50;
/*<                   END IF >*/
                    }
/*<    40          CONTINUE >*/
/* L40: */
                }
/*<    50       CONTINUE >*/
L50:
                ;
            }

/*<          END IF >*/
        }

/*<       ELSE IF( ENDD-START.GT.SELECT ) THEN >*/
    } else if (endd - start > 20) {

/*        Partition D( START:ENDD ) and stack parts, largest one first */

/*        Choose partition entry as median of 3 */

/*<          D1 = D( START ) >*/
        d1 = d__[start];
/*<          D2 = D( ENDD ) >*/
        d2 = d__[endd];
/*<          I = ( START+ENDD ) / 2 >*/
        i__ = (start + endd) / 2;
/*<          D3 = D( I ) >*/
        d3 = d__[i__];
/*<          IF( D1.LT.D2 ) THEN >*/
        if (d1 < d2) {
/*<             IF( D3.LT.D1 ) THEN >*/
            if (d3 < d1) {
/*<                DMNMX = D1 >*/
                dmnmx = d1;
/*<             ELSE IF( D3.LT.D2 ) THEN >*/
            } else if (d3 < d2) {
/*<                DMNMX = D3 >*/
                dmnmx = d3;
/*<             ELSE >*/
            } else {
/*<                DMNMX = D2 >*/
                dmnmx = d2;
/*<             END IF >*/
            }
/*<          ELSE >*/
        } else {
/*<             IF( D3.LT.D2 ) THEN >*/
            if (d3 < d2) {
/*<                DMNMX = D2 >*/
                dmnmx = d2;
/*<             ELSE IF( D3.LT.D1 ) THEN >*/
            } else if (d3 < d1) {
/*<                DMNMX = D3 >*/
                dmnmx = d3;
/*<             ELSE >*/
            } else {
/*<                DMNMX = D1 >*/
                dmnmx = d1;
/*<             END IF >*/
            }
/*<          END IF >*/
        }

/*<          IF( DIR.EQ.0 ) THEN >*/
        if (dir == 0) {

/*           Sort into decreasing order */

/*<             I = START - 1 >*/
            i__ = start - 1;
/*<             J = ENDD + 1 >*/
            j = endd + 1;
/*<    60       CONTINUE >*/
L60:
/*<    70       CONTINUE >*/
L70:
/*<             J = J - 1 >*/
            --j;
/*<        >*/
            if (d__[j] < dmnmx) {
                goto L70;
            }
/*<    80       CONTINUE >*/
L80:
/*<             I = I + 1 >*/
            ++i__;
/*<        >*/
            if (d__[i__] > dmnmx) {
                goto L80;
            }
/*<             IF( I.LT.J ) THEN >*/
            if (i__ < j) {
/*<                TMP = D( I ) >*/
                tmp = d__[i__];
/*<                D( I ) = D( J ) >*/
                d__[i__] = d__[j];
/*<                D( J ) = TMP >*/
                d__[j] = tmp;
/*<                GO TO 60 >*/
                goto L60;
/*<             END IF >*/
            }
/*<             IF( J-START.GT.ENDD-J-1 ) THEN >*/
            if (j - start > endd - j - 1) {
/*<                STKPNT = STKPNT + 1 >*/
                ++stkpnt;
/*<                STACK( 1, STKPNT ) = START >*/
                stack[(stkpnt << 1) - 2] = start;
/*<                STACK( 2, STKPNT ) = J >*/
                stack[(stkpnt << 1) - 1] = j;
/*<                STKPNT = STKPNT + 1 >*/
                ++stkpnt;
/*<                STACK( 1, STKPNT ) = J + 1 >*/
                stack[(stkpnt << 1) - 2] = j + 1;
/*<                STACK( 2, STKPNT ) = ENDD >*/
                stack[(stkpnt << 1) - 1] = endd;
/*<             ELSE >*/
            } else {
/*<                STKPNT = STKPNT + 1 >*/
                ++stkpnt;
/*<                STACK( 1, STKPNT ) = J + 1 >*/
                stack[(stkpnt << 1) - 2] = j + 1;
/*<                STACK( 2, STKPNT ) = ENDD >*/
                stack[(stkpnt << 1) - 1] = endd;
/*<                STKPNT = STKPNT + 1 >*/
                ++stkpnt;
/*<                STACK( 1, STKPNT ) = START >*/
                stack[(stkpnt << 1) - 2] = start;
/*<                STACK( 2, STKPNT ) = J >*/
                stack[(stkpnt << 1) - 1] = j;
/*<             END IF >*/
            }
/*<          ELSE >*/
        } else {

/*           Sort into increasing order */

/*<             I = START - 1 >*/
            i__ = start - 1;
/*<             J = ENDD + 1 >*/
            j = endd + 1;
/*<    90       CONTINUE >*/
L90:
/*<   100       CONTINUE >*/
L100:
/*<             J = J - 1 >*/
            --j;
/*<        >*/
            if (d__[j] > dmnmx) {
                goto L100;
            }
/*<   110       CONTINUE >*/
L110:
/*<             I = I + 1 >*/
            ++i__;
/*<        >*/
            if (d__[i__] < dmnmx) {
                goto L110;
            }
/*<             IF( I.LT.J ) THEN >*/
            if (i__ < j) {
/*<                TMP = D( I ) >*/
                tmp = d__[i__];
/*<                D( I ) = D( J ) >*/
                d__[i__] = d__[j];
/*<                D( J ) = TMP >*/
                d__[j] = tmp;
/*<                GO TO 90 >*/
                goto L90;
/*<             END IF >*/
            }
/*<             IF( J-START.GT.ENDD-J-1 ) THEN >*/
            if (j - start > endd - j - 1) {
/*<                STKPNT = STKPNT + 1 >*/
                ++stkpnt;
/*<                STACK( 1, STKPNT ) = START >*/
                stack[(stkpnt << 1) - 2] = start;
/*<                STACK( 2, STKPNT ) = J >*/
                stack[(stkpnt << 1) - 1] = j;
/*<                STKPNT = STKPNT + 1 >*/
                ++stkpnt;
/*<                STACK( 1, STKPNT ) = J + 1 >*/
                stack[(stkpnt << 1) - 2] = j + 1;
/*<                STACK( 2, STKPNT ) = ENDD >*/
                stack[(stkpnt << 1) - 1] = endd;
/*<             ELSE >*/
            } else {
/*<                STKPNT = STKPNT + 1 >*/
                ++stkpnt;
/*<                STACK( 1, STKPNT ) = J + 1 >*/
                stack[(stkpnt << 1) - 2] = j + 1;
/*<                STACK( 2, STKPNT ) = ENDD >*/
                stack[(stkpnt << 1) - 1] = endd;
/*<                STKPNT = STKPNT + 1 >*/
                ++stkpnt;
/*<                STACK( 1, STKPNT ) = START >*/
                stack[(stkpnt << 1) - 2] = start;
/*<                STACK( 2, STKPNT ) = J >*/
                stack[(stkpnt << 1) - 1] = j;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<        >*/
    if (stkpnt > 0) {
        goto L10;
    }
/*<       RETURN >*/
    return 0;

/*     End of DLASRT */

/*<       END >*/
} /* dlasrt_ */

#ifdef __cplusplus
        }
#endif

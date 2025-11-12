/* lapack/complex16/zlaswp.f -- translated by f2c (version 20090411).
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

/*<       SUBROUTINE ZLASWP( N, A, LDA, K1, K2, IPIV, INCX ) >*/
/* Subroutine */ int zlaswp_(integer *n, doublecomplex *a, integer *lda,
        integer *k1, integer *k2, integer *ipiv, integer *incx)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4, i__5, i__6;

    /* Local variables */
    integer i__, j, k, i1, i2, n32, ip, ix, ix0, inc;
    doublecomplex temp;


/*  -- LAPACK auxiliary routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INCX, K1, K2, LDA, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            IPIV( * ) >*/
/*<       COMPLEX*16         A( LDA, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLASWP performs a series of row interchanges on the matrix A. */
/*  One row interchange is initiated for each of rows K1 through K2 of A. */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N) */
/*          On entry, the matrix of column dimension N to which the row */
/*          interchanges will be applied. */
/*          On exit, the permuted matrix. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. */

/*  K1      (input) INTEGER */
/*          The first element of IPIV for which a row interchange will */
/*          be done. */

/*  K2      (input) INTEGER */
/*          The last element of IPIV for which a row interchange will */
/*          be done. */

/*  IPIV    (input) INTEGER array, dimension (K2*abs(INCX)) */
/*          The vector of pivot indices.  Only the elements in positions */
/*          K1 through K2 of IPIV are accessed. */
/*          IPIV(K) = L implies rows K and L are to be interchanged. */

/*  INCX    (input) INTEGER */
/*          The increment between successive values of IPIV.  If IPIV */
/*          is negative, the pivots are applied in reverse order. */

/*  Further Details */
/*  =============== */

/*  Modified by */
/*   R. C. Whaley, Computer Science Dept., Univ. of Tenn., Knoxville, USA */

/* ===================================================================== */

/*     .. Local Scalars .. */
/*<       INTEGER            I, I1, I2, INC, IP, IX, IX0, J, K, N32 >*/
/*<       COMPLEX*16         TEMP >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Interchange row I with row IPIV(I) for each of rows K1 through K2. */

/*<       IF( INCX.GT.0 ) THEN >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --ipiv;

    /* Function Body */
    if (*incx > 0) {
/*<          IX0 = K1 >*/
        ix0 = *k1;
/*<          I1 = K1 >*/
        i1 = *k1;
/*<          I2 = K2 >*/
        i2 = *k2;
/*<          INC = 1 >*/
        inc = 1;
/*<       ELSE IF( INCX.LT.0 ) THEN >*/
    } else if (*incx < 0) {
/*<          IX0 = 1 + ( 1-K2 )*INCX >*/
        ix0 = (1 - *k2) * *incx + 1;
/*<          I1 = K2 >*/
        i1 = *k2;
/*<          I2 = K1 >*/
        i2 = *k1;
/*<          INC = -1 >*/
        inc = -1;
/*<       ELSE >*/
    } else {
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       N32 = ( N / 32 )*32 >*/
    n32 = *n / 32 << 5;
/*<       IF( N32.NE.0 ) THEN >*/
    if (n32 != 0) {
/*<          DO 30 J = 1, N32, 32 >*/
        i__1 = n32;
        for (j = 1; j <= i__1; j += 32) {
/*<             IX = IX0 >*/
            ix = ix0;
/*<             DO 20 I = I1, I2, INC >*/
            i__2 = i2;
            i__3 = inc;
            for (i__ = i1; i__3 < 0 ? i__ >= i__2 : i__ <= i__2; i__ += i__3)
                    {
/*<                IP = IPIV( IX ) >*/
                ip = ipiv[ix];
/*<                IF( IP.NE.I ) THEN >*/
                if (ip != i__) {
/*<                   DO 10 K = J, J + 31 >*/
                    i__4 = j + 31;
                    for (k = j; k <= i__4; ++k) {
/*<                      TEMP = A( I, K ) >*/
                        i__5 = i__ + k * a_dim1;
                        temp.r = a[i__5].r, temp.i = a[i__5].i;
/*<                      A( I, K ) = A( IP, K ) >*/
                        i__5 = i__ + k * a_dim1;
                        i__6 = ip + k * a_dim1;
                        a[i__5].r = a[i__6].r, a[i__5].i = a[i__6].i;
/*<                      A( IP, K ) = TEMP >*/
                        i__5 = ip + k * a_dim1;
                        a[i__5].r = temp.r, a[i__5].i = temp.i;
/*<    10             CONTINUE >*/
/* L10: */
                    }
/*<                END IF >*/
                }
/*<                IX = IX + INCX >*/
                ix += *incx;
/*<    20       CONTINUE >*/
/* L20: */
            }
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<       END IF >*/
    }
/*<       IF( N32.NE.N ) THEN >*/
    if (n32 != *n) {
/*<          N32 = N32 + 1 >*/
        ++n32;
/*<          IX = IX0 >*/
        ix = ix0;
/*<          DO 50 I = I1, I2, INC >*/
        i__1 = i2;
        i__3 = inc;
        for (i__ = i1; i__3 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__3) {
/*<             IP = IPIV( IX ) >*/
            ip = ipiv[ix];
/*<             IF( IP.NE.I ) THEN >*/
            if (ip != i__) {
/*<                DO 40 K = N32, N >*/
                i__2 = *n;
                for (k = n32; k <= i__2; ++k) {
/*<                   TEMP = A( I, K ) >*/
                    i__4 = i__ + k * a_dim1;
                    temp.r = a[i__4].r, temp.i = a[i__4].i;
/*<                   A( I, K ) = A( IP, K ) >*/
                    i__4 = i__ + k * a_dim1;
                    i__5 = ip + k * a_dim1;
                    a[i__4].r = a[i__5].r, a[i__4].i = a[i__5].i;
/*<                   A( IP, K ) = TEMP >*/
                    i__4 = ip + k * a_dim1;
                    a[i__4].r = temp.r, a[i__4].i = temp.i;
/*<    40          CONTINUE >*/
/* L40: */
                }
/*<             END IF >*/
            }
/*<             IX = IX + INCX >*/
            ix += *incx;
/*<    50    CONTINUE >*/
/* L50: */
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of ZLASWP */

/*<       END >*/
} /* zlaswp_ */

#ifdef __cplusplus
        }
#endif

/* eispack/reduc.f -- translated by f2c (version 20050501).
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

/*<       subroutine reduc(nm,n,a,b,dl,ierr) >*/
/* Subroutine */ int reduc_(integer *nm, integer *n, doublereal *a,
        doublereal *b, doublereal *dl, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__, j, k;
    doublereal x, y=0;
    integer i1, j1, nn;


/*<       integer i,j,k,n,i1,j1,nm,nn,ierr >*/
/*<       double precision a(nm,n),b(nm,n),dl(n) >*/
/*<       double precision x,y >*/

/*     this subroutine is a translation of the algol procedure reduc1, */
/*     num. math. 11, 99-110(1968) by martin and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 303-314(1971). */

/*     this subroutine reduces the generalized symmetric eigenproblem */
/*     ax=(lambda)bx, where b is positive definite, to the standard */
/*     symmetric eigenproblem using the cholesky factorization of b. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrices a and b.  if the cholesky */
/*          factor l of b is already available, n should be prefixed */
/*          with a minus sign. */

/*        a and b contain the real symmetric input matrices.  only the */
/*          full upper triangles of the matrices need be supplied.  if */
/*          n is negative, the strict lower triangle of b contains, */
/*          instead, the strict lower triangle of its cholesky factor l. */

/*        dl contains, if n is negative, the diagonal elements of l. */

/*     on output */

/*        a contains in its full lower triangle the full lower triangle */
/*          of the symmetric matrix derived from the reduction to the */
/*          standard form.  the strict upper triangle of a is unaltered. */

/*        b contains in its strict lower triangle the strict lower */
/*          triangle of its cholesky factor l.  the full upper */
/*          triangle of b is unaltered. */

/*        dl contains the diagonal elements of l. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          7*n+1      if b is not positive definite. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*<       ierr = 0 >*/
    /* Parameter adjustments */
    --dl;
    b_dim1 = *nm;
    b_offset = 1 + b_dim1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    *ierr = 0;
/*<       nn = iabs(n) >*/
    nn = abs(*n);
/*<       if (n .lt. 0) go to 100 >*/
    if (*n < 0) {
        goto L100;
    }
/*     .......... form l in the arrays b and dl .......... */
/*<       do 80 i = 1, n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          i1 = i - 1 >*/
        i1 = i__ - 1;

/*<          do 80 j = i, n >*/
        i__2 = *n;
        for (j = i__; j <= i__2; ++j) {
/*<             x = b(i,j) >*/
            x = b[i__ + j * b_dim1];
/*<             if (i .eq. 1) go to 40 >*/
            if (i__ == 1) {
                goto L40;
            }

/*<             do 20 k = 1, i1 >*/
            i__3 = i1;
            for (k = 1; k <= i__3; ++k) {
/*<    20       x = x - b(i,k) * b(j,k) >*/
/* L20: */
                x -= b[i__ + k * b_dim1] * b[j + k * b_dim1];
            }

/*<    40       if (j .ne. i) go to 60 >*/
L40:
            if (j != i__) {
                goto L60;
            }
/*<             if (x .le. 0.0d0) go to 1000 >*/
            if (x <= 0.) {
                goto L1000;
            }
/*<             y = dsqrt(x) >*/
            y = sqrt(x);
/*<             dl(i) = y >*/
            dl[i__] = y;
/*<             go to 80 >*/
            goto L80;
/*<    60       b(j,i) = x / y >*/
L60:
            b[j + i__ * b_dim1] = x / y;
/*<    80 continue >*/
L80:
            ;
        }
    }
/*     .......... form the transpose of the upper triangle of inv(l)*a */
/*                in the lower triangle of the array a .......... */
/*<   100 do 200 i = 1, nn >*/
L100:
    i__2 = nn;
    for (i__ = 1; i__ <= i__2; ++i__) {
/*<          i1 = i - 1 >*/
        i1 = i__ - 1;
/*<          y = dl(i) >*/
        y = dl[i__];

/*<          do 200 j = i, nn >*/
        i__1 = nn;
        for (j = i__; j <= i__1; ++j) {
/*<             x = a(i,j) >*/
            x = a[i__ + j * a_dim1];
/*<             if (i .eq. 1) go to 180 >*/
            if (i__ == 1) {
                goto L180;
            }

/*<             do 160 k = 1, i1 >*/
            i__3 = i1;
            for (k = 1; k <= i__3; ++k) {
/*<   160       x = x - b(i,k) * a(j,k) >*/
/* L160: */
                x -= b[i__ + k * b_dim1] * a[j + k * a_dim1];
            }

/*<   180       a(j,i) = x / y >*/
L180:
            a[j + i__ * a_dim1] = x / y;
/*<   200 continue >*/
/* L200: */
        }
    }
/*     .......... pre-multiply by inv(l) and overwrite .......... */
/*<       do 300 j = 1, nn >*/
    i__1 = nn;
    for (j = 1; j <= i__1; ++j) {
/*<          j1 = j - 1 >*/
        j1 = j - 1;

/*<          do 300 i = j, nn >*/
        i__2 = nn;
        for (i__ = j; i__ <= i__2; ++i__) {
/*<             x = a(i,j) >*/
            x = a[i__ + j * a_dim1];
/*<             if (i .eq. j) go to 240 >*/
            if (i__ == j) {
                goto L240;
            }
/*<             i1 = i - 1 >*/
            i1 = i__ - 1;

/*<             do 220 k = j, i1 >*/
            i__3 = i1;
            for (k = j; k <= i__3; ++k) {
/*<   220       x = x - a(k,j) * b(i,k) >*/
/* L220: */
                x -= a[k + j * a_dim1] * b[i__ + k * b_dim1];
            }

/*<   240       if (j .eq. 1) go to 280 >*/
L240:
            if (j == 1) {
                goto L280;
            }

/*<             do 260 k = 1, j1 >*/
            i__3 = j1;
            for (k = 1; k <= i__3; ++k) {
/*<   260       x = x - a(j,k) * b(i,k) >*/
/* L260: */
                x -= a[j + k * a_dim1] * b[i__ + k * b_dim1];
            }

/*<   280       a(i,j) = x / dl(i) >*/
L280:
            a[i__ + j * a_dim1] = x / dl[i__];
/*<   300 continue >*/
/* L300: */
        }
    }

/*<       go to 1001 >*/
    goto L1001;
/*     .......... set error -- b is not positive definite .......... */
/*<  1000 ierr = 7 * n + 1 >*/
L1000:
    *ierr = *n * 7 + 1;
/*<  1001 return >*/
L1001:
    return 0;
/*<       end >*/
} /* reduc_ */

#ifdef __cplusplus
        }
#endif

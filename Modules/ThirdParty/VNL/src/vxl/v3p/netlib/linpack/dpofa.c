/* linpack/dpofa.f -- translated by f2c (version 20050501).
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

static integer c__1 = 1;

/*<       subroutine dpofa(a,lda,n,info) >*/
/* Subroutine */ int dpofa_(doublereal *a, integer *lda, integer *n, integer *
        info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer j, k;
    doublereal s, t;
    integer jm1;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);

/*<       integer lda,n,info >*/
/*<       double precision a(lda,1) >*/

/*     dpofa factors a double precision symmetric positive definite */
/*     matrix. */

/*     dpofa is usually called by dpoco, but it can be called */
/*     directly with a saving in time if  rcond  is not needed. */
/*     (time for dpoco) = (1 + 18/n)*(time for dpofa) . */

/*     on entry */

/*        a       double precision(lda, n) */
/*                the symmetric matrix to be factored.  only the */
/*                diagonal and upper triangle are used. */

/*        lda     integer */
/*                the leading dimension of the array  a . */

/*        n       integer */
/*                the order of the matrix  a . */

/*     on return */

/*        a       an upper triangular matrix  r  so that  a = trans(r)*r */
/*                where  trans(r)  is the transpose. */
/*                the strict lower triangle is unaltered. */
/*                if  info .ne. 0 , the factorization is not complete. */

/*        info    integer */
/*                = 0  for normal return. */
/*                = k  signals an error condition.  the leading minor */
/*                     of order  k  is not positive definite. */

/*     linpack.  this version dated 08/14/78 . */
/*     cleve moler, university of new mexico, argonne national lab. */

/*     subroutines and functions */

/*     blas ddot */
/*     fortran dsqrt */

/*     internal variables */

/*<       double precision ddot,t >*/
/*<       double precision s >*/
/*<       integer j,jm1,k >*/
/*     begin block with ...exits to 40 */


/*<          do 30 j = 1, n >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<             info = j >*/
        *info = j;
/*<             s = 0.0d0 >*/
        s = 0.;
/*<             jm1 = j - 1 >*/
        jm1 = j - 1;
/*<             if (jm1 .lt. 1) go to 20 >*/
        if (jm1 < 1) {
            goto L20;
        }
/*<             do 10 k = 1, jm1 >*/
        i__2 = jm1;
        for (k = 1; k <= i__2; ++k) {
/*<                t = a(k,j) - ddot(k-1,a(1,k),1,a(1,j),1) >*/
            i__3 = k - 1;
            t = a[k + j * a_dim1] - ddot_(&i__3, &a[k * a_dim1 + 1], &c__1, &
                    a[j * a_dim1 + 1], &c__1);
/*<                t = t/a(k,k) >*/
            t /= a[k + k * a_dim1];
/*<                a(k,j) = t >*/
            a[k + j * a_dim1] = t;
/*<                s = s + t*t >*/
            s += t * t;
/*<    10       continue >*/
/* L10: */
        }
/*<    20       continue >*/
L20:
/*<             s = a(j,j) - s >*/
        s = a[j + j * a_dim1] - s;
/*     ......exit */
/*<             if (s .le. 0.0d0) go to 40 >*/
        if (s <= 0.) {
            goto L40;
        }
/*<             a(j,j) = dsqrt(s) >*/
        a[j + j * a_dim1] = sqrt(s);
/*<    30    continue >*/
/* L30: */
    }
/*<          info = 0 >*/
    *info = 0;
/*<    40 continue >*/
L40:
/*<       return >*/
    return 0;
/*<       end >*/
} /* dpofa_ */

#ifdef __cplusplus
        }
#endif

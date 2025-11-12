/* linpack/dposl.f -- translated by f2c (version 20050501).
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

/*<       subroutine dposl(a,lda,n,b) >*/
/* Subroutine */ int dposl_(doublereal *a, integer *lda, integer *n,
        doublereal *b)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    integer k;
    doublereal t;
    integer kb;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *,
            integer *, doublereal *, integer *);

/*<       integer lda,n >*/
/*<       double precision a(lda,1),b(1) >*/

/*     dposl solves the double precision symmetric positive definite */
/*     system a * x = b */
/*     using the factors computed by dpoco or dpofa. */

/*     on entry */

/*        a       double precision(lda, n) */
/*                the output from dpoco or dpofa. */

/*        lda     integer */
/*                the leading dimension of the array  a . */

/*        n       integer */
/*                the order of the matrix  a . */

/*        b       double precision(n) */
/*                the right hand side vector. */

/*     on return */

/*        b       the solution vector  x . */

/*     error condition */

/*        a division by zero will occur if the input factor contains */
/*        a zero on the diagonal.  technically this indicates */
/*        singularity but it is usually caused by improper subroutine */
/*        arguments.  it will not occur if the subroutines are called */
/*        correctly and  info .eq. 0 . */

/*     to compute  inverse(a) * c  where  c  is a matrix */
/*     with  p  columns */
/*           call dpoco(a,lda,n,rcond,z,info) */
/*           if (rcond is too small .or. info .ne. 0) go to ... */
/*           do 10 j = 1, p */
/*              call dposl(a,lda,n,c(1,j)) */
/*        10 continue */

/*     linpack.  this version dated 08/14/78 . */
/*     cleve moler, university of new mexico, argonne national lab. */

/*     subroutines and functions */

/*     blas daxpy,ddot */

/*     internal variables */

/*<       double precision ddot,t >*/
/*<       integer k,kb >*/

/*     solve trans(r)*y = b */

/*<       do 10 k = 1, n >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --b;

    /* Function Body */
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
/*<          t = ddot(k-1,a(1,k),1,b(1),1) >*/
        i__2 = k - 1;
        t = ddot_(&i__2, &a[k * a_dim1 + 1], &c__1, &b[1], &c__1);
/*<          b(k) = (b(k) - t)/a(k,k) >*/
        b[k] = (b[k] - t) / a[k + k * a_dim1];
/*<    10 continue >*/
/* L10: */
    }

/*     solve r*x = y */

/*<       do 20 kb = 1, n >*/
    i__1 = *n;
    for (kb = 1; kb <= i__1; ++kb) {
/*<          k = n + 1 - kb >*/
        k = *n + 1 - kb;
/*<          b(k) = b(k)/a(k,k) >*/
        b[k] /= a[k + k * a_dim1];
/*<          t = -b(k) >*/
        t = -b[k];
/*<          call daxpy(k-1,t,a(1,k),1,b(1),1) >*/
        i__2 = k - 1;
        daxpy_(&i__2, &t, &a[k * a_dim1 + 1], &c__1, &b[1], &c__1);
/*<    20 continue >*/
/* L20: */
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* dposl_ */

#ifdef __cplusplus
        }
#endif

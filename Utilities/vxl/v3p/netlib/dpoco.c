#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__1 = 1;

/* Subroutine */ void dpoco_(a, lda, n, rcond, z, info)
doublereal *a;
integer *lda, *n;
doublereal *rcond, *z;
integer *info;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1;
    doublereal d__1;

    /* Local variables */
    static integer i, j, k;
    static doublereal s, t;
    static doublereal anorm;
    static doublereal ynorm;
    static integer kb;
    static doublereal ek, sm, wk;
    static integer jm1, kp1;
    static doublereal wkm;

/*     dpoco factors a double precision symmetric positive definite     */
/*     matrix and estimates the condition of the matrix.                */
/*                                                                      */
/*     if  rcond  is not needed, dpofa is slightly faster.              */
/*     to solve  a*x = b , follow dpoco by dposl.                       */
/*     to compute  inverse(a)*c , follow dpoco by dposl.                */
/*     to compute  determinant(a) , follow dpoco by dpodi.              */
/*     to compute  inverse(a) , follow dpoco by dpodi.                  */
/*                                                                      */
/*     on entry                                                         */
/*                                                                      */
/*        a       double precision(lda, n)                              */
/*                the symmetric matrix to be factored.  only the        */
/*                diagonal and upper triangle are used.                 */
/*                                                                      */
/*        lda     integer                                               */
/*                the leading dimension of the array  a .               */
/*                                                                      */
/*        n       integer                                               */
/*                the order of the matrix  a .                          */
/*                                                                      */
/*     on return                                                        */
/*                                                                      */
/*        a       an upper triangular matrix  r  so that a = trans(r)*r */
/*                where  trans(r)  is the transpose.                    */
/*                the strict lower triangle is unaltered.               */
/*                if  info .ne. 0 , the factorization is not complete.  */
/*                                                                      */
/*        rcond   double precision                                      */
/*                an estimate of the reciprocal condition of  a .       */
/*                for the system  a*x = b , relative perturbations      */
/*                in  a  and  b  of size  epsilon  may cause            */
/*                relative perturbations in  x  of size  epsilon/rcond .*/
/*                if  rcond  is so small that the logical expression    */
/*                           1.0 + rcond .eq. 1.0                       */
/*                is true, then  a  may be singular to working          */
/*                precision.  in particular,  rcond  is zero  if        */
/*                exact singularity is detected or the estimate         */
/*                underflows.  if info .ne. 0 , rcond is unchanged.     */
/*                                                                      */
/*        z       double precision(n)                                   */
/*                a work vector whose contents are usually unimportant. */
/*                if  a  is close to a singular matrix, then  z  is     */
/*                an approximate null vector in the sense that          */
/*                norm(a*z) = rcond*norm(a)*norm(z) .                   */
/*                if  info .ne. 0 , z  is unchanged.                    */
/*                                                                      */
/*        info    integer                                               */
/*                = 0  for normal return.                               */
/*                = k  signals an error condition.  the leading minor   */
/*                     of order  k  is not positive definite.           */
/*                                                                      */
/*     linpack.  this version dated 08/14/78 .                          */
/*     cleve moler, university of new mexico, argonne national lab.     */

/*     subroutines and functions      */
/*                                    */
/*     linpack dpofa                  */
/*     blas daxpy,ddot,dscal,dasum    */
/*     fortran dabs,dmax1,dreal,dsign */


    /* Parameter adjustments */
    --z;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

/*     find norm of a using only upper half */

    for (j = 1; j <= *n; ++j) {
        z[j] = dasum_(&j, &a[j * a_dim1 + 1], &c__1);
        jm1 = j - 1;
        if (jm1 < 1) {
            goto L20;
        }
        for (i = 1; i <= jm1; ++i) {
            z[i] += abs(a[i + j * a_dim1]);
        }
L20:
        ;
    }
    anorm = 0.;
    for (j = 1; j <= *n; ++j) {
        anorm = max(anorm,z[j]);
    }

/*     factor */

    dpofa_(&a[a_offset], lda, n, info);
    if (*info != 0) {
        goto L180;
    }

/*        rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) . */
/*        estimate = norm(z)/norm(y) where  a*z = y  and  a*y = e . */
/*        the components of  e  are chosen to cause maximum local */
/*        growth in the elements of w  where  trans(r)*w = e . */
/*        the vectors are frequently rescaled to avoid overflow. */

/*        solve trans(r)*w = e */

    ek = 1.;
    for (j = 1; j <= *n; ++j) {
        z[j] = 0.;
    }
    for (k = 1; k <= *n; ++k) {
        if (z[k] != 0.) {
            d__1 = -z[k];
            ek = d_sign(&ek, &d__1);
        }
        if (abs(ek - z[k]) <= a[k + k * a_dim1]) {
            goto L60;
        }
        s = a[k + k * a_dim1] / abs(ek - z[k]);
        dscal_(n, &s, &z[1], &c__1);
        ek *= s;
L60:
        wk = ek - z[k];
        wkm = -ek - z[k];
        s = abs(wk);
        sm = abs(wkm);
        wk /= a[k + k * a_dim1];
        wkm /= a[k + k * a_dim1];
        kp1 = k + 1;
        if (kp1 > *n) {
            goto L100;
        }
        for (j = kp1; j <= *n; ++j) {
            sm += abs(z[j] + wkm * a[k + j * a_dim1]);
            z[j] += wk * a[k + j * a_dim1];
            s += abs(z[j]);
        }
        if (s >= sm) {
            goto L90;
        }
        t = wkm - wk;
        wk = wkm;
        for (j = kp1; j <= *n; ++j) {
            z[j] += t * a[k + j * a_dim1];
        }
L90:
L100:
        z[k] = wk;
    }
    s = 1. / dasum_(n, &z[1], &c__1);
    dscal_(n, &s, &z[1], &c__1);

/*        solve r*y = w */

    for (kb = 1; kb <= *n; ++kb) {
        k = *n + 1 - kb;
        if (abs(z[k]) <= a[k + k * a_dim1]) {
            goto L120;
        }
        s = a[k + k * a_dim1] / abs(z[k]);
        dscal_(n, &s, &z[1], &c__1);
L120:
        z[k] /= a[k + k * a_dim1];
        t = -z[k];
        i__1 = k - 1;
        daxpy_(&i__1, &t, &a[k * a_dim1 + 1], &c__1, &z[1], &c__1);
    }
    s = 1. / dasum_(n, &z[1], &c__1);
    dscal_(n, &s, &z[1], &c__1);

    ynorm = 1.;

/*        solve trans(r)*v = y */

    for (k = 1; k <= *n; ++k) {
        i__1 = k - 1;
        z[k] -= ddot_(&i__1, &a[k * a_dim1 + 1], &c__1, &z[1], &c__1);
        if (abs(z[k]) <= a[k + k * a_dim1]) {
            goto L140;
        }
        s = a[k + k * a_dim1] / abs(z[k]);
        dscal_(n, &s, &z[1], &c__1);
        ynorm *= s;
L140:
        z[k] /= a[k + k * a_dim1];
    }
    s = 1. / dasum_(n, &z[1], &c__1);
    dscal_(n, &s, &z[1], &c__1);
    ynorm *= s;

/*        solve r*z = v */

    for (kb = 1; kb <= *n; ++kb) {
        k = *n + 1 - kb;
        if (abs(z[k]) <= a[k + k * a_dim1]) {
            goto L160;
        }
        s = a[k + k * a_dim1] / abs(z[k]);
        dscal_(n, &s, &z[1], &c__1);
        ynorm *= s;
L160:
        z[k] /= a[k + k * a_dim1];
        t = -z[k];
        i__1 = k - 1;
        daxpy_(&i__1, &t, &a[k * a_dim1 + 1], &c__1, &z[1], &c__1);
    }
/*        make znorm = 1.0 */
    s = 1. / dasum_(n, &z[1], &c__1);
    dscal_(n, &s, &z[1], &c__1);
    ynorm *= s;

    if (anorm != 0.) {
        *rcond = ynorm / anorm;
    }
    if (anorm == 0.) {
        *rcond = 0.;
    }
L180:
    return;
} /* dpoco_ */

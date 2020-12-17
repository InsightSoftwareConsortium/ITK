/* linpack/dpoco.f -- translated by f2c (version 20050501).
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

/*<       subroutine dpoco(a,lda,n,rcond,z,info) >*/
/* Subroutine */ int dpoco_(doublereal *a, integer *lda, integer *n,
        doublereal *rcond, doublereal *z__, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    integer i__, j, k;
    doublereal s, t;
    integer kb;
    doublereal ek, sm, wk;
    integer jm1, kp1;
    doublereal wkm;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *), dpofa_(doublereal *, integer *, integer *, integer *);
    extern doublereal dasum_(integer *, doublereal *, integer *);
    doublereal anorm;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *,
            integer *, doublereal *, integer *);
    doublereal ynorm;

/*<       integer lda,n,info >*/
/*<       double precision a(lda,1),z(1) >*/
/*<       double precision rcond >*/

/*     dpoco factors a double precision symmetric positive definite */
/*     matrix and estimates the condition of the matrix. */

/*     if  rcond  is not needed, dpofa is slightly faster. */
/*     to solve  a*x = b , follow dpoco by dposl. */
/*     to compute  inverse(a)*c , follow dpoco by dposl. */
/*     to compute  determinant(a) , follow dpoco by dpodi. */
/*     to compute  inverse(a) , follow dpoco by dpodi. */

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

/*        rcond   double precision */
/*                an estimate of the reciprocal condition of  a . */
/*                for the system  a*x = b , relative perturbations */
/*                in  a  and  b  of size  epsilon  may cause */
/*                relative perturbations in  x  of size  epsilon/rcond . */
/*                if  rcond  is so small that the logical expression */
/*                           1.0 + rcond .eq. 1.0 */
/*                is true, then  a  may be singular to working */
/*                precision.  in particular,  rcond  is zero  if */
/*                exact singularity is detected or the estimate */
/*                underflows.  if info .ne. 0 , rcond is unchanged. */

/*        z       double precision(n) */
/*                a work vector whose contents are usually unimportant. */
/*                if  a  is close to a singular matrix, then  z  is */
/*                an approximate null vector in the sense that */
/*                norm(a*z) = rcond*norm(a)*norm(z) . */
/*                if  info .ne. 0 , z  is unchanged. */

/*        info    integer */
/*                = 0  for normal return. */
/*                = k  signals an error condition.  the leading minor */
/*                     of order  k  is not positive definite. */

/*     linpack.  this version dated 08/14/78 . */
/*     cleve moler, university of new mexico, argonne national lab. */

/*     subroutines and functions */

/*     linpack dpofa */
/*     blas daxpy,ddot,dscal,dasum */
/*     fortran dabs,dmax1,dreal,dsign */

/*     internal variables */

/*<       double precision ddot,ek,t,wk,wkm >*/
/*<       double precision anorm,s,dasum,sm,ynorm >*/
/*<       integer i,j,jm1,k,kb,kp1 >*/


/*     find norm of a using only upper half */

/*<       do 30 j = 1, n >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --z__;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          z(j) = dasum(j,a(1,j),1) >*/
        z__[j] = dasum_(&j, &a[j * a_dim1 + 1], &c__1);
/*<          jm1 = j - 1 >*/
        jm1 = j - 1;
/*<          if (jm1 .lt. 1) go to 20 >*/
        if (jm1 < 1) {
            goto L20;
        }
/*<          do 10 i = 1, jm1 >*/
        i__2 = jm1;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             z(i) = z(i) + dabs(a(i,j)) >*/
            z__[i__] += (d__1 = a[i__ + j * a_dim1], abs(d__1));
/*<    10    continue >*/
/* L10: */
        }
/*<    20    continue >*/
L20:
/*<    30 continue >*/
/* L30: */
        ;
    }
/*<       anorm = 0.0d0 >*/
    anorm = 0.;
/*<       do 40 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          anorm = dmax1(anorm,z(j)) >*/
/* Computing MAX */
        d__1 = anorm, d__2 = z__[j];
        anorm = max(d__1,d__2);
/*<    40 continue >*/
/* L40: */
    }

/*     factor */

/*<       call dpofa(a,lda,n,info) >*/
    dpofa_(&a[a_offset], lda, n, info);
/*<       if (info .ne. 0) go to 180 >*/
    if (*info != 0) {
        goto L180;
    }

/*        rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) . */
/*        estimate = norm(z)/norm(y) where  a*z = y  and  a*y = e . */
/*        the components of  e  are chosen to cause maximum local */
/*        growth in the elements of w  where  trans(r)*w = e . */
/*        the vectors are frequently rescaled to avoid overflow. */

/*        solve trans(r)*w = e */

/*<          ek = 1.0d0 >*/
    ek = 1.;
/*<          do 50 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<             z(j) = 0.0d0 >*/
        z__[j] = 0.;
/*<    50    continue >*/
/* L50: */
    }
/*<          do 110 k = 1, n >*/
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
/*<             if (z(k) .ne. 0.0d0) ek = dsign(ek,-z(k)) >*/
        if (z__[k] != 0.) {
            d__1 = -z__[k];
            ek = d_sign(&ek, &d__1);
        }
/*<             if (dabs(ek-z(k)) .le. a(k,k)) go to 60 >*/
        if ((d__1 = ek - z__[k], abs(d__1)) <= a[k + k * a_dim1]) {
            goto L60;
        }
/*<                s = a(k,k)/dabs(ek-z(k)) >*/
        s = a[k + k * a_dim1] / (d__1 = ek - z__[k], abs(d__1));
/*<                call dscal(n,s,z,1) >*/
        dscal_(n, &s, &z__[1], &c__1);
/*<                ek = s*ek >*/
        ek = s * ek;
/*<    60       continue >*/
L60:
/*<             wk = ek - z(k) >*/
        wk = ek - z__[k];
/*<             wkm = -ek - z(k) >*/
        wkm = -ek - z__[k];
/*<             s = dabs(wk) >*/
        s = abs(wk);
/*<             sm = dabs(wkm) >*/
        sm = abs(wkm);
/*<             wk = wk/a(k,k) >*/
        wk /= a[k + k * a_dim1];
/*<             wkm = wkm/a(k,k) >*/
        wkm /= a[k + k * a_dim1];
/*<             kp1 = k + 1 >*/
        kp1 = k + 1;
/*<             if (kp1 .gt. n) go to 100 >*/
        if (kp1 > *n) {
            goto L100;
        }
/*<                do 70 j = kp1, n >*/
        i__2 = *n;
        for (j = kp1; j <= i__2; ++j) {
/*<                   sm = sm + dabs(z(j)+wkm*a(k,j)) >*/
            sm += (d__1 = z__[j] + wkm * a[k + j * a_dim1], abs(d__1));
/*<                   z(j) = z(j) + wk*a(k,j) >*/
            z__[j] += wk * a[k + j * a_dim1];
/*<                   s = s + dabs(z(j)) >*/
            s += (d__1 = z__[j], abs(d__1));
/*<    70          continue >*/
/* L70: */
        }
/*<                if (s .ge. sm) go to 90 >*/
        if (s >= sm) {
            goto L90;
        }
/*<                   t = wkm - wk >*/
        t = wkm - wk;
/*<                   wk = wkm >*/
        wk = wkm;
/*<                   do 80 j = kp1, n >*/
        i__2 = *n;
        for (j = kp1; j <= i__2; ++j) {
/*<                      z(j) = z(j) + t*a(k,j) >*/
            z__[j] += t * a[k + j * a_dim1];
/*<    80             continue >*/
/* L80: */
        }
/*<    90          continue >*/
L90:
/*<   100       continue >*/
L100:
/*<             z(k) = wk >*/
        z__[k] = wk;
/*<   110    continue >*/
/* L110: */
    }
/*<          s = 1.0d0/dasum(n,z,1) >*/
    s = 1. / dasum_(n, &z__[1], &c__1);
/*<          call dscal(n,s,z,1) >*/
    dscal_(n, &s, &z__[1], &c__1);

/*        solve r*y = w */

/*<          do 130 kb = 1, n >*/
    i__1 = *n;
    for (kb = 1; kb <= i__1; ++kb) {
/*<             k = n + 1 - kb >*/
        k = *n + 1 - kb;
/*<             if (dabs(z(k)) .le. a(k,k)) go to 120 >*/
        if ((d__1 = z__[k], abs(d__1)) <= a[k + k * a_dim1]) {
            goto L120;
        }
/*<                s = a(k,k)/dabs(z(k)) >*/
        s = a[k + k * a_dim1] / (d__1 = z__[k], abs(d__1));
/*<                call dscal(n,s,z,1) >*/
        dscal_(n, &s, &z__[1], &c__1);
/*<   120       continue >*/
L120:
/*<             z(k) = z(k)/a(k,k) >*/
        z__[k] /= a[k + k * a_dim1];
/*<             t = -z(k) >*/
        t = -z__[k];
/*<             call daxpy(k-1,t,a(1,k),1,z(1),1) >*/
        i__2 = k - 1;
        daxpy_(&i__2, &t, &a[k * a_dim1 + 1], &c__1, &z__[1], &c__1);
/*<   130    continue >*/
/* L130: */
    }
/*<          s = 1.0d0/dasum(n,z,1) >*/
    s = 1. / dasum_(n, &z__[1], &c__1);
/*<          call dscal(n,s,z,1) >*/
    dscal_(n, &s, &z__[1], &c__1);

/*<          ynorm = 1.0d0 >*/
    ynorm = 1.;

/*        solve trans(r)*v = y */

/*<          do 150 k = 1, n >*/
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
/*<             z(k) = z(k) - ddot(k-1,a(1,k),1,z(1),1) >*/
        i__2 = k - 1;
        z__[k] -= ddot_(&i__2, &a[k * a_dim1 + 1], &c__1, &z__[1], &c__1);
/*<             if (dabs(z(k)) .le. a(k,k)) go to 140 >*/
        if ((d__1 = z__[k], abs(d__1)) <= a[k + k * a_dim1]) {
            goto L140;
        }
/*<                s = a(k,k)/dabs(z(k)) >*/
        s = a[k + k * a_dim1] / (d__1 = z__[k], abs(d__1));
/*<                call dscal(n,s,z,1) >*/
        dscal_(n, &s, &z__[1], &c__1);
/*<                ynorm = s*ynorm >*/
        ynorm = s * ynorm;
/*<   140       continue >*/
L140:
/*<             z(k) = z(k)/a(k,k) >*/
        z__[k] /= a[k + k * a_dim1];
/*<   150    continue >*/
/* L150: */
    }
/*<          s = 1.0d0/dasum(n,z,1) >*/
    s = 1. / dasum_(n, &z__[1], &c__1);
/*<          call dscal(n,s,z,1) >*/
    dscal_(n, &s, &z__[1], &c__1);
/*<          ynorm = s*ynorm >*/
    ynorm = s * ynorm;

/*        solve r*z = v */

/*<          do 170 kb = 1, n >*/
    i__1 = *n;
    for (kb = 1; kb <= i__1; ++kb) {
/*<             k = n + 1 - kb >*/
        k = *n + 1 - kb;
/*<             if (dabs(z(k)) .le. a(k,k)) go to 160 >*/
        if ((d__1 = z__[k], abs(d__1)) <= a[k + k * a_dim1]) {
            goto L160;
        }
/*<                s = a(k,k)/dabs(z(k)) >*/
        s = a[k + k * a_dim1] / (d__1 = z__[k], abs(d__1));
/*<                call dscal(n,s,z,1) >*/
        dscal_(n, &s, &z__[1], &c__1);
/*<                ynorm = s*ynorm >*/
        ynorm = s * ynorm;
/*<   160       continue >*/
L160:
/*<             z(k) = z(k)/a(k,k) >*/
        z__[k] /= a[k + k * a_dim1];
/*<             t = -z(k) >*/
        t = -z__[k];
/*<             call daxpy(k-1,t,a(1,k),1,z(1),1) >*/
        i__2 = k - 1;
        daxpy_(&i__2, &t, &a[k * a_dim1 + 1], &c__1, &z__[1], &c__1);
/*<   170    continue >*/
/* L170: */
    }
/*        make znorm = 1.0 */
/*<          s = 1.0d0/dasum(n,z,1) >*/
    s = 1. / dasum_(n, &z__[1], &c__1);
/*<          call dscal(n,s,z,1) >*/
    dscal_(n, &s, &z__[1], &c__1);
/*<          ynorm = s*ynorm >*/
    ynorm = s * ynorm;

/*<          if (anorm .ne. 0.0d0) rcond = ynorm/anorm >*/
    if (anorm != 0.) {
        *rcond = ynorm / anorm;
    }
/*<          if (anorm .eq. 0.0d0) rcond = 0.0d0 >*/
    if (anorm == 0.) {
        *rcond = 0.;
    }
/*<   180 continue >*/
L180:
/*<       return >*/
    return 0;
/*<       end >*/
} /* dpoco_ */

#ifdef __cplusplus
        }
#endif

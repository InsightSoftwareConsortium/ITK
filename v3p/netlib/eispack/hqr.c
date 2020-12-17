/* eispack/hqr.f -- translated by f2c (version 20050501).
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

/*<       subroutine hqr(nm,n,low,igh,h,wr,wi,ierr) >*/
/* Subroutine */ int hqr_(integer *nm, integer *n, integer *low, integer *igh,
         doublereal *h__, doublereal *wr, doublereal *wi, integer *ierr)
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    integer i__, j, k, l=0, m=0;
    doublereal p, q=0, r__=0, s, t, w, x, y;
    integer na, en, ll, mm;
    doublereal zz;
    integer mp2, itn, its, enm2;
    doublereal tst1, tst2, norm;
    logical notlas;

/*  RESTORED CORRECT INDICES OF LOOPS (200,210,230,240). (9/29/89 BSG) */

/*<       integer i,j,k,l,m,n,en,ll,mm,na,nm,igh,itn,its,low,mp2,enm2,ierr >*/
/*<       double precision h(nm,n),wr(n),wi(n) >*/
/*<       double precision p,q,r,s,t,w,x,y,zz,norm,tst1,tst2 >*/
/*<       logical notlas >*/

/*     this subroutine is a translation of the algol procedure hqr, */
/*     num. math. 14, 219-231(1970) by martin, peters, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 359-371(1971). */

/*     this subroutine finds the eigenvalues of a real */
/*     upper hessenberg matrix by the qr method. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        low and igh are integers determined by the balancing */
/*          subroutine  balanc.  if  balanc  has not been used, */
/*          set low=1, igh=n. */

/*        h contains the upper hessenberg matrix.  information about */
/*          the transformations used in the reduction to hessenberg */
/*          form by  elmhes  or  orthes, if performed, is stored */
/*          in the remaining triangle under the hessenberg matrix. */

/*     on output */

/*        h has been destroyed.  therefore, it must be saved */
/*          before calling  hqr  if subsequent calculation and */
/*          back transformation of eigenvectors is to be performed. */

/*        wr and wi contain the real and imaginary parts, */
/*          respectively, of the eigenvalues.  the eigenvalues */
/*          are unordered except that complex conjugate pairs */
/*          of values appear consecutively with the eigenvalue */
/*          having the positive imaginary part first.  if an */
/*          error exit is made, the eigenvalues should be correct */
/*          for indices ierr+1,...,n. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the limit of 30*n iterations is exhausted */
/*                     while the j-th eigenvalue is being sought. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated september 1989. */

/*     ------------------------------------------------------------------ */

/*<       ierr = 0 >*/
    /* Parameter adjustments */
    --wi;
    --wr;
    h_dim1 = *nm;
    h_offset = 1 + h_dim1;
    h__ -= h_offset;

    /* Function Body */
    *ierr = 0;
/*<       norm = 0.0d0 >*/
    norm = 0.;
/*<       k = 1 >*/
    k = 1;
/*     .......... store roots isolated by balanc */
/*                and compute matrix norm .......... */
/*<       do 50 i = 1, n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*<          do 40 j = k, n >*/
        i__2 = *n;
        for (j = k; j <= i__2; ++j) {
/*<    40    norm = norm + dabs(h(i,j)) >*/
/* L40: */
            norm += (d__1 = h__[i__ + j * h_dim1], abs(d__1));
        }

/*<          k = i >*/
        k = i__;
/*<          if (i .ge. low .and. i .le. igh) go to 50 >*/
        if (i__ >= *low && i__ <= *igh) {
            goto L50;
        }
/*<          wr(i) = h(i,i) >*/
        wr[i__] = h__[i__ + i__ * h_dim1];
/*<          wi(i) = 0.0d0 >*/
        wi[i__] = 0.;
/*<    50 continue >*/
L50:
        ;
    }

/*<       en = igh >*/
    en = *igh;
/*<       t = 0.0d0 >*/
    t = 0.;
/*<       itn = 30*n >*/
    itn = *n * 30;
/*     .......... search for next eigenvalues .......... */
/*<    60 if (en .lt. low) go to 1001 >*/
L60:
    if (en < *low) {
        goto L1001;
    }
/*<       its = 0 >*/
    its = 0;
/*<       na = en - 1 >*/
    na = en - 1;
/*<       enm2 = na - 1 >*/
    enm2 = na - 1;
/*     .......... look for single small sub-diagonal element */
/*                for l=en step -1 until low do -- .......... */
/*<    70 do 80 ll = low, en >*/
L70:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
/*<          l = en + low - ll >*/
        l = en + *low - ll;
/*<          if (l .eq. low) go to 100 >*/
        if (l == *low) {
            goto L100;
        }
/*<          s = dabs(h(l-1,l-1)) + dabs(h(l,l)) >*/
        s = (d__1 = h__[l - 1 + (l - 1) * h_dim1], abs(d__1)) + (d__2 = h__[l
                + l * h_dim1], abs(d__2));
/*<          if (s .eq. 0.0d0) s = norm >*/
        if (s == 0.) {
            s = norm;
        }
/*<          tst1 = s >*/
        tst1 = s;
/*<          tst2 = tst1 + dabs(h(l,l-1)) >*/
        tst2 = tst1 + (d__1 = h__[l + (l - 1) * h_dim1], abs(d__1));
/*<          if (tst2 .eq. tst1) go to 100 >*/
        if (tst2 == tst1) {
            goto L100;
        }
/*<    80 continue >*/
/* L80: */
    }
/*     .......... form shift .......... */
/*<   100 x = h(en,en) >*/
L100:
    x = h__[en + en * h_dim1];
/*<       if (l .eq. en) go to 270 >*/
    if (l == en) {
        goto L270;
    }
/*<       y = h(na,na) >*/
    y = h__[na + na * h_dim1];
/*<       w = h(en,na) * h(na,en) >*/
    w = h__[en + na * h_dim1] * h__[na + en * h_dim1];
/*<       if (l .eq. na) go to 280 >*/
    if (l == na) {
        goto L280;
    }
/*<       if (itn .eq. 0) go to 1000 >*/
    if (itn == 0) {
        goto L1000;
    }
/*<       if (its .ne. 10 .and. its .ne. 20) go to 130 >*/
    if (its != 10 && its != 20) {
        goto L130;
    }
/*     .......... form exceptional shift .......... */
/*<       t = t + x >*/
    t += x;

/*<       do 120 i = low, en >*/
    i__1 = en;
    for (i__ = *low; i__ <= i__1; ++i__) {
/*<   120 h(i,i) = h(i,i) - x >*/
/* L120: */
        h__[i__ + i__ * h_dim1] -= x;
    }

/*<       s = dabs(h(en,na)) + dabs(h(na,enm2)) >*/
    s = (d__1 = h__[en + na * h_dim1], abs(d__1)) + (d__2 = h__[na + enm2 *
            h_dim1], abs(d__2));
/*<       x = 0.75d0 * s >*/
    x = s * .75;
/*<       y = x >*/
    y = x;
/*<       w = -0.4375d0 * s * s >*/
    w = s * -.4375 * s;
/*<   130 its = its + 1 >*/
L130:
    ++its;
/*<       itn = itn - 1 >*/
    --itn;
/*     .......... look for two consecutive small */
/*                sub-diagonal elements. */
/*                for m=en-2 step -1 until l do -- .......... */
/*<       do 140 mm = l, enm2 >*/
    i__1 = enm2;
    for (mm = l; mm <= i__1; ++mm) {
/*<          m = enm2 + l - mm >*/
        m = enm2 + l - mm;
/*<          zz = h(m,m) >*/
        zz = h__[m + m * h_dim1];
/*<          r = x - zz >*/
        r__ = x - zz;
/*<          s = y - zz >*/
        s = y - zz;
/*<          p = (r * s - w) / h(m+1,m) + h(m,m+1) >*/
        p = (r__ * s - w) / h__[m + 1 + m * h_dim1] + h__[m + (m + 1) *
                h_dim1];
/*<          q = h(m+1,m+1) - zz - r - s >*/
        q = h__[m + 1 + (m + 1) * h_dim1] - zz - r__ - s;
/*<          r = h(m+2,m+1) >*/
        r__ = h__[m + 2 + (m + 1) * h_dim1];
/*<          s = dabs(p) + dabs(q) + dabs(r) >*/
        s = abs(p) + abs(q) + abs(r__);
/*<          p = p / s >*/
        p /= s;
/*<          q = q / s >*/
        q /= s;
/*<          r = r / s >*/
        r__ /= s;
/*<          if (m .eq. l) go to 150 >*/
        if (m == l) {
            goto L150;
        }
/*<          tst1 = dabs(p)*(dabs(h(m-1,m-1)) + dabs(zz) + dabs(h(m+1,m+1))) >*/
        tst1 = abs(p) * ((d__1 = h__[m - 1 + (m - 1) * h_dim1], abs(d__1)) +
                abs(zz) + (d__2 = h__[m + 1 + (m + 1) * h_dim1], abs(d__2)));
/*<          tst2 = tst1 + dabs(h(m,m-1))*(dabs(q) + dabs(r)) >*/
        tst2 = tst1 + (d__1 = h__[m + (m - 1) * h_dim1], abs(d__1)) * (abs(q)
                + abs(r__));
/*<          if (tst2 .eq. tst1) go to 150 >*/
        if (tst2 == tst1) {
            goto L150;
        }
/*<   140 continue >*/
/* L140: */
    }

/*<   150 mp2 = m + 2 >*/
L150:
    mp2 = m + 2;

/*<       do 160 i = mp2, en >*/
    i__1 = en;
    for (i__ = mp2; i__ <= i__1; ++i__) {
/*<          h(i,i-2) = 0.0d0 >*/
        h__[i__ + (i__ - 2) * h_dim1] = 0.;
/*<          if (i .eq. mp2) go to 160 >*/
        if (i__ == mp2) {
            goto L160;
        }
/*<          h(i,i-3) = 0.0d0 >*/
        h__[i__ + (i__ - 3) * h_dim1] = 0.;
/*<   160 continue >*/
L160:
        ;
    }
/*     .......... double qr step involving rows l to en and */
/*                columns m to en .......... */
/*<       do 260 k = m, na >*/
    i__1 = na;
    for (k = m; k <= i__1; ++k) {
/*<          notlas = k .ne. na >*/
        notlas = k != na;
/*<          if (k .eq. m) go to 170 >*/
        if (k == m) {
            goto L170;
        }
/*<          p = h(k,k-1) >*/
        p = h__[k + (k - 1) * h_dim1];
/*<          q = h(k+1,k-1) >*/
        q = h__[k + 1 + (k - 1) * h_dim1];
/*<          r = 0.0d0 >*/
        r__ = 0.;
/*<          if (notlas) r = h(k+2,k-1) >*/
        if (notlas) {
            r__ = h__[k + 2 + (k - 1) * h_dim1];
        }
/*<          x = dabs(p) + dabs(q) + dabs(r) >*/
        x = abs(p) + abs(q) + abs(r__);
/*<          if (x .eq. 0.0d0) go to 260 >*/
        if (x == 0.) {
            goto L260;
        }
/*<          p = p / x >*/
        p /= x;
/*<          q = q / x >*/
        q /= x;
/*<          r = r / x >*/
        r__ /= x;
/*<   170    s = dsign(dsqrt(p*p+q*q+r*r),p) >*/
L170:
        d__1 = sqrt(p * p + q * q + r__ * r__);
        s = d_sign(&d__1, &p);
/*<          if (k .eq. m) go to 180 >*/
        if (k == m) {
            goto L180;
        }
/*<          h(k,k-1) = -s * x >*/
        h__[k + (k - 1) * h_dim1] = -s * x;
/*<          go to 190 >*/
        goto L190;
/*<   180    if (l .ne. m) h(k,k-1) = -h(k,k-1) >*/
L180:
        if (l != m) {
            h__[k + (k - 1) * h_dim1] = -h__[k + (k - 1) * h_dim1];
        }
/*<   190    p = p + s >*/
L190:
        p += s;
/*<          x = p / s >*/
        x = p / s;
/*<          y = q / s >*/
        y = q / s;
/*<          zz = r / s >*/
        zz = r__ / s;
/*<          q = q / p >*/
        q /= p;
/*<          r = r / p >*/
        r__ /= p;
/*<          if (notlas) go to 225 >*/
        if (notlas) {
            goto L225;
        }
/*     .......... row modification .......... */
/*<          do 200 j = k, EN >*/
        i__2 = en;
        for (j = k; j <= i__2; ++j) {
/*<             p = h(k,j) + q * h(k+1,j) >*/
            p = h__[k + j * h_dim1] + q * h__[k + 1 + j * h_dim1];
/*<             h(k,j) = h(k,j) - p * x >*/
            h__[k + j * h_dim1] -= p * x;
/*<             h(k+1,j) = h(k+1,j) - p * y >*/
            h__[k + 1 + j * h_dim1] -= p * y;
/*<   200    continue >*/
/* L200: */
        }

/*<          j = min0(en,k+3) >*/
/* Computing MIN */
        i__2 = en, i__3 = k + 3;
        j = min(i__2,i__3);
/*     .......... column modification .......... */
/*<          do 210 i = L, j >*/
        i__2 = j;
        for (i__ = l; i__ <= i__2; ++i__) {
/*<             p = x * h(i,k) + y * h(i,k+1) >*/
            p = x * h__[i__ + k * h_dim1] + y * h__[i__ + (k + 1) * h_dim1];
/*<             h(i,k) = h(i,k) - p >*/
            h__[i__ + k * h_dim1] -= p;
/*<             h(i,k+1) = h(i,k+1) - p * q >*/
            h__[i__ + (k + 1) * h_dim1] -= p * q;
/*<   210    continue >*/
/* L210: */
        }
/*<          go to 255 >*/
        goto L255;
/*<   225    continue >*/
L225:
/*     .......... row modification .......... */
/*<          do 230 j = k, EN >*/
        i__2 = en;
        for (j = k; j <= i__2; ++j) {
/*<             p = h(k,j) + q * h(k+1,j) + r * h(k+2,j) >*/
            p = h__[k + j * h_dim1] + q * h__[k + 1 + j * h_dim1] + r__ * h__[
                    k + 2 + j * h_dim1];
/*<             h(k,j) = h(k,j) - p * x >*/
            h__[k + j * h_dim1] -= p * x;
/*<             h(k+1,j) = h(k+1,j) - p * y >*/
            h__[k + 1 + j * h_dim1] -= p * y;
/*<             h(k+2,j) = h(k+2,j) - p * zz >*/
            h__[k + 2 + j * h_dim1] -= p * zz;
/*<   230    continue >*/
/* L230: */
        }

/*<          j = min0(en,k+3) >*/
/* Computing MIN */
        i__2 = en, i__3 = k + 3;
        j = min(i__2,i__3);
/*     .......... column modification .......... */
/*<          do 240 i = L, j >*/
        i__2 = j;
        for (i__ = l; i__ <= i__2; ++i__) {
/*<             p = x * h(i,k) + y * h(i,k+1) + zz * h(i,k+2) >*/
            p = x * h__[i__ + k * h_dim1] + y * h__[i__ + (k + 1) * h_dim1] +
                    zz * h__[i__ + (k + 2) * h_dim1];
/*<             h(i,k) = h(i,k) - p >*/
            h__[i__ + k * h_dim1] -= p;
/*<             h(i,k+1) = h(i,k+1) - p * q >*/
            h__[i__ + (k + 1) * h_dim1] -= p * q;
/*<             h(i,k+2) = h(i,k+2) - p * r >*/
            h__[i__ + (k + 2) * h_dim1] -= p * r__;
/*<   240    continue >*/
/* L240: */
        }
/*<   255    continue >*/
L255:

/*<   260 continue >*/
L260:
        ;
    }

/*<       go to 70 >*/
    goto L70;
/*     .......... one root found .......... */
/*<   270 wr(en) = x + t >*/
L270:
    wr[en] = x + t;
/*<       wi(en) = 0.0d0 >*/
    wi[en] = 0.;
/*<       en = na >*/
    en = na;
/*<       go to 60 >*/
    goto L60;
/*     .......... two roots found .......... */
/*<   280 p = (y - x) / 2.0d0 >*/
L280:
    p = (y - x) / 2.;
/*<       q = p * p + w >*/
    q = p * p + w;
/*<       zz = dsqrt(dabs(q)) >*/
    zz = sqrt((abs(q)));
/*<       x = x + t >*/
    x += t;
/*<       if (q .lt. 0.0d0) go to 320 >*/
    if (q < 0.) {
        goto L320;
    }
/*     .......... real pair .......... */
/*<       zz = p + dsign(zz,p) >*/
    zz = p + d_sign(&zz, &p);
/*<       wr(na) = x + zz >*/
    wr[na] = x + zz;
/*<       wr(en) = wr(na) >*/
    wr[en] = wr[na];
/*<       if (zz .ne. 0.0d0) wr(en) = x - w / zz >*/
    if (zz != 0.) {
        wr[en] = x - w / zz;
    }
/*<       wi(na) = 0.0d0 >*/
    wi[na] = 0.;
/*<       wi(en) = 0.0d0 >*/
    wi[en] = 0.;
/*<       go to 330 >*/
    goto L330;
/*     .......... complex pair .......... */
/*<   320 wr(na) = x + p >*/
L320:
    wr[na] = x + p;
/*<       wr(en) = x + p >*/
    wr[en] = x + p;
/*<       wi(na) = zz >*/
    wi[na] = zz;
/*<       wi(en) = -zz >*/
    wi[en] = -zz;
/*<   330 en = enm2 >*/
L330:
    en = enm2;
/*<       go to 60 >*/
    goto L60;
/*     .......... set error -- all eigenvalues have not */
/*                converged after 30*n iterations .......... */
/*<  1000 ierr = en >*/
L1000:
    *ierr = en;
/*<  1001 return >*/
L1001:
    return 0;
/*<       end >*/
} /* hqr_ */

#ifdef __cplusplus
        }
#endif

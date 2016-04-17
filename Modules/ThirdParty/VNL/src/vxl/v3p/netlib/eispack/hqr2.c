/* eispack/hqr2.f -- translated by f2c (version 20050501).
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

static doublereal c_b49 = 0.;

/*<       subroutine hqr2(nm,n,low,igh,h,wr,wi,z,ierr) >*/
/* Subroutine */ int hqr2_(integer *nm, integer *n, integer *low, integer *
        igh, doublereal *h__, doublereal *wr, doublereal *wi, doublereal *z__,
         integer *ierr)
{
    /* System generated locals */
    integer h_dim1, h_offset, z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    integer i__, j, k, l=0, m=0;
    doublereal p, q, r__=0, s=0, t, w, x, y;
    integer na, ii, en, jj;
    doublereal ra, sa;
    integer ll, mm, nn;
    doublereal vi, vr, zz;
    integer mp2, itn, its, enm2;
    doublereal tst1, tst2;
    extern /* Subroutine */ int cdiv_(doublereal *, doublereal *, doublereal *
            , doublereal *, doublereal *, doublereal *);
    doublereal norm;
    logical notlas;


/*<    >*/
/*<       double precision h(nm,n),wr(n),wi(n),z(nm,n) >*/
/*<       double precision p,q,r,s,t,w,x,y,ra,sa,vi,vr,zz,norm,tst1,tst2 >*/
/*<       logical notlas >*/

/*     this subroutine is a translation of the algol procedure hqr2, */
/*     num. math. 16, 181-204(1970) by peters and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971). */

/*     this subroutine finds the eigenvalues and eigenvectors */
/*     of a real upper hessenberg matrix by the qr method.  the */
/*     eigenvectors of a real general matrix can also be found */
/*     if  elmhes  and  eltran  or  orthes  and  ortran  have */
/*     been used to reduce this general matrix to hessenberg form */
/*     and to accumulate the similarity transformations. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        low and igh are integers determined by the balancing */
/*          subroutine  balanc.  if  balanc  has not been used, */
/*          set low=1, igh=n. */

/*        h contains the upper hessenberg matrix. */

/*        z contains the transformation matrix produced by  eltran */
/*          after the reduction by  elmhes, or by  ortran  after the */
/*          reduction by  orthes, if performed.  if the eigenvectors */
/*          of the hessenberg matrix are desired, z must contain the */
/*          identity matrix. */

/*     on output */

/*        h has been destroyed. */

/*        wr and wi contain the real and imaginary parts, */
/*          respectively, of the eigenvalues.  the eigenvalues */
/*          are unordered except that complex conjugate pairs */
/*          of values appear consecutively with the eigenvalue */
/*          having the positive imaginary part first.  if an */
/*          error exit is made, the eigenvalues should be correct */
/*          for indices ierr+1,...,n. */

/*        z contains the real and imaginary parts of the eigenvectors. */
/*          if the i-th eigenvalue is real, the i-th column of z */
/*          contains its eigenvector.  if the i-th eigenvalue is complex */
/*          with positive imaginary part, the i-th and (i+1)-th */
/*          columns of z contain the real and imaginary parts of its */
/*          eigenvector.  the eigenvectors are unnormalized.  if an */
/*          error exit is made, none of the eigenvectors has been found. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the limit of 30*n iterations is exhausted */
/*                     while the j-th eigenvalue is being sought. */

/*     calls cdiv for complex division. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*<       ierr = 0 >*/
    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
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
/*<    60 if (en .lt. low) go to 340 >*/
L60:
    if (en < *low) {
        goto L340;
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
/*<          do 200 j = k, n >*/
        i__2 = *n;
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
/*<          do 210 i = 1, j >*/
        i__2 = j;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             p = x * h(i,k) + y * h(i,k+1) >*/
            p = x * h__[i__ + k * h_dim1] + y * h__[i__ + (k + 1) * h_dim1];
/*<             h(i,k) = h(i,k) - p >*/
            h__[i__ + k * h_dim1] -= p;
/*<             h(i,k+1) = h(i,k+1) - p * q >*/
            h__[i__ + (k + 1) * h_dim1] -= p * q;
/*<   210    continue >*/
/* L210: */
        }
/*     .......... accumulate transformations .......... */
/*<          do 220 i = low, igh >*/
        i__2 = *igh;
        for (i__ = *low; i__ <= i__2; ++i__) {
/*<             p = x * z(i,k) + y * z(i,k+1) >*/
            p = x * z__[i__ + k * z_dim1] + y * z__[i__ + (k + 1) * z_dim1];
/*<             z(i,k) = z(i,k) - p >*/
            z__[i__ + k * z_dim1] -= p;
/*<             z(i,k+1) = z(i,k+1) - p * q >*/
            z__[i__ + (k + 1) * z_dim1] -= p * q;
/*<   220    continue >*/
/* L220: */
        }
/*<          go to 255 >*/
        goto L255;
/*<   225    continue >*/
L225:
/*     .......... row modification .......... */
/*<          do 230 j = k, n >*/
        i__2 = *n;
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
/*<          do 240 i = 1, j >*/
        i__2 = j;
        for (i__ = 1; i__ <= i__2; ++i__) {
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
/*     .......... accumulate transformations .......... */
/*<          do 250 i = low, igh >*/
        i__2 = *igh;
        for (i__ = *low; i__ <= i__2; ++i__) {
/*<             p = x * z(i,k) + y * z(i,k+1) + zz * z(i,k+2) >*/
            p = x * z__[i__ + k * z_dim1] + y * z__[i__ + (k + 1) * z_dim1] +
                    zz * z__[i__ + (k + 2) * z_dim1];
/*<             z(i,k) = z(i,k) - p >*/
            z__[i__ + k * z_dim1] -= p;
/*<             z(i,k+1) = z(i,k+1) - p * q >*/
            z__[i__ + (k + 1) * z_dim1] -= p * q;
/*<             z(i,k+2) = z(i,k+2) - p * r >*/
            z__[i__ + (k + 2) * z_dim1] -= p * r__;
/*<   250    continue >*/
/* L250: */
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
/*<   270 h(en,en) = x + t >*/
L270:
    h__[en + en * h_dim1] = x + t;
/*<       wr(en) = h(en,en) >*/
    wr[en] = h__[en + en * h_dim1];
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
/*<       h(en,en) = x + t >*/
    h__[en + en * h_dim1] = x + t;
/*<       x = h(en,en) >*/
    x = h__[en + en * h_dim1];
/*<       h(na,na) = y + t >*/
    h__[na + na * h_dim1] = y + t;
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
/*<       x = h(en,na) >*/
    x = h__[en + na * h_dim1];
/*<       s = dabs(x) + dabs(zz) >*/
    s = abs(x) + abs(zz);
/*<       p = x / s >*/
    p = x / s;
/*<       q = zz / s >*/
    q = zz / s;
/*<       r = dsqrt(p*p+q*q) >*/
    r__ = sqrt(p * p + q * q);
/*<       p = p / r >*/
    p /= r__;
/*<       q = q / r >*/
    q /= r__;
/*     .......... row modification .......... */
/*<       do 290 j = na, n >*/
    i__1 = *n;
    for (j = na; j <= i__1; ++j) {
/*<          zz = h(na,j) >*/
        zz = h__[na + j * h_dim1];
/*<          h(na,j) = q * zz + p * h(en,j) >*/
        h__[na + j * h_dim1] = q * zz + p * h__[en + j * h_dim1];
/*<          h(en,j) = q * h(en,j) - p * zz >*/
        h__[en + j * h_dim1] = q * h__[en + j * h_dim1] - p * zz;
/*<   290 continue >*/
/* L290: */
    }
/*     .......... column modification .......... */
/*<       do 300 i = 1, en >*/
    i__1 = en;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          zz = h(i,na) >*/
        zz = h__[i__ + na * h_dim1];
/*<          h(i,na) = q * zz + p * h(i,en) >*/
        h__[i__ + na * h_dim1] = q * zz + p * h__[i__ + en * h_dim1];
/*<          h(i,en) = q * h(i,en) - p * zz >*/
        h__[i__ + en * h_dim1] = q * h__[i__ + en * h_dim1] - p * zz;
/*<   300 continue >*/
/* L300: */
    }
/*     .......... accumulate transformations .......... */
/*<       do 310 i = low, igh >*/
    i__1 = *igh;
    for (i__ = *low; i__ <= i__1; ++i__) {
/*<          zz = z(i,na) >*/
        zz = z__[i__ + na * z_dim1];
/*<          z(i,na) = q * zz + p * z(i,en) >*/
        z__[i__ + na * z_dim1] = q * zz + p * z__[i__ + en * z_dim1];
/*<          z(i,en) = q * z(i,en) - p * zz >*/
        z__[i__ + en * z_dim1] = q * z__[i__ + en * z_dim1] - p * zz;
/*<   310 continue >*/
/* L310: */
    }

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
/*     .......... all roots found.  backsubstitute to find */
/*                vectors of upper triangular form .......... */
/*<   340 if (norm .eq. 0.0d0) go to 1001 >*/
L340:
    if (norm == 0.) {
        goto L1001;
    }
/*     .......... for en=n step -1 until 1 do -- .......... */
/*<       do 800 nn = 1, n >*/
    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {
/*<          en = n + 1 - nn >*/
        en = *n + 1 - nn;
/*<          p = wr(en) >*/
        p = wr[en];
/*<          q = wi(en) >*/
        q = wi[en];
/*<          na = en - 1 >*/
        na = en - 1;
/*<          if (q) 710, 600, 800 >*/
        if (q < 0.) {
            goto L710;
        } else if (q == 0) {
            goto L600;
        } else {
            goto L800;
        }
/*     .......... real vector .......... */
/*<   600    m = en >*/
L600:
        m = en;
/*<          h(en,en) = 1.0d0 >*/
        h__[en + en * h_dim1] = 1.;
/*<          if (na .eq. 0) go to 800 >*/
        if (na == 0) {
            goto L800;
        }
/*     .......... for i=en-1 step -1 until 1 do -- .......... */
/*<          do 700 ii = 1, na >*/
        i__2 = na;
        for (ii = 1; ii <= i__2; ++ii) {
/*<             i = en - ii >*/
            i__ = en - ii;
/*<             w = h(i,i) - p >*/
            w = h__[i__ + i__ * h_dim1] - p;
/*<             r = 0.0d0 >*/
            r__ = 0.;

/*<             do 610 j = m, en >*/
            i__3 = en;
            for (j = m; j <= i__3; ++j) {
/*<   610       r = r + h(i,j) * h(j,en) >*/
/* L610: */
                r__ += h__[i__ + j * h_dim1] * h__[j + en * h_dim1];
            }

/*<             if (wi(i) .ge. 0.0d0) go to 630 >*/
            if (wi[i__] >= 0.) {
                goto L630;
            }
/*<             zz = w >*/
            zz = w;
/*<             s = r >*/
            s = r__;
/*<             go to 700 >*/
            goto L700;
/*<   630       m = i >*/
L630:
            m = i__;
/*<             if (wi(i) .ne. 0.0d0) go to 640 >*/
            if (wi[i__] != 0.) {
                goto L640;
            }
/*<             t = w >*/
            t = w;
/*<             if (t .ne. 0.0d0) go to 635 >*/
            if (t != 0.) {
                goto L635;
            }
/*<                tst1 = norm >*/
            tst1 = norm;
/*<                t = tst1 >*/
            t = tst1;
/*<   632          t = 0.01d0 * t >*/
L632:
            t *= .01;
/*<                tst2 = norm + t >*/
            tst2 = norm + t;
/*<                if (tst2 .gt. tst1) go to 632 >*/
            if (tst2 > tst1) {
                goto L632;
            }
/*<   635       h(i,en) = -r / t >*/
L635:
            h__[i__ + en * h_dim1] = -r__ / t;
/*<             go to 680 >*/
            goto L680;
/*     .......... solve real equations .......... */
/*<   640       x = h(i,i+1) >*/
L640:
            x = h__[i__ + (i__ + 1) * h_dim1];
/*<             y = h(i+1,i) >*/
            y = h__[i__ + 1 + i__ * h_dim1];
/*<             q = (wr(i) - p) * (wr(i) - p) + wi(i) * wi(i) >*/
            q = (wr[i__] - p) * (wr[i__] - p) + wi[i__] * wi[i__];
/*<             t = (x * s - zz * r) / q >*/
            t = (x * s - zz * r__) / q;
/*<             h(i,en) = t >*/
            h__[i__ + en * h_dim1] = t;
/*<             if (dabs(x) .le. dabs(zz)) go to 650 >*/
            if (abs(x) <= abs(zz)) {
                goto L650;
            }
/*<             h(i+1,en) = (-r - w * t) / x >*/
            h__[i__ + 1 + en * h_dim1] = (-r__ - w * t) / x;
/*<             go to 680 >*/
            goto L680;
/*<   650       h(i+1,en) = (-s - y * t) / zz >*/
L650:
            h__[i__ + 1 + en * h_dim1] = (-s - y * t) / zz;

/*     .......... overflow control .......... */
/*<   680       t = dabs(h(i,en)) >*/
L680:
            t = (d__1 = h__[i__ + en * h_dim1], abs(d__1));
/*<             if (t .eq. 0.0d0) go to 700 >*/
            if (t == 0.) {
                goto L700;
            }
/*<             tst1 = t >*/
            tst1 = t;
/*<             tst2 = tst1 + 1.0d0/tst1 >*/
            tst2 = tst1 + 1. / tst1;
/*<             if (tst2 .gt. tst1) go to 700 >*/
            if (tst2 > tst1) {
                goto L700;
            }
/*<             do 690 j = i, en >*/
            i__3 = en;
            for (j = i__; j <= i__3; ++j) {
/*<                h(j,en) = h(j,en)/t >*/
                h__[j + en * h_dim1] /= t;
/*<   690       continue >*/
/* L690: */
            }

/*<   700    continue >*/
L700:
            ;
        }
/*     .......... end real vector .......... */
/*<          go to 800 >*/
        goto L800;
/*     .......... complex vector .......... */
/*<   710    m = na >*/
L710:
        m = na;
/*     .......... last vector component chosen imaginary so that */
/*                eigenvector matrix is triangular .......... */
/*<          if (dabs(h(en,na)) .le. dabs(h(na,en))) go to 720 >*/
        if ((d__1 = h__[en + na * h_dim1], abs(d__1)) <= (d__2 = h__[na + en *
                 h_dim1], abs(d__2))) {
            goto L720;
        }
/*<          h(na,na) = q / h(en,na) >*/
        h__[na + na * h_dim1] = q / h__[en + na * h_dim1];
/*<          h(na,en) = -(h(en,en) - p) / h(en,na) >*/
        h__[na + en * h_dim1] = -(h__[en + en * h_dim1] - p) / h__[en + na *
                h_dim1];
/*<          go to 730 >*/
        goto L730;
/*<   720    call cdiv(0.0d0,-h(na,en),h(na,na)-p,q,h(na,na),h(na,en)) >*/
L720:
        d__1 = -h__[na + en * h_dim1];
        d__2 = h__[na + na * h_dim1] - p;
        cdiv_(&c_b49, &d__1, &d__2, &q, &h__[na + na * h_dim1], &h__[na + en *
                 h_dim1]);
/*<   730    h(en,na) = 0.0d0 >*/
L730:
        h__[en + na * h_dim1] = 0.;
/*<          h(en,en) = 1.0d0 >*/
        h__[en + en * h_dim1] = 1.;
/*<          enm2 = na - 1 >*/
        enm2 = na - 1;
/*<          if (enm2 .eq. 0) go to 800 >*/
        if (enm2 == 0) {
            goto L800;
        }
/*     .......... for i=en-2 step -1 until 1 do -- .......... */
/*<          do 795 ii = 1, enm2 >*/
        i__2 = enm2;
        for (ii = 1; ii <= i__2; ++ii) {
/*<             i = na - ii >*/
            i__ = na - ii;
/*<             w = h(i,i) - p >*/
            w = h__[i__ + i__ * h_dim1] - p;
/*<             ra = 0.0d0 >*/
            ra = 0.;
/*<             sa = 0.0d0 >*/
            sa = 0.;

/*<             do 760 j = m, en >*/
            i__3 = en;
            for (j = m; j <= i__3; ++j) {
/*<                ra = ra + h(i,j) * h(j,na) >*/
                ra += h__[i__ + j * h_dim1] * h__[j + na * h_dim1];
/*<                sa = sa + h(i,j) * h(j,en) >*/
                sa += h__[i__ + j * h_dim1] * h__[j + en * h_dim1];
/*<   760       continue >*/
/* L760: */
            }

/*<             if (wi(i) .ge. 0.0d0) go to 770 >*/
            if (wi[i__] >= 0.) {
                goto L770;
            }
/*<             zz = w >*/
            zz = w;
/*<             r = ra >*/
            r__ = ra;
/*<             s = sa >*/
            s = sa;
/*<             go to 795 >*/
            goto L795;
/*<   770       m = i >*/
L770:
            m = i__;
/*<             if (wi(i) .ne. 0.0d0) go to 780 >*/
            if (wi[i__] != 0.) {
                goto L780;
            }
/*<             call cdiv(-ra,-sa,w,q,h(i,na),h(i,en)) >*/
            d__1 = -ra;
            d__2 = -sa;
            cdiv_(&d__1, &d__2, &w, &q, &h__[i__ + na * h_dim1], &h__[i__ +
                    en * h_dim1]);
/*<             go to 790 >*/
            goto L790;
/*     .......... solve complex equations .......... */
/*<   780       x = h(i,i+1) >*/
L780:
            x = h__[i__ + (i__ + 1) * h_dim1];
/*<             y = h(i+1,i) >*/
            y = h__[i__ + 1 + i__ * h_dim1];
/*<             vr = (wr(i) - p) * (wr(i) - p) + wi(i) * wi(i) - q * q >*/
            vr = (wr[i__] - p) * (wr[i__] - p) + wi[i__] * wi[i__] - q * q;
/*<             vi = (wr(i) - p) * 2.0d0 * q >*/
            vi = (wr[i__] - p) * 2. * q;
/*<             if (vr .ne. 0.0d0 .or. vi .ne. 0.0d0) go to 784 >*/
            if (vr != 0. || vi != 0.) {
                goto L784;
            }
/*<    >*/
            tst1 = norm * (abs(w) + abs(q) + abs(x) + abs(y) + abs(zz));
/*<                vr = tst1 >*/
            vr = tst1;
/*<   783          vr = 0.01d0 * vr >*/
L783:
            vr *= .01;
/*<                tst2 = tst1 + vr >*/
            tst2 = tst1 + vr;
/*<                if (tst2 .gt. tst1) go to 783 >*/
            if (tst2 > tst1) {
                goto L783;
            }
/*<    >*/
L784:
            d__1 = x * r__ - zz * ra + q * sa;
            d__2 = x * s - zz * sa - q * ra;
            cdiv_(&d__1, &d__2, &vr, &vi, &h__[i__ + na * h_dim1], &h__[i__ +
                    en * h_dim1]);
/*<             if (dabs(x) .le. dabs(zz) + dabs(q)) go to 785 >*/
            if (abs(x) <= abs(zz) + abs(q)) {
                goto L785;
            }
/*<             h(i+1,na) = (-ra - w * h(i,na) + q * h(i,en)) / x >*/
            h__[i__ + 1 + na * h_dim1] = (-ra - w * h__[i__ + na * h_dim1] +
                    q * h__[i__ + en * h_dim1]) / x;
/*<             h(i+1,en) = (-sa - w * h(i,en) - q * h(i,na)) / x >*/
            h__[i__ + 1 + en * h_dim1] = (-sa - w * h__[i__ + en * h_dim1] -
                    q * h__[i__ + na * h_dim1]) / x;
/*<             go to 790 >*/
            goto L790;
/*<    >*/
L785:
            d__1 = -r__ - y * h__[i__ + na * h_dim1];
            d__2 = -s - y * h__[i__ + en * h_dim1];
            cdiv_(&d__1, &d__2, &zz, &q, &h__[i__ + 1 + na * h_dim1], &h__[
                    i__ + 1 + en * h_dim1]);

/*     .......... overflow control .......... */
/*<   790       t = dmax1(dabs(h(i,na)), dabs(h(i,en))) >*/
L790:
/* Computing MAX */
            d__3 = (d__1 = h__[i__ + na * h_dim1], abs(d__1)), d__4 = (d__2 =
                    h__[i__ + en * h_dim1], abs(d__2));
            t = max(d__3,d__4);
/*<             if (t .eq. 0.0d0) go to 795 >*/
            if (t == 0.) {
                goto L795;
            }
/*<             tst1 = t >*/
            tst1 = t;
/*<             tst2 = tst1 + 1.0d0/tst1 >*/
            tst2 = tst1 + 1. / tst1;
/*<             if (tst2 .gt. tst1) go to 795 >*/
            if (tst2 > tst1) {
                goto L795;
            }
/*<             do 792 j = i, en >*/
            i__3 = en;
            for (j = i__; j <= i__3; ++j) {
/*<                h(j,na) = h(j,na)/t >*/
                h__[j + na * h_dim1] /= t;
/*<                h(j,en) = h(j,en)/t >*/
                h__[j + en * h_dim1] /= t;
/*<   792       continue >*/
/* L792: */
            }

/*<   795    continue >*/
L795:
            ;
        }
/*     .......... end complex vector .......... */
/*<   800 continue >*/
L800:
        ;
    }
/*     .......... end back substitution. */
/*                vectors of isolated roots .......... */
/*<       do 840 i = 1, n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          if (i .ge. low .and. i .le. igh) go to 840 >*/
        if (i__ >= *low && i__ <= *igh) {
            goto L840;
        }

/*<          do 820 j = i, n >*/
        i__2 = *n;
        for (j = i__; j <= i__2; ++j) {
/*<   820    z(i,j) = h(i,j) >*/
/* L820: */
            z__[i__ + j * z_dim1] = h__[i__ + j * h_dim1];
        }

/*<   840 continue >*/
L840:
        ;
    }
/*     .......... multiply by transformation matrix to give */
/*                vectors of original full matrix. */
/*                for j=n step -1 until low do -- .......... */
/*<       do 880 jj = low, n >*/
    i__1 = *n;
    for (jj = *low; jj <= i__1; ++jj) {
/*<          j = n + low - jj >*/
        j = *n + *low - jj;
/*<          m = min0(j,igh) >*/
        m = min(j,*igh);

/*<          do 880 i = low, igh >*/
        i__2 = *igh;
        for (i__ = *low; i__ <= i__2; ++i__) {
/*<             zz = 0.0d0 >*/
            zz = 0.;

/*<             do 860 k = low, m >*/
            i__3 = m;
            for (k = *low; k <= i__3; ++k) {
/*<   860       zz = zz + z(i,k) * h(k,j) >*/
/* L860: */
                zz += z__[i__ + k * z_dim1] * h__[k + j * h_dim1];
            }

/*<             z(i,j) = zz >*/
            z__[i__ + j * z_dim1] = zz;
/*<   880 continue >*/
/* L880: */
        }
    }

/*<       go to 1001 >*/
    goto L1001;
/*     .......... set error -- all eigenvalues have not */
/*                converged after 30*n iterations .......... */
/*<  1000 ierr = en >*/
L1000:
    *ierr = en;
/*<  1001 return >*/
L1001:
    return 0;
/*<       end >*/
} /* hqr2_ */

#ifdef __cplusplus
        }
#endif

#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

static void balanc_(integer *nm, integer *n, doublereal *a, integer *low, integer *igh, doublereal *scale);
static void balbak_(integer *nm, integer *n, integer *low, integer *igh, doublereal *scale, integer *m, doublereal *z);
static void cdiv_(doublereal *ar, doublereal *ai, doublereal *br, doublereal *bi, doublereal *cr, doublereal *ci);
static void elmhes_(integer *nm, integer *n, integer *low, integer *igh, doublereal *a, integer *int_);
static void eltran_(integer *nm, integer *n, integer *low, integer *igh, doublereal *a, integer *int_, doublereal *z);
static void hqr_(integer *nm, integer *n, integer *low, integer *igh, doublereal *h,
                 doublereal *wr, doublereal *wi, integer *ierr);
static void hqr2_(integer *nm, integer *n, integer *low, integer *igh, doublereal *h,
                  doublereal *wr, doublereal *wi, doublereal *z, integer *ierr);

/* Modified by Peter Vanroose, Sept 2001: manual optimisation and clean-up */

/* Table of constant values */
static doublereal c_b130 = 0.;

/* ====================================================================== */
/* NIST Guide to Available Math Software. */
/* Fullsource for module RG from package EISPACK. */
/* Retrieved from NETLIB on Thu Jan 23 06:12:53 1997. */
/* ====================================================================== */
/* Subroutine */ void rg_(nm, n, a, wr, wi, matz, z, iv1, fv1, ierr)
integer *nm, *n;
doublereal *a, *wr, *wi;
integer *matz;
doublereal *z;
integer *iv1;
doublereal *fv1;
integer *ierr;
{
    /* Local variables */
    static integer is1, is2;

/*     this subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (eispack) */
/*     to find the eigenvalues and eigenvectors (if desired) */
/*     of a real general matrix. */

/*     on input */

/*        nm  must be set to the row dimension of the two-dimensional */
/*        array parameters as declared in the calling program */
/*        dimension statement. */

/*        n  is the order of the matrix  a. */

/*        a  contains the real general matrix. */

/*        matz  is an integer variable set equal to zero if */
/*        only eigenvalues are desired.  otherwise it is set to */
/*        any non-zero integer for both eigenvalues and eigenvectors. */

/*     on output */

/*        wr  and  wi  contain the real and imaginary parts, */
/*        respectively, of the eigenvalues.  complex conjugate */
/*        pairs of eigenvalues appear consecutively with the */
/*        eigenvalue having the positive imaginary part first. */

/*        z  contains the real and imaginary parts of the eigenvectors */
/*        if matz is not zero.  if the j-th eigenvalue is real, the */
/*        j-th column of  z  contains its eigenvector.  if the j-th */
/*        eigenvalue is complex with positive imaginary part, the */
/*        j-th and (j+1)-th columns of  z  contain the real and */
/*        imaginary parts of its eigenvector.  the conjugate of this */
/*        vector is the eigenvector for the conjugate eigenvalue. */

/*        ierr  is an integer output variable set equal to an error */
/*           completion code described in the documentation for hqr */
/*           and hqr2.  the normal completion code is zero. */

/*        iv1  and  fv1  are temporary storage arrays. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    if (*n <= *nm) {
        goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    balanc_(nm, n, a, &is1, &is2, fv1);
    elmhes_(nm, n, &is1, &is2, a, iv1);
    if (*matz != 0) {
        goto L20;
    }
/*     .......... find eigenvalues only .......... */
    hqr_(nm, n, &is1, &is2, a, wr, wi, ierr);
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
L20:
    eltran_(nm, n, &is1, &is2, a, iv1, z);
    hqr2_(nm, n, &is1, &is2, a, wr, wi, z, ierr);
    if (*ierr != 0) {
        goto L50;
    }
    balbak_(nm, n, &is1, &is2, fv1, n, z);
L50:
    return;
} /* rg_ */

/* Subroutine */
static void balanc_(nm, n, a, low, igh, scale)
integer *nm, *n;
doublereal *a;
integer *low, *igh;
doublereal *scale;
{
    /* Local variables */
    static integer iexc;
    static doublereal c, f, g;
    static integer i, j, k, l, m;
    static doublereal r, s, radix, b2;
    static logical noconv;


/*     this subroutine is a translation of the algol procedure balance, */
/*     num. math. 13, 293-304(1969) by parlett and reinsch. */
/*     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971). */

/*     this subroutine balances a real matrix and isolates */
/*     eigenvalues whenever possible. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        a contains the input matrix to be balanced. */

/*     on output */

/*        a contains the balanced matrix. */

/*        low and igh are two integers such that a(i,j) */
/*          is equal to zero if */
/*           (1) i is greater than j and */
/*           (2) j=1,...,low-1 or i=igh+1,...,n. */

/*        scale contains information determining the */
/*           permutations and scaling factors used. */

/*     suppose that the principal submatrix in rows low through igh */
/*     has been balanced, that p(j) denotes the index interchanged */
/*     with j during the permutation step, and that the elements */
/*     of the diagonal matrix used are denoted by d(i,j).  then */
/*        scale(j) = p(j),    for j = 1,...,low-1 */
/*                 = d(j,j),      j = low,...,igh */
/*                 = p(j)         j = igh+1,...,n. */
/*     the order in which the interchanges are made is n to igh+1, */
/*     then 1 to low-1. */

/*     note that 1 is returned for igh if igh is zero formally. */

/*     the algol procedure exc contained in balance appears in */
/*     balanc  in line.  (note that the algol roles of identifiers */
/*     k,l have been reversed.) */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    radix = 16.;

    b2 = radix * radix;
    k = 0;
    l = *n;
    goto L100;
/*     .......... in-line procedure for row and column exchange .......... */
L20:
    scale[m] = (doublereal) j+1;
    if (j == m) {
        goto L50;
    }

    for (i = 0; i < l; ++i) {
        f = a[i + j * *nm];
        a[i + j * *nm] = a[i + m * *nm];
        a[i + m * *nm] = f;
    }

    for (i = k; i < *n; ++i) {
        f = a[j + i * *nm];
        a[j + i * *nm] = a[m + i * *nm];
        a[m + i * *nm] = f;
    }

L50:
    switch ((int)iexc) {
        case 1:  goto L80;
        case 2:  goto L130;
    }
/*     .......... search for rows isolating an eigenvalue and push them down .......... */
L80:
    if (l == 1) {
        goto L280;
    }
    --l;
/*     .......... for j=l step -1 until 1 do -- .......... */
L100:
    for (j = l-1; j >= 0; --j) {
        for (i = 0; i < l; ++i) {
            if (i != j && a[j + i * *nm] != 0.) {
                goto L120; /* continue outer loop */
            }
        }

        m = l-1;
        iexc = 1;
        goto L20;
L120:
        ;
    }

    goto L140;
/*     .......... search for columns isolating an eigenvalue and push them left .......... */
L130:
    ++k;

L140:
    for (j = k; j < l; ++j) {
        for (i = k; i < l; ++i) {
            if (i != j && a[i + j * *nm] != 0.) {
                goto L170; /* continue outer loop */
            }
        }

        m = k;
        iexc = 2;
        goto L20;
L170:
        ;
    }
/*     .......... now balance the submatrix in rows k to l .......... */
    for (i = k; i < l; ++i) {
        scale[i] = 1.;
    }
/*     .......... iterative loop for norm reduction .......... */
L190:
    noconv = FALSE_;

    for (i = k; i < l; ++i) {
        c = 0.;
        r = 0.;

        for (j = k; j < l; ++j) {
            if (j != i) {
                c += abs(a[j + i * *nm]);
                r += abs(a[i + j * *nm]);
            }
        }
/*     .......... guard against zero c or r due to underflow .......... */
        if (c == 0. || r == 0.) {
            continue;
        }
        g = r / radix;
        f = 1.;
        s = c + r;
L210:
        if (c >= g) {
            goto L220;
        }
        f *= radix;
        c *= b2;
        goto L210;
L220:
        g = r * radix;
L230:
        if (c < g) {
            goto L240;
        }
        f /= radix;
        c /= b2;
        goto L230;
/*     .......... now balance .......... */
L240:
        if ((c + r) / f >= s * .95) {
            continue;
        }
        g = 1. / f;
        scale[i] *= f;
        noconv = TRUE_;

        for (j = k; j < *n; ++j) {
            a[i + j * *nm] *= g;
        }

        for (j = 0; j < l; ++j) {
            a[j + i * *nm] *= f;
        }
    }

    if (noconv) {
        goto L190;
    }

L280:
    *low = k+1;
    *igh = l;
    return;
} /* balanc_ */

/* Subroutine */
static void balbak_(nm, n, low, igh, scale, m, z)
integer *nm, *n, *low, *igh;
doublereal *scale;
integer *m;
doublereal *z;
{
    /* Local variables */
    static integer i, j, k;
    static doublereal s;
    static integer ii;


/*     this subroutine is a translation of the algol procedure balbak, */
/*     num. math. 13, 293-304(1969) by parlett and reinsch. */
/*     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971). */

/*     this subroutine forms the eigenvectors of a real general */
/*     matrix by back transforming those of the corresponding */
/*     balanced matrix determined by  balanc. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        low and igh are integers determined by  balanc. */

/*        scale contains information determining the permutations */
/*          and scaling factors used by  balanc. */

/*        m is the number of columns of z to be back transformed. */

/*        z contains the real and imaginary parts of the eigen- */
/*          vectors to be back transformed in its first m columns. */

/*     on output */

/*        z contains the real and imaginary parts of the */
/*          transformed eigenvectors in its first m columns. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    if (*m == 0) {
        return; /* exit from balbak_ */
    }
    if (*igh == *low) {
        goto L120;
    }

    for (i = *low-1; i < *igh; ++i) {
        s = scale[i];
/*     .......... left hand eigenvectors are back transformed */
/*                if the foregoing statement is replaced by s=1.0d0/scale(i). .......... */
        for (j = 0; j < *m; ++j) {
            z[i + j * *nm] *= s;
        }
    }
/*     ......... for i=low-1 step -1 until 1, */
/*               igh+1 step 1 until n do -- .......... */
L120:
    for (ii = 0; ii < *n; ++ii) {
        i = ii;
        if (i+1 >= *low && i < *igh) {
            continue;
        }
        if (i+1 < *low) {
            i = *low - ii - 2;
        }
        k = (integer) scale[i] - 1;
        if (k != i)
        for (j = 0; j < *m; ++j) {
            s = z[i + j * *nm];
            z[i + j * *nm] = z[k + j * *nm];
            z[k + j * *nm] = s;
        }
    }
} /* balbak_ */

/* Subroutine */
static void cdiv_(ar, ai, br, bi, cr, ci)
doublereal *ar, *ai, *br, *bi, *cr, *ci;
{
    /* Local variables */
    static doublereal s, ais, bis, ars, brs;

/*     complex division, (cr,ci) = (ar,ai)/(br,bi) */

    s = abs(*br) + abs(*bi);
    ars = *ar / s;
    ais = *ai / s;
    brs = *br / s;
    bis = *bi / s;
    s = brs * brs + bis * bis;
    *cr = (ars * brs + ais * bis) / s;
    *ci = (ais * brs - ars * bis) / s;
} /* cdiv_ */

/* Subroutine */
static void elmhes_(nm, n, low, igh, a, int_)
integer *nm, *n, *low, *igh;
doublereal *a;
integer *int_;
{
    /* Local variables */
    static integer i, j, m;
    static doublereal x, y;
    static integer la, mm1;


/*     this subroutine is a translation of the algol procedure elmhes, */
/*     num. math. 12, 349-368(1968) by martin and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971). */

/*     given a real general matrix, this subroutine */
/*     reduces a submatrix situated in rows and columns */
/*     low through igh to upper hessenberg form by */
/*     stabilized elementary similarity transformations. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        low and igh are integers determined by the balancing */
/*          subroutine  balanc.  if  balanc  has not been used, */
/*          set low=1, igh=n. */

/*        a contains the input matrix. */

/*     on output */

/*        a contains the hessenberg matrix.  the multipliers */
/*          which were used in the reduction are stored in the */
/*          remaining triangle under the hessenberg matrix. */

/*        int contains information on the rows and columns */
/*          interchanged in the reduction. */
/*          only elements low through igh are used. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    la = *igh - 1;
    if (la < *low + 1) {
        return; /* exit from elmhes_ */
    }

    for (m = *low; m < la; ++m) {
        mm1 = m-1;
        x = 0.;
        i = m;
        for (j = m; j < *igh; ++j) {
            if (abs(a[j + mm1 * *nm]) > abs(x)) {
                x = a[j + mm1 * *nm];
                i = j;
            }
        }

        int_[m] = i+1;
        if (i == m) {
            goto L130;
        }
/*     .......... interchange rows and columns of a .......... */
        for (j = mm1; j < *n; ++j) {
            y = a[i + j * *nm];
            a[i + j * *nm] = a[m + j * *nm];
            a[m + j * *nm] = y;
        }

        for (j = 0; j < *igh; ++j) {
            y = a[j + i * *nm];
            a[j + i * *nm] = a[j + m * *nm];
            a[j + m * *nm] = y;
        }
/*     .......... end interchange .......... */
L130:
        if (x != 0.)
        for (i = m+1; i < *igh; ++i) {
            y = a[i + mm1 * *nm];
            if (y == 0.) {
                continue;
            }
            y /= x;
            a[i + mm1 * *nm] = y;

            for (j = m; j < *n; ++j) {
                a[i + j * *nm] -= y * a[m + j * *nm];
            }

            for (j = 0; j < *igh; ++j) {
                a[j + m * *nm] += y * a[j + i * *nm];
            }
        }
    }
} /* elmhes_ */

/* Subroutine */
static void eltran_(nm, n, low, igh, a, int_, z)
integer *nm, *n, *low, *igh;
doublereal *a;
integer *int_;
doublereal *z;
{
    /* Local variables */
    static integer i, j, kl, mp;

/*     this subroutine is a translation of the algol procedure elmtrans, */
/*     num. math. 16, 181-204(1970) by peters and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971). */

/*     this subroutine accumulates the stabilized elementary */
/*     similarity transformations used in the reduction of a */
/*     real general matrix to upper hessenberg form by  elmhes. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        low and igh are integers determined by the balancing */
/*          subroutine  balanc.  if  balanc  has not been used, */
/*          set low=1, igh=n. */

/*        a contains the multipliers which were used in the */
/*          reduction by  elmhes  in its lower triangle */
/*          below the subdiagonal. */

/*        int contains information on the rows and columns */
/*          interchanged in the reduction by  elmhes. */
/*          only elements low through igh are used. */

/*     on output */

/*        z contains the transformation matrix produced in the */
/*          reduction by  elmhes. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*     .......... initialize z to identity matrix .......... */

    for (j = 0; j < *n; ++j) {
        for (i = 0; i < *n; ++i) {
            z[i + j * *nm] = 0.;
        }

        z[j + j * *nm] = 1.;
    }

    kl = *igh - *low - 1;
    if (kl < 1) {
        return;
    }
/*     .......... for mp=igh-1 step -1 until low+1 do -- .......... */
    for (mp = *igh - 2; mp > *igh -kl-2; --mp) {
        for (i = mp+1; i < *igh; ++i) {
            z[i + mp * *nm] = a[i + (mp - 1) * *nm];
        }

        i = int_[mp] - 1;
        if (i == mp) {
            continue;
        }

        for (j = mp; j < *igh; ++j) {
            z[mp + j * *nm] = z[i + j * *nm];
            z[i + j * *nm] = 0.;
        }

        z[i + mp * *nm] = 1.;
    }
} /* eltran_ */

/* Subroutine */
static void hqr_(nm, n, low, igh, h, wr, wi, ierr)
integer *nm, *n, *low, *igh;
doublereal *h, *wr, *wi;
integer *ierr;
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal norm;
    static integer i, j, k, l, m;
    static doublereal p, q, r, s, t, w, x, y;
    static integer na, en;
    static doublereal zz;
    static logical notlas;
    static integer itn, its, enm2;
    static doublereal tst1, tst2;

/*  RESTORED CORRECT INDICES OF LOOPS (200,210,230,240). (9/29/89 BSG) */

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

    *ierr = 0;
    norm = 0.;
    k = 0;
/*     .......... store roots isolated by balanc */
/*                and compute matrix norm .......... */
    for (i = 0; i < *n; ++i) {
        for (j = k; j < *n; ++j) {
            norm += abs(h[i + j * *nm]);
        }
        k = i;
        if (i+1 < *low || i >= *igh) {
            wr[i] = h[i + i * *nm];
            wi[i] = 0.;
        }
    }

    en = *igh - 1;
    t = 0.;
    itn = *n * 30;
/*     .......... search for next eigenvalues .......... */
L60:
    if (en+1 < *low) {
        goto L1001;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... look for single small sub-diagonal element */
/*                for l=en step -1 until low do -- .......... */
L70:
    for (l = en; l+1 >= *low; --l) {
        s = abs(h[l-1 + (l-1) * *nm]) + abs(h[l + l * *nm]);
        if (s == 0.) {
            s = norm;
        }
        tst1 = s;
        tst2 = tst1 + abs(h[l + (l-1) * *nm]);
        if (tst2 == tst1) {
            break;
        }
    }
/*     .......... form shift .......... */
    x = h[en + en * *nm];
    if (l == en) {
        goto L270;
    }
    y = h[na + na * *nm];
    w = h[en + na * *nm] * h[na + en * *nm];
    if (l == na) {
        goto L280;
    }
    if (itn == 0) {
        goto L1000;
    }
    if (its != 10 && its != 20) {
        goto L130;
    }
/*     .......... form exceptional shift .......... */
    t += x;

    for (i = *low - 1; i <= en; ++i) {
        h[i + i * *nm] -= x;
    }

    s = abs(h[en + na * *nm]) + abs(h[na + enm2 * *nm]);
    x = s * .75;
    y = x;
    w = s * -.4375 * s;
L130:
    ++its;
    --itn;
/*     .......... look for two consecutive small */
/*                sub-diagonal elements. */
/*                for m=en-2 step -1 until l do -- .......... */
    for (m = enm2; m >= l; --m) {
        zz = h[m + m * *nm];
        r = x - zz;
        s = y - zz;
        p = (r * s - w) / h[m+1 + m * *nm] + h[m + (m+1) * *nm];
        q = h[m+1 + (m+1) * *nm] - zz - r - s;
        r = h[m+2 + (m+1) * *nm];
        s = abs(p) + abs(q) + abs(r);
        p /= s;
        q /= s;
        r /= s;
        if (m == l) {
            goto L150;
        }
        tst1 = abs(p) * (abs(h[m-1 + (m-1) * *nm]) + abs(zz) + abs(h[m+1 + (m+1) * *nm]));
        tst2 = tst1 + abs(h[m + (m-1) * *nm]) * (abs(q) + abs(r));
        if (tst2 == tst1) {
            goto L150;
        }
    }

L150:
    for (i = m+2; i <= en; ++i) {
        h[i + (i-2) * *nm] = 0.;
        if (i != m+2) {
            h[i + (i-3) * *nm] = 0.;
        }
    }
/*     .......... double qr step involving rows l to en and */
/*                columns m to en .......... */
    for (k = m; k <= na; ++k) {
        notlas = k != na;
        if (k == m) {
            goto L170;
        }
        p = h[k + (k-1) * *nm];
        q = h[k+1 + (k-1) * *nm];
        r = 0.;
        if (notlas) {
            r = h[k+2 + (k-1) * *nm];
        }
        x = abs(p) + abs(q) + abs(r);
        if (x == 0.) {
            continue;
        }
        p /= x;
        q /= x;
        r /= x;
L170:
        d__1 = sqrt(p * p + q * q + r * r);
        s = d_sign(&d__1, &p);
        if (k == m) {
            goto L180;
        }
        h[k + (k-1) * *nm] = -s * x;
        goto L190;
L180:
        if (l != m) {
            h[k + (k-1) * *nm] = -h[k + (k-1) * *nm];
        }
L190:
        p += s;
        x = p / s;
        y = q / s;
        zz = r / s;
        q /= p;
        r /= p;
        if (notlas) {
            goto L225;
        }
/*     .......... row modification .......... */
        for (j = k; j <= en; ++j) {
            p = h[k + j * *nm] + q * h[k+1 + j * *nm];
            h[k + j * *nm] -= p * x;
            h[k+1 + j * *nm] -= p * y;
        }

        j = min(en,k+3);
/*     .......... column modification .......... */
        for (i = l; i <= j; ++i) {
            p = x * h[i + k * *nm] + y * h[i + (k+1) * *nm];
            h[i + k * *nm] -= p;
            h[i + (k+1) * *nm] -= p * q;
        }
        continue;
L225:
/*     .......... row modification .......... */
        for (j = k; j <= en; ++j) {
            p = h[k + j * *nm] + q * h[k+1 + j * *nm] + r * h[k+2 + j * *nm];
            h[k + j * *nm] -= p * x;
            h[k+1 + j * *nm] -= p * y;
            h[k+2 + j * *nm] -= p * zz;
        }

        j = min(en,k+3);
/*     .......... column modification .......... */
        for (i = l; i <= j; ++i) {
            p = x * h[i + k * *nm] + y * h[i + (k+1) * *nm] + zz * h[i + (k+2) * *nm];
            h[i + k * *nm] -= p;
            h[i + (k+1) * *nm] -= p * q;
            h[i + (k+2) * *nm] -= p * r;
        }
    }

    goto L70;
/*     .......... one root found .......... */
L270:
    wr[en] = x + t;
    wi[en] = 0.;
    en = na;
    goto L60;
/*     .......... two roots found .......... */
L280:
    p = (y - x) / 2.;
    q = p * p + w;
    zz = sqrt(abs(q));
    x += t;
    if (q < 0.) {
        goto L320;
    }
/*     .......... real pair .......... */
    zz = p + d_sign(&zz, &p);
    wr[na] = x + zz;
    wr[en] = wr[na];
    if (zz != 0.) {
        wr[en] = x - w / zz;
    }
    wi[na] = 0.;
    wi[en] = 0.;
    goto L330;
/*     .......... complex pair .......... */
L320:
    wr[na] = x + p;
    wr[en] = x + p;
    wi[na] = zz;
    wi[en] = -zz;
L330:
    en = enm2;
    goto L60;
/*     .......... set error -- all eigenvalues have not */
/*                converged after 30*n iterations .......... */
L1000:
    *ierr = en-1;
L1001:
    return;
} /* hqr_ */

/* Subroutine */
static void hqr2_(nm, n, low, igh, h, wr, wi, z, ierr)
integer *nm, *n, *low, *igh;
doublereal *h, *wr, *wi, *z;
integer *ierr;
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    static doublereal norm;
    static integer i, j, k, l, m;
    static doublereal p, q, r, s, t, w, x, y;
    static integer na, en;
    static doublereal ra, sa;
    static doublereal vi, vr, zz;
    static logical notlas;
    static integer itn, its, enm2;
    static doublereal tst1, tst2;


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
/*          error exit is made, none of the eigenvectors has been found.  */

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the limit of 30*n iterations is exhausted */
/*                     while the j-th eigenvalue is being sought. */

/*     calls cdiv for complex division. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    *ierr = 0;
    norm = 0.;
    k = 0;
/*     .......... store roots isolated by balanc */
/*                and compute matrix norm .......... */
    for (i = 0; i < *n; ++i) {
        for (j = k; j < *n; ++j) {
            norm += abs(h[i + j * *nm]);
        }
        k = i;
        if (i+1 < *low || i >= *igh) {
            wr[i] = h[i + i * *nm];
            wi[i] = 0.;
        }
    }

    en = *igh - 1;
    t = 0.;
    itn = *n * 30;
/*     .......... search for next eigenvalues .......... */
L60:
    if (en+1 < *low) {
        goto L340;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... look for single small sub-diagonal element */
/*                for l=en step -1 until low do -- .......... */
L70:
    for (l = en; l+1 > *low; --l) {
        s = abs(h[l-1 + (l-1) * *nm]) + abs(h[l + l * *nm]);
        if (s == 0.) {
            s = norm;
        }
        tst1 = s;
        tst2 = tst1 + abs(h[l + (l-1) * *nm]);
        if (tst2 == tst1) {
            break;
        }
    }
/*     .......... form shift .......... */
    x = h[en + en * *nm];
    if (l == en) {
        goto L270;
    }
    y = h[na + na * *nm];
    w = h[en + na * *nm] * h[na + en * *nm];
    if (l == na) {
        goto L280;
    }
    if (itn == 0) {
        goto L1000;
    }
    if (its != 10 && its != 20) {
        goto L130;
    }
/*     .......... form exceptional shift .......... */
    t += x;

    for (i = *low - 1; i <= en; ++i) {
        h[i + i * *nm] -= x;
    }

    s = abs(h[en + na * *nm]) + abs(h[na + enm2 * *nm]);
    x = s * .75;
    y = x;
    w = s * -.4375 * s;
L130:
    ++its;
    --itn;
/*     .......... look for two consecutive small */
/*                sub-diagonal elements. */
/*                for m=en-2 step -1 until l do -- .......... */
    for (m = enm2; m >= l; --m) {
        zz = h[m + m * *nm];
        r = x - zz;
        s = y - zz;
        p = (r * s - w) / h[m+1 + m * *nm] + h[m + (m+1) * *nm];
        q = h[m+1 + (m+1) * *nm] - zz - r - s;
        r = h[m+2 + (m+1) * *nm];
        s = abs(p) + abs(q) + abs(r);
        p /= s;
        q /= s;
        r /= s;
        if (m == l) {
            goto L150;
        }
        tst1 = abs(p) * (abs(h[m-1 + (m-1) * *nm]) + abs(zz) + abs(h[m+1 + (m+1) * *nm]));
        tst2 = tst1 + abs(h[m + (m-1) * *nm]) * (abs(q) + abs(r));
        if (tst2 == tst1) {
            goto L150;
        }
    }

L150:
    for (i = m+2; i <= en; ++i) {
        h[i + (i-2) * *nm] = 0.;
        if (i != m+2) {
            h[i + (i-3) * *nm] = 0.;
        }
    }
/*     .......... double qr step involving rows l to en and */
/*                columns m to en .......... */
    for (k = m; k <= na; ++k) {
        notlas = k != na;
        if (k == m) {
            goto L170;
        }
        p = h[k + (k-1) * *nm];
        q = h[k+1 + (k-1) * *nm];
        r = 0.;
        if (notlas) {
            r = h[k+2 + (k-1) * *nm];
        }
        x = abs(p) + abs(q) + abs(r);
        if (x == 0.) {
            continue;
        }
        p /= x;
        q /= x;
        r /= x;
L170:
        d__1 = sqrt(p * p + q * q + r * r);
        s = d_sign(&d__1, &p);
        if (k == m) {
            goto L180;
        }
        h[k + (k-1) * *nm] = -s * x;
        goto L190;
L180:
        if (l != m) {
            h[k + (k-1) * *nm] = -h[k + (k-1) * *nm];
        }
L190:
        p += s;
        x = p / s;
        y = q / s;
        zz = r / s;
        q /= p;
        r /= p;
        if (notlas) {
            goto L225;
        }
/*     .......... row modification .......... */
        for (j = k; j < *n; ++j) {
            p = h[k + j * *nm] + q * h[k+1 + j * *nm];
            h[k + j * *nm] -= p * x;
            h[k+1 + j * *nm] -= p * y;
        }

        j = min(en,k+3);
/*     .......... column modification .......... */
        for (i = 0; i <= j; ++i) {
            p = x * h[i + k * *nm] + y * h[i + (k+1) * *nm];
            h[i + k * *nm] -= p;
            h[i + (k+1) * *nm] -= p * q;
        }
/*     .......... accumulate transformations .......... */
        for (i = *low - 1; i < *igh; ++i) {
            p = x * z[i + k * *nm] + y * z[i + (k+1) * *nm];
            z[i + k * *nm] -= p;
            z[i + (k+1) * *nm] -= p * q;
        }
        continue;
L225:
/*     .......... row modification .......... */
        for (j = k; j < *n; ++j) {
            p = h[k + j * *nm] + q * h[k+1 + j * *nm] + r * h[k+2 + j * *nm];
            h[k + j * *nm] -= p * x;
            h[k+1 + j * *nm] -= p * y;
            h[k+2 + j * *nm] -= p * zz;
        }

        j = min(en,k+3);
/*     .......... column modification .......... */
        for (i = 0; i <= j; ++i) {
            p = x * h[i + k * *nm] + y * h[i + (k+1) * *nm] + zz * h[i + (k+2) * *nm];
            h[i + k * *nm] -= p;
            h[i + (k+1) * *nm] -= p * q;
            h[i + (k+2) * *nm] -= p * r;
        }
/*     .......... accumulate transformations .......... */
        for (i = *low - 1; i < *igh; ++i) {
            p = x * z[i + k * *nm] + y * z[i + (k+1) * *nm] + zz * z[i + (k+2) * *nm];
            z[i + k * *nm] -= p;
            z[i + (k+1) * *nm] -= p * q;
            z[i + (k+2) * *nm] -= p * r;
        }
    }

    goto L70;
/*     .......... one root found .......... */
L270:
    h[en + en * *nm] = x + t;
    wr[en] = h[en + en * *nm];
    wi[en] = 0.;
    en = na;
    goto L60;
/*     .......... two roots found .......... */
L280:
    p = (y - x) / 2.;
    q = p * p + w;
    zz = sqrt(abs(q));
    h[en + en * *nm] = x + t;
    x = h[en + en * *nm];
    h[na + na * *nm] = y + t;
    if (q < 0.) {
        goto L320;
    }
/*     .......... real pair .......... */
    zz = p + d_sign(&zz, &p);
    wr[na] = x + zz;
    wr[en] = wr[na];
    if (zz != 0.) {
        wr[en] = x - w / zz;
    }
    wi[na] = 0.;
    wi[en] = 0.;
    x = h[en + na * *nm];
    s = abs(x) + abs(zz);
    p = x / s;
    q = zz / s;
    r = sqrt(p * p + q * q);
    p /= r;
    q /= r;
/*     .......... row modification .......... */
    for (j = na; j < *n; ++j) {
        zz = h[na + j * *nm];
        h[na + j * *nm] = q * zz + p * h[en + j * *nm];
        h[en + j * *nm] = q * h[en + j * *nm] - p * zz;
    }
/*     .......... column modification .......... */
    for (i = 0; i <= en; ++i) {
        zz = h[i + na * *nm];
        h[i + na * *nm] = q * zz + p * h[i + en * *nm];
        h[i + en * *nm] = q * h[i + en * *nm] - p * zz;
    }
/*     .......... accumulate transformations .......... */
    for (i = *low - 1; i < *igh; ++i) {
        zz = z[i + na * *nm];
        z[i + na * *nm] = q * zz + p * z[i + en * *nm];
        z[i + en * *nm] = q * z[i + en * *nm] - p * zz;
    }

    goto L330;
/*     .......... complex pair .......... */
L320:
    wr[na] = x + p;
    wr[en] = x + p;
    wi[na] = zz;
    wi[en] = -zz;
L330:
    en = enm2;
    goto L60;
/*     .......... all roots found.  backsubstitute to find */
/*                vectors of upper triangular form .......... */
L340:
    if (norm == 0.) {
        goto L1001;
    }
/*     .......... for en=n step -1 until 1 do -- .......... */
    for (en = *n - 1; en >= 0; --en) {
        p = wr[en]; q = wi[en];
        na = en - 1;
        if (q < 0.) {
            goto L710;
        } else if (q != 0) {
            continue ;
        }
/*     .......... real vector .......... */
        m = en;
        h[en + en * *nm] = 1.;
        if (en == 0) {
            continue ;
        }
/*     .......... for i=en-1 step -1 until 1 do -- .......... */
        for (i = na; i >= 0; --i) {
            w = h[i + i * *nm] - p;
            r = 0.;

            for (j = m; j <= en; ++j) {
                r += h[i + j * *nm] * h[j + en * *nm];
            }

            if (wi[i] >= 0.) {
                goto L630;
            }
            zz = w;
            s = r;
            continue;
L630:
            m = i;
            if (wi[i] != 0.) {
                goto L640;
            }
            t = w;
            if (t != 0.) {
                goto L635;
            }
            tst1 = norm;
            t = tst1;
L632:
            t *= .01;
            tst2 = norm + t;
            if (tst2 > tst1) {
                goto L632;
            }
L635:
            h[i + en * *nm] = -r / t;
            goto L680;
/*     .......... solve real equations .......... */
L640:
            x = h[i + (i+1) * *nm];
            y = h[i+1 + i * *nm];
            q = (wr[i] - p) * (wr[i] - p) + wi[i] * wi[i];
            t = (x * s - zz * r) / q;
            h[i + en * *nm] = t;
            if (abs(x) <= abs(zz)) {
                goto L650;
            }
            h[i+1 + en * *nm] = (-r - w * t) / x;
            goto L680;
L650:
            h[i+1 + en * *nm] = (-s - y * t) / zz;

/*     .......... overflow control .......... */
L680:
            t = abs(h[i + en * *nm]);
            if (t == 0.) {
                continue;
            }
            tst1 = t;
            tst2 = tst1 + 1. / tst1;
            if (tst2 > tst1) {
                continue;
            }
            for (j = i; j <= en; ++j) {
                h[j + en * *nm] /= t;
            }
        }
/*     .......... end real vector .......... */
        continue;
/*     .......... complex vector .......... */
L710:
        m = na;
/*     .......... last vector component chosen imaginary so that */
/*                eigenvector matrix is triangular .......... */
        if (abs(h[en + na * *nm]) <= abs(h[na + en * *nm])) {
            goto L720;
        }
        h[na + na * *nm] = q / h[en + na * *nm];
        h[na + en * *nm] = -(h[en + en * *nm] - p) / h[en + na * *nm];
        goto L730;
L720:
        d__1 = -h[na + en * *nm];
        d__2 = h[na + na * *nm] - p;
        cdiv_(&c_b130, &d__1, &d__2, &q, &h[na + na * *nm], &h[na + en * *nm]);
L730:
        h[en + na * *nm] = 0.;
        h[en + en * *nm] = 1.;
        enm2 = na - 1;
/*     .......... for i=en-2 step -1 until 1 do -- .......... */
        if (na != 0)
        for (i = enm2; i >= 0; --i) {
            w = h[i + i * *nm] - p;
            ra = 0.;
            sa = 0.;

            for (j = m; j <= en; ++j) {
                ra += h[i + j * *nm] * h[j + na * *nm];
                sa += h[i + j * *nm] * h[j + en * *nm];
            }

            if (wi[i] >= 0.) {
                goto L770;
            }
            zz = w;
            r = ra;
            s = sa;
            continue;
L770:
            m = i;
            if (wi[i] != 0.) {
                goto L780;
            }
            d__1 = -ra;
            d__2 = -sa;
            cdiv_(&d__1, &d__2, &w, &q, &h[i + na * *nm], &h[i + en * *nm]);
            goto L790;
/*     .......... solve complex equations .......... */
L780:
            x = h[i + (i+1) * *nm];
            y = h[i+1 + i * *nm];
            vr = (wr[i] - p) * (wr[i] - p) + wi[i] * wi[i] - q * q;
            vi = (wr[i] - p) * 2. * q;
            if (vr != 0. || vi != 0.) {
                goto L784;
            }
            tst1 = norm * (abs(w) + abs(q) + abs(x) + abs(y) + abs(zz));
            vr = tst1;
L783:
            vr *= .01;
            tst2 = tst1 + vr;
            if (tst2 > tst1) {
                goto L783;
            }
L784:
            d__1 = x * r - zz * ra + q * sa;
            d__2 = x * s - zz * sa - q * ra;
            cdiv_(&d__1, &d__2, &vr, &vi, &h[i + na * *nm], &h[i + en * *nm]);
            if (abs(x) <= abs(zz) + abs(q)) {
                goto L785;
            }
            h[i+1 + na * *nm] = (-ra - w * h[i + na * *nm] + q * h[i + en * *nm]) / x;
            h[i+1 + en * *nm] = (-sa - w * h[i + en * *nm] - q * h[i + na * *nm]) / x;
            goto L790;
L785:
            d__1 = -r - y * h[i + na * *nm];
            d__2 = -s - y * h[i + en * *nm];
            cdiv_(&d__1, &d__2, &zz, &q, &h[i+1 + na * *nm], &h[i+1 + en * *nm]);

/*     .......... overflow control .......... */
L790:
            d__1 = abs(h[i + na * *nm]), d__2 = abs(h[i + en * *nm]);
            t = max(d__1,d__2);
            if (t == 0.) {
                continue;
            }
            tst1 = t;
            tst2 = tst1 + 1. / tst1;
            if (tst2 <= tst1)
            for (j = i; j <= en; ++j) {
                h[j + na * *nm] /= t;
                h[j + en * *nm] /= t;
            }
        }
/*     .......... end complex vector .......... */
    }
/*     .......... end back substitution. */
/*                vectors of isolated roots .......... */
    for (i = 0; i < *n; ++i) {
        if (i+1 < *low || i >= *igh)
        for (j = i; j < *n; ++j) {
            z[i + j * *nm] = h[i + j * *nm];
        }
    }
/*     .......... multiply by transformation matrix to give */
/*                vectors of original full matrix. */
/*                for j=n step -1 until low do -- .......... */
    for (j = *n - 1; j+1 >= *low; --j) {
        m = min(j,*igh-1);

        for (i = *low - 1; i < *igh; ++i) {
            zz = 0.;

            for (k = *low - 1; k <= m; ++k) {
                zz += z[i + k * *nm] * h[k + j * *nm];
            }

            z[i + j * *nm] = zz;
        }
    }

    goto L1001;
/*     .......... set error -- all eigenvalues have not */
/*                converged after 30*n iterations .......... */
L1000:
    *ierr = en-1;
L1001:
    return;
} /* hqr2_ */

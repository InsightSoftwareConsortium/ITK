/* rg.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b130 = 0.;

/* ====================================================================== */
/* NIST Guide to Available Math Software. */
/* Fullsource for module RG from package EISPACK. */
/* Retrieved from NETLIB on Thu Jan 23 06:12:53 1997. */
/* ====================================================================== */
/* Subroutine */ int rg_(nm, n, a, wr, wi, matz, z, iv1, fv1, ierr)
integer *nm, *n;
doublereal *a, *wr, *wi;
integer *matz;
doublereal *z;
integer *iv1;
doublereal *fv1;
integer *ierr;
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int balbak_(), balanc_(), elmhes_(), eltran_();
    static integer is1, is2;
    extern /* Subroutine */ int hqr_(), hqr2_();



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
/*     mathematics and computer science div, argonne national laboratory
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------
*/

    /* Parameter adjustments */
    --fv1;
    --iv1;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --wi;
    --wr;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (*n <= *nm) {
        goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    balanc_(nm, n, &a[a_offset], &is1, &is2, &fv1[1]);
    elmhes_(nm, n, &is1, &is2, &a[a_offset], &iv1[1]);
    if (*matz != 0) {
        goto L20;
    }
/*     .......... find eigenvalues only .......... */
    hqr_(nm, n, &is1, &is2, &a[a_offset], &wr[1], &wi[1], ierr);
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
L20:
    eltran_(nm, n, &is1, &is2, &a[a_offset], &iv1[1], &z[z_offset]);
    hqr2_(nm, n, &is1, &is2, &a[a_offset], &wr[1], &wi[1], &z[z_offset], ierr)
            ;
    if (*ierr != 0) {
        goto L50;
    }
    balbak_(nm, n, &is1, &is2, &fv1[1], n, &z[z_offset]);
L50:
    return 0;
} /* rg_ */

/* Subroutine */ int balanc_(nm, n, a, low, igh, scale)
integer *nm, *n;
doublereal *a;
integer *low, *igh;
doublereal *scale;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    doublereal d__1;

    /* Local variables */
    static integer iexc;
    static doublereal c, f, g;
    static integer i, j, k, l, m;
    static doublereal r, s, radix, b2;
    static integer jj;
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
/*     mathematics and computer science div, argonne national laboratory
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------
*/

    /* Parameter adjustments */
    --scale;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    radix = 16.;

    b2 = radix * radix;
    k = 1;
    l = *n;
    goto L100;
/*     .......... in-line procedure for row and */
/*                column exchange .......... */
L20:
    scale[m] = (doublereal) j;
    if (j == m) {
        goto L50;
    }

    i__1 = l;
    for (i = 1; i <= i__1; ++i) {
        f = a[i + j * a_dim1];
        a[i + j * a_dim1] = a[i + m * a_dim1];
        a[i + m * a_dim1] = f;
/* L30: */
    }

    i__1 = *n;
    for (i = k; i <= i__1; ++i) {
        f = a[j + i * a_dim1];
        a[j + i * a_dim1] = a[m + i * a_dim1];
        a[m + i * a_dim1] = f;
/* L40: */
    }

L50:
    switch ((int)iexc) {
        case 1:  goto L80;
        case 2:  goto L130;
    }
/*     .......... search for rows isolating an eigenvalue */
/*                and push them down .......... */
L80:
    if (l == 1) {
        goto L280;
    }
    --l;
/*     .......... for j=l step -1 until 1 do -- .......... */
L100:
    i__1 = l;
    for (jj = 1; jj <= i__1; ++jj) {
        j = l + 1 - jj;

        i__2 = l;
        for (i = 1; i <= i__2; ++i) {
            if (i == j) {
                goto L110;
            }
            if (a[j + i * a_dim1] != 0.) {
                goto L120;
            }
L110:
            ;
        }

        m = l;
        iexc = 1;
        goto L20;
L120:
        ;
    }

    goto L140;
/*     .......... search for columns isolating an eigenvalue */
/*                and push them left .......... */
L130:
    ++k;

L140:
    i__1 = l;
    for (j = k; j <= i__1; ++j) {

        i__2 = l;
        for (i = k; i <= i__2; ++i) {
            if (i == j) {
                goto L150;
            }
            if (a[i + j * a_dim1] != 0.) {
                goto L170;
            }
L150:
            ;
        }

        m = k;
        iexc = 2;
        goto L20;
L170:
        ;
    }
/*     .......... now balance the submatrix in rows k to l .......... */
    i__1 = l;
    for (i = k; i <= i__1; ++i) {
/* L180: */
        scale[i] = 1.;
    }
/*     .......... iterative loop for norm reduction .......... */
L190:
    noconv = FALSE_;

    i__1 = l;
    for (i = k; i <= i__1; ++i) {
        c = 0.;
        r = 0.;

        i__2 = l;
        for (j = k; j <= i__2; ++j) {
            if (j == i) {
                goto L200;
            }
            c += (d__1 = a[j + i * a_dim1], abs(d__1));
            r += (d__1 = a[i + j * a_dim1], abs(d__1));
L200:
            ;
        }
/*     .......... guard against zero c or r due to underflow .........
. */
        if (c == 0. || r == 0.) {
            goto L270;
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
            goto L270;
        }
        g = 1. / f;
        scale[i] *= f;
        noconv = TRUE_;

        i__2 = *n;
        for (j = k; j <= i__2; ++j) {
/* L250: */
            a[i + j * a_dim1] *= g;
        }

        i__2 = l;
        for (j = 1; j <= i__2; ++j) {
/* L260: */
            a[j + i * a_dim1] *= f;
        }

L270:
        ;
    }

    if (noconv) {
        goto L190;
    }

L280:
    *low = k;
    *igh = l;
    return 0;
} /* balanc_ */

/* Subroutine */ int balbak_(nm, n, low, igh, scale, m, z)
integer *nm, *n, *low, *igh;
doublereal *scale;
integer *m;
doublereal *z;
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2;

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
/*     mathematics and computer science div, argonne national laboratory
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------
*/

    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --scale;

    /* Function Body */
    if (*m == 0) {
        goto L200;
    }
    if (*igh == *low) {
        goto L120;
    }

    i__1 = *igh;
    for (i = *low; i <= i__1; ++i) {
        s = scale[i];
/*     .......... left hand eigenvectors are back transformed */
/*                if the foregoing statement is replaced by */
/*                s=1.0d0/scale(i). .......... */
        i__2 = *m;
        for (j = 1; j <= i__2; ++j) {
/* L100: */
            z[i + j * z_dim1] *= s;
        }

/* L110: */
    }
/*     ......... for i=low-1 step -1 until 1, */
/*               igh+1 step 1 until n do -- .......... */
L120:
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
        i = ii;
        if (i >= *low && i <= *igh) {
            goto L140;
        }
        if (i < *low) {
            i = *low - ii;
        }
        k = (integer) scale[i];
        if (k == i) {
            goto L140;
        }

        i__2 = *m;
        for (j = 1; j <= i__2; ++j) {
            s = z[i + j * z_dim1];
            z[i + j * z_dim1] = z[k + j * z_dim1];
            z[k + j * z_dim1] = s;
/* L130: */
        }

L140:
        ;
    }

L200:
    return 0;
} /* balbak_ */

/* Subroutine */ int cdiv_(ar, ai, br, bi, cr, ci)
doublereal *ar, *ai, *br, *bi, *cr, *ci;
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    static doublereal s, ais, bis, ars, brs;


/*     complex division, (cr,ci) = (ar,ai)/(br,bi) */

    s = abs(*br) + abs(*bi);
    ars = *ar / s;
    ais = *ai / s;
    brs = *br / s;
    bis = *bi / s;
/* Computing 2nd power */
    d__1 = brs;
/* Computing 2nd power */
    d__2 = bis;
    s = d__1 * d__1 + d__2 * d__2;
    *cr = (ars * brs + ais * bis) / s;
    *ci = (ais * brs - ars * bis) / s;
    return 0;
} /* cdiv_ */

/* Subroutine */ int elmhes_(nm, n, low, igh, a, int_)
integer *nm, *n, *low, *igh;
doublereal *a;
integer *int_;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Local variables */
    static integer i, j, m;
    static doublereal x, y;
    static integer la, mm1, kp1, mp1;



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
/*     mathematics and computer science div, argonne national laboratory
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------
*/

    /* Parameter adjustments */
    --int_;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
        goto L200;
    }

    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
        mm1 = m - 1;
        x = 0.;
        i = m;

        i__2 = *igh;
        for (j = m; j <= i__2; ++j) {
            if ((d__1 = a[j + mm1 * a_dim1], abs(d__1)) <= abs(x)) {
                goto L100;
            }
            x = a[j + mm1 * a_dim1];
            i = j;
L100:
            ;
        }

        int_[m] = i;
        if (i == m) {
            goto L130;
        }
/*     .......... interchange rows and columns of a .......... */
        i__2 = *n;
        for (j = mm1; j <= i__2; ++j) {
            y = a[i + j * a_dim1];
            a[i + j * a_dim1] = a[m + j * a_dim1];
            a[m + j * a_dim1] = y;
/* L110: */
        }

        i__2 = *igh;
        for (j = 1; j <= i__2; ++j) {
            y = a[j + i * a_dim1];
            a[j + i * a_dim1] = a[j + m * a_dim1];
            a[j + m * a_dim1] = y;
/* L120: */
        }
/*     .......... end interchange .......... */
L130:
        if (x == 0.) {
            goto L180;
        }
        mp1 = m + 1;

        i__2 = *igh;
        for (i = mp1; i <= i__2; ++i) {
            y = a[i + mm1 * a_dim1];
            if (y == 0.) {
                goto L160;
            }
            y /= x;
            a[i + mm1 * a_dim1] = y;

            i__3 = *n;
            for (j = m; j <= i__3; ++j) {
/* L140: */
                a[i + j * a_dim1] -= y * a[m + j * a_dim1];
            }

            i__3 = *igh;
            for (j = 1; j <= i__3; ++j) {
/* L150: */
                a[j + m * a_dim1] += y * a[j + i * a_dim1];
            }

L160:
            ;
        }

L180:
        ;
    }

L200:
    return 0;
} /* elmhes_ */

/* Subroutine */ int eltran_(nm, n, low, igh, a, int_, z)
integer *nm, *n, *low, *igh;
doublereal *a;
integer *int_;
doublereal *z;
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2;

    /* Local variables */
    static integer i, j, kl, mm, mp, mp1;



/*     this subroutine is a translation of the algol procedure elmtrans,
*/
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
/*     mathematics and computer science div, argonne national laboratory
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------
*/

/*     .......... initialize z to identity matrix .......... */
    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --int_;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

        i__2 = *n;
        for (i = 1; i <= i__2; ++i) {
/* L60: */
            z[i + j * z_dim1] = 0.;
        }

        z[j + j * z_dim1] = 1.;
/* L80: */
    }

    kl = *igh - *low - 1;
    if (kl < 1) {
        goto L200;
    }
/*     .......... for mp=igh-1 step -1 until low+1 do -- .......... */
    i__1 = kl;
    for (mm = 1; mm <= i__1; ++mm) {
        mp = *igh - mm;
        mp1 = mp + 1;

        i__2 = *igh;
        for (i = mp1; i <= i__2; ++i) {
/* L100: */
            z[i + mp * z_dim1] = a[i + (mp - 1) * a_dim1];
        }

        i = int_[mp];
        if (i == mp) {
            goto L140;
        }

        i__2 = *igh;
        for (j = mp; j <= i__2; ++j) {
            z[mp + j * z_dim1] = z[i + j * z_dim1];
            z[i + j * z_dim1] = 0.;
/* L130: */
        }

        z[i + mp * z_dim1] = 1.;
L140:
        ;
    }

L200:
    return 0;
} /* eltran_ */

/* Subroutine */ int hqr_(nm, n, low, igh, h, wr, wi, ierr)
integer *nm, *n, *low, *igh;
doublereal *h, *wr, *wi;
integer *ierr;
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(), d_sign();

    /* Local variables */
    static doublereal norm;
    static integer i, j, k, l, m;
    static doublereal p, q, r, s, t, w, x, y;
    static integer na, en, ll, mm;
    static doublereal zz;
    static logical notlas;
    static integer mp2, itn, its, enm2;
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
/*     mathematics and computer science div, argonne national laboratory
*/

/*     this version dated september 1989. */

/*     ------------------------------------------------------------------
*/

    /* Parameter adjustments */
    --wi;
    --wr;
    h_dim1 = *nm;
    h_offset = h_dim1 + 1;
    h -= h_offset;

    /* Function Body */
    *ierr = 0;
    norm = 0.;
    k = 1;
/*     .......... store roots isolated by balanc */
/*                and compute matrix norm .......... */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {

        i__2 = *n;
        for (j = k; j <= i__2; ++j) {
/* L40: */
            norm += (d__1 = h[i + j * h_dim1], abs(d__1));
        }

        k = i;
        if (i >= *low && i <= *igh) {
            goto L50;
        }
        wr[i] = h[i + i * h_dim1];
        wi[i] = 0.;
L50:
        ;
    }

    en = *igh;
    t = 0.;
    itn = *n * 30;
/*     .......... search for next eigenvalues .......... */
L60:
    if (en < *low) {
        goto L1001;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... look for single small sub-diagonal element */
/*                for l=en step -1 until low do -- .......... */
L70:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
        l = en + *low - ll;
        if (l == *low) {
            goto L100;
        }
        s = (d__1 = h[l - 1 + (l - 1) * h_dim1], abs(d__1)) + (d__2 = h[l + l
                * h_dim1], abs(d__2));
        if (s == 0.) {
            s = norm;
        }
        tst1 = s;
        tst2 = tst1 + (d__1 = h[l + (l - 1) * h_dim1], abs(d__1));
        if (tst2 == tst1) {
            goto L100;
        }
/* L80: */
    }
/*     .......... form shift .......... */
L100:
    x = h[en + en * h_dim1];
    if (l == en) {
        goto L270;
    }
    y = h[na + na * h_dim1];
    w = h[en + na * h_dim1] * h[na + en * h_dim1];
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

    i__1 = en;
    for (i = *low; i <= i__1; ++i) {
/* L120: */
        h[i + i * h_dim1] -= x;
    }

    s = (d__1 = h[en + na * h_dim1], abs(d__1)) + (d__2 = h[na + enm2 *
            h_dim1], abs(d__2));
    x = s * .75;
    y = x;
    w = s * -.4375 * s;
L130:
    ++its;
    --itn;
/*     .......... look for two consecutive small */
/*                sub-diagonal elements. */
/*                for m=en-2 step -1 until l do -- .......... */
    i__1 = enm2;
    for (mm = l; mm <= i__1; ++mm) {
        m = enm2 + l - mm;
        zz = h[m + m * h_dim1];
        r = x - zz;
        s = y - zz;
        p = (r * s - w) / h[m + 1 + m * h_dim1] + h[m + (m + 1) * h_dim1];
        q = h[m + 1 + (m + 1) * h_dim1] - zz - r - s;
        r = h[m + 2 + (m + 1) * h_dim1];
        s = abs(p) + abs(q) + abs(r);
        p /= s;
        q /= s;
        r /= s;
        if (m == l) {
            goto L150;
        }
        tst1 = abs(p) * ((d__1 = h[m - 1 + (m - 1) * h_dim1], abs(d__1)) +
                abs(zz) + (d__2 = h[m + 1 + (m + 1) * h_dim1], abs(d__2)));
        tst2 = tst1 + (d__1 = h[m + (m - 1) * h_dim1], abs(d__1)) * (abs(q) +
                abs(r));
        if (tst2 == tst1) {
            goto L150;
        }
/* L140: */
    }

L150:
    mp2 = m + 2;

    i__1 = en;
    for (i = mp2; i <= i__1; ++i) {
        h[i + (i - 2) * h_dim1] = 0.;
        if (i == mp2) {
            goto L160;
        }
        h[i + (i - 3) * h_dim1] = 0.;
L160:
        ;
    }
/*     .......... double qr step involving rows l to en and */
/*                columns m to en .......... */
    i__1 = na;
    for (k = m; k <= i__1; ++k) {
        notlas = k != na;
        if (k == m) {
            goto L170;
        }
        p = h[k + (k - 1) * h_dim1];
        q = h[k + 1 + (k - 1) * h_dim1];
        r = 0.;
        if (notlas) {
            r = h[k + 2 + (k - 1) * h_dim1];
        }
        x = abs(p) + abs(q) + abs(r);
        if (x == 0.) {
            goto L260;
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
        h[k + (k - 1) * h_dim1] = -s * x;
        goto L190;
L180:
        if (l != m) {
            h[k + (k - 1) * h_dim1] = -h[k + (k - 1) * h_dim1];
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
        i__2 = en;
        for (j = k; j <= i__2; ++j) {
            p = h[k + j * h_dim1] + q * h[k + 1 + j * h_dim1];
            h[k + j * h_dim1] -= p * x;
            h[k + 1 + j * h_dim1] -= p * y;
/* L200: */
        }

/* Computing MIN */
        i__2 = en, i__3 = k + 3;
        j = min(i__2,i__3);
/*     .......... column modification .......... */
        i__2 = j;
        for (i = l; i <= i__2; ++i) {
            p = x * h[i + k * h_dim1] + y * h[i + (k + 1) * h_dim1];
            h[i + k * h_dim1] -= p;
            h[i + (k + 1) * h_dim1] -= p * q;
/* L210: */
        }
        goto L255;
L225:
/*     .......... row modification .......... */
        i__2 = en;
        for (j = k; j <= i__2; ++j) {
            p = h[k + j * h_dim1] + q * h[k + 1 + j * h_dim1] + r * h[k + 2 +
                    j * h_dim1];
            h[k + j * h_dim1] -= p * x;
            h[k + 1 + j * h_dim1] -= p * y;
            h[k + 2 + j * h_dim1] -= p * zz;
/* L230: */
        }

/* Computing MIN */
        i__2 = en, i__3 = k + 3;
        j = min(i__2,i__3);
/*     .......... column modification .......... */
        i__2 = j;
        for (i = l; i <= i__2; ++i) {
            p = x * h[i + k * h_dim1] + y * h[i + (k + 1) * h_dim1] + zz * h[
                    i + (k + 2) * h_dim1];
            h[i + k * h_dim1] -= p;
            h[i + (k + 1) * h_dim1] -= p * q;
            h[i + (k + 2) * h_dim1] -= p * r;
/* L240: */
        }
L255:

L260:
        ;
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
    zz = sqrt((abs(q)));
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
    *ierr = en;
L1001:
    return 0;
} /* hqr_ */

/* Subroutine */ int hqr2_(nm, n, low, igh, h, wr, wi, z, ierr)
integer *nm, *n, *low, *igh;
doublereal *h, *wr, *wi, *z;
integer *ierr;
{
    /* System generated locals */
    integer h_dim1, h_offset, z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(), d_sign();

    /* Local variables */
    extern /* Subroutine */ int cdiv_();
    static doublereal norm;
    static integer i, j, k, l, m;
    static doublereal p, q, r, s, t, w, x, y;
    static integer na, ii, en, jj;
    static doublereal ra, sa;
    static integer ll, mm, nn;
    static doublereal vi, vr, zz;
    static logical notlas;
    static integer mp2, itn, its, enm2;
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
/*          contains its eigenvector.  if the i-th eigenvalue is complex
*/
/*          with positive imaginary part, the i-th and (i+1)-th */
/*          columns of z contain the real and imaginary parts of its */
/*          eigenvector.  the eigenvectors are unnormalized.  if an */
/*          error exit is made, none of the eigenvectors has been found.
*/

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the limit of 30*n iterations is exhausted */
/*                     while the j-th eigenvalue is being sought. */

/*     calls cdiv for complex division. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------
*/

    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --wi;
    --wr;
    h_dim1 = *nm;
    h_offset = h_dim1 + 1;
    h -= h_offset;

    /* Function Body */
    *ierr = 0;
    norm = 0.;
    k = 1;
/*     .......... store roots isolated by balanc */
/*                and compute matrix norm .......... */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {

        i__2 = *n;
        for (j = k; j <= i__2; ++j) {
/* L40: */
            norm += (d__1 = h[i + j * h_dim1], abs(d__1));
        }

        k = i;
        if (i >= *low && i <= *igh) {
            goto L50;
        }
        wr[i] = h[i + i * h_dim1];
        wi[i] = 0.;
L50:
        ;
    }

    en = *igh;
    t = 0.;
    itn = *n * 30;
/*     .......... search for next eigenvalues .......... */
L60:
    if (en < *low) {
        goto L340;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... look for single small sub-diagonal element */
/*                for l=en step -1 until low do -- .......... */
L70:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
        l = en + *low - ll;
        if (l == *low) {
            goto L100;
        }
        s = (d__1 = h[l - 1 + (l - 1) * h_dim1], abs(d__1)) + (d__2 = h[l + l
                * h_dim1], abs(d__2));
        if (s == 0.) {
            s = norm;
        }
        tst1 = s;
        tst2 = tst1 + (d__1 = h[l + (l - 1) * h_dim1], abs(d__1));
        if (tst2 == tst1) {
            goto L100;
        }
/* L80: */
    }
/*     .......... form shift .......... */
L100:
    x = h[en + en * h_dim1];
    if (l == en) {
        goto L270;
    }
    y = h[na + na * h_dim1];
    w = h[en + na * h_dim1] * h[na + en * h_dim1];
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

    i__1 = en;
    for (i = *low; i <= i__1; ++i) {
/* L120: */
        h[i + i * h_dim1] -= x;
    }

    s = (d__1 = h[en + na * h_dim1], abs(d__1)) + (d__2 = h[na + enm2 *
            h_dim1], abs(d__2));
    x = s * .75;
    y = x;
    w = s * -.4375 * s;
L130:
    ++its;
    --itn;
/*     .......... look for two consecutive small */
/*                sub-diagonal elements. */
/*                for m=en-2 step -1 until l do -- .......... */
    i__1 = enm2;
    for (mm = l; mm <= i__1; ++mm) {
        m = enm2 + l - mm;
        zz = h[m + m * h_dim1];
        r = x - zz;
        s = y - zz;
        p = (r * s - w) / h[m + 1 + m * h_dim1] + h[m + (m + 1) * h_dim1];
        q = h[m + 1 + (m + 1) * h_dim1] - zz - r - s;
        r = h[m + 2 + (m + 1) * h_dim1];
        s = abs(p) + abs(q) + abs(r);
        p /= s;
        q /= s;
        r /= s;
        if (m == l) {
            goto L150;
        }
        tst1 = abs(p) * ((d__1 = h[m - 1 + (m - 1) * h_dim1], abs(d__1)) +
                abs(zz) + (d__2 = h[m + 1 + (m + 1) * h_dim1], abs(d__2)));
        tst2 = tst1 + (d__1 = h[m + (m - 1) * h_dim1], abs(d__1)) * (abs(q) +
                abs(r));
        if (tst2 == tst1) {
            goto L150;
        }
/* L140: */
    }

L150:
    mp2 = m + 2;

    i__1 = en;
    for (i = mp2; i <= i__1; ++i) {
        h[i + (i - 2) * h_dim1] = 0.;
        if (i == mp2) {
            goto L160;
        }
        h[i + (i - 3) * h_dim1] = 0.;
L160:
        ;
    }
/*     .......... double qr step involving rows l to en and */
/*                columns m to en .......... */
    i__1 = na;
    for (k = m; k <= i__1; ++k) {
        notlas = k != na;
        if (k == m) {
            goto L170;
        }
        p = h[k + (k - 1) * h_dim1];
        q = h[k + 1 + (k - 1) * h_dim1];
        r = 0.;
        if (notlas) {
            r = h[k + 2 + (k - 1) * h_dim1];
        }
        x = abs(p) + abs(q) + abs(r);
        if (x == 0.) {
            goto L260;
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
        h[k + (k - 1) * h_dim1] = -s * x;
        goto L190;
L180:
        if (l != m) {
            h[k + (k - 1) * h_dim1] = -h[k + (k - 1) * h_dim1];
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
        i__2 = *n;
        for (j = k; j <= i__2; ++j) {
            p = h[k + j * h_dim1] + q * h[k + 1 + j * h_dim1];
            h[k + j * h_dim1] -= p * x;
            h[k + 1 + j * h_dim1] -= p * y;
/* L200: */
        }

/* Computing MIN */
        i__2 = en, i__3 = k + 3;
        j = min(i__2,i__3);
/*     .......... column modification .......... */
        i__2 = j;
        for (i = 1; i <= i__2; ++i) {
            p = x * h[i + k * h_dim1] + y * h[i + (k + 1) * h_dim1];
            h[i + k * h_dim1] -= p;
            h[i + (k + 1) * h_dim1] -= p * q;
/* L210: */
        }
/*     .......... accumulate transformations .......... */
        i__2 = *igh;
        for (i = *low; i <= i__2; ++i) {
            p = x * z[i + k * z_dim1] + y * z[i + (k + 1) * z_dim1];
            z[i + k * z_dim1] -= p;
            z[i + (k + 1) * z_dim1] -= p * q;
/* L220: */
        }
        goto L255;
L225:
/*     .......... row modification .......... */
        i__2 = *n;
        for (j = k; j <= i__2; ++j) {
            p = h[k + j * h_dim1] + q * h[k + 1 + j * h_dim1] + r * h[k + 2 +
                    j * h_dim1];
            h[k + j * h_dim1] -= p * x;
            h[k + 1 + j * h_dim1] -= p * y;
            h[k + 2 + j * h_dim1] -= p * zz;
/* L230: */
        }

/* Computing MIN */
        i__2 = en, i__3 = k + 3;
        j = min(i__2,i__3);
/*     .......... column modification .......... */
        i__2 = j;
        for (i = 1; i <= i__2; ++i) {
            p = x * h[i + k * h_dim1] + y * h[i + (k + 1) * h_dim1] + zz * h[
                    i + (k + 2) * h_dim1];
            h[i + k * h_dim1] -= p;
            h[i + (k + 1) * h_dim1] -= p * q;
            h[i + (k + 2) * h_dim1] -= p * r;
/* L240: */
        }
/*     .......... accumulate transformations .......... */
        i__2 = *igh;
        for (i = *low; i <= i__2; ++i) {
            p = x * z[i + k * z_dim1] + y * z[i + (k + 1) * z_dim1] + zz * z[
                    i + (k + 2) * z_dim1];
            z[i + k * z_dim1] -= p;
            z[i + (k + 1) * z_dim1] -= p * q;
            z[i + (k + 2) * z_dim1] -= p * r;
/* L250: */
        }
L255:

L260:
        ;
    }

    goto L70;
/*     .......... one root found .......... */
L270:
    h[en + en * h_dim1] = x + t;
    wr[en] = h[en + en * h_dim1];
    wi[en] = 0.;
    en = na;
    goto L60;
/*     .......... two roots found .......... */
L280:
    p = (y - x) / 2.;
    q = p * p + w;
    zz = sqrt((abs(q)));
    h[en + en * h_dim1] = x + t;
    x = h[en + en * h_dim1];
    h[na + na * h_dim1] = y + t;
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
    x = h[en + na * h_dim1];
    s = abs(x) + abs(zz);
    p = x / s;
    q = zz / s;
    r = sqrt(p * p + q * q);
    p /= r;
    q /= r;
/*     .......... row modification .......... */
    i__1 = *n;
    for (j = na; j <= i__1; ++j) {
        zz = h[na + j * h_dim1];
        h[na + j * h_dim1] = q * zz + p * h[en + j * h_dim1];
        h[en + j * h_dim1] = q * h[en + j * h_dim1] - p * zz;
/* L290: */
    }
/*     .......... column modification .......... */
    i__1 = en;
    for (i = 1; i <= i__1; ++i) {
        zz = h[i + na * h_dim1];
        h[i + na * h_dim1] = q * zz + p * h[i + en * h_dim1];
        h[i + en * h_dim1] = q * h[i + en * h_dim1] - p * zz;
/* L300: */
    }
/*     .......... accumulate transformations .......... */
    i__1 = *igh;
    for (i = *low; i <= i__1; ++i) {
        zz = z[i + na * z_dim1];
        z[i + na * z_dim1] = q * zz + p * z[i + en * z_dim1];
        z[i + en * z_dim1] = q * z[i + en * z_dim1] - p * zz;
/* L310: */
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
    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {
        en = *n + 1 - nn;
        p = wr[en];
        q = wi[en];
        na = en - 1;
        if (q < 0.) {
            goto L710;
        } else if (q == 0) {
            goto L600;
        } else {
            goto L800;
        }
/*     .......... real vector .......... */
L600:
        m = en;
        h[en + en * h_dim1] = 1.;
        if (na == 0) {
            goto L800;
        }
/*     .......... for i=en-1 step -1 until 1 do -- .......... */
        i__2 = na;
        for (ii = 1; ii <= i__2; ++ii) {
            i = en - ii;
            w = h[i + i * h_dim1] - p;
            r = 0.;

            i__3 = en;
            for (j = m; j <= i__3; ++j) {
/* L610: */
                r += h[i + j * h_dim1] * h[j + en * h_dim1];
            }

            if (wi[i] >= 0.) {
                goto L630;
            }
            zz = w;
            s = r;
            goto L700;
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
            h[i + en * h_dim1] = -r / t;
            goto L680;
/*     .......... solve real equations .......... */
L640:
            x = h[i + (i + 1) * h_dim1];
            y = h[i + 1 + i * h_dim1];
            q = (wr[i] - p) * (wr[i] - p) + wi[i] * wi[i];
            t = (x * s - zz * r) / q;
            h[i + en * h_dim1] = t;
            if (abs(x) <= abs(zz)) {
                goto L650;
            }
            h[i + 1 + en * h_dim1] = (-r - w * t) / x;
            goto L680;
L650:
            h[i + 1 + en * h_dim1] = (-s - y * t) / zz;

/*     .......... overflow control .......... */
L680:
            t = (d__1 = h[i + en * h_dim1], abs(d__1));
            if (t == 0.) {
                goto L700;
            }
            tst1 = t;
            tst2 = tst1 + 1. / tst1;
            if (tst2 > tst1) {
                goto L700;
            }
            i__3 = en;
            for (j = i; j <= i__3; ++j) {
                h[j + en * h_dim1] /= t;
/* L690: */
            }

L700:
            ;
        }
/*     .......... end real vector .......... */
        goto L800;
/*     .......... complex vector .......... */
L710:
        m = na;
/*     .......... last vector component chosen imaginary so that */
/*                eigenvector matrix is triangular .......... */
        if ((d__1 = h[en + na * h_dim1], abs(d__1)) <= (d__2 = h[na + en *
                h_dim1], abs(d__2))) {
            goto L720;
        }
        h[na + na * h_dim1] = q / h[en + na * h_dim1];
        h[na + en * h_dim1] = -(h[en + en * h_dim1] - p) / h[en + na * h_dim1]
                ;
        goto L730;
L720:
        d__1 = -h[na + en * h_dim1];
        d__2 = h[na + na * h_dim1] - p;
        cdiv_(&c_b130, &d__1, &d__2, &q, &h[na + na * h_dim1], &h[na + en *
                h_dim1]);
L730:
        h[en + na * h_dim1] = 0.;
        h[en + en * h_dim1] = 1.;
        enm2 = na - 1;
        if (enm2 == 0) {
            goto L800;
        }
/*     .......... for i=en-2 step -1 until 1 do -- .......... */
        i__2 = enm2;
        for (ii = 1; ii <= i__2; ++ii) {
            i = na - ii;
            w = h[i + i * h_dim1] - p;
            ra = 0.;
            sa = 0.;

            i__3 = en;
            for (j = m; j <= i__3; ++j) {
                ra += h[i + j * h_dim1] * h[j + na * h_dim1];
                sa += h[i + j * h_dim1] * h[j + en * h_dim1];
/* L760: */
            }

            if (wi[i] >= 0.) {
                goto L770;
            }
            zz = w;
            r = ra;
            s = sa;
            goto L795;
L770:
            m = i;
            if (wi[i] != 0.) {
                goto L780;
            }
            d__1 = -ra;
            d__2 = -sa;
            cdiv_(&d__1, &d__2, &w, &q, &h[i + na * h_dim1], &h[i + en *
                    h_dim1]);
            goto L790;
/*     .......... solve complex equations .......... */
L780:
            x = h[i + (i + 1) * h_dim1];
            y = h[i + 1 + i * h_dim1];
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
            cdiv_(&d__1, &d__2, &vr, &vi, &h[i + na * h_dim1], &h[i + en *
                    h_dim1]);
            if (abs(x) <= abs(zz) + abs(q)) {
                goto L785;
            }
            h[i + 1 + na * h_dim1] = (-ra - w * h[i + na * h_dim1] + q * h[i
                    + en * h_dim1]) / x;
            h[i + 1 + en * h_dim1] = (-sa - w * h[i + en * h_dim1] - q * h[i
                    + na * h_dim1]) / x;
            goto L790;
L785:
            d__1 = -r - y * h[i + na * h_dim1];
            d__2 = -s - y * h[i + en * h_dim1];
            cdiv_(&d__1, &d__2, &zz, &q, &h[i + 1 + na * h_dim1], &h[i + 1 +
                    en * h_dim1]);

/*     .......... overflow control .......... */
L790:
/* Computing MAX */
            d__3 = (d__1 = h[i + na * h_dim1], abs(d__1)), d__4 = (d__2 = h[i
                    + en * h_dim1], abs(d__2));
            t = max(d__3,d__4);
            if (t == 0.) {
                goto L795;
            }
            tst1 = t;
            tst2 = tst1 + 1. / tst1;
            if (tst2 > tst1) {
                goto L795;
            }
            i__3 = en;
            for (j = i; j <= i__3; ++j) {
                h[j + na * h_dim1] /= t;
                h[j + en * h_dim1] /= t;
/* L792: */
            }

L795:
            ;
        }
/*     .......... end complex vector .......... */
L800:
        ;
    }
/*     .......... end back substitution. */
/*                vectors of isolated roots .......... */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        if (i >= *low && i <= *igh) {
            goto L840;
        }

        i__2 = *n;
        for (j = i; j <= i__2; ++j) {
/* L820: */
            z[i + j * z_dim1] = h[i + j * h_dim1];
        }

L840:
        ;
    }
/*     .......... multiply by transformation matrix to give */
/*                vectors of original full matrix. */
/*                for j=n step -1 until low do -- .......... */
    i__1 = *n;
    for (jj = *low; jj <= i__1; ++jj) {
        j = *n + *low - jj;
        m = min(j,*igh);

        i__2 = *igh;
        for (i = *low; i <= i__2; ++i) {
            zz = 0.;

            i__3 = m;
            for (k = *low; k <= i__3; ++k) {
/* L860: */
                zz += z[i + k * z_dim1] * h[k + j * h_dim1];
            }

            z[i + j * z_dim1] = zz;
/* L880: */
        }
    }

    goto L1001;
/*     .......... set error -- all eigenvalues have not */
/*                converged after 30*n iterations .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
} /* hqr2_ */


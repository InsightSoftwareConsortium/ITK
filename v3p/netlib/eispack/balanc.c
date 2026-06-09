/* eispack/balanc.f -- translated by f2c (version 20050501).
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

/*<       subroutine balanc(nm,n,a,low,igh,scale) >*/
/* Subroutine */ int balanc_(integer *nm, integer *n, doublereal *a, integer *
        low, integer *igh, doublereal *scale)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    doublereal d__1;

    /* Local variables */
    doublereal c__, f, g;
    integer i__, j, k, l, m;
    doublereal r__, s, b2;
    integer jj, iexc;
    doublereal radix;
    logical noconv;


/*<       integer i,j,k,l,m,n,jj,nm,igh,low,iexc >*/
/*<       double precision a(nm,n),scale(n) >*/
/*<       double precision c,f,g,r,s,b2,radix >*/
/*<       logical noconv >*/

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

/*<       radix = 16.0d0 >*/
    /* Parameter adjustments */
    --scale;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    radix = 16.;

/*<       b2 = radix * radix >*/
    b2 = radix * radix;
/*<       k = 1 >*/
    k = 1;
/*<       l = n >*/
    l = *n;
/*<       go to 100 >*/
    goto L100;
/*     .......... in-line procedure for row and */
/*                column exchange .......... */
/*<    20 scale(m) = j >*/
L20:
    scale[m] = (doublereal) j;
/*<       if (j .eq. m) go to 50 >*/
    if (j == m) {
        goto L50;
    }

/*<       do 30 i = 1, l >*/
    i__1 = l;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          f = a(i,j) >*/
        f = a[i__ + j * a_dim1];
/*<          a(i,j) = a(i,m) >*/
        a[i__ + j * a_dim1] = a[i__ + m * a_dim1];
/*<          a(i,m) = f >*/
        a[i__ + m * a_dim1] = f;
/*<    30 continue >*/
/* L30: */
    }

/*<       do 40 i = k, n >*/
    i__1 = *n;
    for (i__ = k; i__ <= i__1; ++i__) {
/*<          f = a(j,i) >*/
        f = a[j + i__ * a_dim1];
/*<          a(j,i) = a(m,i) >*/
        a[j + i__ * a_dim1] = a[m + i__ * a_dim1];
/*<          a(m,i) = f >*/
        a[m + i__ * a_dim1] = f;
/*<    40 continue >*/
/* L40: */
    }

/*<    50 go to (80,130), iexc >*/
L50:
    switch (iexc) {
        case 1:  goto L80;
        case 2:  goto L130;
    }
/*     .......... search for rows isolating an eigenvalue */
/*                and push them down .......... */
/*<    80 if (l .eq. 1) go to 280 >*/
L80:
    if (l == 1) {
        goto L280;
    }
/*<       l = l - 1 >*/
    --l;
/*     .......... for j=l step -1 until 1 do -- .......... */
/*<   100 do 120 jj = 1, l >*/
L100:
    i__1 = l;
    for (jj = 1; jj <= i__1; ++jj) {
/*<          j = l + 1 - jj >*/
        j = l + 1 - jj;

/*<          do 110 i = 1, l >*/
        i__2 = l;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             if (i .eq. j) go to 110 >*/
            if (i__ == j) {
                goto L110;
            }
/*<             if (a(j,i) .ne. 0.0d0) go to 120 >*/
            if (a[j + i__ * a_dim1] != 0.) {
                goto L120;
            }
/*<   110    continue >*/
L110:
            ;
        }

/*<          m = l >*/
        m = l;
/*<          iexc = 1 >*/
        iexc = 1;
/*<          go to 20 >*/
        goto L20;
/*<   120 continue >*/
L120:
        ;
    }

/*<       go to 140 >*/
    goto L140;
/*     .......... search for columns isolating an eigenvalue */
/*                and push them left .......... */
/*<   130 k = k + 1 >*/
L130:
    ++k;

/*<   140 do 170 j = k, l >*/
L140:
    i__1 = l;
    for (j = k; j <= i__1; ++j) {

/*<          do 150 i = k, l >*/
        i__2 = l;
        for (i__ = k; i__ <= i__2; ++i__) {
/*<             if (i .eq. j) go to 150 >*/
            if (i__ == j) {
                goto L150;
            }
/*<             if (a(i,j) .ne. 0.0d0) go to 170 >*/
            if (a[i__ + j * a_dim1] != 0.) {
                goto L170;
            }
/*<   150    continue >*/
L150:
            ;
        }

/*<          m = k >*/
        m = k;
/*<          iexc = 2 >*/
        iexc = 2;
/*<          go to 20 >*/
        goto L20;
/*<   170 continue >*/
L170:
        ;
    }
/*     .......... now balance the submatrix in rows k to l .......... */
/*<       do 180 i = k, l >*/
    i__1 = l;
    for (i__ = k; i__ <= i__1; ++i__) {
/*<   180 scale(i) = 1.0d0 >*/
/* L180: */
        scale[i__] = 1.;
    }
/*     .......... iterative loop for norm reduction .......... */
/*<   190 noconv = .false. >*/
L190:
    noconv = FALSE_;

/*<       do 270 i = k, l >*/
    i__1 = l;
    for (i__ = k; i__ <= i__1; ++i__) {
/*<          c = 0.0d0 >*/
        c__ = 0.;
/*<          r = 0.0d0 >*/
        r__ = 0.;

/*<          do 200 j = k, l >*/
        i__2 = l;
        for (j = k; j <= i__2; ++j) {
/*<             if (j .eq. i) go to 200 >*/
            if (j == i__) {
                goto L200;
            }
/*<             c = c + dabs(a(j,i)) >*/
            c__ += (d__1 = a[j + i__ * a_dim1], abs(d__1));
/*<             r = r + dabs(a(i,j)) >*/
            r__ += (d__1 = a[i__ + j * a_dim1], abs(d__1));
/*<   200    continue >*/
L200:
            ;
        }
/*     .......... guard against zero c or r due to underflow .......... */
/*<          if (c .eq. 0.0d0 .or. r .eq. 0.0d0) go to 270 >*/
        if (c__ == 0. || r__ == 0.) {
            goto L270;
        }
/*<          g = r / radix >*/
        g = r__ / radix;
/*<          f = 1.0d0 >*/
        f = 1.;
/*<          s = c + r >*/
        s = c__ + r__;
/*<   210    if (c .ge. g) go to 220 >*/
L210:
        if (c__ >= g) {
            goto L220;
        }
/*<          f = f * radix >*/
        f *= radix;
/*<          c = c * b2 >*/
        c__ *= b2;
/*<          go to 210 >*/
        goto L210;
/*<   220    g = r * radix >*/
L220:
        g = r__ * radix;
/*<   230    if (c .lt. g) go to 240 >*/
L230:
        if (c__ < g) {
            goto L240;
        }
/*<          f = f / radix >*/
        f /= radix;
/*<          c = c / b2 >*/
        c__ /= b2;
/*<          go to 230 >*/
        goto L230;
/*     .......... now balance .......... */
/*<   240    if ((c + r) / f .ge. 0.95d0 * s) go to 270 >*/
L240:
        if ((c__ + r__) / f >= s * .95) {
            goto L270;
        }
/*<          g = 1.0d0 / f >*/
        g = 1. / f;
/*<          scale(i) = scale(i) * f >*/
        scale[i__] *= f;
/*<          noconv = .true. >*/
        noconv = TRUE_;

/*<          do 250 j = k, n >*/
        i__2 = *n;
        for (j = k; j <= i__2; ++j) {
/*<   250    a(i,j) = a(i,j) * g >*/
/* L250: */
            a[i__ + j * a_dim1] *= g;
        }

/*<          do 260 j = 1, l >*/
        i__2 = l;
        for (j = 1; j <= i__2; ++j) {
/*<   260    a(j,i) = a(j,i) * f >*/
/* L260: */
            a[j + i__ * a_dim1] *= f;
        }

/*<   270 continue >*/
L270:
        ;
    }

/*<       if (noconv) go to 190 >*/
    if (noconv) {
        goto L190;
    }

/*<   280 low = k >*/
L280:
    *low = k;
/*<       igh = l >*/
    *igh = l;
/*<       return >*/
    return 0;
/*<       end >*/
} /* balanc_ */

#ifdef __cplusplus
        }
#endif

/* eisrsg.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b17 = 1.;

/* ====================================================================== */
/* NIST Guide to Available Math Software. */
/* Fullsource for module RSG from package EISPACK. */
/* Retrieved from NETLIB on Thu Aug 29 08:25:55 1996. */
/* ====================================================================== */
/* Subroutine */ int rsg_(nm, n, a, b, w, matz, z, fv1, fv2, ierr)
integer *nm, *n;
doublereal *a, *b, *w;
integer *matz;
doublereal *z, *fv1, *fv2;
integer *ierr;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int tred1_(), tred2_(), rebak_(), reduc_(),
            tqlrat_(), tql2_();



/*     this subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (eispack) */
/*     to find the eigenvalues and eigenvectors (if desired) */
/*     for the real symmetric generalized eigenproblem  ax = (lambda)bx.
*/

/*     on input */

/*        nm  must be set to the row dimension of the two-dimensional */
/*        array parameters as declared in the calling program */
/*        dimension statement. */

/*        n  is the order of the matrices  a  and  b. */

/*        a  contains a real symmetric matrix. */

/*        b  contains a positive definite real symmetric matrix. */

/*        matz  is an integer variable set equal to zero if */
/*        only eigenvalues are desired.  otherwise it is set to */
/*        any non-zero integer for both eigenvalues and eigenvectors. */

/*     on output */

/*        w  contains the eigenvalues in ascending order. */

/*        z  contains the eigenvectors if matz is not zero. */

/*        ierr  is an integer output variable set equal to an error */
/*           completion code described in the documentation for tqlrat */
/*           and tql2.  the normal completion code is zero. */

/*        fv1  and  fv2  are temporary storage arrays. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------
*/

    /* Parameter adjustments */
    --fv2;
    --fv1;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --w;
    b_dim1 = *nm;
    b_offset = b_dim1 + 1;
    b -= b_offset;
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
    reduc_(nm, n, &a[a_offset], &b[b_offset], &fv2[1], ierr);
    if (*ierr != 0) {
        goto L50;
    }
    if (*matz != 0) {
        goto L20;
    }
/*     .......... find eigenvalues only .......... */
    tred1_(nm, n, &a[a_offset], &w[1], &fv1[1], &fv2[1]);
    tqlrat_(n, &w[1], &fv2[1], ierr);
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
L20:
    tred2_(nm, n, &a[a_offset], &w[1], &fv1[1], &z[z_offset]);
    tql2_(nm, n, &w[1], &fv1[1], &z[z_offset], ierr);
    if (*ierr != 0) {
        goto L50;
    }
    rebak_(nm, n, &b[b_offset], &fv2[1], n, &z[z_offset]);
L50:
    return 0;
} /* rsg_ */

doublereal epslon_(x)
doublereal *x;
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Local variables */
    static doublereal a, b, c, eps;


/*     estimate unit roundoff in quantities of size x. */


/*     this program should function properly on all systems */
/*     satisfying the following two assumptions, */
/*        1.  the base used in representing floating point */
/*            numbers is not a power of three. */
/*        2.  the quantity  a  in statement 10 is represented to */
/*            the accuracy used in floating point variables */
/*            that are stored in memory. */
/*     the statement number 10 and the go to 10 are intended to */
/*     force optimizing compilers to generate code satisfying */
/*     assumption 2. */
/*     under these assumptions, it should be true that, */
/*            a  is not exactly equal to four-thirds, */
/*            b  has a zero for its last bit or digit, */
/*            c  is not exactly equal to one, */
/*            eps  measures the separation of 1.0 from */
/*                 the next larger floating point number. */
/*     the developers of eispack would appreciate being informed */
/*     about any systems where these assumptions do not hold. */

/*     this version dated 4/6/83. */

    a = 1.3333333333333333;
L10:
    b = a - 1.;
    c = b + b + b;
    eps = (d__1 = c - 1., abs(d__1));
    if (eps == 0.) {
        goto L10;
    }
    ret_val = eps * abs(*x);
    return ret_val;
} /* epslon_ */

/* Subroutine */ int tqlrat_(n, d, e2, ierr)
integer *n;
doublereal *d, *e2;
integer *ierr;
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(), d_sign();

    /* Local variables */
    static doublereal b, c, f, g, h;
    static integer i, j, l, m;
    static doublereal p, r, s, t;
    static integer l1, ii;
    extern doublereal pythag_(), epslon_();
    static integer mml;



/*     this subroutine is a translation of the algol procedure tqlrat, */
/*     algorithm 464, comm. acm 16, 689(1973) by reinsch. */

/*     this subroutine finds the eigenvalues of a symmetric */
/*     tridiagonal matrix by the rational ql method. */

/*     on input */

/*        n is the order of the matrix. */

/*        d contains the diagonal elements of the input matrix. */

/*        e2 contains the squares of the subdiagonal elements of the */
/*          input matrix in its last n-1 positions.  e2(1) is arbitrary.
*/

/*      on output */

/*        d contains the eigenvalues in ascending order.  if an */
/*          error exit is made, the eigenvalues are correct and */
/*          ordered for indices 1,2,...ierr-1, but may not be */
/*          the smallest eigenvalues. */

/*        e2 has been destroyed. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the j-th eigenvalue has not been */
/*                     determined after 30 iterations. */

/*     calls pythag for  dsqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------
*/

    /* Parameter adjustments */
    --e2;
    --d;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
        goto L1001;
    }

    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
/* L100: */
        e2[i - 1] = e2[i];
    }

    f = 0.;
    t = 0.;
    e2[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
        j = 0;
        h = (d__1 = d[l], abs(d__1)) + sqrt(e2[l]);
        if (t > h) {
            goto L105;
        }
        t = h;
        b = epslon_(&t);
        c = b * b;
/*     .......... look for small squared sub-diagonal element ........
.. */
L105:
        i__2 = *n;
        for (m = l; m <= i__2; ++m) {
            if (e2[m] <= c) {
                goto L120;
            }
/*     .......... e2(n) is always zero, so there is no exit */
/*                through the bottom of the loop .......... */
/* L110: */
        }

L120:
        if (m == l) {
            goto L210;
        }
L130:
        if (j == 30) {
            goto L1000;
        }
        ++j;
/*     .......... form shift .......... */
        l1 = l + 1;
        s = sqrt(e2[l]);
        g = d[l];
        p = (d[l1] - g) / (s * 2.);
        r = pythag_(&p, &c_b17);
        d[l] = s / (p + d_sign(&r, &p));
        h = g - d[l];

        i__2 = *n;
        for (i = l1; i <= i__2; ++i) {
/* L140: */
            d[i] -= h;
        }

        f += h;
/*     .......... rational ql transformation .......... */
        g = d[m];
        if (g == 0.) {
            g = b;
        }
        h = g;
        s = 0.;
        mml = m - l;
/*     .......... for i=m-1 step -1 until l do -- .......... */
        i__2 = mml;
        for (ii = 1; ii <= i__2; ++ii) {
            i = m - ii;
            p = g * h;
            r = p + e2[i];
            e2[i + 1] = s * r;
            s = e2[i] / r;
            d[i + 1] = h + s * (h + d[i]);
            g = d[i] - e2[i] / g;
            if (g == 0.) {
                g = b;
            }
            h = g * p / r;
/* L200: */
        }

        e2[l] = s * g;
        d[l] = h;
/*     .......... guard against underflow in convergence test ........
.. */
        if (h == 0.) {
            goto L210;
        }
        if ((d__1 = e2[l], abs(d__1)) <= (d__2 = c / h, abs(d__2))) {
            goto L210;
        }
        e2[l] = h * e2[l];
        if (e2[l] != 0.) {
            goto L130;
        }
L210:
        p = d[l] + f;
/*     .......... order eigenvalues .......... */
        if (l == 1) {
            goto L250;
        }
/*     .......... for i=l step -1 until 2 do -- .......... */
        i__2 = l;
        for (ii = 2; ii <= i__2; ++ii) {
            i = l + 2 - ii;
            if (p >= d[i - 1]) {
                goto L270;
            }
            d[i] = d[i - 1];
/* L230: */
        }

L250:
        i = 1;
L270:
        d[i] = p;
/* L290: */
    }

    goto L1001;
/*     .......... set error -- no convergence to an */
/*                eigenvalue after 30 iterations .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* tqlrat_ */

/* Subroutine */ int rebak_(nm, n, b, dl, m, z)
integer *nm, *n;
doublereal *b, *dl;
integer *m;
doublereal *z;
{
    /* System generated locals */
    integer b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, i__3;

    /* Local variables */
    static integer i, j, k;
    static doublereal x;
    static integer i1, ii;



/*     this subroutine is a translation of the algol procedure rebaka, */
/*     num. math. 11, 99-110(1968) by martin and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 303-314(1971). */

/*     this subroutine forms the eigenvectors of a generalized */
/*     symmetric eigensystem by back transforming those of the */
/*     derived symmetric matrix determined by  reduc. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix system. */

/*        b contains information about the similarity transformation */
/*          (cholesky decomposition) used in the reduction by  reduc */
/*          in its strict lower triangle. */

/*        dl contains further information about the transformation. */

/*        m is the number of eigenvectors to be back transformed. */

/*        z contains the eigenvectors to be back transformed */
/*          in its first m columns. */

/*     on output */

/*        z contains the transformed eigenvectors */
/*          in its first m columns. */

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
    --dl;
    b_dim1 = *nm;
    b_offset = b_dim1 + 1;
    b -= b_offset;

    /* Function Body */
    if (*m == 0) {
        goto L200;
    }

    i__1 = *m;
    for (j = 1; j <= i__1; ++j) {
/*     .......... for i=n step -1 until 1 do -- .......... */
        i__2 = *n;
        for (ii = 1; ii <= i__2; ++ii) {
            i = *n + 1 - ii;
            i1 = i + 1;
            x = z[i + j * z_dim1];
            if (i == *n) {
                goto L80;
            }

            i__3 = *n;
            for (k = i1; k <= i__3; ++k) {
/* L60: */
                x -= b[k + i * b_dim1] * z[k + j * z_dim1];
            }

L80:
            z[i + j * z_dim1] = x / dl[i];
/* L100: */
        }
    }

L200:
    return 0;
} /* rebak_ */

/* Subroutine */ int reduc_(nm, n, a, b, dl, ierr)
integer *nm, *n;
doublereal *a, *b, *dl;
integer *ierr;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static integer i, j, k;
    static doublereal x, y;
    static integer i1, j1, nn;



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
/*          instead, the strict lower triangle of its cholesky factor l.
*/

/*        dl contains, if n is negative, the diagonal elements of l. */

/*     on output */

/*        a contains in its full lower triangle the full lower triangle */
/*          of the symmetric matrix derived from the reduction to the */
/*          standard form.  the strict upper triangle of a is unaltered.
*/

/*        b contains in its strict lower triangle the strict lower */
/*          triangle of its cholesky factor l.  the full upper */
/*          triangle of b is unaltered. */

/*        dl contains the diagonal elements of l. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          7*n+1      if b is not positive definite. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------
*/

    /* Parameter adjustments */
    --dl;
    b_dim1 = *nm;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    *ierr = 0;
    nn = abs(*n);
    if (*n < 0) {
        goto L100;
    }
/*     .......... form l in the arrays b and dl .......... */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        i1 = i - 1;

        i__2 = *n;
        for (j = i; j <= i__2; ++j) {
            x = b[i + j * b_dim1];
            if (i == 1) {
                goto L40;
            }

            i__3 = i1;
            for (k = 1; k <= i__3; ++k) {
/* L20: */
                x -= b[i + k * b_dim1] * b[j + k * b_dim1];
            }

L40:
            if (j != i) {
                goto L60;
            }
            if (x <= 0.) {
                goto L1000;
            }
            y = sqrt(x);
            dl[i] = y;
            goto L80;
L60:
            b[j + i * b_dim1] = x / y;
L80:
            ;
        }
    }
/*     .......... form the transpose of the upper triangle of inv(l)*a */
/*                in the lower triangle of the array a .......... */
L100:
    i__2 = nn;
    for (i = 1; i <= i__2; ++i) {
        i1 = i - 1;
        y = dl[i];

        i__1 = nn;
        for (j = i; j <= i__1; ++j) {
            x = a[i + j * a_dim1];
            if (i == 1) {
                goto L180;
            }

            i__3 = i1;
            for (k = 1; k <= i__3; ++k) {
/* L160: */
                x -= b[i + k * b_dim1] * a[j + k * a_dim1];
            }

L180:
            a[j + i * a_dim1] = x / y;
/* L200: */
        }
    }
/*     .......... pre-multiply by inv(l) and overwrite .......... */
    i__1 = nn;
    for (j = 1; j <= i__1; ++j) {
        j1 = j - 1;

        i__2 = nn;
        for (i = j; i <= i__2; ++i) {
            x = a[i + j * a_dim1];
            if (i == j) {
                goto L240;
            }
            i1 = i - 1;

            i__3 = i1;
            for (k = j; k <= i__3; ++k) {
/* L220: */
                x -= a[k + j * a_dim1] * b[i + k * b_dim1];
            }

L240:
            if (j == 1) {
                goto L280;
            }

            i__3 = j1;
            for (k = 1; k <= i__3; ++k) {
/* L260: */
                x -= a[j + k * a_dim1] * b[i + k * b_dim1];
            }

L280:
            a[i + j * a_dim1] = x / dl[i];
/* L300: */
        }
    }

    goto L1001;
/*     .......... set error -- b is not positive definite .......... */
L1000:
    *ierr = *n * 7 + 1;
L1001:
    return 0;
} /* reduc_ */


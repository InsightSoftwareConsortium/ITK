#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

static void rebak_(const integer *nm, const integer *n, const doublereal *b, const doublereal *dl, const integer *m, doublereal *z);
static void reduc_(const integer *nm, const integer *n, doublereal *a, doublereal *b, doublereal *dl, integer *ierr);
static void tqlrat_(const integer *n, doublereal *d, doublereal *e2, integer *ierr);
static doublereal epslon_(doublereal *x);

/* Table of constant values */
static doublereal c_b17 = 1.;

/* ====================================================================== */
/* NIST Guide to Available Math Software. */
/* Fullsource for module RSG from package EISPACK. */
/* Retrieved from NETLIB on Thu Aug 29 08:25:55 1996. */
/* ====================================================================== */
/* Subroutine */ void rsg_(nm, n, a, b, w, matz, z, fv1, fv2, ierr)
const integer *nm, *n;
doublereal *a, *b;
doublereal *w;
const integer *matz;
doublereal *z, *fv1, *fv2;
integer *ierr;
{
/*     this subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (eispack) */
/*     to find the eigenvalues and eigenvectors (if desired) */
/*     for the real symmetric generalized eigenproblem  ax = (lambda)bx.  */

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
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    if (*n > *nm) {
        *ierr = *n * 10;
        return;
    }
    reduc_(nm, n, a, b, fv2, ierr);
    if (*ierr != 0) {
        return;
    }
    if (*matz == 0) {
/*     .......... find eigenvalues only .......... */
        tred1_(nm, n, a, w, fv1, fv2);
        tqlrat_(n, w, fv2, ierr);
        return;
    }
/*     .......... find both eigenvalues and eigenvectors .......... */
    tred2_(nm, n, a, w, fv1, z);
    tql2_(nm, n, w, fv1, z, ierr);
    if (*ierr == 0)
        rebak_(nm, n, b, fv2, n, z);
} /* rsg_ */

static doublereal epslon_(x)
doublereal *x;
{
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
    do {
        b = a - 1.;
        c = b + b + b;
        eps = abs(c - 1.);
    } while (eps == 0.);
    return eps * abs(*x);
} /* epslon_ */

/* Subroutine */
static void tqlrat_(n, d, e2, ierr)
const integer *n;
doublereal *d, *e2;
integer *ierr;
{
    /* Local variables */
    static doublereal b, c, f, g, h;
    static integer i, j, l, m;
    static doublereal p, r, s, t;

/*     this subroutine is a translation of the algol procedure tqlrat, */
/*     algorithm 464, comm. acm 16, 689(1973) by reinsch. */

/*     this subroutine finds the eigenvalues of a symmetric */
/*     tridiagonal matrix by the rational ql method. */

/*     on input */

/*        n is the order of the matrix. */

/*        d contains the diagonal elements of the input matrix. */

/*        e2 contains the squares of the subdiagonal elements of the */
/*          input matrix in its last n-1 positions.  e2(1) is arbitrary.  */

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

/*     calls pythag for  sqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    *ierr = 0;
    if (*n == 1) {
        return;
    }

    for (i = 1; i < *n; ++i) {
        e2[i-1] = e2[i];
    }

    f = 0.;
    t = 0.;
    e2[*n-1] = 0.;

    for (l = 0; l < *n; ++l) {
        j = 0;
        h = abs(d[l]) + sqrt(e2[l]);
        if (t <= h) {
            t = h;
            b = epslon_(&t);
            c = b * b;
        }
/*     .......... look for small squared sub-diagonal element .......... */
        for (m = l; m < *n; ++m) {
            if (e2[m] <= c) {
                break;
            }
/*     .......... e2(n) is always zero, so there is no exit */
/*                through the bottom of the loop .......... */
        }

        if (m == l) {
            goto L210;
        }
L130:
        if (j == 30) {
/*     .......... set error -- no convergence to an */
/*                eigenvalue after 30 iterations .......... */
            *ierr = l+1;
            return;
        }
        ++j;
/*     .......... form shift .......... */
        s = sqrt(e2[l]);
        g = d[l];
        p = (d[l+1] - g) / (s * 2.);
        r = pythag_(&p, &c_b17);
        d[l] = s / (p + d_sign(&r, &p));
        h = g - d[l];

        for (i = l+1; i < *n; ++i) {
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
        for (i = m-1; i >= l; --i) {
            p = g * h;
            r = p + e2[i];
            e2[i+1] = s * r;
            s = e2[i] / r;
            d[i+1] = h + s * (h + d[i]);
            g = d[i] - e2[i] / g;
            if (g == 0.) {
                g = b;
            }
            h = g * p / r;
        }

        e2[l] = s * g;
        d[l] = h;
/*     .......... guard against underflow in convergence test ........ .. */
        if (h == 0.) {
            goto L210;
        }
        if (abs(e2[l]) <= abs(c / h)) {
            goto L210;
        }
        e2[l] *= h;
        if (e2[l] != 0.) {
            goto L130;
        }
L210:
        p = d[l] + f;
/*     .......... order eigenvalues .......... */
        for (i = l; i > 0; --i) {
            if (p >= d[i-1]) {
                break;
            }
            d[i] = d[i-1];
        }
        d[i] = p;
    }
} /* tqlrat_ */

/* Subroutine */
static void rebak_(nm, n, b, dl, m, z)
const integer *nm, *n;
const doublereal *b, *dl;
const integer *m;
doublereal *z;
{
    /* Local variables */
    static integer i, j, k;
    static doublereal x;

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
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    for (j = 0; j < *m; ++j) {
        for (i = *n-1; i >= 0; --i) {
            x = z[i + j * *nm];
            for (k = i+1; k < *n; ++k) {
                x -= b[k + i * *nm] * z[k + j * *nm];
            }
            z[i + j * *nm] = x / dl[i];
        }
    }
} /* rebak_ */

/* Subroutine */
static void reduc_(nm, n, a, b, dl, ierr)
const integer *nm, *n;
doublereal *a, *b;
doublereal *dl;
integer *ierr;
{
    /* Local variables */
    static integer i, j, k;
    static doublereal x, y;
    static integer nn;

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
/*          instead, the strict lower triangle of its cholesky factor l.  */

/*        dl contains, if n is negative, the diagonal elements of l. */

/*     on output */

/*        a contains in its full lower triangle the full lower triangle */
/*          of the symmetric matrix derived from the reduction to the */
/*          standard form.  the strict upper triangle of a is unaltered.  */

/*        b contains in its strict lower triangle the strict lower */
/*          triangle of its cholesky factor l.  the full upper */
/*          triangle of b is unaltered. */

/*        dl contains the diagonal elements of l. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          7*n+1      if b is not positive definite. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    *ierr = 0;
    nn = abs(*n);
/*     .......... form l in the arrays b and dl .......... */
    for (i = 0; i < *n; ++i) {
        for (j = i; j < *n; ++j) {
            x = b[i + j * *nm];
            for (k = 0; k < i; ++k) {
                x -= b[i + k * *nm] * b[j + k * *nm];
            }
            if (j != i) {
                b[j + i * *nm] = x / y;
                continue;
            }
            if (x <= 0.) {
/*     .......... set error -- b is not positive definite .......... */
                *ierr = *n * 7 + 1;
                return;
            }
            y = sqrt(x);
            dl[i] = y;
        }
    }
/*     .......... form the transpose of the upper triangle of inv(l)*a */
/*                in the lower triangle of the array a .......... */
    for (i = 0; i < nn; ++i) {
        y = dl[i];

        for (j = i; j < nn; ++j) {
            x = a[i + j * *nm];
            for (k = 0; k < i; ++k) {
                x -= b[i + k * *nm] * a[j + k * *nm];
            }
            a[j + i * *nm] = x / y;
        }
    }
/*     .......... pre-multiply by inv(l) and overwrite .......... */
    for (j = 0; j < nn; ++j) {
        for (i = j; i < nn; ++i) {
            x = a[i + j * *nm];
            for (k = j; k < i; ++k) {
                x -= a[k + j * *nm] * b[i + k * *nm];
            }
            for (k = 0; k < j; ++k) {
                x -= a[j + k * *nm] * b[i + k * *nm];
            }
            a[i + j * *nm] = x / dl[i];
        }
    }
} /* reduc_ */

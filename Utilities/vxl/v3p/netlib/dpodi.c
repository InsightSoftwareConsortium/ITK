#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__1 = 1;

/* Subroutine */ void dpodi_(a, lda, n, det, job)
doublereal *a;
const integer *lda, *n;
doublereal *det;
const integer *job;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    static integer i, j, k;
    static doublereal s, t;

/*     dpodi computes the determinant and inverse of a certain */
/*     double precision symmetric positive definite matrix (see below) */
/*     using the factors computed by dpoco, dpofa or dqrdc. */

/*     on entry */

/*        a       double precision(lda, n) */
/*                the output  a  from dpoco or dpofa */
/*                or the output  x  from dqrdc. */

/*        lda     integer */
/*                the leading dimension of the array  a . */

/*        n       integer */
/*                the order of the matrix  a . */

/*        job     integer */
/*                = 11   both determinant and inverse. */
/*                = 01   inverse only. */
/*                = 10   determinant only. */

/*     on return */

/*        a       if dpoco or dpofa was used to factor  a  then */
/*                dpodi produces the upper half of inverse(a) . */
/*                if dqrdc was used to decompose  x  then */
/*                dpodi produces the upper half of inverse(trans(x)*x) */
/*                where trans(x) is the transpose. */
/*                elements of  a  below the diagonal are unchanged. */
/*                if the units digit of job is zero,  a  is unchanged. */

/*        det     double precision(2) */
/*                determinant of  a  or of  trans(x)*x  if requested. */
/*                otherwise not referenced. */
/*                determinant = det(1) * 10.0**det(2) */
/*                with  1.0 .le. det(1) .lt. 10.0 */
/*                or  det(1) .eq. 0.0 . */

/*     error condition */

/*        a division by zero will occur if the input factor contains */
/*        a zero on the diagonal and the inverse is requested. */
/*        it will not occur if the subroutines are called correctly */
/*        and if dpoco or dpofa has set info .eq. 0 . */

/*     linpack.  this version dated 08/14/78 . */
/*     cleve moler, university of new mexico, argonne national lab. */

/*     subroutines and functions */

/*     blas daxpy,dscal */
/*     fortran mod */

/*     compute determinant */

    if (*job / 10 == 0) {
        goto L70;
    }

    det[0] = 1.;
    det[1] = 0.;
    s = 10.;
    for (i = 0; i < *n; ++i) {
        d__1 = a[i + i * *lda];
        det[0] *= d__1 * d__1;
        if (det[0] == 0.) {
            break;
        }
        while (det[0] < 1.) {
            det[0] *= s;
            det[1] += -1.;
        }
        while (det[0] >= s) {
            det[0] /= s;
            det[1] += 1.;
        }
    }

/*     compute inverse(r) */

L70:
    if (*job % 10 == 0) {
        return;
    }
    for (k = 0; k < *n; ++k) {
        a[k + k * *lda] = 1. / a[k + k * *lda];
        t = -a[k + k * *lda];
        dscal_(&k, &t, &a[k * *lda], &c__1);
        for (j = k+1; j < *n; ++j) {
            t = a[k + j * *lda];
            a[k + j * *lda] = 0.;
            i__1 = k+1;
            daxpy_(&i__1, &t, &a[k * *lda], &c__1, &a[j * *lda], &c__1);
        }
    }

/*        form  inverse(r) * trans(inverse(r)) */

    for (j = 0; j < *n; ++j) {
        for (k = 0; k < j; ++k) {
            t = a[k + j * *lda];
            i__1 = k+1;
            daxpy_(&i__1, &t, &a[j * *lda], &c__1, &a[k * *lda], &c__1);
        }
        t = a[j + j * *lda];
        i__1 = j+1;
        dscal_(&i__1, &t, &a[j * *lda], &c__1);
    }
} /* dpodi_ */

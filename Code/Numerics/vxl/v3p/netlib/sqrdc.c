/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int sqrdc_(x, ldx, n, p, qraux, jpvt, work, job)
real *x;
integer *ldx, *n, *p;
real *qraux;
integer *jpvt;
real *work;
integer *job;
{
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double r_sign(), sqrt();

    /* Local variables */
    static logical negj;
    static integer maxj;
    extern doublereal sdot_(), snrm2_();
    static integer j, l;
    static real t;
    extern /* Subroutine */ int sscal_();
    static logical swapj;
    extern /* Subroutine */ int sswap_();
    static real nrmxl;
    extern /* Subroutine */ int saxpy_();
    static integer jj, jp, pl, pu;
    static real tt, maxnrm;
    static integer lp1, lup;


/*     sqrdc uses householder transformations to compute the qr */
/*     factorization of an n by p matrix x.  column pivoting */
/*     based on the 2-norms of the reduced columns may be */
/*     performed at the users option. */

/*     on entry */

/*        x       real(ldx,p), where ldx .ge. n. */
/*                x contains the matrix whose decomposition is to be */
/*                computed. */

/*        ldx     integer. */
/*                ldx is the leading dimension of the array x. */

/*        n       integer. */
/*                n is the number of rows of the matrix x. */

/*        p       integer. */
/*                p is the number of columns of the matrix x. */

/*        jpvt    integer(p). */
/*                jpvt contains integers that control the selection */
/*                of the pivot columns.  the k-th column x(k) of x */
/*                is placed in one of three classes according to the */
/*                value of jpvt(k). */

/*                   if jpvt(k) .gt. 0, then x(k) is an initial */
/*                                      column. */

/*                   if jpvt(k) .eq. 0, then x(k) is a free column. */

/*                   if jpvt(k) .lt. 0, then x(k) is a final column. */

/*                before the decomposition is computed, initial columns */
/*                are moved to the beginning of the array x and final */
/*                columns to the end.  both initial and final columns */
/*                are frozen in place during the computation and only */
/*                free columns are moved.  at the k-th stage of the */
/*                reduction, if x(k) is occupied by a free column */
/*                it is interchanged with the free column of largest */
/*                reduced norm.  jpvt is not referenced if */
/*                job .eq. 0. */

/*        work    real(p). */
/*                work is a work array.  work is not referenced if */
/*                job .eq. 0. */

/*        job     integer. */
/*                job is an integer that initiates column pivoting. */
/*                if job .eq. 0, no pivoting is done. */
/*                if job .ne. 0, pivoting is done. */

/*     on return */

/*        x       x contains in its upper triangle the upper */
/*                triangular matrix r of the qr factorization. */
/*                below its diagonal x contains information from */
/*                which the orthogonal part of the decomposition */
/*                can be recovered.  note that if pivoting has */
/*                been requested, the decomposition is not that */
/*                of the original matrix x but that of x */
/*                with its columns permuted as described by jpvt. */

/*        qraux   real(p). */
/*                qraux contains further information required to recover 
*/
/*                the orthogonal part of the decomposition. */

/*        jpvt    jpvt(k) contains the index of the column of the */
/*                original matrix that has been interchanged into */
/*                the k-th column, if pivoting was requested. */

/*     linpack. this version dated 08/14/78 . */
/*     g.w. stewart, university of maryland, argonne national lab. */

/*     sqrdc uses the following functions and subprograms. */

/*     blas saxpy,sdot,sscal,sswap,snrm2 */
/*     fortran abs,amax1,min0,sqrt */

/*     internal variables */



    /* Parameter adjustments */
    --work;
    --jpvt;
    --qraux;
    x_dim1 = *ldx;
    x_offset = x_dim1 + 1;
    x -= x_offset;

    /* Function Body */
    pl = 1;
    pu = 0;
    if (*job == 0) {
	goto L60;
    }

/*        pivoting has been requested.  rearrange the columns */
/*        according to jpvt. */

    i__1 = *p;
    for (j = 1; j <= i__1; ++j) {
	swapj = jpvt[j] > 0;
	negj = jpvt[j] < 0;
	jpvt[j] = j;
	if (negj) {
	    jpvt[j] = -j;
	}
	if (! swapj) {
	    goto L10;
	}
	if (j != pl) {
	    sswap_(n, &x[pl * x_dim1 + 1], &c__1, &x[j * x_dim1 + 1], &c__1);
	}
	jpvt[j] = jpvt[pl];
	jpvt[pl] = j;
	++pl;
L10:
/* L20: */
	;
    }
    pu = *p;
    i__1 = *p;
    for (jj = 1; jj <= i__1; ++jj) {
	j = *p - jj + 1;
	if (jpvt[j] >= 0) {
	    goto L40;
	}
	jpvt[j] = -jpvt[j];
	if (j == pu) {
	    goto L30;
	}
	sswap_(n, &x[pu * x_dim1 + 1], &c__1, &x[j * x_dim1 + 1], &c__1);
	jp = jpvt[pu];
	jpvt[pu] = jpvt[j];
	jpvt[j] = jp;
L30:
	--pu;
L40:
/* L50: */
	;
    }
L60:

/*     compute the norms of the free columns. */

    if (pu < pl) {
	goto L80;
    }
    i__1 = pu;
    for (j = pl; j <= i__1; ++j) {
	qraux[j] = snrm2_(n, &x[j * x_dim1 + 1], &c__1);
	work[j] = qraux[j];
/* L70: */
    }
L80:

/*     perform the householder reduction of x. */

    lup = min(*n,*p);
    i__1 = lup;
    for (l = 1; l <= i__1; ++l) {
	if (l < pl || l >= pu) {
	    goto L120;
	}

/*           locate the column of largest norm and bring it */
/*           into the pivot position. */

	maxnrm = (float)0.;
	maxj = l;
	i__2 = pu;
	for (j = l; j <= i__2; ++j) {
	    if (qraux[j] <= maxnrm) {
		goto L90;
	    }
	    maxnrm = qraux[j];
	    maxj = j;
L90:
/* L100: */
	    ;
	}
	if (maxj == l) {
	    goto L110;
	}
	sswap_(n, &x[l * x_dim1 + 1], &c__1, &x[maxj * x_dim1 + 1], &c__1);
	qraux[maxj] = qraux[l];
	work[maxj] = work[l];
	jp = jpvt[maxj];
	jpvt[maxj] = jpvt[l];
	jpvt[l] = jp;
L110:
L120:
	qraux[l] = (float)0.;
	if (l == *n) {
	    goto L190;
	}

/*           compute the householder transformation for column l. */

	i__2 = *n - l + 1;
	nrmxl = snrm2_(&i__2, &x[l + l * x_dim1], &c__1);
	if (nrmxl == (float)0.) {
	    goto L180;
	}
	if (x[l + l * x_dim1] != (float)0.) {
	    nrmxl = r_sign(&nrmxl, &x[l + l * x_dim1]);
	}
	i__2 = *n - l + 1;
	r__1 = (float)1. / nrmxl;
	sscal_(&i__2, &r__1, &x[l + l * x_dim1], &c__1);
	x[l + l * x_dim1] += (float)1.;

/*              apply the transformation to the remaining columns, */
/*              updating the norms. */

	lp1 = l + 1;
	if (*p < lp1) {
	    goto L170;
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l + 1;
	    t = -(doublereal)sdot_(&i__3, &x[l + l * x_dim1], &c__1, &x[l + j 
		    * x_dim1], &c__1) / x[l + l * x_dim1];
	    i__3 = *n - l + 1;
	    saxpy_(&i__3, &t, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
		    c__1);
	    if (j < pl || j > pu) {
		goto L150;
	    }
	    if (qraux[j] == (float)0.) {
		goto L150;
	    }
/* Computing 2nd power */
	    r__2 = (r__1 = x[l + j * x_dim1], dabs(r__1)) / qraux[j];
	    tt = (float)1. - r__2 * r__2;
	    tt = dmax(tt,(float)0.);
	    t = tt;
/* Computing 2nd power */
	    r__1 = qraux[j] / work[j];
	    tt = tt * (float).05 * (r__1 * r__1) + (float)1.;
	    if (tt == (float)1.) {
		goto L130;
	    }
	    qraux[j] *= sqrt(t);
	    goto L140;
L130:
	    i__3 = *n - l;
	    qraux[j] = snrm2_(&i__3, &x[l + 1 + j * x_dim1], &c__1);
	    work[j] = qraux[j];
L140:
L150:
/* L160: */
	    ;
	}
L170:

/*              save the transformation. */

	qraux[l] = x[l + l * x_dim1];
	x[l + l * x_dim1] = -(doublereal)nrmxl;
L180:
L190:
/* L200: */
	;
    }
    return 0;
} /* sqrdc_ */


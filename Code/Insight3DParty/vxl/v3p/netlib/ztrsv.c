/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int ztrsv_(uplo, trans, diag, n, a, lda, x, incx, uplo_len, 
	trans_len, diag_len)
char *uplo, *trans, *diag;
integer *n;
doublecomplex *a;
integer *lda;
doublecomplex *x;
integer *incx;
ftnlen uplo_len;
ftnlen trans_len;
ftnlen diag_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4, i__5;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    void z_div(), d_cnjg();

    /* Local variables */
    static integer info;
    static doublecomplex temp;
    static integer i, j;
    extern logical lsame_();
    static integer ix, jx, kx;
    extern /* Subroutine */ int xerbla_();
    static logical noconj, nounit;

/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZTRSV  solves one of the systems of equations */

/*     A*x = b,   or   A'*x = b,   or   conjg( A' )*x = b, */

/*  where b and x are n element vectors and A is an n by n unit, or */
/*  non-unit, upper or lower triangular matrix. */

/*  No test for singularity or near-singularity is included in this */
/*  routine. Such tests must be performed before calling this routine. */

/*  Parameters */
/*  ========== */

/*  UPLO   - CHARACTER*1. */
/*           On entry, UPLO specifies whether the matrix is an upper or */
/*           lower triangular matrix as follows: */

/*              UPLO = 'U' or 'u'   A is an upper triangular matrix. */

/*              UPLO = 'L' or 'l'   A is a lower triangular matrix. */

/*           Unchanged on exit. */

/*  TRANS  - CHARACTER*1. */
/*           On entry, TRANS specifies the equations to be solved as */
/*           follows: */

/*              TRANS = 'N' or 'n'   A*x = b. */

/*              TRANS = 'T' or 't'   A'*x = b. */

/*              TRANS = 'C' or 'c'   conjg( A' )*x = b. */

/*           Unchanged on exit. */

/*  DIAG   - CHARACTER*1. */
/*           On entry, DIAG specifies whether or not A is unit */
/*           triangular as follows: */

/*              DIAG = 'U' or 'u'   A is assumed to be unit triangular. */

/*              DIAG = 'N' or 'n'   A is not assumed to be unit */
/*                                  triangular. */

/*           Unchanged on exit. */

/*  N      - INTEGER. */
/*           On entry, N specifies the order of the matrix A. */
/*           N must be at least zero. */
/*           Unchanged on exit. */

/*  A      - COMPLEX*16       array of DIMENSION ( LDA, n ). */
/*           Before entry with  UPLO = 'U' or 'u', the leading n by n */
/*           upper triangular part of the array A must contain the upper 
*/
/*           triangular matrix and the strictly lower triangular part of 
*/
/*           A is not referenced. */
/*           Before entry with UPLO = 'L' or 'l', the leading n by n */
/*           lower triangular part of the array A must contain the lower 
*/
/*           triangular matrix and the strictly upper triangular part of 
*/
/*           A is not referenced. */
/*           Note that when  DIAG = 'U' or 'u', the diagonal elements of 
*/
/*           A are not referenced either, but are assumed to be unity. */
/*           Unchanged on exit. */

/*  LDA    - INTEGER. */
/*           On entry, LDA specifies the first dimension of A as declared 
*/
/*           in the calling (sub) program. LDA must be at least */
/*           max( 1, n ). */
/*           Unchanged on exit. */

/*  X      - COMPLEX*16       array of dimension at least */
/*           ( 1 + ( n - 1 )*abs( INCX ) ). */
/*           Before entry, the incremented array X must contain the n */
/*           element right-hand side vector b. On exit, X is overwritten 
*/
/*           with the solution vector x. */

/*  INCX   - INTEGER. */
/*           On entry, INCX specifies the increment for the elements of */
/*           X. INCX must not be zero. */
/*           Unchanged on exit. */


/*  Level 2 Blas routine. */

/*  -- Written on 22-October-1986. */
/*     Jack Dongarra, Argonne National Lab. */
/*     Jeremy Du Croz, Nag Central Office. */
/*     Sven Hammarling, Nag Central Office. */
/*     Richard Hanson, Sandia National Labs. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. External Functions .. */
/*     .. External Subroutines .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters. */

    /* Parameter adjustments */
    --x;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    info = 0;
    if (! lsame_(uplo, "U", 1L, 1L) && ! lsame_(uplo, "L", 1L, 1L)) {
	info = 1;
    } else if (! lsame_(trans, "N", 1L, 1L) && ! lsame_(trans, "T", 1L, 1L) &&
	     ! lsame_(trans, "C", 1L, 1L)) {
	info = 2;
    } else if (! lsame_(diag, "U", 1L, 1L) && ! lsame_(diag, "N", 1L, 1L)) {
	info = 3;
    } else if (*n < 0) {
	info = 4;
    } else if (*lda < max(1,*n)) {
	info = 6;
    } else if (*incx == 0) {
	info = 8;
    }
    if (info != 0) {
	xerbla_("ZTRSV ", &info, 6L);
	return 0;
    }

/*     Quick return if possible. */

    if (*n == 0) {
	return 0;
    }

    noconj = lsame_(trans, "T", 1L, 1L);
    nounit = lsame_(diag, "N", 1L, 1L);

/*     Set up the start point in X if the increment is not unity. This */
/*     will be  ( N - 1 )*INCX  too small for descending loops. */

    if (*incx <= 0) {
	kx = 1 - (*n - 1) * *incx;
    } else if (*incx != 1) {
	kx = 1;
    }

/*     Start the operations. In this version the elements of A are */
/*     accessed sequentially with one pass through A. */

    if (lsame_(trans, "N", 1L, 1L)) {

/*        Form  x := inv( A )*x. */

	if (lsame_(uplo, "U", 1L, 1L)) {
	    if (*incx == 1) {
		for (j = *n; j >= 1; --j) {
		    i__1 = j;
		    if (x[i__1].r != 0. || x[i__1].i != 0.) {
			if (nounit) {
			    i__1 = j;
			    z_div(&z__1, &x[j], &a[j + j * a_dim1]);
			    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
			}
			i__1 = j;
			temp.r = x[i__1].r, temp.i = x[i__1].i;
			for (i = j - 1; i >= 1; --i) {
			    i__1 = i;
			    i__2 = i;
			    i__3 = i + j * a_dim1;
			    z__2.r = temp.r * a[i__3].r - temp.i * a[i__3].i, 
				    z__2.i = temp.r * a[i__3].i + temp.i * a[
				    i__3].r;
			    z__1.r = x[i__2].r - z__2.r, z__1.i = x[i__2].i - 
				    z__2.i;
			    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
/* L10: */
			}
		    }
/* L20: */
		}
	    } else {
		jx = kx + (*n - 1) * *incx;
		for (j = *n; j >= 1; --j) {
		    i__1 = jx;
		    if (x[i__1].r != 0. || x[i__1].i != 0.) {
			if (nounit) {
			    i__1 = jx;
			    z_div(&z__1, &x[jx], &a[j + j * a_dim1]);
			    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
			}
			i__1 = jx;
			temp.r = x[i__1].r, temp.i = x[i__1].i;
			ix = jx;
			for (i = j - 1; i >= 1; --i) {
			    ix -= *incx;
			    i__1 = ix;
			    i__2 = ix;
			    i__3 = i + j * a_dim1;
			    z__2.r = temp.r * a[i__3].r - temp.i * a[i__3].i, 
				    z__2.i = temp.r * a[i__3].i + temp.i * a[
				    i__3].r;
			    z__1.r = x[i__2].r - z__2.r, z__1.i = x[i__2].i - 
				    z__2.i;
			    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
/* L30: */
			}
		    }
		    jx -= *incx;
/* L40: */
		}
	    }
	} else {
	    if (*incx == 1) {
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    i__2 = j;
		    if (x[i__2].r != 0. || x[i__2].i != 0.) {
			if (nounit) {
			    i__2 = j;
			    z_div(&z__1, &x[j], &a[j + j * a_dim1]);
			    x[i__2].r = z__1.r, x[i__2].i = z__1.i;
			}
			i__2 = j;
			temp.r = x[i__2].r, temp.i = x[i__2].i;
			i__2 = *n;
			for (i = j + 1; i <= i__2; ++i) {
			    i__3 = i;
			    i__4 = i;
			    i__5 = i + j * a_dim1;
			    z__2.r = temp.r * a[i__5].r - temp.i * a[i__5].i, 
				    z__2.i = temp.r * a[i__5].i + temp.i * a[
				    i__5].r;
			    z__1.r = x[i__4].r - z__2.r, z__1.i = x[i__4].i - 
				    z__2.i;
			    x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/* L50: */
			}
		    }
/* L60: */
		}
	    } else {
		jx = kx;
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    i__2 = jx;
		    if (x[i__2].r != 0. || x[i__2].i != 0.) {
			if (nounit) {
			    i__2 = jx;
			    z_div(&z__1, &x[jx], &a[j + j * a_dim1]);
			    x[i__2].r = z__1.r, x[i__2].i = z__1.i;
			}
			i__2 = jx;
			temp.r = x[i__2].r, temp.i = x[i__2].i;
			ix = jx;
			i__2 = *n;
			for (i = j + 1; i <= i__2; ++i) {
			    ix += *incx;
			    i__3 = ix;
			    i__4 = ix;
			    i__5 = i + j * a_dim1;
			    z__2.r = temp.r * a[i__5].r - temp.i * a[i__5].i, 
				    z__2.i = temp.r * a[i__5].i + temp.i * a[
				    i__5].r;
			    z__1.r = x[i__4].r - z__2.r, z__1.i = x[i__4].i - 
				    z__2.i;
			    x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/* L70: */
			}
		    }
		    jx += *incx;
/* L80: */
		}
	    }
	}
    } else {

/*        Form  x := inv( A' )*x  or  x := inv( conjg( A' ) )*x. */

	if (lsame_(uplo, "U", 1L, 1L)) {
	    if (*incx == 1) {
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    i__2 = j;
		    temp.r = x[i__2].r, temp.i = x[i__2].i;
		    if (noconj) {
			i__2 = j - 1;
			for (i = 1; i <= i__2; ++i) {
			    i__3 = i + j * a_dim1;
			    i__4 = i;
			    z__2.r = a[i__3].r * x[i__4].r - a[i__3].i * x[
				    i__4].i, z__2.i = a[i__3].r * x[i__4].i + 
				    a[i__3].i * x[i__4].r;
			    z__1.r = temp.r - z__2.r, z__1.i = temp.i - 
				    z__2.i;
			    temp.r = z__1.r, temp.i = z__1.i;
/* L90: */
			}
			if (nounit) {
			    z_div(&z__1, &temp, &a[j + j * a_dim1]);
			    temp.r = z__1.r, temp.i = z__1.i;
			}
		    } else {
			i__2 = j - 1;
			for (i = 1; i <= i__2; ++i) {
			    d_cnjg(&z__3, &a[i + j * a_dim1]);
			    i__3 = i;
			    z__2.r = z__3.r * x[i__3].r - z__3.i * x[i__3].i, 
				    z__2.i = z__3.r * x[i__3].i + z__3.i * x[
				    i__3].r;
			    z__1.r = temp.r - z__2.r, z__1.i = temp.i - 
				    z__2.i;
			    temp.r = z__1.r, temp.i = z__1.i;
/* L100: */
			}
			if (nounit) {
			    d_cnjg(&z__2, &a[j + j * a_dim1]);
			    z_div(&z__1, &temp, &z__2);
			    temp.r = z__1.r, temp.i = z__1.i;
			}
		    }
		    i__2 = j;
		    x[i__2].r = temp.r, x[i__2].i = temp.i;
/* L110: */
		}
	    } else {
		jx = kx;
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    ix = kx;
		    i__2 = jx;
		    temp.r = x[i__2].r, temp.i = x[i__2].i;
		    if (noconj) {
			i__2 = j - 1;
			for (i = 1; i <= i__2; ++i) {
			    i__3 = i + j * a_dim1;
			    i__4 = ix;
			    z__2.r = a[i__3].r * x[i__4].r - a[i__3].i * x[
				    i__4].i, z__2.i = a[i__3].r * x[i__4].i + 
				    a[i__3].i * x[i__4].r;
			    z__1.r = temp.r - z__2.r, z__1.i = temp.i - 
				    z__2.i;
			    temp.r = z__1.r, temp.i = z__1.i;
			    ix += *incx;
/* L120: */
			}
			if (nounit) {
			    z_div(&z__1, &temp, &a[j + j * a_dim1]);
			    temp.r = z__1.r, temp.i = z__1.i;
			}
		    } else {
			i__2 = j - 1;
			for (i = 1; i <= i__2; ++i) {
			    d_cnjg(&z__3, &a[i + j * a_dim1]);
			    i__3 = ix;
			    z__2.r = z__3.r * x[i__3].r - z__3.i * x[i__3].i, 
				    z__2.i = z__3.r * x[i__3].i + z__3.i * x[
				    i__3].r;
			    z__1.r = temp.r - z__2.r, z__1.i = temp.i - 
				    z__2.i;
			    temp.r = z__1.r, temp.i = z__1.i;
			    ix += *incx;
/* L130: */
			}
			if (nounit) {
			    d_cnjg(&z__2, &a[j + j * a_dim1]);
			    z_div(&z__1, &temp, &z__2);
			    temp.r = z__1.r, temp.i = z__1.i;
			}
		    }
		    i__2 = jx;
		    x[i__2].r = temp.r, x[i__2].i = temp.i;
		    jx += *incx;
/* L140: */
		}
	    }
	} else {
	    if (*incx == 1) {
		for (j = *n; j >= 1; --j) {
		    i__1 = j;
		    temp.r = x[i__1].r, temp.i = x[i__1].i;
		    if (noconj) {
			i__1 = j + 1;
			for (i = *n; i >= i__1; --i) {
			    i__2 = i + j * a_dim1;
			    i__3 = i;
			    z__2.r = a[i__2].r * x[i__3].r - a[i__2].i * x[
				    i__3].i, z__2.i = a[i__2].r * x[i__3].i + 
				    a[i__2].i * x[i__3].r;
			    z__1.r = temp.r - z__2.r, z__1.i = temp.i - 
				    z__2.i;
			    temp.r = z__1.r, temp.i = z__1.i;
/* L150: */
			}
			if (nounit) {
			    z_div(&z__1, &temp, &a[j + j * a_dim1]);
			    temp.r = z__1.r, temp.i = z__1.i;
			}
		    } else {
			i__1 = j + 1;
			for (i = *n; i >= i__1; --i) {
			    d_cnjg(&z__3, &a[i + j * a_dim1]);
			    i__2 = i;
			    z__2.r = z__3.r * x[i__2].r - z__3.i * x[i__2].i, 
				    z__2.i = z__3.r * x[i__2].i + z__3.i * x[
				    i__2].r;
			    z__1.r = temp.r - z__2.r, z__1.i = temp.i - 
				    z__2.i;
			    temp.r = z__1.r, temp.i = z__1.i;
/* L160: */
			}
			if (nounit) {
			    d_cnjg(&z__2, &a[j + j * a_dim1]);
			    z_div(&z__1, &temp, &z__2);
			    temp.r = z__1.r, temp.i = z__1.i;
			}
		    }
		    i__1 = j;
		    x[i__1].r = temp.r, x[i__1].i = temp.i;
/* L170: */
		}
	    } else {
		kx += (*n - 1) * *incx;
		jx = kx;
		for (j = *n; j >= 1; --j) {
		    ix = kx;
		    i__1 = jx;
		    temp.r = x[i__1].r, temp.i = x[i__1].i;
		    if (noconj) {
			i__1 = j + 1;
			for (i = *n; i >= i__1; --i) {
			    i__2 = i + j * a_dim1;
			    i__3 = ix;
			    z__2.r = a[i__2].r * x[i__3].r - a[i__2].i * x[
				    i__3].i, z__2.i = a[i__2].r * x[i__3].i + 
				    a[i__2].i * x[i__3].r;
			    z__1.r = temp.r - z__2.r, z__1.i = temp.i - 
				    z__2.i;
			    temp.r = z__1.r, temp.i = z__1.i;
			    ix -= *incx;
/* L180: */
			}
			if (nounit) {
			    z_div(&z__1, &temp, &a[j + j * a_dim1]);
			    temp.r = z__1.r, temp.i = z__1.i;
			}
		    } else {
			i__1 = j + 1;
			for (i = *n; i >= i__1; --i) {
			    d_cnjg(&z__3, &a[i + j * a_dim1]);
			    i__2 = ix;
			    z__2.r = z__3.r * x[i__2].r - z__3.i * x[i__2].i, 
				    z__2.i = z__3.r * x[i__2].i + z__3.i * x[
				    i__2].r;
			    z__1.r = temp.r - z__2.r, z__1.i = temp.i - 
				    z__2.i;
			    temp.r = z__1.r, temp.i = z__1.i;
			    ix -= *incx;
/* L190: */
			}
			if (nounit) {
			    d_cnjg(&z__2, &a[j + j * a_dim1]);
			    z_div(&z__1, &temp, &z__2);
			    temp.r = z__1.r, temp.i = z__1.i;
			}
		    }
		    i__1 = jx;
		    x[i__1].r = temp.r, x[i__1].i = temp.i;
		    jx -= *incx;
/* L200: */
		}
	    }
	}
    }

    return 0;

/*     End of ZTRSV . */

} /* ztrsv_ */


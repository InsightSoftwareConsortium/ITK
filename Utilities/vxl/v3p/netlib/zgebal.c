#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static integer c__1 = 1;

/* Subroutine */ void zgebal_(job, n, a, lda, ilo, ihi, scale, info)
const char *job;
const integer *n;
doublecomplex *a;
const integer *lda;
integer *ilo, *ihi;
doublereal *scale;
integer *info;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer iexc;
    static doublereal c, f, g;
    static integer i, j, k, l, m;
    static doublereal r, s;
    static doublereal sfmin1, sfmin2, sfmax1, sfmax2, ca, ra;
    static logical noconv;
    static integer ica, ira;


/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZGEBAL balances a general complex matrix A.  This involves, first,    */
/*  permuting A by a similarity transformation to isolate eigenvalues     */
/*  in the first 1 to ILO-1 and last IHI+1 to N elements on the           */
/*  diagonal; and second, applying a diagonal similarity transformation   */
/*  to rows and columns ILO to IHI to make the rows and columns as        */
/*  close in norm as possible.  Both steps are optional.                  */
/*                                                                        */
/*  Balancing may reduce the 1-norm of the matrix, and improve the        */
/*  accuracy of the computed eigenvalues and/or eigenvectors.             */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  JOB     (input) CHARACTER*1                                           */
/*          Specifies the operations to be performed on A:                */
/*          = 'N':  none:  simply set ILO = 1, IHI = N, SCALE(I) = 1.0    */
/*                  for i = 1,...,N;                                      */
/*          = 'P':  permute only;                                         */
/*          = 'S':  scale only;                                           */
/*          = 'B':  both permute and scale.                               */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrix A.  N >= 0.                           */
/*                                                                        */
/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)            */
/*          On entry, the input matrix A.                                 */
/*          On exit,  A is overwritten by the balanced matrix.            */
/*          If JOB = 'N', A is not referenced.                            */
/*          See Further Details.                                          */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max(1,N).       */
/*                                                                        */
/*  ILO     (output) INTEGER                                              */
/*  IHI     (output) INTEGER                                              */
/*          ILO and IHI are set to integers such that on exit             */
/*          A(i,j) = 0 if i > j and j = 1,...,ILO-1 or I = IHI+1,...,N.   */
/*          If JOB = 'N' or 'S', ILO = 1 and IHI = N.                     */
/*                                                                        */
/*  SCALE   (output) DOUBLE PRECISION array, dimension (N)                */
/*          Details of the permutations and scaling factors applied to    */
/*          A.  If P(j) is the index of the row and column interchanged   */
/*          with row and column j and D(j) is the scaling factor          */
/*          applied to row and column j, then                             */
/*          SCALE(j) = P(j)    for j = 1,...,ILO-1                        */
/*                   = D(j)    for j = ILO,...,IHI                        */
/*                   = P(j)    for j = IHI+1,...,N.                       */
/*          The order in which the interchanges are made is N to IHI+1,   */
/*          then 1 to ILO-1.                                              */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0:  successful exit.                                        */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value.   */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  The permutations consist of row and column interchanges which put     */
/*  the matrix in the form                                                */
/*                                                                        */
/*             ( T1   X   Y  )                                            */
/*     P A P = (  0   B   Z  )                                            */
/*             (  0   0   T2 )                                            */
/*                                                                        */
/*  where T1 and T2 are upper triangular matrices whose eigenvalues lie   */
/*  along the diagonal.  The column indices ILO and IHI mark the starting */
/*  and ending columns of the submatrix B. Balancing consists of applying */
/*  a diagonal similarity transformation inv(D) * B * D to make the       */
/*  1-norms of each row of B and its corresponding column nearly equal.   */
/*  The output matrix is                                                  */
/*                                                                        */
/*     ( T1     X*D          Y    )                                       */
/*     (  0  inv(D)*B*D  inv(D)*Z ).                                      */
/*     (  0      0           T2   )                                       */
/*                                                                        */
/*  Information about the permutations P and the diagonal matrix D is     */
/*  returned in the vector SCALE.                                         */
/*                                                                        */
/*  This subroutine is based on the EISPACK routine CBAL.                 */
/*                                                                        */
/*  ===================================================================== */

    *info = 0;
    if (! lsame_(job, "N") && ! lsame_(job, "P") && ! lsame_(job, "S") && ! lsame_(job, "B")) {
        *info = -1;
    } else if (*n < 0) {
        *info = -2;
    } else if (*lda < max(1,*n)) {
        *info = -4;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("ZGEBAL", &i__1);
        return;
    }

    k = 0;
    l = *n;

    if (*n == 0) {
        goto L210;
    }

    if (lsame_(job, "N")) {
        for (i = 0; i < *n; ++i) {
            scale[i] = 1.;
        }
        goto L210;
    }

    if (lsame_(job, "S")) {
        goto L120;
    }

/*     Permutation to isolate eigenvalues if possible */

    goto L50;

/*     Row and column exchange. */

L20:
    scale[m] = (doublereal) j+1;
    if (j == m) {
        goto L30;
    }

    zswap_(&l, &a[j * *lda], &c__1, &a[m * *lda], &c__1);
    i__1 = *n - k;
    zswap_(&i__1, &a[j + k * *lda], lda, &a[m + k * *lda], lda);

L30:
    switch ((int)iexc) {
        case 1:  goto L40;
        case 2:  goto L80;
    }

/*     Search for rows isolating an eigenvalue and push them down. */

L40:
    if (l == 1) {
        goto L210;
    }
    --l;

L50:
    for (j = l-1; j >= 0; --j) {
        for (i = 0; i < l; ++i) {
            if (i == j) {
                continue; /* next i */
            }
            i__1 = j + i * *lda;
            if (a[i__1].r != 0. || a[i__1].i != 0.) {
                goto L70; /* next j */
            }
        }
        m = l-1;
        iexc = 1;
        goto L20;
L70:
        ;
    }

    goto L90;

/*     Search for columns isolating an eigenvalue and push them left. */

L80:
    ++k;

L90:
    for (j = k; j < l; ++j) {
        for (i = k; i < l; ++i) {
            if (i == j) {
                continue; /* next i */
            }
            i__1 = i + j * *lda;
            if (a[i__1].r != 0. || a[i__1].i != 0.) {
                goto L110; /* next j */
            }
        }
        m = k;
        iexc = 2;
        goto L20;
L110:
        ;
    }

L120:
    for (i = k; i < l; ++i) {
        scale[i] = 1.;
    }

    if (lsame_(job, "P")) {
        goto L210;
    }

/*     Balance the submatrix in rows K to L. */

/*     Iterative loop for norm reduction */

    sfmin1 = dlamch_("S") / dlamch_("P");
    sfmax1 = 1. / sfmin1;
    sfmin2 = sfmin1 * 10.;
    sfmax2 = 1. / sfmin2;
L140:
    noconv = FALSE_;

    for (i = k; i < l; ++i) {
        c = 0.; r = 0.;

        for (j = k; j < l; ++j) {
            if (j == i) {
                continue; /* next j */
            }
            i__1 = j + i * *lda;
            c += abs(a[i__1].r) + abs(a[i__1].i);
            i__1 = i + j * *lda;
            r += abs(a[i__1].r) + abs(a[i__1].i);
        }
        ica = izamax_(&l, &a[i * *lda], &c__1) - 1;
        ca = z_abs(&a[ica + i * *lda]);
        i__1 = *n - k;
        ira = izamax_(&i__1, &a[i + k * *lda], lda) - 1;
        ra = z_abs(&a[i + (ira + k) * *lda]);

/*        Guard against zero C or R due to underflow. */

        if (c == 0. || r == 0.) {
            continue; /* next i */
        }
        g = r / 10.; f = 1.;
        s = c + r;
L160:
        if (c >= g || max(max(f,c),ca) >= sfmax2 || min(min(r,g),ra) <= sfmin2) {
            goto L170;
        }
        f *= 10.; c *= 10.; ca *= 10.;
        r /= 10.; g /= 10.; ra /= 10.;
        goto L160;

L170:
        g = c / 10.;
L180:
        if (g < r || max(r,ra) >= sfmax2 || min(min(min(f,c),g),ca) <= sfmin2) {
            goto L190;
        }
        f /= 10.; c /= 10.; g /= 10.; ca /= 10.;
        r *= 10.; ra *= 10.;
        goto L180;

/*        Now balance. */

L190:
        if (c + r >= s * .95) {
            continue; /* next i */
        }
        if (f < 1. && scale[i] < 1.) {
            if (f * scale[i] <= sfmin1) {
                continue; /* next i */
            }
        }
        if (f > 1. && scale[i] > 1.) {
            if (scale[i] >= sfmax1 / f) {
                continue; /* next i */
            }
        }
        g = 1. / f;
        scale[i] *= f;
        noconv = TRUE_;

        i__1 = *n - k;
        zdscal_(&i__1, &g, &a[i + k * *lda], lda);
        zdscal_(&l, &f, &a[i * *lda], &c__1);
    }

    if (noconv) {
        goto L140;
    }

L210:
    *ilo = k+1;
    *ihi = l;
} /* zgebal_ */

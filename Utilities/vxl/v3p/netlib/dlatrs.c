#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, Oct 2003: manual optimisation and clean-up */

/* Table of constant values */
static integer c__1 = 1;
static doublereal c_b36 = .5;

/* Subroutine */ void dlatrs_(uplo, trans, diag, normin, n, a, lda, x, scale, cnorm, info)
const char *uplo, *trans, *diag, *normin;
const integer *n;
const doublereal *a;
const integer *lda;
doublereal *x, *scale, *cnorm;
integer *info;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    static integer jinc;
    static doublereal xbnd;
    static integer imax;
    static doublereal tmax, tjjs, xmax, grow, sumj;
    static integer i, j;
    static doublereal tscal, uscal;
    static integer jlast;
    static logical upper;
    static doublereal xj;
    static doublereal bignum;
    static logical notran;
    static integer jfirst;
    static doublereal smlnum;
    static logical nounit;
    static doublereal rec, tjj;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1992 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLATRS solves one of the triangular systems                           */
/*                                                                        */
/*     A *x = s*b  or  A'*x = s*b                                         */
/*                                                                        */
/*  with scaling to prevent overflow.  Here A is an upper or lower        */
/*  triangular matrix, A' denotes the transpose of A, x and b are         */
/*  n-element vectors, and s is a scaling factor, usually less than       */
/*  or equal to 1, chosen so that the components of x will be less than   */
/*  the overflow threshold.  If the unscaled problem will not cause       */
/*  overflow, the Level 2 BLAS routine DTRSV is called.  If the matrix A  */
/*  is singular (A(j,j) = 0 for some j), then s is set to 0 and a         */
/*  non-trivial solution to A*x = 0 is returned.                          */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  UPLO    (input) CHARACTER*1                                           */
/*          Specifies whether the matrix A is upper or lower triangular.  */
/*          = 'U':  Upper triangular                                      */
/*          = 'L':  Lower triangular                                      */
/*                                                                        */
/*  TRANS   (input) CHARACTER*1                                           */
/*          Specifies the operation applied to A.                         */
/*          = 'N':  Solve A * x = s*b  (No transpose)                     */
/*          = 'T':  Solve A'* x = s*b  (Transpose)                        */
/*          = 'C':  Solve A'* x = s*b  (Conjugate transpose = Transpose)  */
/*                                                                        */
/*  DIAG    (input) CHARACTER*1                                           */
/*          Specifies whether or not the matrix A is unit triangular.     */
/*          = 'N':  Non-unit triangular                                   */
/*          = 'U':  Unit triangular                                       */
/*                                                                        */
/*  NORMIN  (input) CHARACTER*1                                           */
/*          Specifies whether CNORM has been set or not.                  */
/*          = 'Y':  CNORM contains the column norms on entry              */
/*          = 'N':  CNORM is not set on entry.  On exit, the norms will   */
/*                  be computed and stored in CNORM.                      */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrix A.  N >= 0.                           */
/*                                                                        */
/*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)             */
/*          The triangular matrix A.  If UPLO = 'U', the leading n by n   */
/*          upper triangular part of the array A contains the upper       */
/*          triangular matrix, and the strictly lower triangular part of  */
/*          A is not referenced.  If UPLO = 'L', the leading n by n lower */
/*          triangular part of the array A contains the lower triangular  */
/*          matrix, and the strictly upper triangular part of A is not    */
/*          referenced.  If DIAG = 'U', the diagonal elements of A are    */
/*          also not referenced and are assumed to be 1.                  */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max (1,N).      */
/*                                                                        */
/*  X       (input/output) DOUBLE PRECISION array, dimension (N)          */
/*          On entry, the right hand side b of the triangular system.     */
/*          On exit, X is overwritten by the solution vector x.           */
/*                                                                        */
/*  SCALE   (output) DOUBLE PRECISION                                     */
/*          The scaling factor s for the triangular system                */
/*             A * x = s*b  or  A'* x = s*b.                              */
/*          If SCALE = 0, the matrix A is singular or badly scaled, and   */
/*          the vector x is an exact or approximate solution to A*x = 0.  */
/*                                                                        */
/*  CNORM   (input or output) DOUBLE PRECISION array, dimension (N)       */
/*                                                                        */
/*          If NORMIN = 'Y', CNORM is an input argument and CNORM(j)      */
/*          contains the norm of the off-diagonal part of the j-th column */
/*          of A.  If TRANS = 'N', CNORM(j) must be greater than or equal */
/*          to the infinity-norm, and if TRANS = 'T' or 'C', CNORM(j)     */
/*          must be greater than or equal to the 1-norm.                  */
/*                                                                        */
/*          If NORMIN = 'N', CNORM is an output argument and CNORM(j)     */
/*          returns the 1-norm of the offdiagonal part of the j-th column */
/*          of A.                                                         */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0:  successful exit                                         */
/*          < 0:  if INFO = -k, the k-th argument had an illegal value    */
/*                                                                        */
/*  Further Details                                                       */
/*  ======= =======                                                       */
/*                                                                        */
/*  A rough bound on x is computed; if that is less than overflow, DTRSV  */
/*  is called, otherwise, specific code is used which checks for possible */
/*  overflow or divide-by-zero at every operation.                        */
/*                                                                        */
/*  A columnwise scheme is used for solving A*x = b.  The basic algorithm */
/*  if A is lower triangular is                                           */
/*                                                                        */
/*       x[1:n] := b[1:n]                                                 */
/*       for j = 1, ..., n                                                */
/*            x(j) := x(j) / A(j,j)                                       */
/*            x[j+1:n] := x[j+1:n] - x(j) * A[j+1:n,j]                    */
/*       end                                                              */
/*                                                                        */
/*  Define bounds on the components of x after j iterations of the loop:  */
/*     M(j) = bound on x[1:j]                                             */
/*     G(j) = bound on x[j+1:n]                                           */
/*  Initially, let M(0) = 0 and G(0) = max{x(i), i=1,...,n}.              */
/*                                                                        */
/*  Then for iteration j+1 we have                                        */
/*     M(j+1) <= G(j) / | A(j+1,j+1) |                                    */
/*     G(j+1) <= G(j) + M(j+1) * | A[j+2:n,j+1] |                         */
/*            <= G(j) ( 1 + CNORM(j+1) / | A(j+1,j+1) | )                 */
/*                                                                        */
/*  where CNORM(j+1) is greater than or equal to the infinity-norm of     */
/*  column j+1 of A, not counting the diagonal.  Hence                    */
/*                                                                        */
/*     G(j) <= G(0) product ( 1 + CNORM(i) / | A(i,i) | )                 */
/*                  1<=i<=j                                               */
/*  and                                                                   */
/*                                                                        */
/*     |x(j)| <= ( G(0) / |A(j,j)| ) product ( 1 + CNORM(i) / |A(i,i)| )  */
/*                                   1<=i< j                              */
/*                                                                        */
/*  Since |x(j)| <= M(j), we use the Level 2 BLAS routine DTRSV if the    */
/*  reciprocal of the largest M(j), j=1,..,n, is larger than              */
/*  max(underflow, 1/overflow).                                           */
/*                                                                        */
/*  The bound on x(j) is also used to determine when a step in the        */
/*  columnwise method can be performed without fear of overflow.  If      */
/*  the computed bound is greater than a large constant, x is scaled to   */
/*  prevent overflow, but if the bound overflows, x is set to 0, x(j) to  */
/*  1, and scale to 0, and a non-trivial solution to A*x = 0 is found.    */
/*                                                                        */
/*  Similarly, a row-wise scheme is used to solve A'*x = b.  The basic    */
/*  algorithm for A upper triangular is                                   */
/*                                                                        */
/*       for j = 1, ..., n                                                */
/*            x(j) := ( b(j) - A[1:j-1,j]' * x[1:j-1] ) / A(j,j)          */
/*       end                                                              */
/*                                                                        */
/*  We simultaneously compute two bounds                                  */
/*       G(j) = bound on ( b(i) - A[1:i-1,i]' * x[1:i-1] ), 1<=i<=j       */
/*       M(j) = bound on x(i), 1<=i<=j                                    */
/*                                                                        */
/*  The initial values are G(0) = 0, M(0) = max{b(i), i=1,..,n}, and we   */
/*  add the constraint G(j) >= G(j-1) and M(j) >= M(j-1) for j >= 1.      */
/*  Then the bound on x(j) is                                             */
/*                                                                        */
/*       M(j) <= M(j-1) * ( 1 + CNORM(j) ) / | A(j,j) |                   */
/*                                                                        */
/*            <= M(0) * product ( ( 1 + CNORM(i) ) / |A(i,i)| )           */
/*                      1<=i<=j                                           */
/*                                                                        */
/*  and we can safely call DTRSV if 1/M(n) and 1/G(n) are both greater    */
/*  than max(underflow, 1/overflow).                                      */
/*                                                                        */
/*  ===================================================================== */

    *info = 0;
    upper = lsame_(uplo, "U");
    notran = lsame_(trans, "N");
    nounit = lsame_(diag, "N");

    /* Test the input parameters. */

    if      (!upper  && !lsame_(uplo,  "L"))                        *info = 1;
    else if (!notran && !lsame_(trans, "T") && !lsame_(trans, "C")) *info = 2;
    else if (!nounit && !lsame_(diag,  "U"))                        *info = 3;
    else if (         !lsame_(normin, "Y") && !lsame_(normin, "N")) *info = 4;
    else if (*n < 0)                                                *info = 5;
    else if (*lda < max(1,*n))                                      *info = 7;
    if (*info != 0) {
        xerbla_("DLATRS", info);
        *info = -(*info);
        return;
    }

    /* Quick return if possible */

    if (*n == 0)
        return;

    /* Determine machine dependent parameters to control overflow. */

    smlnum = dlamch_("Safe minimum") / dlamch_("Precision");
    bignum = 1. / smlnum;
    *scale = 1.;

    if (lsame_(normin, "N"))
    {
        /* Compute the 1-norm of each column, not including the diagonal. */

        if (upper) /* A is upper triangular. */
        {
            for (j = 0; j < *n; ++j)
                cnorm[j] = dasum_(&j, &a[j * *lda], &c__1);
        }
        else /* A is lower triangular. */
        {
            for (j = 0; j < *n - 1; ++j) {
                i__1 = *n - j - 1;
                cnorm[j] = dasum_(&i__1, &a[j + 1 + j * *lda], &c__1);
            }
            cnorm[*n-1] = 0.;
        }
    }

    /* Scale the column norms by TSCAL if the maximum element in CNORM is greater than BIGNUM. */

    imax = idamax_(n, cnorm, &c__1);
    tmax = cnorm[imax-1];
    if (tmax <= bignum)
        tscal = 1.;
    else {
        tscal = 1. / (smlnum * tmax);
        dscal_(n, &tscal, cnorm, &c__1);
    }

    /* Compute a bound on the computed solution vector to see if the */
    /* Level 2 BLAS routine DTRSV can be used. */

    j = idamax_(n, x, &c__1) - 1;
    xmax = abs(x[j]);
    xbnd = xmax;
    if (notran)
    {
        /* Compute the growth in A * x = b. */

        if (upper) {
            jfirst = *n;
            jlast = 1;
            jinc = -1;
        }
        else {
            jfirst = 1;
            jlast = *n;
            jinc = 1;
        }

        if (tscal != 1.) {
            grow = 0.;
            goto L50;
        }

        if (nounit)
        {
            /* A is non-unit triangular. */

            /* Compute GROW = 1/G(j) and XBND = 1/M(j). */
            /* Initially, G(0) = max{x(i), i=1,...,n}. */

            grow = 1. / max(xbnd,smlnum);
            xbnd = grow;
            for (j = jfirst-1; jinc < 0 ? j > jlast-2 : j < jlast; j += jinc)
            {
                /* Exit the loop if the growth factor is too small. */
                if (grow <= smlnum)
                    goto L50;

                /* M(j) = G(j-1) / abs(A(j,j)) */
                tjj = abs(a[j + j * *lda]);
                xbnd = min(xbnd, min(1.,tjj) * grow);
                if (tjj + cnorm[j] >= smlnum)
                {
                    /* G(j) = G(j-1)*( 1 + CNORM(j) / abs(A(j,j)) ) */
                    grow *= tjj / (tjj + cnorm[j]);
                }
                else
                {
                    /* G(j) could overflow, set GROW to 0. */
                    grow = 0.;
                }
            }
            grow = xbnd;
        }
        else
        {
            /* A is unit triangular. */

            /* Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}. */

            grow = min(1., 1./max(xbnd,smlnum));
            for (j = jfirst-1; jinc < 0 ? j > jlast-2 : j < jlast; j += jinc)
            {
                /* Exit the loop if the growth factor is too small. */
                if (grow <= smlnum)
                    goto L50;

                /* G(j) = G(j-1)*( 1 + CNORM(j) ) */
                grow *= 1. / (cnorm[j] + 1.);
            }
        }
    }
    else
    {
        /* Compute the growth in A' * x = b. */

        if (upper) {
            jfirst = 1;
            jlast = *n;
            jinc = 1;
        }
        else {
            jfirst = *n;
            jlast = 1;
            jinc = -1;
        }

        if (tscal != 1.) {
            grow = 0.;
            goto L50;
        }

        if (nounit)
        {
            /* A is non-unit triangular. */

            /* Compute GROW = 1/G(j) and XBND = 1/M(j). */
            /* Initially, M(0) = max{x(i), i=1,...,n}.  */

            grow = 1. / max(xbnd,smlnum);
            xbnd = grow;
            for (j = jfirst-1; jinc < 0 ? j > jlast-2 : j < jlast; j += jinc)
            {
                /* Exit the loop if the growth factor is too small. */
                if (grow <= smlnum)
                    goto L50;

                /* G(j) = max( G(j-1), M(j-1)*( 1 + CNORM(j) ) ) */
                xj = cnorm[j] + 1.;
                grow = min(grow, xbnd / xj);

                /* M(j) = M(j-1)*( 1 + CNORM(j) ) / abs(A(j,j)) */
                tjj = abs(a[j + j * *lda]);
                if (xj > tjj)
                    xbnd *= tjj / xj;
            }
            grow = min(grow,xbnd);
        }
        else
        {
            /* A is unit triangular. */

            /* Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}. */

            grow = min(1., 1./max(xbnd,smlnum));
            for (j = jfirst-1; jinc < 0 ? j > jlast-2 : j < jlast; j += jinc)
            {
                /* Exit the loop if the growth factor is too small. */
                if (grow <= smlnum)
                    goto L50;

                /* G(j) = ( 1 + CNORM(j) )*G(j-1) */
                xj = cnorm[j] + 1.;
                grow /= xj;
            }
        }
    }
L50:
    if (grow * tscal > smlnum)
    {
        /* Use the Level 2 BLAS solve if the reciprocal of the bound on */
        /* elements of X is not too small. */

        dtrsv_(uplo, trans, diag, n, a, lda, x, &c__1);
    }
    else
    {
        /* Use a Level 1 BLAS solve, scaling intermediate results. */

        if (xmax > bignum)
        {
            /* Scale X so that its components are less than or equal to BIGNUM in absolute value. */
            *scale = bignum / xmax;
            dscal_(n, scale, x, &c__1);
            xmax = bignum;
        }

        if (notran)
        {
            /* Solve A * x = b */
            for (j = jfirst-1; jinc < 0 ? j > jlast-2 : j < jlast; j += jinc)
            {
                /* Compute x(j) = b(j) / A(j,j), scaling x if necessary. */
                xj = abs(x[j]);
                if (nounit)
                    tjjs = a[j + j * *lda] * tscal;
                else {
                    tjjs = tscal;
                    if (tscal == 1.)
                        goto L100;
                }
                tjj = abs(tjjs);
                if (tjj > smlnum)
                {
                    /* abs(A(j,j)) > SMLNUM: */
                    if (tjj < 1.) {
                        if (xj > tjj * bignum)
                        {
                            /* Scale x by 1/b(j). */
                            rec = 1. / xj;
                            dscal_(n, &rec, x, &c__1);
                            *scale *= rec;
                            xmax *= rec;
                        }
                    }
                    x[j] /= tjjs;
                    xj = abs(x[j]);
                }
                else if (tjj > 0.)
                {
                    /* 0 < abs(A(j,j)) <= SMLNUM: */
                    if (xj > tjj * bignum)
                    {
                        /* Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM */
                        /* to avoid overflow when dividing by A(j,j). */

                        rec = tjj * bignum / xj;
                        if (cnorm[j] > 1.)
                        {
                            /* Scale by 1/CNORM(j) to avoid overflow when */
                            /* multiplying x(j) times column j. */
                            rec /= cnorm[j];
                        }
                        dscal_(n, &rec, x, &c__1);
                        *scale *= rec;
                        xmax *= rec;
                    }
                    x[j] /= tjjs;
                    xj = abs(x[j]);
                }
                else
                {
                    /* A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and */
                    /* scale = 0, and compute a solution to A*x = 0. */
                    for (i = 0; i < *n; ++i)
                        x[i] = 0.;
                    x[j] = 1.;
                    xj = 1.;
                    *scale = 0.;
                    xmax = 0.;
                }
L100:

                /* Scale x if necessary to avoid overflow when adding a */
                /* multiple of column j of A. */

                if (xj > 1.) {
                    rec = 1. / xj;
                    if (cnorm[j] > (bignum - xmax) * rec)
                    {
                        /* Scale x by 1/(2*abs(x(j))). */
                        rec *= .5;
                        dscal_(n, &rec, x, &c__1);
                        *scale *= rec;
                    }
                }
                else if (xj * cnorm[j] > bignum - xmax)
                {
                    /* Scale x by 1/2. */
                    dscal_(n, &c_b36, x, &c__1);
                    *scale *= .5;
                }

                if (upper) {
                    if (j > 0)
                    {
                        /* Compute the update                       */
                        /* x(1:j-1) := x(1:j-1) - x(j) * A(1:j-1,j) */
                        d__1 = -x[j] * tscal;
                        daxpy_(&j, &d__1, &a[j * *lda], &c__1, x, &c__1);
                        i = idamax_(&j, x, &c__1) - 1;
                        xmax = abs(x[i]);
                    }
                }
                else {
                    if (j+1 < *n)
                    {
                        /* Compute the update                       */
                        /* x(j+1:n) := x(j+1:n) - x(j) * A(j+1:n,j) */
                        i__1 = *n - j - 1;
                        d__1 = -x[j] * tscal;
                        daxpy_(&i__1, &d__1, &a[j + 1 + j * *lda], &c__1, &x[j+1], &c__1);
                        i = j + idamax_(&i__1, &x[j+1], &c__1);
                        xmax = abs(x[i]);
                    }
                }
            }
        }
        else
        {
            /* Solve A' * x = b */
            for (j = jfirst-1; jinc < 0 ? j > jlast-2 : j < jlast; j += jinc)
            {
                /* Compute x(j) = b(j) - sum A(k,j)*x(k). */
                /*                       k!=j             */

                xj = abs(x[j]);
                uscal = tscal;
                rec = 1. / max(xmax,1.);
                if (cnorm[j] > (bignum - xj) * rec)
                {
                    /* If x(j) could overflow, scale x by 1/(2*XMAX). */
                    rec *= .5;
                    if (nounit)
                        tjjs = a[j + j * *lda] * tscal;
                    else
                        tjjs = tscal;
                    tjj = abs(tjjs);
                    if (tjj > 1.)
                    {
                        /* Divide by A(j,j) when scaling x if A(j,j) > 1. */
                        rec = min(1., rec * tjj);
                        uscal /= tjjs;
                    }
                    if (rec < 1.) {
                        dscal_(n, &rec, x, &c__1);
                        *scale *= rec;
                        xmax *= rec;
                    }
                }

                sumj = 0.;
                if (uscal == 1.)
                {
                    /* If the scaling needed for A in the dot product is 1, */
                    /* call DDOT to perform the dot product. */
                    if (upper)
                        sumj = ddot_(&j, &a[j * *lda], &c__1, x, &c__1);
                    else if (j < *n) {
                        i__1 = *n - j - 1;
                        sumj = ddot_(&i__1, &a[j + 1 + j * *lda], &c__1, &x[j+1], &c__1);
                    }
                }
                else
                {
                    /* Otherwise, use in-line code for the dot product. */
                    if (upper)
                        for (i = 0; i < j; ++i)
                            sumj += a[i + j * *lda] * uscal * x[i];
                    else if (j+1 < *n)
                        for (i = j + 1; i < *n; ++i)
                            sumj += a[i + j * *lda] * uscal * x[i];
                }

                if (uscal == tscal)
                {
                   /* Compute x(j) := ( x(j) - sumj ) / A(j,j) if 1/A(j,j) */
                   /* was not used to scale the dotproduct. */
                    x[j] -= sumj;
                    xj = abs(x[j]);
                    if (nounit)
                        tjjs = a[j + j * *lda] * tscal;
                    else {
                        tjjs = tscal;
                        if (tscal == 1.)
                            goto L150;
                    }

                    /* Compute x(j) = x(j) / A(j,j), scaling if necessary. */
                    tjj = abs(tjjs);
                    if (tjj > smlnum)
                    {
                        /* abs(A(j,j)) > SMLNUM: */
                        if (tjj < 1. && xj > tjj * bignum)
                        {
                            /* Scale X by 1/abs(x(j)). */
                            rec = 1. / xj;
                            dscal_(n, &rec, x, &c__1);
                            *scale *= rec;
                            xmax *= rec;
                        }
                        x[j] /= tjjs;
                    }
                    else if (tjj > 0.)
                    {
                        /* 0 < abs(A(j,j)) <= SMLNUM: */
                        if (xj > tjj * bignum)
                        {
                            /* Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM. */
                            rec = tjj * bignum / xj;
                            dscal_(n, &rec, x, &c__1);
                            *scale *= rec;
                            xmax *= rec;
                        }
                        x[j] /= tjjs;
                    }
                    else
                    {
                        /* A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and */
                        /* scale = 0, and compute a solution to A'*x = 0. */
                        for (i = 0; i < *n; ++i)
                            x[i] = 0.;
                        x[j] = 1.;
                        *scale = 0.;
                        xmax = 0.;
                    }
                }
                else
                {
                    /* Compute x(j) := x(j) / A(j,j)  - sumj if the dot */
                    /* product has already been divided by 1/A(j,j). */
                    x[j] = x[j] / tjjs - sumj;
                }
L150:
                xmax = max(xmax, abs(x[j]));
            }
        }
        *scale /= tscal;
    }

    /* Scale the column norms by 1/TSCAL for return. */
    if (tscal != 1.) {
        d__1 = 1. / tscal;
        dscal_(n, &d__1, cnorm, &c__1);
    }
} /* dlatrs_ */

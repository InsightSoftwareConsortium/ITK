#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__1 = 1;
static doublereal c_b34 = 10.;
static doublereal c_b70 = .5;

/* Subroutine */ void dggbal_(job, n, a, lda, b, ldb, ilo, ihi, lscale, rscale, work, info)
const char *job;
const integer *n;
doublereal *a;
const integer *lda;
doublereal *b;
const integer *ldb;
integer *ilo, *ihi;
doublereal *lscale, *rscale, *work;
integer *info;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1;
    doublereal d__1;

    /* Local variables */
    static integer lcab;
    static doublereal beta, coef;
    static integer irab, lrab;
    static doublereal basl, cmax;
    static doublereal coef2, coef5;
    static integer i, j, k, l, m;
    static doublereal gamma, t, alpha;
    static doublereal sfmin, sfmax;
    static integer iflow;
    static integer kount, jc;
    static doublereal ta, tb, tc;
    static integer ir, it;
    static doublereal ew;
    static integer nr;
    static doublereal pgamma;
    static integer lsfmin, lsfmax, ip1, jp1, lm1;
    static doublereal cab, rab, ewc, cor, sum;
    static integer nrp2, icab;


/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DGGBAL balances a pair of general real matrices (A,B).  This          */
/*  involves, first, permuting A and B by similarity transformations to   */
/*  isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N     */
/*  elements on the diagonal; and second, applying a diagonal similarity  */
/*  transformation to rows and columns ILO to IHI to make the rows        */
/*  and columns as close in norm as possible. Both steps are optional.    */
/*                                                                        */
/*  Balancing may reduce the 1-norm of the matrices, and improve the      */
/*  accuracy of the computed eigenvalues and/or eigenvectors in the       */
/*  generalized eigenvalue problem A*x = lambda*B*x.                      */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  JOB     (input) CHARACTER*1                                           */
/*          Specifies the operations to be performed on A and B:          */
/*          = 'N':  none:  simply set ILO = 1, IHI = N, LSCALE(I) = 1.0   */
/*                  and RSCALE(I) = 1.0 for i = 1,...,N.                  */
/*          = 'P':  permute only;                                         */
/*          = 'S':  scale only;                                           */
/*          = 'B':  both permute and scale.                               */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrices A and B.  N >= 0.                   */
/*                                                                        */
/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)      */
/*          On entry, the input matrix A.                                 */
/*          On exit,  A is overwritten by the balanced matrix.            */
/*          If JOB = 'N', A is not referenced.                            */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A. LDA >= max(1,N).        */
/*                                                                        */
/*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)      */
/*          On entry, the input matrix B.                                 */
/*          On exit,  B is overwritten by the balanced matrix.            */
/*          If JOB = 'N', B is not referenced.                            */
/*                                                                        */
/*  LDB     (input) INTEGER                                               */
/*          The leading dimension of the array B. LDB >= max(1,N).        */
/*                                                                        */
/*  ILO     (output) INTEGER                                              */
/*  IHI     (output) INTEGER                                              */
/*          ILO and IHI are set to integers such that on exit             */
/*          A(i,j) = 0 and B(i,j) = 0 if i > j and                        */
/*          j = 1,...,ILO-1 or i = IHI+1,...,N.                           */
/*          If JOB = 'N' or 'S', ILO = 1 and IHI = N.                     */
/*                                                                        */
/*  LSCALE  (output) DOUBLE PRECISION array, dimension (N)                */
/*          Details of the permutations and scaling factors applied       */
/*          to the left side of A and B.  If P(j) is the index of the     */
/*          row interchanged with row j, and D(j)                         */
/*          is the scaling factor applied to row j, then                  */
/*            LSCALE(j) = P(j)    for J = 1,...,ILO-1                     */
/*                      = D(j)    for J = ILO,...,IHI                     */
/*                      = P(j)    for J = IHI+1,...,N.                    */
/*          The order in which the interchanges are made is N to IHI+1,   */
/*          then 1 to ILO-1.                                              */
/*                                                                        */
/*  RSCALE  (output) DOUBLE PRECISION array, dimension (N)                */
/*          Details of the permutations and scaling factors applied       */
/*          to the right side of A and B.  If P(j) is the index of the    */
/*          column interchanged with column j, and D(j)                   */
/*          is the scaling factor applied to column j, then               */
/*            LSCALE(j) = P(j)    for J = 1,...,ILO-1                     */
/*                      = D(j)    for J = ILO,...,IHI                     */
/*                      = P(j)    for J = IHI+1,...,N.                    */
/*          The order in which the interchanges are made is N to IHI+1,   */
/*          then 1 to ILO-1.                                              */
/*                                                                        */
/*  WORK    (workspace) DOUBLE PRECISION array, dimension (6*N)           */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0:  successful exit                                         */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value.   */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  See R.C. WARD, Balancing the generalized eigenvalue problem,          */
/*                 SIAM J. Sci. Stat. Comp. 2 (1981), 141-152.            */
/*                                                                        */
/*  ===================================================================== */

    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    --lscale;
    --rscale;
    --work;

/*     Test the input parameters */

    *info = 0;
    if (! lsame_(job, "N") && ! lsame_(job, "P") && ! lsame_(job, "S") && ! lsame_(job, "B")) {
        *info = -1;
    } else if (*n < 0) {
        *info = -2;
    } else if (*lda < max(1,*n)) {
        *info = -4;
    } else if (*ldb < max(1,*n)) {
        *info = -5;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("DGGBAL", &i__1);
        return;
    }

    k = 1;
    l = *n;

/*     Quick return if possible */

    if (*n == 0) {
        return;
    }

    if (lsame_(job, "N")) {
        *ilo = 1;
        *ihi = *n;
        for (i = 1; i <= *n; ++i) {
            lscale[i] = 1.;
            rscale[i] = 1.;
        }
        return;
    }

    if (k == l) {
        *ilo = 1;
        *ihi = 1;
        lscale[1] = 1.;
        rscale[1] = 1.;
        return;
    }

    if (lsame_(job, "S")) {
        goto L190;
    }

    goto L30;

/*     Permute the matrices A and B to isolate the eigenvalues. */

/*     Find row with one nonzero in columns 1 through L */

L20:
    l = lm1;
    if (l != 1) {
        goto L30;
    }

    rscale[1] = 1.;
    lscale[1] = 1.;
    goto L190;

L30:
    lm1 = l - 1;
    for (i = l; i >= 1; --i) {
        for (j = 1; j <= lm1; ++j) {
            jp1 = j + 1;
            if (a[i + j * a_dim1] != 0. || b[i + j * b_dim1] != 0.) {
                goto L50;
            }
        }
        j = l;
        goto L70;

L50:
        for (j = jp1; j <= l; ++j) {
            if (a[i + j * a_dim1] != 0. || b[i + j * b_dim1] != 0.) {
                goto L80;
            }
        }
        j = jp1 - 1;

L70:
        m = l;
        iflow = 1;
        goto L160;
L80:
        ;
    }
    goto L100;

/*     Find column with one nonzero in rows K through N */

L90:
    ++k;

L100:
    for (j = k; j <= l; ++j) {
        for (i = k; i <= lm1; ++i) {
            ip1 = i + 1;
            if (a[i + j * a_dim1] != 0. || b[i + j * b_dim1] != 0.) {
                goto L120;
            }
        }
        i = l;
        goto L140;
L120:
        for (i = ip1; i <= l; ++i) {
            if (a[i + j * a_dim1] != 0. || b[i + j * b_dim1] != 0.) {
                goto L150;
            }
        }
        i = ip1 - 1;
L140:
        m = k;
        iflow = 2;
        goto L160;
L150:
        ;
    }
    goto L190;

/*     Permute rows M and I */

L160:
    lscale[m] = (doublereal) i;
    if (i == m) {
        goto L170;
    }
    i__1 = *n - k + 1;
    dswap_(&i__1, &a[i + k * a_dim1], lda, &a[m + k * a_dim1], lda);
    dswap_(&i__1, &b[i + k * b_dim1], ldb, &b[m + k * b_dim1], ldb);

/*     Permute columns M and J */

L170:
    rscale[m] = (doublereal) j;
    if (j == m) {
        goto L180;
    }
    dswap_(&l, &a[j * a_dim1 + 1], &c__1, &a[m * a_dim1 + 1], &c__1);
    dswap_(&l, &b[j * b_dim1 + 1], &c__1, &b[m * b_dim1 + 1], &c__1);

L180:
    switch ((int)iflow) {
        case 1:  goto L20;
        case 2:  goto L90;
    }

L190:
    *ilo = k;
    *ihi = l;

    if (*ilo == *ihi) {
        return;
    }

    if (lsame_(job, "P")) {
        return;
    }

/*     Balance the submatrix in rows ILO to IHI. */

    nr = *ihi - *ilo + 1;
    for (i = *ilo; i <= *ihi; ++i) {
        rscale[i] = 0.;
        lscale[i] = 0.;

        work[i] = 0.;
        work[i + *n] = 0.;
        work[i + *n * 2] = 0.;
        work[i + *n * 3] = 0.;
        work[i + *n * 4] = 0.;
        work[i + *n * 5] = 0.;
    }

/*     Compute right side vector in resulting linear equations */

    basl = d_lg10(&c_b34);
    for (i = *ilo; i <= *ihi; ++i) {
        for (j = *ilo; j <= *ihi; ++j) {
            tb = b[i + j * b_dim1];
            ta = a[i + j * a_dim1];
            if (ta == 0.) {
                goto L210;
            }
            d__1 = abs(ta);
            ta = d_lg10(&d__1) / basl;
L210:
            if (tb == 0.) {
                goto L220;
            }
            d__1 = abs(tb);
            tb = d_lg10(&d__1) / basl;
L220:
            work[i + *n * 4] -= ta + tb;
            work[j + *n * 5] -= ta + tb;
        }
    }

    coef = 1. / (doublereal) (nr << 1);
    coef2 = coef * coef;
    coef5 = coef2 * .5;
    nrp2 = nr + 2;
    beta = 0.;
    it = 1;

/*     Start generalized conjugate gradient iteration */

L250:

    gamma = ddot_(&nr, &work[*ilo + *n * 4], &c__1, &work[*ilo + *n * 4], &c__1)
          + ddot_(&nr, &work[*ilo + *n * 5], &c__1, &work[*ilo + *n * 5], &c__1);

    ew = 0.;
    ewc = 0.;
    for (i = *ilo; i <= *ihi; ++i) {
        ew +=  work[i + *n * 4];
        ewc += work[i + *n * 5];
    }

    gamma = coef * gamma - coef2 * (ew*ew + ewc*ewc) - coef5 * ((ew-ewc)*(ew-ewc));
    if (gamma == 0.) {
        goto L350;
    }
    if (it != 1) {
        beta = gamma / pgamma;
    }
    t = coef5 * (ewc - ew * 3.);
    tc = coef5 * (ew - ewc * 3.);

    dscal_(&nr, &beta, &work[*ilo], &c__1);
    dscal_(&nr, &beta, &work[*ilo + *n], &c__1);

    daxpy_(&nr, &coef, &work[*ilo + *n * 4], &c__1, &work[*ilo + *n], &c__1);
    daxpy_(&nr, &coef, &work[*ilo + *n * 5], &c__1, &work[*ilo], &c__1);

    for (i = *ilo; i <= *ihi; ++i) {
        work[i] += tc;
        work[i + *n] += t;
    }

/*     Apply matrix to vector */

    for (i = *ilo; i <= *ihi; ++i) {
        kount = 0;
        sum = 0.;
        for (j = *ilo; j <= *ihi; ++j) {
            if (a[i + j * a_dim1] == 0.) {
                goto L280;
            }
            ++kount;
            sum += work[j];
L280:
            if (b[i + j * b_dim1] == 0.) {
                goto L290;
            }
            ++kount;
            sum += work[j];
L290:
            ;
        }
        work[i + *n * 2] = (doublereal) kount * work[i + *n] + sum;
    }

    for (j = *ilo; j <= *ihi; ++j) {
        kount = 0;
        sum = 0.;
        for (i = *ilo; i <= *ihi; ++i) {
            if (a[i + j * a_dim1] == 0.) {
                goto L310;
            }
            ++kount;
            sum += work[i + *n];
L310:
            if (b[i + j * b_dim1] == 0.) {
                goto L320;
            }
            ++kount;
            sum += work[i + *n];
L320:
            ;
        }
        work[j + *n * 3] = (doublereal) kount * work[j] + sum;
    }

    sum = ddot_(&nr, &work[*ilo + *n], &c__1, &work[*ilo + *n * 2], &c__1)
        + ddot_(&nr, &work[*ilo     ], &c__1, &work[*ilo + *n * 3], &c__1);
    alpha = gamma / sum;

/*     Determine correction to current iteration */

    cmax = 0.;
    for (i = *ilo; i <= *ihi; ++i) {
        cor = alpha * work[i + *n];
        if (abs(cor) > cmax) {
            cmax = abs(cor);
        }
        lscale[i] += cor;
        cor = alpha * work[i];
        if (abs(cor) > cmax) {
            cmax = abs(cor);
        }
        rscale[i] += cor;
    }
    if (cmax < .5) {
        goto L350;
    }

    d__1 = -alpha;
    daxpy_(&nr, &d__1, &work[*ilo + *n * 2], &c__1, &work[*ilo + *n * 4], &c__1);
    d__1 = -alpha;
    daxpy_(&nr, &d__1, &work[*ilo + *n * 3], &c__1, &work[*ilo + *n * 5], &c__1);

    pgamma = gamma;
    ++it;
    if (it <= nrp2) {
        goto L250;
    }

/*     End generalized conjugate gradient iteration */

L350:
    sfmin = dlamch_("S");
    sfmax = 1. / sfmin;
    lsfmin = (integer) (d_lg10(&sfmin) / basl + 1.);
    lsfmax = (integer) (d_lg10(&sfmax) / basl);
    for (i = *ilo; i <= *ihi; ++i) {
        i__1 = *n - *ilo + 1;
        irab = idamax_(&i__1, &a[i + *ilo * a_dim1], lda);
        rab = abs(a[i + (irab + *ilo - 1) * a_dim1]);
        irab = idamax_(&i__1, &b[i + *ilo * b_dim1], lda);
        rab = max(rab, abs(b[i + (irab + *ilo - 1) * b_dim1]));
        d__1 = rab + sfmin;
        lrab = (integer) (d_lg10(&d__1) / basl + 1.);
        ir = (integer) (lscale[i] + d_sign(&c_b70, &lscale[i]));
        ir = min(min(max(ir,lsfmin),lsfmax),lsfmax-lrab);
        lscale[i] = pow_di(&c_b34, &ir);
        icab = idamax_(ihi, &a[i * a_dim1 + 1], &c__1);
        cab = abs(a[icab + i * a_dim1]);
        icab = idamax_(ihi, &b[i * b_dim1 + 1], &c__1);
        cab = max(cab, abs(b[icab + i * b_dim1]));
        d__1 = cab + sfmin;
        lcab = (integer) (d_lg10(&d__1) / basl + 1.);
        jc = (integer) (rscale[i] + d_sign(&c_b70, &rscale[i]));
        jc = min(min(max(jc,lsfmin),lsfmax),lsfmax-lcab);
        rscale[i] = pow_di(&c_b34, &jc);
    }

/*     Row scaling of matrices A and B */

    for (i = *ilo; i <= *ihi; ++i) {
        i__1 = *n - *ilo + 1;
        dscal_(&i__1, &lscale[i], &a[i + *ilo * a_dim1], lda);
        dscal_(&i__1, &lscale[i], &b[i + *ilo * b_dim1], ldb);
    }

/*     Column scaling of matrices A and B */

    for (j = *ilo; j <= *ihi; ++j) {
        dscal_(ihi, &rscale[j], &a[j * a_dim1 + 1], &c__1);
        dscal_(ihi, &rscale[j], &b[j * b_dim1 + 1], &c__1);
    }
} /* dggbal_ */

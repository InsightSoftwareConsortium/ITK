#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static doublecomplex c_b4 = {-1.,0.};
static doublecomplex c_b5 = {1.,0.};
static integer c__1 = 1;
static doublecomplex c_b39 = {0.,0.};

/* Subroutine */ void zlahrd_(n, k, nb, a, lda, tau, t, ldt, y, ldy)
const integer *n, *k, *nb;
doublecomplex *a;
const integer *lda;
doublecomplex *tau, *t;
const integer *ldt;
doublecomplex *y;
const integer *ldy;
{
    /* System generated locals */
    integer i__1;
    doublecomplex z__1;

    /* Local variables */
    static integer i;
    static doublecomplex ei;

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZLAHRD reduces the first NB columns of a complex general n-by-(n-k+1) */
/*  matrix A so that elements below the k-th subdiagonal are zero. The    */
/*  reduction is performed by a unitary similarity transformation         */
/*  Q' * A * Q. The routine returns the matrices V and T which determine  */
/*  Q as a block reflector I - V*T*V', and also the matrix Y = A * V * T. */
/*                                                                        */
/*  This is an auxiliary routine called by ZGEHRD.                        */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrix A.                                    */
/*                                                                        */
/*  K       (input) INTEGER                                               */
/*          The offset for the reduction. Elements below the k-th         */
/*          subdiagonal in the first NB columns are reduced to zero.      */
/*                                                                        */
/*  NB      (input) INTEGER                                               */
/*          The number of columns to be reduced.                          */
/*                                                                        */
/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N-K+1)        */
/*          On entry, the n-by-(n-k+1) general matrix A.                  */
/*          On exit, the elements on and above the k-th subdiagonal in    */
/*          the first NB columns are overwritten with the corresponding   */
/*          elements of the reduced matrix; the elements below the k-th   */
/*          subdiagonal, with the array TAU, represent the matrix Q as a  */
/*          product of elementary reflectors. The other columns of A are  */
/*          unchanged. See Further Details.                               */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max(1,N).       */
/*                                                                        */
/*  TAU     (output) COMPLEX*16 array, dimension (NB)                     */
/*          The scalar factors of the elementary reflectors. See Further  */
/*          Details.                                                      */
/*                                                                        */
/*  T       (output) COMPLEX*16 array, dimension (NB,NB)                  */
/*          The upper triangular matrix T.                                */
/*                                                                        */
/*  LDT     (input) INTEGER                                               */
/*          The leading dimension of the array T.  LDT >= NB.             */
/*                                                                        */
/*  Y       (output) COMPLEX*16 array, dimension (LDY,NB)                 */
/*          The n-by-nb matrix Y.                                         */
/*                                                                        */
/*  LDY     (input) INTEGER                                               */
/*          The leading dimension of the array Y. LDY >= max(1,N).        */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  The matrix Q is represented as a product of nb elementary reflectors  */
/*                                                                        */
/*     Q = H(1) H(2) . . . H(nb).                                         */
/*                                                                        */
/*  Each H(i) has the form                                                */
/*                                                                        */
/*     H(i) = I - tau * v * v'                                            */
/*                                                                        */
/*  where tau is a complex scalar, and v is a complex vector with         */
/*  v(1:i+k-1) = 0, v(i+k) = 1; v(i+k+1:n) is stored on exit in           */
/*  A(i+k+1:n,i), and tau in TAU(i).                                      */
/*                                                                        */
/*  The elements of the vectors v together form the (n-k+1)-by-nb matrix  */
/*  V which is needed, with T and Y, to apply the transformation to the   */
/*  unreduced part of the matrix, using an update of the form:            */
/*  A := (I - V*T*V') * (A - Y*V').                                       */
/*                                                                        */
/*  The contents of A on exit are illustrated by the following example    */
/*  with n = 7, k = 3 and nb = 2:                                         */
/*                                                                        */
/*     ( a   h   a   a   a )                                              */
/*     ( a   h   a   a   a )                                              */
/*     ( a   h   a   a   a )                                              */
/*     ( h   h   a   a   a )                                              */
/*     ( v1  h   a   a   a )                                              */
/*     ( v1  v2  a   a   a )                                              */
/*     ( v1  v2  a   a   a )                                              */
/*                                                                        */
/*  where a denotes an element of the original matrix A, h denotes a      */
/*  modified element of the upper Hessenberg matrix H, and vi denotes an  */
/*  element of the vector defining H(i).                                  */
/*                                                                        */
/*  ===================================================================== */

/*     Quick return if possible */

    if (*n <= 1) {
        return;
    }

    for (i = 0; i < *nb; ++i) {
        if (i > 0) {

/*           Update A(1:n,i) */

/*           Compute i-th column of A - Y * V' */

            zlacgv_(&i, &a[*k + i - 1], lda);
            zgemv_("No transpose", n, &i, &c_b4, y, ldy,
                   &a[*k + i - 1], lda, &c_b5, &a[i * *lda], &c__1);
            zlacgv_(&i, &a[*k + i - 1], lda);

/*           Apply I - V * T' * V' to this column (call it b) from the */
/*           left, using the last column of T as workspace */

/*           Let  V = ( V1 )   and   b = ( b1 )   (first I-1 rows) */
/*                    ( V2 )             ( b2 ) */

/*           where V1 is unit lower triangular */

/*           w := V1' * b1 */

            zcopy_(&i, &a[*k+i* *lda], &c__1, &t[(*nb-1)* *ldt], &c__1);
            ztrmv_("Lower", "Conjugate transpose", "Unit", &i, &a[*k], lda, &t[(*nb-1) * *ldt], &c__1);

/*           w := w + V2'*b2 */

            i__1 = *n - *k - i;
            zgemv_("Conjugate transpose", &i__1, &i, &c_b5,
                   &a[*k-1+i+*lda], lda, &a[*k+i+i* *lda], &c__1,
                   &c_b5, &t[(*nb-1) * *ldt], &c__1);

/*           w := T'*w */

            ztrmv_("Upper", "Conjugate transpose", "Non-unit", &i, t, ldt, &t[(*nb-1) * *ldt], &c__1);

/*           b2 := b2 - V2*w */

            i__1 = *n - *k - i;
            zgemv_("No transpose", &i__1, &i, &c_b4, &a[*k-1+i+ *lda], lda,
                   &t[(*nb-1)* *ldt], &c__1, &c_b5, &a[*k+i+i* *lda], &c__1);

/*           b1 := b1 - V1*w */

            ztrmv_("Lower", "No transpose", "Unit", &i, &a[*k], lda, &t[(*nb-1) * *ldt], &c__1);
            zaxpy_(&i, &c_b4, &t[(*nb-1) * *ldt], &c__1, &a[*k+i* *lda], &c__1);

            i__1 = *k + i - 1 + (i - 1) * *lda;
            a[i__1].r = ei.r, a[i__1].i = ei.i;
        }

/*        Generate the elementary reflector H(i) to annihilate */
/*        A(k+i+1:n,i) */

        i__1 = *k + i + i * *lda;
        ei.r = a[i__1].r, ei.i = a[i__1].i;
        i__1 = *n - *k - i;
        zlarfg_(&i__1, &ei, &a[min(*k+i+1, *n-1)+i* *lda], &c__1, &tau[i]);
        i__1 = *k + i + i * *lda;
        a[i__1].r = 1., a[i__1].i = 0.;

/*        Compute  Y(1:n,i) */

        i__1 = *n - *k - i;
        zgemv_("No transpose", n, &i__1, &c_b5, &a[(i+1) * *lda], lda,
               &a[*k+i+i* *lda], &c__1, &c_b39, &y[i* *ldy], &c__1);
        zgemv_("Conjugate transpose", &i__1, &i, &c_b5, &a[*k+i],
               lda, &a[*k+i+i* *lda], &c__1, &c_b39, &t[i* *ldt], &c__1);
        zgemv_("No transpose", n, &i, &c_b4, y, ldy, &t[i* *ldt],
               &c__1, &c_b5, &y[i* *ldy], &c__1);
        zscal_(n, &tau[i], &y[i* *ldy], &c__1);

/*        Compute T(1:i,i) */

        z__1.r = -tau[i].r, z__1.i = -tau[i].i;
        zscal_(&i, &z__1, &t[i* *ldt], &c__1);
        ztrmv_("Upper", "No transpose", "Non-unit", &i, t, ldt, &t[i* *ldt], &c__1);
        i__1 = i + i * *ldt;
        t[i__1].r = tau[i].r, t[i__1].i = tau[i].i;
    }
    i__1 = *k + (*nb-1) + (*nb-1) * *lda;
    a[i__1].r = ei.r, a[i__1].i = ei.i;
} /* zlahrd_ */

/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublecomplex c_b9 = {0.,0.};
static doublecomplex c_b10 = {1.,0.};
static integer c__1 = 1;
static integer c__4 = 4;
static integer c_n1 = -1;
static integer c__2 = 2;
static integer c__8 = 8;
static integer c__15 = 15;
static integer c__0 = 0;

/* Subroutine */ int zhseqr_(job, compz, n, ilo, ihi, h, ldh, w, z, ldz, work,
         lwork, info, job_len, compz_len)
char *job, *compz;
integer *n, *ilo, *ihi;
doublecomplex *h;
integer *ldh;
doublecomplex *w, *z;
integer *ldz;
doublecomplex *work;
integer *lwork, *info;
ftnlen job_len;
ftnlen compz_len;
{
    /* System generated locals */
    address a__1[2];
    integer h_dim1, h_offset, z_dim1, z_offset, i__1, i__2, i__3, i__4[2],
            i__5, i__6;
    doublereal d__1, d__2, d__3, d__4;
    doublecomplex z__1;
    char ch__1[2];

    /* Builtin functions */
    double d_imag();
    void d_cnjg();
    /* Subroutine */ int s_cat();

    /* Local variables */
    static integer maxb, ierr;
    static doublereal unfl;
    static doublecomplex temp;
    static doublereal ovfl;
    static integer i, j, k, l;
    static doublecomplex s[225] /* was [15][15] */, v[16];
    extern logical lsame_();
    extern /* Subroutine */ int zscal_();
    static integer itemp;
    static doublereal rtemp;
    static integer i1, i2;
    extern /* Subroutine */ int zgemv_();
    static logical initz, wantt, wantz;
    static doublereal rwork[1];
    extern /* Subroutine */ int zcopy_();
    extern doublereal dlapy2_();
    extern /* Subroutine */ int dlabad_();
    static integer ii, nh;
    extern doublereal dlamch_();
    static integer nr, ns, nv;
    static doublecomplex vv[16];
    extern /* Subroutine */ int xerbla_();
    extern integer ilaenv_();
    extern /* Subroutine */ int zdscal_(), zlarfg_();
    extern integer izamax_();
    extern doublereal zlanhs_();
    extern /* Subroutine */ int zlahqr_(), zlacpy_(), zlaset_(), zlarfx_();
    static doublereal smlnum;
    static integer itn;
    static doublecomplex tau;
    static integer its;
    static doublereal ulp, tst1;


/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZHSEQR computes the eigenvalues of a complex upper Hessenberg */
/*  matrix H, and, optionally, the matrices T and Z from the Schur */
/*  decomposition H = Z T Z**H, where T is an upper triangular matrix */
/*  (the Schur form), and Z is the unitary matrix of Schur vectors. */

/*  Optionally Z may be postmultiplied into an input unitary matrix Q, */
/*  so that this routine can give the Schur factorization of a matrix A */
/*  which has been reduced to the Hessenberg form H by the unitary */
/*  matrix Q:  A = Q*H*Q**H = (QZ)*T*(QZ)**H. */

/*  Arguments */
/*  ========= */

/*  JOB     (input) CHARACTER*1 */
/*          = 'E': compute eigenvalues only; */
/*          = 'S': compute eigenvalues and the Schur form T. */

/*  COMPZ   (input) CHARACTER*1 */
/*          = 'N': no Schur vectors are computed; */
/*          = 'I': Z is initialized to the unit matrix and the matrix Z */
/*                 of Schur vectors of H is returned; */
/*          = 'V': Z must contain an unitary matrix Q on entry, and */
/*                 the product Q*Z is returned. */

/*  N       (input) INTEGER */
/*          The order of the matrix H.  N >= 0. */

/*  ILO     (input) INTEGER */
/*  IHI     (input) INTEGER */
/*          It is assumed that H is already upper triangular in rows */
/*          and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally */
/*          set by a previous call to ZGEBAL, and then passed to CGEHRD */
/*          when the matrix output by ZGEBAL is reduced to Hessenberg */
/*          form. Otherwise ILO and IHI should be set to 1 and N */
/*          respectively. */
/*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0. */

/*  H       (input/output) COMPLEX*16 array, dimension (LDH,N) */
/*          On entry, the upper Hessenberg matrix H. */
/*          On exit, if JOB = 'S', H contains the upper triangular matrix
*/
/*          T from the Schur decomposition (the Schur form). If */
/*          JOB = 'E', the contents of H are unspecified on exit. */

/*  LDH     (input) INTEGER */
/*          The leading dimension of the array H. LDH >= max(1,N). */

/*  W       (output) COMPLEX*16 array, dimension (N) */
/*          The computed eigenvalues. If JOB = 'S', the eigenvalues are */
/*          stored in the same order as on the diagonal of the Schur form
*/
/*          returned in H, with W(i) = H(i,i). */

/*  Z       (input/output) COMPLEX*16 array, dimension (LDZ,N) */
/*          If COMPZ = 'N': Z is not referenced. */
/*          If COMPZ = 'I': on entry, Z need not be set, and on exit, Z */
/*          contains the unitary matrix Z of the Schur vectors of H. */
/*          If COMPZ = 'V': on entry Z must contain an N-by-N matrix Q, */
/*          which is assumed to be equal to the unit matrix except for */
/*          the submatrix Z(ILO:IHI,ILO:IHI); on exit Z contains Q*Z. */
/*          Normally Q is the unitary matrix generated by ZUNGHR after */
/*          the call to ZGEHRD which formed the Hessenberg matrix H. */

/*  LDZ     (input) INTEGER */
/*          The leading dimension of the array Z. */
/*          LDZ >= max(1,N) if COMPZ = 'I' or 'V'; LDZ >= 1 otherwise. */

/*  WORK    (workspace) COMPLEX*16 array, dimension (N) */

/*  LWORK   (input) INTEGER */
/*          This argument is currently redundant. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value */
/*          > 0:  if INFO = i, ZHSEQR failed to compute all the */
/*                eigenvalues in a total of 30*(IHI-ILO+1) iterations; */
/*                elements 1:ilo-1 and i+1:n of W contain those */
/*                eigenvalues which have been successfully computed. */

/*  =====================================================================
*/

/*     .. Parameters .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Local Arrays .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Statement Functions .. */
/*     .. */
/*     .. Statement Function definitions .. */
/*     .. */
/*     .. Executable Statements .. */

/*     Decode and test the input parameters */

    /* Parameter adjustments */
    --work;
    z_dim1 = *ldz;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --w;
    h_dim1 = *ldh;
    h_offset = h_dim1 + 1;
    h -= h_offset;

    /* Function Body */
    wantt = lsame_(job, "S", 1L, 1L);
    initz = lsame_(compz, "I", 1L, 1L);
    wantz = initz || lsame_(compz, "V", 1L, 1L);

    *info = 0;
    if (! lsame_(job, "E", 1L, 1L) && ! wantt) {
        *info = -1;
    } else if (! lsame_(compz, "N", 1L, 1L) && ! wantz) {
        *info = -2;
    } else if (*n < 0) {
        *info = -3;
    } else if (*ilo < 1 || *ilo > max(1,*n)) {
        *info = -4;
    } else if (*ihi < min(*ilo,*n) || *ihi > *n) {
        *info = -5;
    } else if (*ldh < max(1,*n)) {
        *info = -7;
    } else if (*ldz < 1 || wantz && *ldz < max(1,*n)) {
        *info = -10;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("ZHSEQR", &i__1, 6L);
        return 0;
    }

/*     Initialize Z, if necessary */

    if (initz) {
        zlaset_("Full", n, n, &c_b9, &c_b10, &z[z_offset], ldz, 4L);
    }

/*     Store the eigenvalues isolated by ZGEBAL. */

    i__1 = *ilo - 1;
    for (i = 1; i <= i__1; ++i) {
        i__2 = i;
        i__3 = i + i * h_dim1;
        w[i__2].r = h[i__3].r, w[i__2].i = h[i__3].i;
/* L10: */
    }
    i__1 = *n;
    for (i = *ihi + 1; i <= i__1; ++i) {
        i__2 = i;
        i__3 = i + i * h_dim1;
        w[i__2].r = h[i__3].r, w[i__2].i = h[i__3].i;
/* L20: */
    }

/*     Quick return if possible. */

    if (*n == 0) {
        return 0;
    }
    if (*ilo == *ihi) {
        i__1 = *ilo;
        i__2 = *ilo + *ilo * h_dim1;
        w[i__1].r = h[i__2].r, w[i__1].i = h[i__2].i;
        return 0;
    }

/*     Set rows and columns ILO to IHI to zero below the first */
/*     subdiagonal. */

    i__1 = *ihi - 2;
    for (j = *ilo; j <= i__1; ++j) {
        i__2 = *n;
        for (i = j + 2; i <= i__2; ++i) {
            i__3 = i + j * h_dim1;
            h[i__3].r = 0., h[i__3].i = 0.;
/* L30: */
        }
/* L40: */
    }
    nh = *ihi - *ilo + 1;

/*     I1 and I2 are the indices of the first row and last column of H */
/*     to which transformations must be applied. If eigenvalues only are
*/
/*     being computed, I1 and I2 are re-set inside the main loop. */

    if (wantt) {
        i1 = 1;
        i2 = *n;
    } else {
        i1 = *ilo;
        i2 = *ihi;
    }

/*     Ensure that the subdiagonal elements are real. */

    i__1 = *ihi;
    for (i = *ilo + 1; i <= i__1; ++i) {
        i__2 = i + (i - 1) * h_dim1;
        temp.r = h[i__2].r, temp.i = h[i__2].i;
        if (d_imag(&temp) != 0.) {
            d__1 = temp.r;
            d__2 = d_imag(&temp);
            rtemp = dlapy2_(&d__1, &d__2);
            i__2 = i + (i - 1) * h_dim1;
            h[i__2].r = rtemp, h[i__2].i = 0.;
            z__1.r = temp.r / rtemp, z__1.i = temp.i / rtemp;
            temp.r = z__1.r, temp.i = z__1.i;
            if (i2 > i) {
                i__2 = i2 - i;
                d_cnjg(&z__1, &temp);
                zscal_(&i__2, &z__1, &h[i + (i + 1) * h_dim1], ldh);
            }
            i__2 = i - i1;
            zscal_(&i__2, &temp, &h[i1 + i * h_dim1], &c__1);
            if (i < *ihi) {
                i__2 = i + 1 + i * h_dim1;
                i__3 = i + 1 + i * h_dim1;
                z__1.r = temp.r * h[i__3].r - temp.i * h[i__3].i, z__1.i =
                        temp.r * h[i__3].i + temp.i * h[i__3].r;
                h[i__2].r = z__1.r, h[i__2].i = z__1.i;
            }
            if (wantz) {
                zscal_(&nh, &temp, &z[*ilo + i * z_dim1], &c__1);
            }
        }
/* L50: */
    }

/*     Determine the order of the multi-shift QR algorithm to be used. */

/* Writing concatenation */
    i__4[0] = 1, a__1[0] = job;
    i__4[1] = 1, a__1[1] = compz;
    s_cat(ch__1, a__1, i__4, &c__2, 2L);
    ns = ilaenv_(&c__4, "ZHSEQR", ch__1, n, ilo, ihi, &c_n1, 6L, 2L);
/* Writing concatenation */
    i__4[0] = 1, a__1[0] = job;
    i__4[1] = 1, a__1[1] = compz;
    s_cat(ch__1, a__1, i__4, &c__2, 2L);
    maxb = ilaenv_(&c__8, "ZHSEQR", ch__1, n, ilo, ihi, &c_n1, 6L, 2L);
    if (ns <= 1 || ns > nh || maxb >= nh) {

/*        Use the standard double-shift algorithm */

        zlahqr_(&wantt, &wantz, n, ilo, ihi, &h[h_offset], ldh, &w[1], ilo,
                ihi, &z[z_offset], ldz, info);
        return 0;
    }
    maxb = max(2,maxb);
/* Computing MIN */
    i__1 = min(ns,maxb);
    ns = min(i__1,15);

/*     Now 1 < NS <= MAXB < NH. */

/*     Set machine-dependent constants for the stopping criterion. */
/*     If norm(H) <= sqrt(OVFL), overflow should not occur. */

    unfl = dlamch_("Safe minimum", 12L);
    ovfl = 1. / unfl;
    dlabad_(&unfl, &ovfl);
    ulp = dlamch_("Precision", 9L);
    smlnum = unfl * (nh / ulp);

/*     ITN is the total number of multiple-shift QR iterations allowed. */

    itn = nh * 30;

/*     The main loop begins here. I is the loop index and decreases from
*/
/*     IHI to ILO in steps of at most MAXB. Each iteration of the loop */
/*     works with the active submatrix in rows and columns L to I. */
/*     Eigenvalues I+1 to IHI have already converged. Either L = ILO, or
*/
/*     H(L,L-1) is negligible so that the matrix splits. */

    i = *ihi;
L60:
    if (i < *ilo) {
        goto L180;
    }

/*     Perform multiple-shift QR iterations on rows and columns ILO to I
*/
/*     until a submatrix of order at most MAXB splits off at the bottom */
/*     because a subdiagonal element has become negligible. */

    l = *ilo;
    i__1 = itn;
    for (its = 0; its <= i__1; ++its) {

/*        Look for a single small subdiagonal element. */

        i__2 = l + 1;
        for (k = i; k >= i__2; --k) {
            i__3 = k - 1 + (k - 1) * h_dim1;
            i__5 = k + k * h_dim1;
            tst1 = (d__1 = h[i__3].r, abs(d__1)) + (d__2 = d_imag(&h[k - 1 + (
                    k - 1) * h_dim1]), abs(d__2)) + ((d__3 = h[i__5].r, abs(
                    d__3)) + (d__4 = d_imag(&h[k + k * h_dim1]), abs(d__4)));
            if (tst1 == 0.) {
                i__3 = i - l + 1;
                tst1 = zlanhs_("1", &i__3, &h[l + l * h_dim1], ldh, rwork, 1L)
                        ;
            }
            i__3 = k + (k - 1) * h_dim1;
/* Computing MAX */
            d__2 = ulp * tst1;
            if ((d__1 = h[i__3].r, abs(d__1)) <= max(d__2,smlnum)) {
                goto L80;
            }
/* L70: */
        }
L80:
        l = k;
        if (l > *ilo) {

/*           H(L,L-1) is negligible. */

            i__2 = l + (l - 1) * h_dim1;
            h[i__2].r = 0., h[i__2].i = 0.;
        }

/*        Exit from loop if a submatrix of order <= MAXB has split off
. */

        if (l >= i - maxb + 1) {
            goto L170;
        }

/*        Now the active submatrix is in rows and columns L to I. If
*/
/*        eigenvalues only are being computed, only the active submatr
ix */
/*        need be transformed. */

        if (! wantt) {
            i1 = l;
            i2 = i;
        }

        if (its == 20 || its == 30) {

/*           Exceptional shifts. */

            i__2 = i;
            for (ii = i - ns + 1; ii <= i__2; ++ii) {
                i__3 = ii;
                i__5 = ii + (ii - 1) * h_dim1;
                i__6 = ii + ii * h_dim1;
                d__3 = ((d__1 = h[i__5].r, abs(d__1)) + (d__2 = h[i__6].r,
                        abs(d__2))) * 1.5;
                w[i__3].r = d__3, w[i__3].i = 0.;
/* L90: */
            }
        } else {

/*           Use eigenvalues of trailing submatrix of order NS as
shifts. */

            zlacpy_("Full", &ns, &ns, &h[i - ns + 1 + (i - ns + 1) * h_dim1],
                    ldh, s, &c__15, 4L);
            zlahqr_(&c__0, &c__0, &ns, &c__1, &ns, s, &c__15, &w[i - ns + 1],
                    &c__1, &ns, &z[z_offset], ldz, &ierr);
            if (ierr > 0) {

/*              If ZLAHQR failed to compute all NS eigenvalues
, use the */
/*              unconverged diagonal elements as the remaining
 shifts. */

                i__2 = ierr;
                for (ii = 1; ii <= i__2; ++ii) {
                    i__3 = i - ns + ii;
                    i__5 = ii + ii * 15 - 16;
                    w[i__3].r = s[i__5].r, w[i__3].i = s[i__5].i;
/* L100: */
                }
            }
        }

/*        Form the first column of (G-w(1)) (G-w(2)) . . . (G-w(ns))
*/
/*        where G is the Hessenberg submatrix H(L:I,L:I) and w is */
/*        the vector of shifts (stored in W). The result is */
/*        stored in the local array V. */

        v[0].r = 1., v[0].i = 0.;
        i__2 = ns + 1;
        for (ii = 2; ii <= i__2; ++ii) {
            i__3 = ii - 1;
            v[i__3].r = 0., v[i__3].i = 0.;
/* L110: */
        }
        nv = 1;
        i__2 = i;
        for (j = i - ns + 1; j <= i__2; ++j) {
            i__3 = nv + 1;
            zcopy_(&i__3, v, &c__1, vv, &c__1);
            i__3 = nv + 1;
            i__5 = j;
            z__1.r = -w[i__5].r, z__1.i = -w[i__5].i;
            zgemv_("No transpose", &i__3, &nv, &c_b10, &h[l + l * h_dim1],
                    ldh, vv, &c__1, &z__1, v, &c__1, 12L);
            ++nv;

/*           Scale V(1:NV) so that max(abs(V(i))) = 1. If V is zer
o, */
/*           reset it to the unit vector. */

            itemp = izamax_(&nv, v, &c__1);
            i__3 = itemp - 1;
            rtemp = (d__1 = v[i__3].r, abs(d__1)) + (d__2 = d_imag(&v[itemp -
                    1]), abs(d__2));
            if (rtemp == 0.) {
                v[0].r = 1., v[0].i = 0.;
                i__3 = nv;
                for (ii = 2; ii <= i__3; ++ii) {
                    i__5 = ii - 1;
                    v[i__5].r = 0., v[i__5].i = 0.;
/* L120: */
                }
            } else {
                rtemp = max(rtemp,smlnum);
                d__1 = 1. / rtemp;
                zdscal_(&nv, &d__1, v, &c__1);
            }
/* L130: */
        }

/*        Multiple-shift QR step */

        i__2 = i - 1;
        for (k = l; k <= i__2; ++k) {

/*           The first iteration of this loop determines a reflect
ion G */
/*           from the vector V and applies it from left and right
to H, */
/*           thus creating a nonzero bulge below the subdiagonal.
*/

/*           Each subsequent iteration determines a reflection G t
o */
/*           restore the Hessenberg form in the (K-1)th column, an
d thus */
/*           chases the bulge one step toward the bottom of the ac
tive */
/*           submatrix. NR is the order of G. */

/* Computing MIN */
            i__3 = ns + 1, i__5 = i - k + 1;
            nr = min(i__3,i__5);
            if (k > l) {
                zcopy_(&nr, &h[k + (k - 1) * h_dim1], &c__1, v, &c__1);
            }
            zlarfg_(&nr, v, &v[1], &c__1, &tau);
            if (k > l) {
                i__3 = k + (k - 1) * h_dim1;
                h[i__3].r = v[0].r, h[i__3].i = v[0].i;
                i__3 = i;
                for (ii = k + 1; ii <= i__3; ++ii) {
                    i__5 = ii + (k - 1) * h_dim1;
                    h[i__5].r = 0., h[i__5].i = 0.;
/* L140: */
                }
            }
            v[0].r = 1., v[0].i = 0.;

/*           Apply G' from the left to transform the rows of the m
atrix */
/*           in columns K to I2. */

            i__3 = i2 - k + 1;
            d_cnjg(&z__1, &tau);
            zlarfx_("Left", &nr, &i__3, v, &z__1, &h[k + k * h_dim1], ldh, &
                    work[1], 4L);

/*           Apply G from the right to transform the columns of th
e */
/*           matrix in rows I1 to min(K+NR,I). */

/* Computing MIN */
            i__5 = k + nr;
            i__3 = min(i__5,i) - i1 + 1;
            zlarfx_("Right", &i__3, &nr, v, &tau, &h[i1 + k * h_dim1], ldh, &
                    work[1], 5L);

            if (wantz) {

/*              Accumulate transformations in the matrix Z */

                zlarfx_("Right", &nh, &nr, v, &tau, &z[*ilo + k * z_dim1],
                        ldz, &work[1], 5L);
            }
/* L150: */
        }

/*        Ensure that H(I,I-1) is real. */

        i__2 = i + (i - 1) * h_dim1;
        temp.r = h[i__2].r, temp.i = h[i__2].i;
        if (d_imag(&temp) != 0.) {
            d__1 = temp.r;
            d__2 = d_imag(&temp);
            rtemp = dlapy2_(&d__1, &d__2);
            i__2 = i + (i - 1) * h_dim1;
            h[i__2].r = rtemp, h[i__2].i = 0.;
            z__1.r = temp.r / rtemp, z__1.i = temp.i / rtemp;
            temp.r = z__1.r, temp.i = z__1.i;
            if (i2 > i) {
                i__2 = i2 - i;
                d_cnjg(&z__1, &temp);
                zscal_(&i__2, &z__1, &h[i + (i + 1) * h_dim1], ldh);
            }
            i__2 = i - i1;
            zscal_(&i__2, &temp, &h[i1 + i * h_dim1], &c__1);
            if (wantz) {
                zscal_(&nh, &temp, &z[*ilo + i * z_dim1], &c__1);
            }
        }

/* L160: */
    }

/*     Failure to converge in remaining number of iterations */

    *info = i;
    return 0;

L170:

/*     A submatrix of order <= MAXB in rows and columns L to I has split
*/
/*     off. Use the double-shift QR algorithm to handle it. */

    zlahqr_(&wantt, &wantz, n, &l, &i, &h[h_offset], ldh, &w[1], ilo, ihi, &z[
            z_offset], ldz, info);
    if (*info > 0) {
        return 0;
    }

/*     Decrement number of remaining iterations, and return to start of */
/*     the main loop with a new value of I. */

    itn -= its;
    i = l - 1;
    goto L60;

L180:
    return 0;

/*     End of ZHSEQR */

} /* zhseqr_ */


#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static doublecomplex c_b9 = {0.,0.};
static doublecomplex c_b10 = {1.,0.};
static integer c__1 = 1;
static integer c__4 = 4;
static integer c_n1 = -1;
static ftnlen c__2 = 2;
static integer c__8 = 8;
static integer c__15 = 15;
static integer c__0 = 0;

/* Subroutine */ void zhseqr_(job, compz, n, ilo, ihi, h, ldh, w, z, ldz, work, lwork, info)
const char *job, *compz;
const integer *n;
integer *ilo, *ihi;
doublecomplex *h;
const integer *ldh;
doublecomplex *w, *z;
const integer *ldz;
doublecomplex *work;
integer *lwork, *info;
{
    /* System generated locals */
    address a__1[2];
    integer i__1, i__2;
    ftnlen ii__4[2];
    doublereal d__1;
    doublecomplex z__1;
    char ch__1[2];

    /* Local variables */
    static integer maxb, ierr;
    static doublereal unfl;
    static doublecomplex temp;
    static doublereal ovfl;
    static integer i, j, k, l;
    static doublecomplex s[225] /* was [15][15] */, v[16];
    static integer itemp;
    static doublereal rtemp;
    static integer i1, i2;
    static logical initz, wantt, wantz;
    static doublereal rwork[1];
    static integer ii, nh;
    static integer nr, ns, nv;
    static doublecomplex vv[16];
    static doublereal smlnum;
    static integer itn;
    static doublecomplex tau;
    static integer its;
    static doublereal ulp, tst1;

    (void)lwork;
/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZHSEQR computes the eigenvalues of a complex upper Hessenberg         */
/*  matrix H, and, optionally, the matrices T and Z from the Schur        */
/*  decomposition H = Z T Z**H, where T is an upper triangular matrix     */
/*  (the Schur form), and Z is the unitary matrix of Schur vectors.       */
/*                                                                        */
/*  Optionally Z may be postmultiplied into an input unitary matrix Q,    */
/*  so that this routine can give the Schur factorization of a matrix A   */
/*  which has been reduced to the Hessenberg form H by the unitary        */
/*  matrix Q:  A = Q*H*Q**H = (QZ)*T*(QZ)**H.                             */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  JOB     (input) CHARACTER*1                                           */
/*          = 'E': compute eigenvalues only;                              */
/*          = 'S': compute eigenvalues and the Schur form T.              */
/*                                                                        */
/*  COMPZ   (input) CHARACTER*1                                           */
/*          = 'N': no Schur vectors are computed;                         */
/*          = 'I': Z is initialized to the unit matrix and the matrix Z   */
/*                 of Schur vectors of H is returned;                     */
/*          = 'V': Z must contain an unitary matrix Q on entry, and       */
/*                 the product Q*Z is returned.                           */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrix H.  N >= 0.                           */
/*                                                                        */
/*  ILO     (input) INTEGER                                               */
/*  IHI     (input) INTEGER                                               */
/*          It is assumed that H is already upper triangular in rows      */
/*          and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally     */
/*          set by a previous call to ZGEBAL, and then passed to CGEHRD   */
/*          when the matrix output by ZGEBAL is reduced to Hessenberg     */
/*          form. Otherwise ILO and IHI should be set to 1 and N          */
/*          respectively.                                                 */
/*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.      */
/*                                                                        */
/*  H       (input/output) COMPLEX*16 array, dimension (LDH,N)            */
/*          On entry, the upper Hessenberg matrix H.                      */
/*          On exit, if JOB = 'S', H contains the upper triangular matrix */
/*          T from the Schur decomposition (the Schur form). If           */
/*          JOB = 'E', the contents of H are unspecified on exit.         */
/*                                                                        */
/*  LDH     (input) INTEGER                                               */
/*          The leading dimension of the array H. LDH >= max(1,N).        */
/*                                                                        */
/*  W       (output) COMPLEX*16 array, dimension (N)                      */
/*          The computed eigenvalues. If JOB = 'S', the eigenvalues are   */
/*          stored in the same order as on the diagonal of the Schur form */
/*          returned in H, with W(i) = H(i,i).                            */
/*                                                                        */
/*  Z       (input/output) COMPLEX*16 array, dimension (LDZ,N)            */
/*          If COMPZ = 'N': Z is not referenced.                          */
/*          If COMPZ = 'I': on entry, Z need not be set, and on exit, Z   */
/*          contains the unitary matrix Z of the Schur vectors of H.      */
/*          If COMPZ = 'V': on entry Z must contain an N-by-N matrix Q,   */
/*          which is assumed to be equal to the unit matrix except for    */
/*          the submatrix Z(ILO:IHI,ILO:IHI); on exit Z contains Q*Z.     */
/*          Normally Q is the unitary matrix generated by ZUNGHR after    */
/*          the call to ZGEHRD which formed the Hessenberg matrix H.      */
/*                                                                        */
/*  LDZ     (input) INTEGER                                               */
/*          The leading dimension of the array Z.                         */
/*          LDZ >= max(1,N) if COMPZ = 'I' or 'V'; LDZ >= 1 otherwise.    */
/*                                                                        */
/*  WORK    (workspace) COMPLEX*16 array, dimension (N)                   */
/*                                                                        */
/*  LWORK   (input) INTEGER                                               */
/*          This argument is currently redundant.                         */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0:  successful exit                                         */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value    */
/*          > 0:  if INFO = i, ZHSEQR failed to compute all the           */
/*                eigenvalues in a total of 30*(IHI-ILO+1) iterations;    */
/*                elements 1:ilo-1 and i+1:n of W contain those           */
/*                eigenvalues which have been successfully computed.      */
/*                                                                        */
/*  ===================================================================== */

    wantt = lsame_(job, "S");
    initz = lsame_(compz, "I");
    wantz = initz || lsame_(compz, "V");

    *info = 0;
    if (! lsame_(job, "E") && ! wantt) {
        *info = -1;
    } else if (! lsame_(compz, "N") && ! wantz) {
        *info = -2;
    } else if (*n < 0) {
        *info = -3;
    } else if (*ilo < 1 || *ilo > max(1,*n)) {
        *info = -4;
    } else if (*ihi < min(*ilo,*n) || *ihi > *n) {
        *info = -5;
    } else if (*ldh < max(1,*n)) {
        *info = -7;
    } else if (*ldz < 1 || (wantz && *ldz < max(1,*n))) {
        *info = -10;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("ZHSEQR", &i__1);
        return;
    }

/*     Initialize Z, if necessary */

    if (initz) {
        zlaset_("Full", n, n, &c_b9, &c_b10, z, ldz);
    }

/*     Store the eigenvalues isolated by ZGEBAL. */

    for (i = 0; i < *ilo - 1; ++i) {
        i__1 = i + i * *ldh; /* index [i,i] */
        w[i].r = h[i__1].r, w[i].i = h[i__1].i;
    }
    for (i = *ihi; i < *n; ++i) {
        i__1 = i + i * *ldh; /* index [i,i] */
        w[i].r = h[i__1].r, w[i].i = h[i__1].i;
    }

/*     Quick return if possible. */

    if (*n == 0) {
        return;
    }
    if (*ilo == *ihi) {
        i__1 = (*ilo-1) + (*ilo-1) * *ldh; /* index [*ilo-1,*ilo-1] */
        w[*ilo-1].r = h[i__1].r, w[*ilo-1].i = h[i__1].i;
        return;
    }

/*     Set rows and columns ILO to IHI to zero below the first */
/*     subdiagonal. */

    for (j = *ilo-1; j < *ihi - 2; ++j) {
        for (i = j + 2; i < *n; ++i) {
            i__1 = i + j * *ldh; /* index [i,j] */
            h[i__1].r = 0., h[i__1].i = 0.;
        }
    }
    nh = *ihi - *ilo + 1;

/*     I1 and I2 are the indices of the first row and last column of H */
/*     to which transformations must be applied. If eigenvalues only are */
/*     being computed, I1 and I2 are re-set inside the main loop. */

    if (wantt) {
        i1 = 0;
        i2 = *n-1;
    } else {
        i1 = *ilo-1;
        i2 = *ihi-1;
    }

/*     Ensure that the subdiagonal elements are real. */

    for (i = *ilo; i < *ihi; ++i) {
        i__1 = i + (i - 1) * *ldh; /* index [i,i-1] */
        temp.r = h[i__1].r, temp.i = h[i__1].i;
        if (temp.i != 0.) {
            rtemp = dlapy2_(&(temp.r), &(temp.i));
            i__1 = i + (i - 1) * *ldh; /* index [i,i-1] */
            h[i__1].r = rtemp, h[i__1].i = 0.;
            temp.r /= rtemp, temp.i /= rtemp;
            if (i2 > i) {
                i__1 = i2 - i;
                d_cnjg(&z__1, &temp);
                zscal_(&i__1, &z__1, &h[i + (i + 1) * *ldh], ldh);
            }
            i__1 = i - i1;
            zscal_(&i__1, &temp, &h[i1 + i * *ldh], &c__1);
            if (i < *ihi-1) {
                i__1 = i + 1 + i * *ldh; /* index [i+1,i] */
                z__1.r = temp.r * h[i__1].r - temp.i * h[i__1].i,
                z__1.i = temp.r * h[i__1].i + temp.i * h[i__1].r;
                h[i__1].r = z__1.r, h[i__1].i = z__1.i;
            }
            if (wantz) {
                zscal_(&nh, &temp, &z[*ilo-1 + i * *ldz], &c__1);
            }
        }
    }

/*     Determine the order of the multi-shift QR algorithm to be used. */

/* Writing concatenation */
    ii__4[0] = 1, a__1[0] = job;
    ii__4[1] = 1, a__1[1] = compz;
    s_cat(ch__1, a__1, ii__4, &c__2, 2L);
    ns = ilaenv_(&c__4, "ZHSEQR", ch__1, n, ilo, ihi, &c_n1) - 1;
/* Writing concatenation */
    ii__4[0] = 1, a__1[0] = job;
    ii__4[1] = 1, a__1[1] = compz;
    s_cat(ch__1, a__1, ii__4, &c__2, 2L);
    maxb = ilaenv_(&c__8, "ZHSEQR", ch__1, n, ilo, ihi, &c_n1);
    if (ns <= 0 || ns >= nh || maxb >= nh) {

/*        Use the standard double-shift algorithm */

        zlahqr_(&wantt, &wantz, n, ilo, ihi, h, ldh, w, ilo, ihi, z, ldz, info);
        return;
    }
    maxb = max(2,maxb);
    ns = min(min(ns,maxb-1),14);

/*     Now 1 < NS <= MAXB < NH. */

/*     Set machine-dependent constants for the stopping criterion. */
/*     If norm(H) <= sqrt(OVFL), overflow should not occur. */

    unfl = dlamch_("Safe minimum");
    ovfl = 1. / unfl;
    dlabad_(&unfl, &ovfl);
    ulp = dlamch_("Precision");
    smlnum = unfl * (nh / ulp);

/*     ITN is the total number of multiple-shift QR iterations allowed. */

    itn = nh * 30;

/*     The main loop begins here. I is the loop index and decreases from */
/*     IHI to ILO in steps of at most MAXB. Each iteration of the loop */
/*     works with the active submatrix in rows and columns L to I. */
/*     Eigenvalues I+1 to IHI have already converged. Either L = ILO, or */
/*     H(L,L-1) is negligible so that the matrix splits. */

    i = *ihi-1;
L60:
    if (i < *ilo-1) {
        return; /* exit from zhseqr_ */
    }

/*     Perform multiple-shift QR iterations on rows and columns ILO to I */
/*     until a submatrix of order at most MAXB splits off at the bottom */
/*     because a subdiagonal element has become negligible. */

    l = *ilo-1;
    for (its = 0; its <= itn; ++its) {

/*        Look for a single small subdiagonal element. */

        for (k = i; k > l; --k) {
            i__1 = (k - 1) * (*ldh + 1); /* index [k-1,k-1] */
            i__2 = k + k * *ldh; /* index [k,k] */
            tst1 = abs(h[i__1].r) + abs(h[i__1].i) + abs(h[i__2].r) + abs(h[i__2].i);
            if (tst1 == 0.) {
                i__1 = i - l + 1;
                tst1 = zlanhs_("1", &i__1, &h[l + l * *ldh], ldh, rwork);
            }
            if (abs(h[k + (k - 1) * *ldh].r) <= max(ulp*tst1, smlnum)) {
                break;
            }
        }
        l = k;
        if (l > *ilo-1) {

/*           H(L,L-1) is negligible. */

            i__1 = l + (l - 1) * *ldh; /* index [l,l-1] */
            h[i__1].r = 0., h[i__1].i = 0.;
        }

/*        Exit from loop if a submatrix of order <= MAXB has split off. */

        if (l > i - maxb) {
            goto L170;
        }

/*        Now the active submatrix is in rows and columns L to I. If */
/*        eigenvalues only are being computed, only the active submatrix */
/*        need be transformed. */

        if (! wantt) {
            i1 = l;
            i2 = i;
        }

        if (its == 20 || its == 30) {

/*           Exceptional shifts. */

            for (ii = i - ns; ii <= i; ++ii) {
                w[ii].r = (abs(h[ii + (ii - 1) * *ldh].r) + abs(h[ii + ii * *ldh].r)) * 1.5,
                w[ii].i = 0.;
            }
        } else {

/*           Use eigenvalues of trailing submatrix of order NS as shifts. */

            i__1 = ns + 1;
            zlacpy_("Full", &i__1, &i__1, &h[i - ns + (i - ns) * *ldh], ldh, s, &c__15);
            zlahqr_(&c__0, &c__0, &i__1, &c__1, &i__1, s, &c__15, &w[i - ns], &c__1, &i__1, z, ldz, &ierr);

            if (ierr > 0) {

/*              If ZLAHQR failed to compute all NS eigenvalues, use the */
/*              unconverged diagonal elements as the remaining shifts. */

                for (ii = 0; ii < ierr; ++ii) {
                    i__1 = i - ns + ii;
                    i__2 = ii + ii * 15;
                    w[i__1].r = s[i__2].r, w[i__1].i = s[i__2].i;
                }
            }
        }

/*        Form the first column of (G-w(1)) (G-w(2)) . . . (G-w(ns)) */
/*        where G is the Hessenberg submatrix H(L:I,L:I) and w is */
/*        the vector of shifts (stored in W). The result is */
/*        stored in the local array V. */

        v[0].r = 1., v[0].i = 0.;
        for (ii = 1; ii <= ns+1; ++ii) {
            v[ii].r = 0., v[ii].i = 0.;
        }
        nv = 1;
        for (j = i - ns; j <= i; ++j) {
            i__1 = nv + 1;
            zcopy_(&i__1, v, &c__1, vv, &c__1);
            z__1.r = -w[j].r, z__1.i = -w[j].i;
            zgemv_("No transpose", &i__1, &nv, &c_b10, &h[l + l * *ldh], ldh, vv, &c__1, &z__1, v, &c__1);
            ++nv;

/*           Scale V(1:NV) so that max(abs(V(i))) = 1. If V is zero, */
/*           reset it to the unit vector. */

            itemp = izamax_(&nv, v, &c__1);
            i__1 = itemp - 1;
            rtemp = abs(v[i__1].r) + abs(v[i__1].i);
            if (rtemp == 0.) {
                v[0].r = 1., v[0].i = 0.;
                for (ii = 1; ii < nv; ++ii) {
                    v[ii].r = 0., v[ii].i = 0.;
                }
            } else {
                rtemp = max(rtemp,smlnum);
                d__1 = 1. / rtemp;
                zdscal_(&nv, &d__1, v, &c__1);
            }
        }

/*        Multiple-shift QR step */

        for (k = l; k < i; ++k) {

/*           The first iteration of this loop determines a reflection G */
/*           from the vector V and applies it from left and right to H, */
/*           thus creating a nonzero bulge below the subdiagonal. */

/*           Each subsequent iteration determines a reflection G to */
/*           restore the Hessenberg form in the (K-1)th column, and thus */
/*           chases the bulge one step toward the bottom of the active */
/*           submatrix. NR is the order of G. */

            nr = min(ns+2, i-k+1);
            if (k > l) {
                zcopy_(&nr, &h[k + (k - 1) * *ldh], &c__1, v, &c__1);
            }
            zlarfg_(&nr, v, &v[1], &c__1, &tau);
            if (k > l) {
                i__1 = k + (k - 1) * *ldh; /* index [k,k-1] */
                h[i__1].r = v[0].r, h[i__1].i = v[0].i;
                for (ii = k+1; ii <= i; ++ii) {
                    i__1 = ii + (k - 1) * *ldh; /* index [ii,k-1] */
                    h[i__1].r = 0., h[i__1].i = 0.;
                }
            }
            v[0].r = 1., v[0].i = 0.;

/*           Apply G' from the left to transform the rows of the matrix */
/*           in columns K to I2. */

            i__1 = i2 - k + 1;
            d_cnjg(&z__1, &tau);
            zlarfx_("Left", &nr, &i__1, v, &z__1, &h[k + k * *ldh], ldh, work);

/*           Apply G from the right to transform the columns of the */
/*           matrix in rows I1 to min(K+NR,I). */

            i__1 = min(k+nr,i) - i1 + 1;
            zlarfx_("Right", &i__1, &nr, v, &tau, &h[i1 + k * *ldh], ldh, work);

            if (wantz) {

/*              Accumulate transformations in the matrix Z */

                zlarfx_("Right", &nh, &nr, v, &tau, &z[*ilo-1 + k * *ldz], ldz, work);
            }
        }

/*        Ensure that H(I,I-1) is real. */

        i__1 = i + (i - 1) * *ldh; /* index [i,i-1] */
        temp.r = h[i__1].r, temp.i = h[i__1].i;
        if (temp.i != 0.) {
            rtemp = dlapy2_(&(temp.r), &(temp.i));
            i__1 = i + (i - 1) * *ldh; /* index [i,i-1] */
            h[i__1].r = rtemp, h[i__1].i = 0.;
            temp.r /= rtemp, temp.i /= rtemp;
            if (i2 > i) {
                i__1 = i2 - i;
                d_cnjg(&z__1, &temp);
                zscal_(&i__1, &z__1, &h[i + (i + 1) * *ldh], ldh);
            }
            i__1 = i - i1;
            zscal_(&i__1, &temp, &h[i1 + i * *ldh], &c__1);
            if (wantz) {
                zscal_(&nh, &temp, &z[*ilo-1 + i * *ldz], &c__1);
            }
        }
    }

/*     Failure to converge in remaining number of iterations */

    *info = i+1;
    return;

L170:

/*     A submatrix of order <= MAXB in rows and columns L to I has split */
/*     off. Use the double-shift QR algorithm to handle it. */

    i__1 = l+1; i__2 = i+1;
    zlahqr_(&wantt, &wantz, n, &i__1, &i__2, h, ldh, w, ilo, ihi, z, ldz, info);
    if (*info > 0) {
        return;
    }

/*     Decrement number of remaining iterations, and return to start of */
/*     the main loop with a new value of I. */

    itn -= its;
    i = l-1;
    goto L60;
} /* zhseqr_ */

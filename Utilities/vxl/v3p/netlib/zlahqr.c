#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static integer c__2 = 2;
static integer c__1 = 1;

/* Subroutine */ void zlahqr_(wantt, wantz, n, ilo, ihi, h, ldh, w, iloz, ihiz, z, ldz, info)
const logical *wantt, *wantz;
const integer *n, *ilo, *ihi;
doublecomplex *h;
const integer *ldh;
doublecomplex *w;
integer *iloz, *ihiz;
doublecomplex *z;
const integer *ldz;
integer *info;
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2;

    /* Local variables */
    static doublereal unfl, ovfl;
    static doublecomplex temp;
    static integer i, j, k, l, m;
    static doublereal s;
    static doublecomplex t, u, v[2], x, y;
    static doublereal rtemp;
    static integer i1, i2;
    static doublereal rwork[1];
    static doublecomplex t1;
    static doublereal t2;
    static doublecomplex v2;
    static doublereal h10;
    static doublecomplex h11;
    static doublereal h21;
    static doublecomplex h22;
    static integer nh;
    static integer nz;
    static doublereal smlnum;
    static doublecomplex h11s;
    static integer itn, its;
    static doublereal ulp;
    static doublecomplex sum;
    static doublereal tst1;


/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZLAHQR is an auxiliary routine called by ZHSEQR to update the         */
/*  eigenvalues and Schur decomposition already computed by ZHSEQR, by    */
/*  dealing with the Hessenberg submatrix in rows and columns ILO to IHI. */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  WANTT   (input) LOGICAL                                               */
/*          = .TRUE. : the full Schur form T is required;                 */
/*          = .FALSE.: only eigenvalues are required.                     */
/*                                                                        */
/*  WANTZ   (input) LOGICAL                                               */
/*          = .TRUE. : the matrix of Schur vectors Z is required;         */
/*          = .FALSE.: Schur vectors are not required.                    */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrix H.  N >= 0.                           */
/*                                                                        */
/*  ILO     (input) INTEGER                                               */
/*  IHI     (input) INTEGER                                               */
/*          It is assumed that H is already upper triangular in rows and  */
/*          columns IHI+1:N, and that H(ILO,ILO-1) = 0 (unless ILO = 1).  */
/*          ZLAHQR works primarily with the Hessenberg submatrix in rows  */
/*          and columns ILO to IHI, but applies transformations to all of */
/*          H if WANTT is .TRUE..                                         */
/*          1 <= ILO <= max(1,IHI); IHI <= N.                             */
/*                                                                        */
/*  H       (input/output) COMPLEX*16 array, dimension (LDH,N)            */
/*          On entry, the upper Hessenberg matrix H.                      */
/*          On exit, if WANTT is .TRUE., H is upper triangular in rows    */
/*          and columns ILO:IHI, with any 2-by-2 diagonal blocks in       */
/*          standard form. If WANTT is .FALSE., the contents of H are     */
/*          unspecified on exit.                                          */
/*                                                                        */
/*  LDH     (input) INTEGER                                               */
/*          The leading dimension of the array H. LDH >= max(1,N).        */
/*                                                                        */
/*  W       (output) COMPLEX*16 array, dimension (N)                      */
/*          The computed eigenvalues ILO to IHI are stored in the         */
/*          corresponding elements of W. If WANTT is .TRUE., the          */
/*          eigenvalues are stored in the same order as on the diagonal   */
/*          of the Schur form returned in H, with W(i) = H(i,i).          */
/*                                                                        */
/*  ILOZ    (input) INTEGER                                               */
/*  IHIZ    (input) INTEGER                                               */
/*          Specify the rows of Z to which transformations must be        */
/*          applied if WANTZ is .TRUE..                                   */
/*          1 <= ILOZ <= ILO; IHI <= IHIZ <= N.                           */
/*                                                                        */
/*  Z       (input/output) COMPLEX*16 array, dimension (LDZ,N)            */
/*          If WANTZ is .TRUE., on entry Z must contain the current       */
/*          matrix Z of transformations accumulated by ZHSEQR, and on     */
/*          exit Z has been updated; transformations are applied only to  */
/*          the submatrix Z(ILOZ:IHIZ,ILO:IHI).                           */
/*          If WANTZ is .FALSE., Z is not referenced.                     */
/*                                                                        */
/*  LDZ     (input) INTEGER                                               */
/*          The leading dimension of the array Z. LDZ >= max(1,N).        */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0: successful exit                                          */
/*          > 0: if INFO = i, ZLAHQR failed to compute all the            */
/*               eigenvalues ILO to IHI in a total of 30*(IHI-ILO+1)      */
/*               iterations; elements i+1:ihi of W contain those          */
/*               eigenvalues which have been successfully computed.       */
/*                                                                        */
/*  ===================================================================== */

    *info = 0;

/*     Quick return if possible */

    if (*n == 0) {
        return;
    }
    if (*ilo == *ihi) {
        i__1 = *ilo-1 + (*ilo-1) * *ldh;
        w[*ilo-1].r = h[i__1].r, w[*ilo-1].i = h[i__1].i;
        return;
    }

    nh = *ihi - *ilo + 1;
    nz = *ihiz - *iloz + 1;

/*     Set machine-dependent constants for the stopping criterion. */
/*     If norm(H) <= sqrt(OVFL), overflow should not occur. */

    unfl = dlamch_("Safe minimum");
    ovfl = 1. / unfl;
    dlabad_(&unfl, &ovfl);
    ulp = dlamch_("Precision");
    smlnum = unfl * (nh / ulp);

/*     I1 and I2 are the indices of the first row and last column of H */
/*     to which transformations must be applied. If eigenvalues only are */
/*     being computed, I1 and I2 are set inside the main loop. */

    if (*wantt) {
        i1 = 0;
        i2 = *n-1;
    }

/*     ITN is the total number of QR iterations allowed. */

    itn = nh * 30;

/*     The main loop begins here. I is the loop index and decreases from */
/*     IHI to ILO in steps of 1. Each iteration of the loop works */
/*     with the active submatrix in rows and columns L to I. */
/*     Eigenvalues I+1 to IHI have already converged. Either L = ILO, or */
/*     H(L,L-1) is negligible so that the matrix splits. */

    i = *ihi-1;
L10:
    if (i < *ilo-1) {
        return; /* exit from zlahqr_ */
    }

/*     Perform QR iterations on rows and columns ILO to I until a */
/*     submatrix of order 1 splits off at the bottom because a */
/*     subdiagonal element has become negligible. */

    l = *ilo-1;
    for (its = 0; /*fsm*/ 1 /*its <= itn*/; ++its) {

/*        Look for a single small subdiagonal element. */

        for (k = i; k > l; --k) {
            i__1 = k - 1 + (k - 1) * *ldh;
            i__2 = k + k * *ldh;
            tst1 = abs(h[i__1].r) + abs(h[i__1].i) + abs(h[i__2].r) + abs(h[i__2].i);
            if (tst1 == 0.) {
                i__1 = i - l + 1;
                tst1 = zlanhs_("1", &i__1, &h[l + l * *ldh], ldh, rwork);
            }
            if (abs(h[k + (k - 1) * *ldh].r) <= max(ulp * tst1,smlnum)) {
                break;
            }
        }
        l = k;
        if (l > *ilo-1) {

/*           H(L,L-1) is negligible */

            i__1 = l + (l - 1) * *ldh;
            h[i__1].r = h[i__1].i = 0.;
        }

/*        Exit from loop if a submatrix of order 1 has split off. */

        if (l >= i) {
/*         H(I,I-1) is negligible: one eigenvalue has converged. */

            i__1 = i + i * *ldh;
            w[i].r = h[i__1].r, w[i].i = h[i__1].i;

/*         Decrement number of remaining iterations, and return to start of */
/*         the main loop with new value of I. */

            itn -= its;
            i = l - 1;
            goto L10;
        }

/*        Now the active submatrix is in rows and columns L to I. If */
/*        eigenvalues only are being computed, only the active submatrix */
/*        need be transformed. */

        if (! (*wantt)) {
            i1 = l;
            i2 = i;
        }

        if (its == 10 || its == 20) {

/*           Exceptional shift. */

            t.r = abs(h[i + (i - 1) * *ldh].r)
                + abs(h[i - 1 + (i - 2) * *ldh].r);
            t.i = 0.;
        } else {

/*           Wilkinson's shift. */

            i__1 = i + i * *ldh;
            t.r = h[i__1].r, t.i = h[i__1].i;
            d__1 = h[i + (i - 1) * *ldh].r;
            u.r = d__1 * h[i__1].r, u.i = d__1 * h[i__1].i;
            if (u.r != 0. || u.i != 0.) {
                i__1 = i - 1 + (i - 1) * *ldh;
                x.r = .5 * (h[i__1].r - t.r),
                x.i = .5 * (h[i__1].i - t.i);
                z__2.r = u.r + x.r * x.r - x.i * x.i,
                z__2.i = u.i + x.r * x.i + x.i * x.r;
                z_sqrt(&z__1, &z__2);
                y.r = z__1.r, y.i = z__1.i;
                if (x.r * y.r + x.i * y.i < 0.) {
                    y.r = -y.r, y.i = -y.i;
                }
                z__2.r = x.r + y.r, z__2.i = x.i + y.i;
                zladiv_(&z__1, &u, &z__2);
                t.r -= z__1.r, t.i -= z__1.i;
            }
        }

/*        Look for two consecutive small subdiagonal elements. */

        for (m = i - 1; m >= l; --m) {

/*           Determine the effect of starting the single-shift QR */
/*           iteration at row M, and see if this would make H(M,M-1) */
/*           negligible. */

            i__1 = m + m * *ldh;
            h11.r = h[i__1].r, h11.i = h[i__1].i;
            i__1 = m + 1 + (m + 1) * *ldh;
            h22.r = h[i__1].r, h22.i = h[i__1].i;
            h11s.r = h11.r - t.r, h11s.i = h11.i - t.i;
            h21 = h[m + 1 + m * *ldh].r;
            s = abs(h11s.r) + abs(h11s.i) + abs(h21);
            h11s.r /= s, h11s.i /= s;
            h21 /= s;
            v[0].r = h11s.r, v[0].i = h11s.i;
            v[1].r = h21, v[1].i = 0.;
            if (m == l) {
                break;
            }
            h10 = h[m + (m - 1) * *ldh].r;
            tst1 = (abs(h11s.r) + abs(h11s.i)) * (abs(h11.r) + abs(h11.i) + abs(h22.r) + abs(h22.i));
            if (abs(h10 * h21) <= ulp * tst1) {
                break;
            }
        }

/*        Single-shift QR step */

        for (k = m; k < i; ++k) {

/*           The first iteration of this loop determines a reflection G */
/*           from the vector V and applies it from left and right to H, */
/*           thus creating a nonzero bulge below the subdiagonal. */

/*           Each subsequent iteration determines a reflection G to */
/*           restore the Hessenberg form in the (K-1)th column, and thus */
/*           chases the bulge one step toward the bottom of the active */
/*           submatrix. */

/*           V(2) is always real before the call to ZLARFG, and hence */
/*           after the call T2 ( = T1*V(2) ) is also real. */

            if (k > m) {
                zcopy_(&c__2, &h[k + (k - 1) * *ldh], &c__1, v, &c__1);
            }
            zlarfg_(&c__2, v, &v[1], &c__1, &t1);
            if (k > m) {
                i__1 = k + (k - 1) * *ldh;
                h[i__1].r = v[0].r, h[i__1].i = v[0].i;
                i__1 = k + 1 + (k - 1) * *ldh;
                h[i__1].r = 0., h[i__1].i = 0.;
            }
            v2.r = v[1].r, v2.i = v[1].i;
            t2 = t1.r * v2.r - t1.i * v2.i;

/*           Apply G from the left to transform the rows of the matrix */
/*           in columns K to I2. */

            for (j = k; j <= i2; ++j) {
                d_cnjg(&z__1, &t1);
                i__1 = k + j * *ldh;
                i__2 = k + 1 + j * *ldh;
                sum.r = t2 * h[i__2].r + z__1.r * h[i__1].r - z__1.i * h[i__1].i,
                sum.i = t2 * h[i__2].i + z__1.r * h[i__1].i + z__1.i * h[i__1].r;
                h[i__1].r -= sum.r,
                h[i__1].i -= sum.i;
                h[i__2].r -= sum.r * v2.r - sum.i * v2.i,
                h[i__2].i -= sum.r * v2.i + sum.i * v2.r;
            }

/*           Apply G from the right to transform the columns of the */
/*           matrix in rows I1 to min(K+2,I). */

            for (j = i1; j <= k+2 && j <= i; ++j) {
                i__1 = j + k * *ldh;
                i__2 = j + (k + 1) * *ldh;
                sum.r = t2 * h[i__2].r + t1.r * h[i__1].r - t1.i * h[i__1].i,
                sum.i = t2 * h[i__2].i + t1.r * h[i__1].i + t1.i * h[i__1].r;
                h[i__1].r -= sum.r,
                h[i__1].i -= sum.i;
                h[i__2].r -=   sum.r * v2.r + sum.i * v2.i,
                h[i__2].i -= - sum.r * v2.i + sum.i * v2.r;
            }


/*              Accumulate transformations in the matrix Z */

            if (*wantz)
            for (j = *iloz-1; j < *ihiz; ++j) {
                i__1 = j + k * *ldz;
                i__2 = j + (k + 1) * *ldz;
                sum.r = t2 * z[i__2].r + t1.r * z[i__1].r - t1.i * z[i__1].i,
                sum.i = t2 * z[i__2].i + t1.r * z[i__1].i + t1.i * z[i__1].r;
                z[i__1].r -= sum.r,
                z[i__1].i -= sum.i;
                z[i__2].r -=   sum.r * v2.r + sum.i * v2.i,
                z[i__2].i -= - sum.r * v2.i + sum.i * v2.r;
            }

            if (k == m && m > l) {

/*              If the QR step was started at row M > L because two */
/*              consecutive small subdiagonals were found, the n extra */
/*              scaling must be performed to ensure that H(M,M-1) remains */
/*              real. */

                temp.r = 1. - t1.r, temp.i = 0. - t1.i;
                d__1 = dlapy2_(&(temp.r), &(temp.i));
                temp.r /= d__1, temp.i /= d__1;
                i__1 = m + 1 + m * *ldh;
                d_cnjg(&z__2, &temp);
                z__1.r = h[i__1].r * z__2.r - h[i__1].i * z__2.i,
                z__1.i = h[i__1].r * z__2.i + h[i__1].i * z__2.r;
                h[i__1].r = z__1.r, h[i__1].i = z__1.i;
                if (m + 2 <= i) {
                    i__1 = m + 2 + (m + 1) * *ldh;
                    z__1.r = h[i__1].r * temp.r - h[i__1].i * temp.i,
                    z__1.i = h[i__1].r * temp.i + h[i__1].i * temp.r;
                    h[i__1].r = z__1.r, h[i__1].i = z__1.i;
                }
                for (j = m; j <= i; ++j) {
                    if (j != m + 1) {
                        if (i2 > j) {
                            i__1 = i2 - j;
                            zscal_(&i__1, &temp, &h[j + (j + 1) * *ldh], ldh);
                        }
                        i__1 = j - i1;
                        d_cnjg(&z__1, &temp);
                        zscal_(&i__1, &z__1, &h[i1 + j * *ldh], &c__1);
                        if (*wantz) {
                            d_cnjg(&z__1, &temp);
                            zscal_(&nz, &z__1, &z[*iloz-1 + j * *ldz], &c__1);
                        }
                    }
                }
            }
        }

/*        Ensure that H(I,I-1) is real. */

        i__1 = i + (i - 1) * *ldh;
        temp.r = h[i__1].r, temp.i = h[i__1].i;
        if (temp.i != 0.) {
            d__1 = temp.r;
            d__2 = temp.i;
            rtemp = dlapy2_(&d__1, &d__2);
            i__1 = i + (i - 1) * *ldh;
            h[i__1].r = rtemp, h[i__1].i = 0.;
            temp.r /= rtemp, temp.i /= rtemp;
            if (i2 > i) {
                i__1 = i2 - i;
                d_cnjg(&z__1, &temp);
                zscal_(&i__1, &z__1, &h[i + (i + 1) * *ldh], ldh);
            }
            i__1 = i - i1;
            zscal_(&i__1, &temp, &h[i1 + i * *ldh], &c__1);
            if (*wantz) {
                zscal_(&nz, &temp, &z[*iloz-1 + i * *ldz], &c__1);
            }
        }
    }

/*     Failure to converge in remaining number of iterations */

    *info = i+1;

} /* zlahqr_ */

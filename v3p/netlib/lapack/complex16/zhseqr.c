/* lapack/complex16/zhseqr.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "v3p_netlib.h"

/* Table of constant values */

static doublecomplex c_b1 = {0.,0.};
static doublecomplex c_b2 = {1.,0.};
static integer c__1 = 1;
static integer c__4 = 4;
static integer c_n1 = -1;
static integer c__2 = 2;
static integer c__8 = 8;
static integer c__15 = 15;
static logical c_false = FALSE_;

/*<    >*/
/* Subroutine */ int zhseqr_(char *job, char *compz, integer *n, integer *ilo,
         integer *ihi, doublecomplex *h__, integer *ldh, doublecomplex *w,
        doublecomplex *z__, integer *ldz, doublecomplex *work, integer *lwork,
         integer *info, ftnlen job_len, ftnlen compz_len)
{
    /* System generated locals */
    address a__1[2];
    integer h_dim1, h_offset, z_dim1, z_offset, i__1, i__2, i__3, i__4[2],
            i__5, i__6;
    doublereal d__1, d__2, d__3, d__4;
    doublecomplex z__1;
    char ch__1[2];

    /* Builtin functions */
    double d_imag(doublecomplex *);
    void d_cnjg(doublecomplex *, doublecomplex *);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    integer i__, j, k, l;
    doublecomplex s[225]        /* was [15][15] */, v[16];
    integer i1, i2, ii, nh, nr, ns, nv;
    doublecomplex vv[16];
    integer itn;
    doublecomplex tau;
    integer its;
    doublereal ulp, tst1;
    integer maxb, ierr;
    doublereal unfl;
    doublecomplex temp;
    doublereal ovfl;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int zscal_(integer *, doublecomplex *,
            doublecomplex *, integer *);
    integer itemp;
    doublereal rtemp;
    extern /* Subroutine */ int zgemv_(char *, integer *, integer *,
            doublecomplex *, doublecomplex *, integer *, doublecomplex *,
            integer *, doublecomplex *, doublecomplex *, integer *, ftnlen);
    logical initz, wantt, wantz;
    doublereal rwork[1];
    extern /* Subroutine */ int zcopy_(integer *, doublecomplex *, integer *,
            doublecomplex *, integer *);
    extern doublereal dlapy2_(doublereal *, doublereal *);
    extern /* Subroutine */ int dlabad_(doublereal *, doublereal *);
    extern doublereal dlamch_(char *, ftnlen);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *,
            integer *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int zdscal_(integer *, doublereal *,
            doublecomplex *, integer *), zlarfg_(integer *, doublecomplex *,
            doublecomplex *, integer *, doublecomplex *);
    extern integer izamax_(integer *, doublecomplex *, integer *);
    extern doublereal zlanhs_(char *, integer *, doublecomplex *, integer *,
            doublereal *, ftnlen);
    extern /* Subroutine */ int zlahqr_(logical *, logical *, integer *,
            integer *, integer *, doublecomplex *, integer *, doublecomplex *,
             integer *, integer *, doublecomplex *, integer *, integer *),
            zlacpy_(char *, integer *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *, ftnlen), zlaset_(char *, integer *,
            integer *, doublecomplex *, doublecomplex *, doublecomplex *,
            integer *, ftnlen), zlarfx_(char *, integer *, integer *,
            doublecomplex *, doublecomplex *, doublecomplex *, integer *,
            doublecomplex *, ftnlen);
    doublereal smlnum;
    logical lquery;
    (void)job_len;
    (void)compz_len;

/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          COMPZ, JOB >*/
/*<       INTEGER            IHI, ILO, INFO, LDH, LDZ, LWORK, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         H( LDH, * ), W( * ), WORK( * ), Z( LDZ, * ) >*/
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
/*          On exit, if JOB = 'S', H contains the upper triangular matrix */
/*          T from the Schur decomposition (the Schur form). If */
/*          JOB = 'E', the contents of H are unspecified on exit. */

/*  LDH     (input) INTEGER */
/*          The leading dimension of the array H. LDH >= max(1,N). */

/*  W       (output) COMPLEX*16 array, dimension (N) */
/*          The computed eigenvalues. If JOB = 'S', the eigenvalues are */
/*          stored in the same order as on the diagonal of the Schur form */
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

/*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK) */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK.  LWORK >= max(1,N). */

/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value */
/*          > 0:  if INFO = i, ZHSEQR failed to compute all the */
/*                eigenvalues in a total of 30*(IHI-ILO+1) iterations; */
/*                elements 1:ilo-1 and i+1:n of W contain those */
/*                eigenvalues which have been successfully computed. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       COMPLEX*16         ZERO, ONE >*/
/*<    >*/
/*<       DOUBLE PRECISION   RZERO, RONE, CONST >*/
/*<    >*/
/*<       INTEGER            NSMAX, LDS >*/
/*<       PARAMETER          ( NSMAX = 15, LDS = NSMAX ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            INITZ, LQUERY, WANTT, WANTZ >*/
/*<    >*/
/*<       DOUBLE PRECISION   OVFL, RTEMP, SMLNUM, TST1, ULP, UNFL >*/
/*<       COMPLEX*16         CDUM, TAU, TEMP >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       DOUBLE PRECISION   RWORK( 1 ) >*/
/*<       COMPLEX*16         S( LDS, NSMAX ), V( NSMAX+1 ), VV( NSMAX+1 ) >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       INTEGER            ILAENV, IZAMAX >*/
/*<       DOUBLE PRECISION   DLAMCH, DLAPY2, ZLANHS >*/
/*<       EXTERNAL           LSAME, ILAENV, IZAMAX, DLAMCH, DLAPY2, ZLANHS >*/
/*     .. */
/*     .. External Subroutines .. */
/*<    >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, DCONJG, DIMAG, MAX, MIN >*/
/*     .. */
/*     .. Statement Functions .. */
/*<       DOUBLE PRECISION   CABS1 >*/
/*     .. */
/*     .. Statement Function definitions .. */
/*<       CABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) ) >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Decode and test the input parameters */

/*<       WANTT = LSAME( JOB, 'S' ) >*/
    /* Parameter adjustments */
    h_dim1 = *ldh;
    h_offset = 1 + h_dim1;
    h__ -= h_offset;
    --w;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --work;

    /* Function Body */
    wantt = lsame_(job, "S", (ftnlen)1, (ftnlen)1);
/*<       INITZ = LSAME( COMPZ, 'I' ) >*/
    initz = lsame_(compz, "I", (ftnlen)1, (ftnlen)1);
/*<       WANTZ = INITZ .OR. LSAME( COMPZ, 'V' ) >*/
    wantz = initz || lsame_(compz, "V", (ftnlen)1, (ftnlen)1);

/*<       INFO = 0 >*/
    *info = 0;
/*<       WORK( 1 ) = MAX( 1, N ) >*/
    i__1 = max(1,*n);
    work[1].r = (doublereal) i__1, work[1].i = 0.;
/*<       LQUERY = ( LWORK.EQ.-1 ) >*/
    lquery = *lwork == -1;
/*<       IF( .NOT.LSAME( JOB, 'E' ) .AND. .NOT.WANTT ) THEN >*/
    if (! lsame_(job, "E", (ftnlen)1, (ftnlen)1) && ! wantt) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( .NOT.LSAME( COMPZ, 'N' ) .AND. .NOT.WANTZ ) THEN >*/
    } else if (! lsame_(compz, "N", (ftnlen)1, (ftnlen)1) && ! wantz) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( ILO.LT.1 .OR. ILO.GT.MAX( 1, N ) ) THEN >*/
    } else if (*ilo < 1 || *ilo > max(1,*n)) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( IHI.LT.MIN( ILO, N ) .OR. IHI.GT.N ) THEN >*/
    } else if (*ihi < min(*ilo,*n) || *ihi > *n) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( LDH.LT.MAX( 1, N ) ) THEN >*/
    } else if (*ldh < max(1,*n)) {
/*<          INFO = -7 >*/
        *info = -7;
/*<       ELSE IF( LDZ.LT.1 .OR. WANTZ .AND. LDZ.LT.MAX( 1, N ) ) THEN >*/
    } else if (*ldz < 1 || (wantz && *ldz < max(1,*n))) {
/*<          INFO = -10 >*/
        *info = -10;
/*<       ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN >*/
    } else if (*lwork < max(1,*n) && ! lquery) {
/*<          INFO = -12 >*/
        *info = -12;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZHSEQR', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZHSEQR", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       ELSE IF( LQUERY ) THEN >*/
    } else if (lquery) {
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Initialize Z, if necessary */

/*<    >*/
    if (initz) {
        zlaset_("Full", n, n, &c_b1, &c_b2, &z__[z_offset], ldz, (ftnlen)4);
    }

/*     Store the eigenvalues isolated by ZGEBAL. */

/*<       DO 10 I = 1, ILO - 1 >*/
    i__1 = *ilo - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          W( I ) = H( I, I ) >*/
        i__2 = i__;
        i__3 = i__ + i__ * h_dim1;
        w[i__2].r = h__[i__3].r, w[i__2].i = h__[i__3].i;
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       DO 20 I = IHI + 1, N >*/
    i__1 = *n;
    for (i__ = *ihi + 1; i__ <= i__1; ++i__) {
/*<          W( I ) = H( I, I ) >*/
        i__2 = i__;
        i__3 = i__ + i__ * h_dim1;
        w[i__2].r = h__[i__3].r, w[i__2].i = h__[i__3].i;
/*<    20 CONTINUE >*/
/* L20: */
    }

/*     Quick return if possible. */

/*<    >*/
    if (*n == 0) {
        return 0;
    }
/*<       IF( ILO.EQ.IHI ) THEN >*/
    if (*ilo == *ihi) {
/*<          W( ILO ) = H( ILO, ILO ) >*/
        i__1 = *ilo;
        i__2 = *ilo + *ilo * h_dim1;
        w[i__1].r = h__[i__2].r, w[i__1].i = h__[i__2].i;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Set rows and columns ILO to IHI to zero below the first */
/*     subdiagonal. */

/*<       DO 40 J = ILO, IHI - 2 >*/
    i__1 = *ihi - 2;
    for (j = *ilo; j <= i__1; ++j) {
/*<          DO 30 I = J + 2, N >*/
        i__2 = *n;
        for (i__ = j + 2; i__ <= i__2; ++i__) {
/*<             H( I, J ) = ZERO >*/
            i__3 = i__ + j * h_dim1;
            h__[i__3].r = 0., h__[i__3].i = 0.;
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<    40 CONTINUE >*/
/* L40: */
    }
/*<       NH = IHI - ILO + 1 >*/
    nh = *ihi - *ilo + 1;

/*     I1 and I2 are the indices of the first row and last column of H */
/*     to which transformations must be applied. If eigenvalues only are */
/*     being computed, I1 and I2 are re-set inside the main loop. */

/*<       IF( WANTT ) THEN >*/
    if (wantt) {
/*<          I1 = 1 >*/
        i1 = 1;
/*<          I2 = N >*/
        i2 = *n;
/*<       ELSE >*/
    } else {
/*<          I1 = ILO >*/
        i1 = *ilo;
/*<          I2 = IHI >*/
        i2 = *ihi;
/*<       END IF >*/
    }

/*     Ensure that the subdiagonal elements are real. */

/*<       DO 50 I = ILO + 1, IHI >*/
    i__1 = *ihi;
    for (i__ = *ilo + 1; i__ <= i__1; ++i__) {
/*<          TEMP = H( I, I-1 ) >*/
        i__2 = i__ + (i__ - 1) * h_dim1;
        temp.r = h__[i__2].r, temp.i = h__[i__2].i;
/*<          IF( DIMAG( TEMP ).NE.RZERO ) THEN >*/
        if (d_imag(&temp) != 0.) {
/*<             RTEMP = DLAPY2( DBLE( TEMP ), DIMAG( TEMP ) ) >*/
            d__1 = temp.r;
            d__2 = d_imag(&temp);
            rtemp = dlapy2_(&d__1, &d__2);
/*<             H( I, I-1 ) = RTEMP >*/
            i__2 = i__ + (i__ - 1) * h_dim1;
            h__[i__2].r = rtemp, h__[i__2].i = 0.;
/*<             TEMP = TEMP / RTEMP >*/
            z__1.r = temp.r / rtemp, z__1.i = temp.i / rtemp;
            temp.r = z__1.r, temp.i = z__1.i;
/*<    >*/
            if (i2 > i__) {
                i__2 = i2 - i__;
                d_cnjg(&z__1, &temp);
                zscal_(&i__2, &z__1, &h__[i__ + (i__ + 1) * h_dim1], ldh);
            }
/*<             CALL ZSCAL( I-I1, TEMP, H( I1, I ), 1 ) >*/
            i__2 = i__ - i1;
            zscal_(&i__2, &temp, &h__[i1 + i__ * h_dim1], &c__1);
/*<    >*/
            if (i__ < *ihi) {
                i__2 = i__ + 1 + i__ * h_dim1;
                i__3 = i__ + 1 + i__ * h_dim1;
                z__1.r = temp.r * h__[i__3].r - temp.i * h__[i__3].i, z__1.i =
                         temp.r * h__[i__3].i + temp.i * h__[i__3].r;
                h__[i__2].r = z__1.r, h__[i__2].i = z__1.i;
            }
/*<    >*/
            if (wantz) {
                zscal_(&nh, &temp, &z__[*ilo + i__ * z_dim1], &c__1);
            }
/*<          END IF >*/
        }
/*<    50 CONTINUE >*/
/* L50: */
    }

/*     Determine the order of the multi-shift QR algorithm to be used. */

/*<       NS = ILAENV( 4, 'ZHSEQR', JOB // COMPZ, N, ILO, IHI, -1 ) >*/
/* Writing concatenation */
    i__4[0] = 1, a__1[0] = job;
    i__4[1] = 1, a__1[1] = compz;
    s_cat(ch__1, a__1, i__4, &c__2, (ftnlen)2);
    ns = ilaenv_(&c__4, "ZHSEQR", ch__1, n, ilo, ihi, &c_n1, (ftnlen)6, (
            ftnlen)2);
/*<       MAXB = ILAENV( 8, 'ZHSEQR', JOB // COMPZ, N, ILO, IHI, -1 ) >*/
/* Writing concatenation */
    i__4[0] = 1, a__1[0] = job;
    i__4[1] = 1, a__1[1] = compz;
    s_cat(ch__1, a__1, i__4, &c__2, (ftnlen)2);
    maxb = ilaenv_(&c__8, "ZHSEQR", ch__1, n, ilo, ihi, &c_n1, (ftnlen)6, (
            ftnlen)2);
/*<       IF( NS.LE.1 .OR. NS.GT.NH .OR. MAXB.GE.NH ) THEN >*/
    if (ns <= 1 || ns > nh || maxb >= nh) {

/*        Use the standard double-shift algorithm */

/*<    >*/
        zlahqr_(&wantt, &wantz, n, ilo, ihi, &h__[h_offset], ldh, &w[1], ilo,
                ihi, &z__[z_offset], ldz, info);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }
/*<       MAXB = MAX( 2, MAXB ) >*/
    maxb = max(2,maxb);
/*<       NS = MIN( NS, MAXB, NSMAX ) >*/
/* Computing MIN */
    i__1 = min(ns,maxb);
    ns = min(i__1,15);

/*     Now 1 < NS <= MAXB < NH. */

/*     Set machine-dependent constants for the stopping criterion. */
/*     If norm(H) <= sqrt(OVFL), overflow should not occur. */

/*<       UNFL = DLAMCH( 'Safe minimum' ) >*/
    unfl = dlamch_("Safe minimum", (ftnlen)12);
/*<       OVFL = RONE / UNFL >*/
    ovfl = 1. / unfl;
/*<       CALL DLABAD( UNFL, OVFL ) >*/
    dlabad_(&unfl, &ovfl);
/*<       ULP = DLAMCH( 'Precision' ) >*/
    ulp = dlamch_("Precision", (ftnlen)9);
/*<       SMLNUM = UNFL*( NH / ULP ) >*/
    smlnum = unfl * (nh / ulp);

/*     ITN is the total number of multiple-shift QR iterations allowed. */

/*<       ITN = 30*NH >*/
    itn = nh * 30;

/*     The main loop begins here. I is the loop index and decreases from */
/*     IHI to ILO in steps of at most MAXB. Each iteration of the loop */
/*     works with the active submatrix in rows and columns L to I. */
/*     Eigenvalues I+1 to IHI have already converged. Either L = ILO, or */
/*     H(L,L-1) is negligible so that the matrix splits. */

/*<       I = IHI >*/
    i__ = *ihi;
/*<    60 CONTINUE >*/
L60:
/*<    >*/
    if (i__ < *ilo) {
        goto L180;
    }

/*     Perform multiple-shift QR iterations on rows and columns ILO to I */
/*     until a submatrix of order at most MAXB splits off at the bottom */
/*     because a subdiagonal element has become negligible. */

/*<       L = ILO >*/
    l = *ilo;
/*<       DO 160 ITS = 0, ITN >*/
    i__1 = itn;
    for (its = 0; its <= i__1; ++its) {

/*        Look for a single small subdiagonal element. */

/*<          DO 70 K = I, L + 1, -1 >*/
        i__2 = l + 1;
        for (k = i__; k >= i__2; --k) {
/*<             TST1 = CABS1( H( K-1, K-1 ) ) + CABS1( H( K, K ) ) >*/
            i__3 = k - 1 + (k - 1) * h_dim1;
            i__5 = k + k * h_dim1;
            tst1 = (d__1 = h__[i__3].r, abs(d__1)) + (d__2 = d_imag(&h__[k -
                    1 + (k - 1) * h_dim1]), abs(d__2)) + ((d__3 = h__[i__5].r,
                     abs(d__3)) + (d__4 = d_imag(&h__[k + k * h_dim1]), abs(
                    d__4)));
/*<    >*/
            if (tst1 == 0.) {
                i__3 = i__ - l + 1;
                tst1 = zlanhs_("1", &i__3, &h__[l + l * h_dim1], ldh, rwork, (
                        ftnlen)1);
            }
/*<    >*/
            i__3 = k + (k - 1) * h_dim1;
/* Computing MAX */
            d__2 = ulp * tst1;
            if ((d__1 = h__[i__3].r, abs(d__1)) <= max(d__2,smlnum)) {
                goto L80;
            }
/*<    70    CONTINUE >*/
/* L70: */
        }
/*<    80    CONTINUE >*/
L80:
/*<          L = K >*/
        l = k;
/*<          IF( L.GT.ILO ) THEN >*/
        if (l > *ilo) {

/*           H(L,L-1) is negligible. */

/*<             H( L, L-1 ) = ZERO >*/
            i__2 = l + (l - 1) * h_dim1;
            h__[i__2].r = 0., h__[i__2].i = 0.;
/*<          END IF >*/
        }

/*        Exit from loop if a submatrix of order <= MAXB has split off. */

/*<    >*/
        if (l >= i__ - maxb + 1) {
            goto L170;
        }

/*        Now the active submatrix is in rows and columns L to I. If */
/*        eigenvalues only are being computed, only the active submatrix */
/*        need be transformed. */

/*<          IF( .NOT.WANTT ) THEN >*/
        if (! wantt) {
/*<             I1 = L >*/
            i1 = l;
/*<             I2 = I >*/
            i2 = i__;
/*<          END IF >*/
        }

/*<          IF( ITS.EQ.20 .OR. ITS.EQ.30 ) THEN >*/
        if (its == 20 || its == 30) {

/*           Exceptional shifts. */

/*<             DO 90 II = I - NS + 1, I >*/
            i__2 = i__;
            for (ii = i__ - ns + 1; ii <= i__2; ++ii) {
/*<    >*/
                i__3 = ii;
                i__5 = ii + (ii - 1) * h_dim1;
                i__6 = ii + ii * h_dim1;
                d__3 = ((d__1 = h__[i__5].r, abs(d__1)) + (d__2 = h__[i__6].r,
                         abs(d__2))) * 1.5;
                w[i__3].r = d__3, w[i__3].i = 0.;
/*<    90       CONTINUE >*/
/* L90: */
            }
/*<          ELSE >*/
        } else {

/*           Use eigenvalues of trailing submatrix of order NS as shifts. */

/*<    >*/
            zlacpy_("Full", &ns, &ns, &h__[i__ - ns + 1 + (i__ - ns + 1) *
                    h_dim1], ldh, s, &c__15, (ftnlen)4);
/*<    >*/
            zlahqr_(&c_false, &c_false, &ns, &c__1, &ns, s, &c__15, &w[i__ -
                    ns + 1], &c__1, &ns, &z__[z_offset], ldz, &ierr);
/*<             IF( IERR.GT.0 ) THEN >*/
            if (ierr > 0) {

/*              If ZLAHQR failed to compute all NS eigenvalues, use the */
/*              unconverged diagonal elements as the remaining shifts. */

/*<                DO 100 II = 1, IERR >*/
                i__2 = ierr;
                for (ii = 1; ii <= i__2; ++ii) {
/*<                   W( I-NS+II ) = S( II, II ) >*/
                    i__3 = i__ - ns + ii;
                    i__5 = ii + ii * 15 - 16;
                    w[i__3].r = s[i__5].r, w[i__3].i = s[i__5].i;
/*<   100          CONTINUE >*/
/* L100: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }

/*        Form the first column of (G-w(1)) (G-w(2)) . . . (G-w(ns)) */
/*        where G is the Hessenberg submatrix H(L:I,L:I) and w is */
/*        the vector of shifts (stored in W). The result is */
/*        stored in the local array V. */

/*<          V( 1 ) = ONE >*/
        v[0].r = 1., v[0].i = 0.;
/*<          DO 110 II = 2, NS + 1 >*/
        i__2 = ns + 1;
        for (ii = 2; ii <= i__2; ++ii) {
/*<             V( II ) = ZERO >*/
            i__3 = ii - 1;
            v[i__3].r = 0., v[i__3].i = 0.;
/*<   110    CONTINUE >*/
/* L110: */
        }
/*<          NV = 1 >*/
        nv = 1;
/*<          DO 130 J = I - NS + 1, I >*/
        i__2 = i__;
        for (j = i__ - ns + 1; j <= i__2; ++j) {
/*<             CALL ZCOPY( NV+1, V, 1, VV, 1 ) >*/
            i__3 = nv + 1;
            zcopy_(&i__3, v, &c__1, vv, &c__1);
/*<    >*/
            i__3 = nv + 1;
            i__5 = j;
            z__1.r = -w[i__5].r, z__1.i = -w[i__5].i;
            zgemv_("No transpose", &i__3, &nv, &c_b2, &h__[l + l * h_dim1],
                    ldh, vv, &c__1, &z__1, v, &c__1, (ftnlen)12);
/*<             NV = NV + 1 >*/
            ++nv;

/*           Scale V(1:NV) so that max(abs(V(i))) = 1. If V is zero, */
/*           reset it to the unit vector. */

/*<             ITEMP = IZAMAX( NV, V, 1 ) >*/
            itemp = izamax_(&nv, v, &c__1);
/*<             RTEMP = CABS1( V( ITEMP ) ) >*/
            i__3 = itemp - 1;
            rtemp = (d__1 = v[i__3].r, abs(d__1)) + (d__2 = d_imag(&v[itemp -
                    1]), abs(d__2));
/*<             IF( RTEMP.EQ.RZERO ) THEN >*/
            if (rtemp == 0.) {
/*<                V( 1 ) = ONE >*/
                v[0].r = 1., v[0].i = 0.;
/*<                DO 120 II = 2, NV >*/
                i__3 = nv;
                for (ii = 2; ii <= i__3; ++ii) {
/*<                   V( II ) = ZERO >*/
                    i__5 = ii - 1;
                    v[i__5].r = 0., v[i__5].i = 0.;
/*<   120          CONTINUE >*/
/* L120: */
                }
/*<             ELSE >*/
            } else {
/*<                RTEMP = MAX( RTEMP, SMLNUM ) >*/
                rtemp = max(rtemp,smlnum);
/*<                CALL ZDSCAL( NV, RONE / RTEMP, V, 1 ) >*/
                d__1 = 1. / rtemp;
                zdscal_(&nv, &d__1, v, &c__1);
/*<             END IF >*/
            }
/*<   130    CONTINUE >*/
/* L130: */
        }

/*        Multiple-shift QR step */

/*<          DO 150 K = L, I - 1 >*/
        i__2 = i__ - 1;
        for (k = l; k <= i__2; ++k) {

/*           The first iteration of this loop determines a reflection G */
/*           from the vector V and applies it from left and right to H, */
/*           thus creating a nonzero bulge below the subdiagonal. */

/*           Each subsequent iteration determines a reflection G to */
/*           restore the Hessenberg form in the (K-1)th column, and thus */
/*           chases the bulge one step toward the bottom of the active */
/*           submatrix. NR is the order of G. */

/*<             NR = MIN( NS+1, I-K+1 ) >*/
/* Computing MIN */
            i__3 = ns + 1, i__5 = i__ - k + 1;
            nr = min(i__3,i__5);
/*<    >*/
            if (k > l) {
                zcopy_(&nr, &h__[k + (k - 1) * h_dim1], &c__1, v, &c__1);
            }
/*<             CALL ZLARFG( NR, V( 1 ), V( 2 ), 1, TAU ) >*/
            zlarfg_(&nr, v, &v[1], &c__1, &tau);
/*<             IF( K.GT.L ) THEN >*/
            if (k > l) {
/*<                H( K, K-1 ) = V( 1 ) >*/
                i__3 = k + (k - 1) * h_dim1;
                h__[i__3].r = v[0].r, h__[i__3].i = v[0].i;
/*<                DO 140 II = K + 1, I >*/
                i__3 = i__;
                for (ii = k + 1; ii <= i__3; ++ii) {
/*<                   H( II, K-1 ) = ZERO >*/
                    i__5 = ii + (k - 1) * h_dim1;
                    h__[i__5].r = 0., h__[i__5].i = 0.;
/*<   140          CONTINUE >*/
/* L140: */
                }
/*<             END IF >*/
            }
/*<             V( 1 ) = ONE >*/
            v[0].r = 1., v[0].i = 0.;

/*           Apply G' from the left to transform the rows of the matrix */
/*           in columns K to I2. */

/*<    >*/
            i__3 = i2 - k + 1;
            d_cnjg(&z__1, &tau);
            zlarfx_("Left", &nr, &i__3, v, &z__1, &h__[k + k * h_dim1], ldh, &
                    work[1], (ftnlen)4);

/*           Apply G from the right to transform the columns of the */
/*           matrix in rows I1 to min(K+NR,I). */

/*<    >*/
/* Computing MIN */
            i__5 = k + nr;
            i__3 = min(i__5,i__) - i1 + 1;
            zlarfx_("Right", &i__3, &nr, v, &tau, &h__[i1 + k * h_dim1], ldh,
                    &work[1], (ftnlen)5);

/*<             IF( WANTZ ) THEN >*/
            if (wantz) {

/*              Accumulate transformations in the matrix Z */

/*<    >*/
                zlarfx_("Right", &nh, &nr, v, &tau, &z__[*ilo + k * z_dim1],
                        ldz, &work[1], (ftnlen)5);
/*<             END IF >*/
            }
/*<   150    CONTINUE >*/
/* L150: */
        }

/*        Ensure that H(I,I-1) is real. */

/*<          TEMP = H( I, I-1 ) >*/
        i__2 = i__ + (i__ - 1) * h_dim1;
        temp.r = h__[i__2].r, temp.i = h__[i__2].i;
/*<          IF( DIMAG( TEMP ).NE.RZERO ) THEN >*/
        if (d_imag(&temp) != 0.) {
/*<             RTEMP = DLAPY2( DBLE( TEMP ), DIMAG( TEMP ) ) >*/
            d__1 = temp.r;
            d__2 = d_imag(&temp);
            rtemp = dlapy2_(&d__1, &d__2);
/*<             H( I, I-1 ) = RTEMP >*/
            i__2 = i__ + (i__ - 1) * h_dim1;
            h__[i__2].r = rtemp, h__[i__2].i = 0.;
/*<             TEMP = TEMP / RTEMP >*/
            z__1.r = temp.r / rtemp, z__1.i = temp.i / rtemp;
            temp.r = z__1.r, temp.i = z__1.i;
/*<    >*/
            if (i2 > i__) {
                i__2 = i2 - i__;
                d_cnjg(&z__1, &temp);
                zscal_(&i__2, &z__1, &h__[i__ + (i__ + 1) * h_dim1], ldh);
            }
/*<             CALL ZSCAL( I-I1, TEMP, H( I1, I ), 1 ) >*/
            i__2 = i__ - i1;
            zscal_(&i__2, &temp, &h__[i1 + i__ * h_dim1], &c__1);
/*<             IF( WANTZ ) THEN >*/
            if (wantz) {
/*<                CALL ZSCAL( NH, TEMP, Z( ILO, I ), 1 ) >*/
                zscal_(&nh, &temp, &z__[*ilo + i__ * z_dim1], &c__1);
/*<             END IF >*/
            }
/*<          END IF >*/
        }

/*<   160 CONTINUE >*/
/* L160: */
    }

/*     Failure to converge in remaining number of iterations */

/*<       INFO = I >*/
    *info = i__;
/*<       RETURN >*/
    return 0;

/*<   170 CONTINUE >*/
L170:

/*     A submatrix of order <= MAXB in rows and columns L to I has split */
/*     off. Use the double-shift QR algorithm to handle it. */

/*<    >*/
    zlahqr_(&wantt, &wantz, n, &l, &i__, &h__[h_offset], ldh, &w[1], ilo, ihi,
             &z__[z_offset], ldz, info);
/*<    >*/
    if (*info > 0) {
        return 0;
    }

/*     Decrement number of remaining iterations, and return to start of */
/*     the main loop with a new value of I. */

/*<       ITN = ITN - ITS >*/
    itn -= its;
/*<       I = L - 1 >*/
    i__ = l - 1;
/*<       GO TO 60 >*/
    goto L60;

/*<   180 CONTINUE >*/
L180:
/*<       WORK( 1 ) = MAX( 1, N ) >*/
    i__1 = max(1,*n);
    work[1].r = (doublereal) i__1, work[1].i = 0.;
/*<       RETURN >*/
    return 0;

/*     End of ZHSEQR */

/*<       END >*/
} /* zhseqr_ */

#ifdef __cplusplus
        }
#endif

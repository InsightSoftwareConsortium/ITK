/* rpoly.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal p[101], qp[101], k[101], qk[101], svk[101], sr, si, u, v, a, b,
	     c, d, a1, a2, a3, a6, a7, e, f, g, h, szr, szi, lzr, lzi;
    real eta, are, mre;
    integer n, nn;
} global_;

#define global_1 global_

/* Table of constant values */

static doublereal c_b41 = 1.;

/* ====================================================================== */
/* NIST Guide to Available Math Software. */
/* Fullsource for module 493 from package TOMS. */
/* Retrieved from NETLIB on Wed Jul  3 11:47:53 1996. */
/* ====================================================================== */
/* Subroutine */ int rpoly_(op, degree, zeror, zeroi, fail)
doublereal *op;
integer *degree;
doublereal *zeror, *zeroi;
logical *fail;
{
    /* System generated locals */
    integer i__1;
    real r__1;
    doublereal d__1;

    /* Builtin functions */
    double log(), pow_di(), exp();

    /* Local variables */
    static real base;
    extern /* Subroutine */ int quad_();
    static doublereal temp[101];
    static real cosr, sinr;
    static integer i, j, l;
    static doublereal t;
    static real x, infin;
    static logical zerok;
    static doublereal aa, bb, cc;
    static real df, ff;
    static integer jj;
    static real sc, lo, dx, pt[101], xm;
    static integer nz;
    static doublereal factor;
    static real xx, yy, smalno;
    extern /* Subroutine */ int fxshfr_();
    static integer nm1;
    static real bnd, min_, max_;
    static integer cnt;
    static real xxx;

/* FINDS THE ZEROS OF A REAL POLYNOMIAL */
/* OP  - DOUBLE PRECISION VECTOR OF COEFFICIENTS IN */
/*       ORDER OF DECREASING POWERS. */
/* DEGREE   - INTEGER DEGREE OF POLYNOMIAL. */
/* ZEROR, ZEROI - OUTPUT DOUBLE PRECISION VECTORS OF */
/*                REAL AND IMAGINARY PARTS OF THE */
/*                ZEROS. */
/* FAIL  - OUTPUT LOGICAL PARAMETER, TRUE ONLY IF */
/*         LEADING COEFFICIENT IS ZERO OR IF RPOLY */
/*         HAS FOUND FEWER THAN DEGREE ZEROS. */
/*         IN THE LATTER CASE DEGREE IS RESET TO */
/*         THE NUMBER OF ZEROS FOUND. */
/* TO CHANGE THE SIZE OF POLYNOMIALS WHICH CAN BE */
/* SOLVED, RESET THE DIMENSIONS OF THE ARRAYS IN THE */
/* COMMON AREA AND IN THE FOLLOWING DECLARATIONS. */
/* THE SUBROUTINE USES SINGLE PRECISION CALCULATIONS */
/* FOR SCALING, BOUNDS AND ERROR CALCULATIONS. ALL */
/* CALCULATIONS FOR THE ITERATIONS ARE DONE IN DOUBLE */
/* PRECISION. */
/* THE FOLLOWING STATEMENTS SET MACHINE CONSTANTS USED */
/* IN VARIOUS PARTS OF THE PROGRAM. THE MEANING OF THE */
/* FOUR CONSTANTS ARE... */
/* ETA     THE MAXIMUM RELATIVE REPRESENTATION ERROR */
/*         WHICH CAN BE DESCRIBED AS THE SMALLEST */
/*         POSITIVE FLOATING POINT NUMBER SUCH THAT */
/*         1.D0+ETA IS GREATER THAN 1. */
/* INFINY  THE LARGEST FLOATING-POINT NUMBER. */
/* SMALNO  THE SMALLEST POSITIVE FLOATING-POINT NUMBER */
/*         IF THE EXPONENT RANGE DIFFERS IN SINGLE AND */
/*         DOUBLE PRECISION THEN SMALNO AND INFIN */
/*         SHOULD INDICATE THE SMALLER RANGE. */
/* BASE    THE BASE OF THE FLOATING-POINT NUMBER */
/*         SYSTEM USED. */
/* THE VALUES BELOW CORRESPOND TO THE BURROUGHS B6700 */
/* changed for sparc, but these seem better -- awf */
    /* Parameter adjustments */
    --zeroi;
    --zeror;
    --op;

    /* Function Body */
    base = (float)2.;
    global_1.eta = (float)2.23e-16;
    /* the sun compiler will not compile with the number too large for float */
#ifdef __SUNPRO_C
    infin = (float)3.40282346638528860e+38;
#else
    infin = (float)1e50; /* on purpose too large to fit in `float' type */
#endif
    smalno = (float)1e-45;
/* ARE AND MRE REFER TO THE UNIT ERROR IN + AND * */
/* RESPECTIVELY. THEY ARE ASSUMED TO BE THE SAME AS */
/* ETA. */
    global_1.are = global_1.eta;
    global_1.mre = global_1.eta;
    lo = smalno / global_1.eta;
/* INITIALIZATION OF CONSTANTS FOR SHIFT ROTATION */
    xx = (float).70710678;
    yy = -(doublereal)xx;
    cosr = (float)-.069756474;
    sinr = (float).99756405;
    *fail = FALSE_;
    global_1.n = *degree;
    global_1.nn = global_1.n + 1;
/* ALGORITHM FAILS IF THE LEADING COEFFICIENT IS ZERO. */
    if (op[1] != 0.) {
	goto L10;
    }
    *fail = TRUE_;
    *degree = 0;
    return 0;
/* REMOVE THE ZEROS AT THE ORIGIN IF ANY */
L10:
    if (op[global_1.nn] != 0.) {
	goto L20;
    }
    j = *degree - global_1.n + 1;
    zeror[j] = 0.;
    zeroi[j] = 0.;
    --global_1.nn;
    --global_1.n;
    goto L10;
/* MAKE A COPY OF THE COEFFICIENTS */
L20:
    i__1 = global_1.nn;
    for (i = 1; i <= i__1; ++i) {
	global_1.p[i - 1] = op[i];
/* L30: */
    }
/* START THE ALGORITHM FOR ONE ZERO */
L40:
    if (global_1.n > 2) {
	goto L60;
    }
    if (global_1.n < 1) {
	return 0;
    }
/* CALCULATE THE FINAL ZERO OR PAIR OF ZEROS */
    if (global_1.n == 2) {
	goto L50;
    }
    zeror[*degree] = -global_1.p[1] / global_1.p[0];
    zeroi[*degree] = 0.;
    return 0;
L50:
    quad_(global_1.p, &global_1.p[1], &global_1.p[2], &zeror[*degree - 1], &
	    zeroi[*degree - 1], &zeror[*degree], &zeroi[*degree]);
    return 0;
/* FIND LARGEST AND SMALLEST MODULI OF COEFFICIENTS. */
L60:
    max_ = (float)0.;
    min_ = infin;
    i__1 = global_1.nn;
    for (i = 1; i <= i__1; ++i) {
	x = (r__1 = global_1.p[i - 1], dabs(r__1));
	if (x > max_) {
	    max_ = x;
	}
	if (x != (float)0. && x < min_) {
	    min_ = x;
	}
/* L70: */
    }
/* SCALE IF THERE ARE LARGE OR VERY SMALL COEFFICIENTS */
/* COMPUTES A SCALE FACTOR TO MULTIPLY THE */
/* COEFFICIENTS OF THE POLYNOMIAL. THE SCALING IS DONE */
/* TO AVOID OVERFLOW AND TO AVOID UNDETECTED UNDERFLOW */
/* INTERFERING WITH THE CONVERGENCE CRITERION. */
/* THE FACTOR IS A POWER OF THE BASE */
    sc = lo / min_;
    if (sc > (float)1.) {
	goto L80;
    }
    if (max_ < (float)10.) {
	goto L110;
    }
    if (sc == (float)0.) {
	sc = smalno;
    }
    goto L90;
L80:
    if (infin / sc < max_) {
	goto L110;
    }
L90:
    l = log(sc) / log(base) + (float).5;
    d__1 = base * 1.;
    factor = pow_di(&d__1, &l);
    if (factor == 1.) {
	goto L110;
    }
    i__1 = global_1.nn;
    for (i = 1; i <= i__1; ++i) {
	global_1.p[i - 1] = factor * global_1.p[i - 1];
/* L100: */
    }
/* COMPUTE LOWER BOUND ON MODULI OF ZEROS. */
L110:
    i__1 = global_1.nn;
    for (i = 1; i <= i__1; ++i) {
	pt[i - 1] = (r__1 = global_1.p[i - 1], dabs(r__1));
/* L120: */
    }
    pt[global_1.nn - 1] = -(doublereal)pt[global_1.nn - 1];
/* COMPUTE UPPER ESTIMATE OF BOUND */
    x = exp((log(-(doublereal)pt[global_1.nn - 1]) - log(pt[0])) / (real) 
	    global_1.n);
    if (pt[global_1.n - 1] == (float)0.) {
	goto L130;
    }
/* IF NEWTON STEP AT THE ORIGIN IS BETTER, USE IT. */
    xm = -(doublereal)pt[global_1.nn - 1] / pt[global_1.n - 1];
    if (xm < x) {
	x = xm;
    }
/* CHOP THE INTERVAL (0,X) UNTIL FF .LE. 0 */
L130:
    xm = x * (float).1;
    ff = pt[0];
    i__1 = global_1.nn;
    for (i = 2; i <= i__1; ++i) {
	ff = ff * xm + pt[i - 1];
/* L140: */
    }
    if (ff <= (float)0.) {
	goto L150;
    }
    x = xm;
    goto L130;
L150:
    dx = x;
/* DO NEWTON ITERATION UNTIL X CONVERGES TO TWO */
/* DECIMAL PLACES */
L160:
    if ((r__1 = dx / x, dabs(r__1)) <= (float).005) {
	goto L180;
    }
    ff = pt[0];
    df = ff;
    i__1 = global_1.n;
    for (i = 2; i <= i__1; ++i) {
	ff = ff * x + pt[i - 1];
	df = df * x + ff;
/* L170: */
    }
    ff = ff * x + pt[global_1.nn - 1];
    dx = ff / df;
    x -= dx;
    goto L160;
L180:
    bnd = x;
/* COMPUTE THE DERIVATIVE AS THE INTIAL K POLYNOMIAL */
/* AND DO 5 STEPS WITH NO SHIFT */
    nm1 = global_1.n - 1;
    i__1 = global_1.n;
    for (i = 2; i <= i__1; ++i) {
	global_1.k[i - 1] = (real) (global_1.nn - i) * global_1.p[i - 1] / (
		real) global_1.n;
/* L190: */
    }
    global_1.k[0] = global_1.p[0];
    aa = global_1.p[global_1.nn - 1];
    bb = global_1.p[global_1.n - 1];
    zerok = global_1.k[global_1.n - 1] == 0.;
    for (jj = 1; jj <= 5; ++jj) {
	cc = global_1.k[global_1.n - 1];
	if (zerok) {
	    goto L210;
	}
/* USE SCALED FORM OF RECURRENCE IF VALUE OF K AT 0 IS */
/* NONZERO */
	t = -aa / cc;
	i__1 = nm1;
	for (i = 1; i <= i__1; ++i) {
	    j = global_1.nn - i;
	    global_1.k[j - 1] = t * global_1.k[j - 2] + global_1.p[j - 1];
/* L200: */
	}
	global_1.k[0] = global_1.p[0];
	zerok = (d__1 = global_1.k[global_1.n - 1], abs(d__1)) <= abs(bb) * 
		global_1.eta * (float)10.;
	goto L230;
/* USE UNSCALED FORM OF RECURRENCE */
L210:
	i__1 = nm1;
	for (i = 1; i <= i__1; ++i) {
	    j = global_1.nn - i;
	    global_1.k[j - 1] = global_1.k[j - 2];
/* L220: */
	}
	global_1.k[0] = 0.;
	zerok = global_1.k[global_1.n - 1] == 0.;
L230:
	;
    }
/* SAVE K FOR RESTARTS WITH NEW SHIFTS */
    i__1 = global_1.n;
    for (i = 1; i <= i__1; ++i) {
	temp[i - 1] = global_1.k[i - 1];
/* L240: */
    }
/* LOOP TO SELECT THE QUADRATIC  CORRESPONDING TO EACH */
/* NEW SHIFT */
    for (cnt = 1; cnt <= 20; ++cnt) {
/* QUADRATIC CORRESPONDS TO A DOUBLE SHIFT TO A */
/* NON-REAL POINT AND ITS COMPLEX CONJUGATE. THE POINT */
/* HAS MODULUS BND AND AMPLITUDE ROTATED BY 94 DEGREES */
/* FROM THE PREVIOUS SHIFT */
	xxx = cosr * xx - sinr * yy;
	yy = sinr * xx + cosr * yy;
	xx = xxx;
	global_1.sr = bnd * xx;
	global_1.si = bnd * yy;
	global_1.u = global_1.sr * -2.;
	global_1.v = bnd;
/* SECOND STAGE CALCULATION, FIXED QUADRATIC */
	i__1 = cnt * 20;
	fxshfr_(&i__1, &nz);
	if (nz == 0) {
	    goto L260;
	}
/* THE SECOND STAGE JUMPS DIRECTLY TO ONE OF THE THIRD */
/* STAGE ITERATIONS AND RETURNS HERE IF SUCCESSFUL. */
/* DEFLATE THE POLYNOMIAL, STORE THE ZERO OR ZEROS AND */
/* RETURN TO THE MAIN ALGORITHM. */
	j = *degree - global_1.n + 1;
	zeror[j] = global_1.szr;
	zeroi[j] = global_1.szi;
	global_1.nn -= nz;
	global_1.n = global_1.nn - 1;
	i__1 = global_1.nn;
	for (i = 1; i <= i__1; ++i) {
	    global_1.p[i - 1] = global_1.qp[i - 1];
/* L250: */
	}
	if (nz == 1) {
	    goto L40;
	}
	zeror[j + 1] = global_1.lzr;
	zeroi[j + 1] = global_1.lzi;
	goto L40;
/* IF THE ITERATION IS UNSUCCESSFUL ANOTHER QUADRATIC */
/* IS CHOSEN AFTER RESTORING K */
L260:
	i__1 = global_1.n;
	for (i = 1; i <= i__1; ++i) {
	    global_1.k[i - 1] = temp[i - 1];
/* L270: */
	}
/* L280: */
    }
/* RETURN WITH FAILURE IF NO CONVERGENCE WITH 20 */
/* SHIFTS */
    *fail = TRUE_;
    *degree -= global_1.n;
    return 0;
} /* rpoly_ */

/* Subroutine */ int fxshfr_(l2, nz)
integer *l2, *nz;
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1;

    /* Local variables */
    static integer type;
    static logical stry, vtry;
    static integer i, j, iflag;
    static doublereal s;
    static real betas, betav;
    static logical spass;
    extern /* Subroutine */ int nextk_();
    static logical vpass;
    extern /* Subroutine */ int calcsc_();
    static doublereal ui, vi;
    static real ss, ts, tv, vv;
    extern /* Subroutine */ int realit_(), quadsd_(), quadit_(), newest_();
    static real oss, ots, otv, tss, ovv;
    static doublereal svu, svv;
    static real tvv;

/* COMPUTES UP TO  L2  FIXED SHIFT K-POLYNOMIALS, */
/* TESTING FOR CONVERGENCE IN THE LINEAR OR QUADRATIC */
/* CASE. INITIATES ONE OF THE VARIABLE SHIFT */
/* ITERATIONS AND RETURNS WITH THE NUMBER OF ZEROS */
/* FOUND. */
/* L2 - LIMIT OF FIXED SHIFT STEPS */
/* NZ - NUMBER OF ZEROS FOUND */
    *nz = 0;
    betav = (float).25;
    betas = (float).25;
    oss = global_1.sr;
    ovv = global_1.v;
/* EVALUATE POLYNOMIAL BY SYNTHETIC DIVISION */
    quadsd_(&global_1.nn, &global_1.u, &global_1.v, global_1.p, global_1.qp, &
	    global_1.a, &global_1.b);
    calcsc_(&type);
    i__1 = *l2;
    for (j = 1; j <= i__1; ++j) {
/* CALCULATE NEXT K POLYNOMIAL AND ESTIMATE V */
	nextk_(&type);
	calcsc_(&type);
	newest_(&type, &ui, &vi);
	vv = vi;
/* ESTIMATE S */
	ss = (float)0.;
	if (global_1.k[global_1.n - 1] != 0.) {
	    ss = -global_1.p[global_1.nn - 1] / global_1.k[global_1.n - 1];
	}
	tv = (float)1.;
	ts = (float)1.;
	if (j == 1 || type == 3) {
	    goto L70;
	}
/* COMPUTE RELATIVE MEASURES OF CONVERGENCE OF S AND V */
/* SEQUENCES */
	if (vv != (float)0.) {
	    tv = (r__1 = (vv - ovv) / vv, dabs(r__1));
	}
	if (ss != (float)0.) {
	    ts = (r__1 = (ss - oss) / ss, dabs(r__1));
	}
/* IF DECREASING, MULTIPLY TWO MOST RECENT */
/* CONVERGENCE MEASURES */
	tvv = (float)1.;
	if (tv < otv) {
	    tvv = tv * otv;
	}
	tss = (float)1.;
	if (ts < ots) {
	    tss = ts * ots;
	}
/* COMPARE WITH CONVERGENCE CRITERIA */
	vpass = tvv < betav;
	spass = tss < betas;
	if (! (spass || vpass)) {
	    goto L70;
	}
/* AT LEAST ONE SEQUENCE HAS PASSED THE CONVERGENCE */
/* TEST. STORE VARIABLES BEFORE ITERATING */
	svu = global_1.u;
	svv = global_1.v;
	i__2 = global_1.n;
	for (i = 1; i <= i__2; ++i) {
	    global_1.svk[i - 1] = global_1.k[i - 1];
/* L10: */
	}
	s = ss;
/* CHOOSE ITERATION ACCORDING TO THE FASTEST */
/* CONVERGING SEQUENCE */
	vtry = FALSE_;
	stry = FALSE_;
	if (spass && (! vpass || tss < tvv)) {
	    goto L40;
	}
L20:
	quadit_(&ui, &vi, nz);
	if (*nz > 0) {
	    return 0;
	}
/* QUADRATIC ITERATION HAS FAILED. FLAG THAT IT HAS */
/* BEEN TRIED AND DECREASE THE CONVERGENCE CRITERION. */
	vtry = TRUE_;
	betav *= (float).25;
/* TRY LINEAR ITERATION IF IT HAS NOT BEEN TRIED AND */
/* THE S SEQUENCE IS CONVERGING */
	if (stry || ! spass) {
	    goto L50;
	}
	i__2 = global_1.n;
	for (i = 1; i <= i__2; ++i) {
	    global_1.k[i - 1] = global_1.svk[i - 1];
/* L30: */
	}
L40:
	realit_(&s, nz, &iflag);
	if (*nz > 0) {
	    return 0;
	}
/* LINEAR ITERATION HAS FAILED. FLAG THAT IT HAS BEEN */
/* TRIED AND DECREASE THE CONVERGENCE CRITERION */
	stry = TRUE_;
	betas *= (float).25;
	if (iflag == 0) {
	    goto L50;
	}
/* IF LINEAR ITERATION SIGNALS AN ALMOST DOUBLE REAL */
/* ZERO ATTEMPT QUADRATIC INTERATION */
	ui = -(s + s);
	vi = s * s;
	goto L20;
/* RESTORE VARIABLES */
L50:
	global_1.u = svu;
	global_1.v = svv;
	i__2 = global_1.n;
	for (i = 1; i <= i__2; ++i) {
	    global_1.k[i - 1] = global_1.svk[i - 1];
/* L60: */
	}
/* TRY QUADRATIC ITERATION IF IT HAS NOT BEEN TRIED */
/* AND THE V SEQUENCE IS CONVERGING */
	if (vpass && ! vtry) {
	    goto L20;
	}
/* RECOMPUTE QP AND SCALAR VALUES TO CONTINUE THE */
/* SECOND STAGE */
	quadsd_(&global_1.nn, &global_1.u, &global_1.v, global_1.p, 
		global_1.qp, &global_1.a, &global_1.b);
	calcsc_(&type);
L70:
	ovv = vv;
	oss = ss;
	otv = tv;
	ots = ts;
/* L80: */
    }
    return 0;
} /* fxshfr_ */

/* Subroutine */ int quadit_(uu, vv, nz)
doublereal *uu, *vv;
integer *nz;
{
    /* System generated locals */
    integer i__1;
    real r__1, r__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    extern /* Subroutine */ int quad_();
    static integer type, i, j;
    static real t;
    static logical tried;
    extern /* Subroutine */ int nextk_();
    static real ee;
    extern /* Subroutine */ int calcsc_();
    static doublereal ui, vi;
    static real mp, zm;
    extern /* Subroutine */ int quadsd_(), newest_();
    static real relstp, omp;

/* VARIABLE-SHIFT K-POLYNOMIAL ITERATION FOR A */
/* QUADRATIC FACTOR CONVERGES ONLY IF THE ZEROS ARE */
/* EQUIMODULAR OR NEARLY SO. */
/* UU,VV - COEFFICIENTS OF STARTING QUADRATIC */
/* NZ - NUMBER OF ZERO FOUND */
    *nz = 0;
    tried = FALSE_;
    global_1.u = *uu;
    global_1.v = *vv;
    j = 0;
/* MAIN LOOP */
L10:
    quad_(&c_b41, &global_1.u, &global_1.v, &global_1.szr, &global_1.szi, &
	    global_1.lzr, &global_1.lzi);
/* RETURN IF ROOTS OF THE QUADRATIC ARE REAL AND NOT */
/* CLOSE TO MULTIPLE OR NEARLY EQUAL AND  OF OPPOSITE */
/* SIGN */
    if ((d__1 = abs(global_1.szr) - abs(global_1.lzr), abs(d__1)) > abs(
	    global_1.lzr) * .01) {
	return 0;
    }
/* EVALUATE POLYNOMIAL BY QUADRATIC SYNTHETIC DIVISION */
    quadsd_(&global_1.nn, &global_1.u, &global_1.v, global_1.p, global_1.qp, &
	    global_1.a, &global_1.b);
    mp = (d__1 = global_1.a - global_1.szr * global_1.b, abs(d__1)) + (d__2 = 
	    global_1.szi * global_1.b, abs(d__2));
/* COMPUTE A RIGOROUS  BOUND ON THE ROUNDING ERROR IN */
/* EVALUTING P */
    zm = sqrt((r__1 = global_1.v, dabs(r__1)));
    ee = (r__1 = global_1.qp[0], dabs(r__1)) * (float)2.;
    t = -global_1.szr * global_1.b;
    i__1 = global_1.n;
    for (i = 2; i <= i__1; ++i) {
	ee = ee * zm + (r__1 = global_1.qp[i - 1], dabs(r__1));
/* L20: */
    }
    ee = ee * zm + (r__1 = global_1.a + t, dabs(r__1));
    ee = (global_1.mre * (float)5. + global_1.are * (float)4.) * ee - (
	    global_1.mre * (float)5. + global_1.are * (float)2.) * ((r__1 = 
	    global_1.a + t, dabs(r__1)) + (r__2 = global_1.b, dabs(r__2)) * 
	    zm) + global_1.are * (float)2. * dabs(t);
/* ITERATION HAS CONVERGED SUFFICIENTLY IF THE */
/* POLYNOMIAL VALUE IS LESS THAN 20 TIMES THIS BOUND */
    if (mp > ee * (float)20.) {
	goto L30;
    }
    *nz = 2;
    return 0;
L30:
    ++j;
/* STOP ITERATION AFTER 20 STEPS */
    if (j > 20) {
	return 0;
    }
    if (j < 2) {
	goto L50;
    }
    if (relstp > (float).01 || mp < omp || tried) {
	goto L50;
    }
/* A CLUSTER APPEARS TO BE STALLING THE CONVERGENCE. */
/* FIVE FIXED SHIFT STEPS ARE TAKEN WITH A U,V CLOSE */
/* TO THE CLUSTER */
    if (relstp < global_1.eta) {
	relstp = global_1.eta;
    }
    relstp = sqrt(relstp);
    global_1.u -= global_1.u * relstp;
    global_1.v += global_1.v * relstp;
    quadsd_(&global_1.nn, &global_1.u, &global_1.v, global_1.p, global_1.qp, &
	    global_1.a, &global_1.b);
    for (i = 1; i <= 5; ++i) {
	calcsc_(&type);
	nextk_(&type);
/* L40: */
    }
    tried = TRUE_;
    j = 0;
L50:
    omp = mp;
/* CALCULATE NEXT K POLYNOMIAL AND NEW U AND V */
    calcsc_(&type);
    nextk_(&type);
    calcsc_(&type);
    newest_(&type, &ui, &vi);
/* IF VI IS ZERO THE ITERATION IS NOT CONVERGING */
    if (vi == 0.) {
	return 0;
    }
    relstp = (d__1 = (vi - global_1.v) / vi, abs(d__1));
    global_1.u = ui;
    global_1.v = vi;
    goto L10;
} /* quadit_ */

/* Subroutine */ int realit_(sss, nz, iflag)
doublereal *sss;
integer *nz, *iflag;
{
    /* System generated locals */
    integer i__1;
    real r__1;
    doublereal d__1;

    /* Local variables */
    static integer i, j;
    static doublereal s, t;
    static real ee, mp, ms;
    static doublereal kv, pv;
    static integer nm1;
    static real omp;

/* VARIABLE-SHIFT H POLYNOMIAL ITERATION FOR A REAL */
/* ZERO. */
/* SSS   - STARTING ITERATE */
/* NZ    - NUMBER OF ZERO FOUND */
/* IFLAG - FLAG TO INDICATE A PAIR OF ZEROS NEAR REAL */
/*         AXIS. */
    nm1 = global_1.n - 1;
    *nz = 0;
    s = *sss;
    *iflag = 0;
    j = 0;
/* MAIN LOOP */
L10:
    pv = global_1.p[0];
/* EVALUATE P AT S */
    global_1.qp[0] = pv;
    i__1 = global_1.nn;
    for (i = 2; i <= i__1; ++i) {
	pv = pv * s + global_1.p[i - 1];
	global_1.qp[i - 1] = pv;
/* L20: */
    }
    mp = abs(pv);
/* COMPUTE A RIGOROUS BOUND ON THE ERROR IN EVALUATING */
/* P */
    ms = abs(s);
    ee = global_1.mre / (global_1.are + global_1.mre) * (r__1 = global_1.qp[0]
	    , dabs(r__1));
    i__1 = global_1.nn;
    for (i = 2; i <= i__1; ++i) {
	ee = ee * ms + (r__1 = global_1.qp[i - 1], dabs(r__1));
/* L30: */
    }
/* ITERATION HAS CONVERGED SUFFICIENTLY IF THE */
/* POLYNOMIAL VALUE IS LESS THAN 20 TIMES THIS BOUND */
    if (mp > ((global_1.are + global_1.mre) * ee - global_1.mre * mp) * (
	    float)20.) {
	goto L40;
    }
    *nz = 1;
    global_1.szr = s;
    global_1.szi = 0.;
    return 0;
L40:
    ++j;
/* STOP ITERATION AFTER 10 STEPS */
    if (j > 10) {
	return 0;
    }
    if (j < 2) {
	goto L50;
    }
    if (abs(t) > (d__1 = s - t, abs(d__1)) * (float).001 || mp <= omp) {
	goto L50;
    }
/* A CLUSTER OF ZEROS NEAR THE REAL AXIS HAS BEEN */
/* ENCOUNTERED RETURN WITH IFLAG SET TO INITIATE A */
/* QUADRATIC ITERATION */
    *iflag = 1;
    *sss = s;
    return 0;
/* RETURN IF THE POLYNOMIAL VALUE HAS INCREASED */
/* SIGNIFICANTLY */
L50:
    omp = mp;
/* COMPUTE T, THE NEXT POLYNOMIAL, AND THE NEW ITERATE */
    kv = global_1.k[0];
    global_1.qk[0] = kv;
    i__1 = global_1.n;
    for (i = 2; i <= i__1; ++i) {
	kv = kv * s + global_1.k[i - 1];
	global_1.qk[i - 1] = kv;
/* L60: */
    }
    if (abs(kv) <= (d__1 = global_1.k[global_1.n - 1], abs(d__1)) * (float)
	    10. * global_1.eta) {
	goto L80;
    }
/* USE THE SCALED FORM OF THE RECURRENCE IF THE VALUE */
/* OF K AT S IS NONZERO */
    t = -pv / kv;
    global_1.k[0] = global_1.qp[0];
    i__1 = global_1.n;
    for (i = 2; i <= i__1; ++i) {
	global_1.k[i - 1] = t * global_1.qk[i - 2] + global_1.qp[i - 1];
/* L70: */
    }
    goto L100;
/* USE UNSCALED FORM */
L80:
    global_1.k[0] = 0.;
    i__1 = global_1.n;
    for (i = 2; i <= i__1; ++i) {
	global_1.k[i - 1] = global_1.qk[i - 2];
/* L90: */
    }
L100:
    kv = global_1.k[0];
    i__1 = global_1.n;
    for (i = 2; i <= i__1; ++i) {
	kv = kv * s + global_1.k[i - 1];
/* L110: */
    }
    t = 0.;
    if (abs(kv) > (d__1 = global_1.k[global_1.n - 1], abs(d__1)) * (float)10. 
	    * global_1.eta) {
	t = -pv / kv;
    }
    s += t;
    goto L10;
} /* realit_ */

/* Subroutine */ int calcsc_(type)
integer *type;
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    extern /* Subroutine */ int quadsd_();

/* THIS ROUTINE CALCULATES SCALAR QUANTITIES USED TO */
/* COMPUTE THE NEXT K POLYNOMIAL AND NEW ESTIMATES OF */
/* THE QUADRATIC COEFFICIENTS. */
/* TYPE - INTEGER VARIABLE SET HERE INDICATING HOW THE */
/* CALCULATIONS ARE NORMALIZED TO AVOID OVERFLOW */
/* SYNTHETIC DIVISION OF K BY THE QUADRATIC 1,U,V */
    quadsd_(&global_1.n, &global_1.u, &global_1.v, global_1.k, global_1.qk, &
	    global_1.c, &global_1.d);
    if (abs(global_1.c) > (d__1 = global_1.k[global_1.n - 1], abs(d__1)) * (
	    float)100. * global_1.eta) {
	goto L10;
    }
    if (abs(global_1.d) > (d__1 = global_1.k[global_1.n - 2], abs(d__1)) * (
	    float)100. * global_1.eta) {
	goto L10;
    }
    *type = 3;
/* TYPE=3 INDICATES THE QUADRATIC IS ALMOST A FACTOR */
/* OF K */
    return 0;
L10:
    if (abs(global_1.d) < abs(global_1.c)) {
	goto L20;
    }
    *type = 2;
/* TYPE=2 INDICATES THAT ALL FORMULAS ARE DIVIDED BY D */
    global_1.e = global_1.a / global_1.d;
    global_1.f = global_1.c / global_1.d;
    global_1.g = global_1.u * global_1.b;
    global_1.h = global_1.v * global_1.b;
    global_1.a3 = (global_1.a + global_1.g) * global_1.e + global_1.h * (
	    global_1.b / global_1.d);
    global_1.a1 = global_1.b * global_1.f - global_1.a;
    global_1.a7 = (global_1.f + global_1.u) * global_1.a + global_1.h;
    return 0;
L20:
    *type = 1;
/* TYPE=1 INDICATES THAT ALL FORMULAS ARE DIVIDED BY C */
    global_1.e = global_1.a / global_1.c;
    global_1.f = global_1.d / global_1.c;
    global_1.g = global_1.u * global_1.e;
    global_1.h = global_1.v * global_1.b;
    global_1.a3 = global_1.a * global_1.e + (global_1.h / global_1.c + 
	    global_1.g) * global_1.b;
    global_1.a1 = global_1.b - global_1.a * (global_1.d / global_1.c);
    global_1.a7 = global_1.a + global_1.g * global_1.d + global_1.h * 
	    global_1.f;
    return 0;
} /* calcsc_ */

/* Subroutine */ int nextk_(type)
integer *type;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static doublereal temp;
    static integer i;

/* COMPUTES THE NEXT K POLYNOMIALS USING SCALARS */
/* COMPUTED IN CALCSC */
    if (*type == 3) {
	goto L40;
    }
    temp = global_1.a;
    if (*type == 1) {
	temp = global_1.b;
    }
    if (abs(global_1.a1) > abs(temp) * global_1.eta * (float)10.) {
	goto L20;
    }
/* IF A1 IS NEARLY ZERO THEN USE A SPECIAL FORM OF THE */
/* RECURRENCE */
    global_1.k[0] = 0.;
    global_1.k[1] = -global_1.a7 * global_1.qp[0];
    i__1 = global_1.n;
    for (i = 3; i <= i__1; ++i) {
	global_1.k[i - 1] = global_1.a3 * global_1.qk[i - 3] - global_1.a7 * 
		global_1.qp[i - 2];
/* L10: */
    }
    return 0;
/* USE SCALED FORM OF THE RECURRENCE */
L20:
    global_1.a7 /= global_1.a1;
    global_1.a3 /= global_1.a1;
    global_1.k[0] = global_1.qp[0];
    global_1.k[1] = global_1.qp[1] - global_1.a7 * global_1.qp[0];
    i__1 = global_1.n;
    for (i = 3; i <= i__1; ++i) {
	global_1.k[i - 1] = global_1.a3 * global_1.qk[i - 3] - global_1.a7 * 
		global_1.qp[i - 2] + global_1.qp[i - 1];
/* L30: */
    }
    return 0;
/* USE UNSCALED FORM OF THE RECURRENCE IF TYPE IS 3 */
L40:
    global_1.k[0] = 0.;
    global_1.k[1] = 0.;
    i__1 = global_1.n;
    for (i = 3; i <= i__1; ++i) {
	global_1.k[i - 1] = global_1.qk[i - 3];
/* L50: */
    }
    return 0;
} /* nextk_ */

/* Subroutine */ int newest_(type, uu, vv)
integer *type;
doublereal *uu, *vv;
{
    static doublereal temp, a4, a5, b1, b2, c1, c2, c3, c4;

/* COMPUTE NEW ESTIMATES OF THE QUADRATIC COEFFICIENTS */
/* USING THE SCALARS COMPUTED IN CALCSC. */
/* USE FORMULAS APPROPRIATE TO SETTING OF TYPE. */
    if (*type == 3) {
	goto L30;
    }
    if (*type == 2) {
	goto L10;
    }
    a4 = global_1.a + global_1.u * global_1.b + global_1.h * global_1.f;
    a5 = global_1.c + (global_1.u + global_1.v * global_1.f) * global_1.d;
    goto L20;
L10:
    a4 = (global_1.a + global_1.g) * global_1.f + global_1.h;
    a5 = (global_1.f + global_1.u) * global_1.c + global_1.v * global_1.d;
/* EVALUATE NEW QUADRATIC COEFFICIENTS. */
L20:
    b1 = -global_1.k[global_1.n - 1] / global_1.p[global_1.nn - 1];
    b2 = -(global_1.k[global_1.n - 2] + b1 * global_1.p[global_1.n - 1]) / 
	    global_1.p[global_1.nn - 1];
    c1 = global_1.v * b2 * global_1.a1;
    c2 = b1 * global_1.a7;
    c3 = b1 * b1 * global_1.a3;
    c4 = c1 - c2 - c3;
    temp = a5 + b1 * a4 - c4;
    if (temp == 0.) {
	goto L30;
    }
    *uu = global_1.u - (global_1.u * (c3 + c2) + global_1.v * (b1 * 
	    global_1.a1 + b2 * global_1.a7)) / temp;
    *vv = global_1.v * (c4 / temp + (float)1.);
    return 0;
/* IF TYPE=3 THE QUADRATIC IS ZEROED */
L30:
    *uu = 0.;
    *vv = 0.;
    return 0;
} /* newest_ */

/* Subroutine */ int quadsd_(nn, u, v, p, q, a, b)
integer *nn;
doublereal *u, *v, *p, *q, *a, *b;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static doublereal c;
    static integer i;

/* DIVIDES P BY THE QUADRATIC  1,U,V  PLACING THE */
/* QUOTIENT IN Q AND THE REMAINDER IN A,B */
    /* Parameter adjustments */
    --q;
    --p;

    /* Function Body */
    *b = p[1];
    q[1] = *b;
    *a = p[2] - *u * *b;
    q[2] = *a;
    i__1 = *nn;
    for (i = 3; i <= i__1; ++i) {
	c = p[i] - *u * *a - *v * *b;
	q[i] = c;
	*b = *a;
	*a = c;
/* L10: */
    }
    return 0;
} /* quadsd_ */

/* Subroutine */ int quad_(a, b1, c, sr, si, lr, li)
doublereal *a, *b1, *c, *sr, *si, *lr, *li;
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static doublereal b, d, e;

/* CALCULATE THE ZEROS OF THE QUADRATIC A*Z**2+B1*Z+C. */
/* THE QUADRATIC FORMULA, MODIFIED TO AVOID */
/* OVERFLOW, IS USED TO FIND THE LARGER ZERO IF THE */
/* ZEROS ARE REAL AND BOTH ZEROS ARE COMPLEX. */
/* THE SMALLER REAL ZERO IS FOUND DIRECTLY FROM THE */
/* PRODUCT OF THE ZEROS C/A. */
    if (*a != 0.) {
	goto L20;
    }
    *sr = 0.;
    if (*b1 != 0.) {
	*sr = -(*c) / *b1;
    }
    *lr = 0.;
L10:
    *si = 0.;
    *li = 0.;
    return 0;
L20:
    if (*c != 0.) {
	goto L30;
    }
    *sr = 0.;
    *lr = -(*b1) / *a;
    goto L10;
/* COMPUTE DISCRIMINANT AVOIDING OVERFLOW */
L30:
    b = *b1 / 2.;
    if (abs(b) < abs(*c)) {
	goto L40;
    }
    e = 1. - *a / b * (*c / b);
    d = sqrt((abs(e))) * abs(b);
    goto L50;
L40:
    e = *a;
    if (*c < 0.) {
	e = -(*a);
    }
    e = b * (b / abs(*c)) - e;
    d = sqrt((abs(e))) * sqrt((abs(*c)));
L50:
    if (e < 0.) {
	goto L60;
    }
/* REAL ZEROS */
    if (b >= 0.) {
	d = -d;
    }
    *lr = (-b + d) / *a;
    *sr = 0.;
    if (*lr != 0.) {
	*sr = *c / *lr / *a;
    }
    goto L10;
/* COMPLEX CONJUGATE ZEROS */
L60:
    *sr = -b / *a;
    *lr = *sr;
    *si = (d__1 = d / *a, abs(d__1));
    *li = -(*si);
    return 0;
} /* quad_ */


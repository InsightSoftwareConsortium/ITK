/* cg.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "assert.h"
#include "stdio.h"
#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;


/*      ________________________________________________________ */
/*     |                                                        | */
/*     |   MINIMIZE A FUNCTION USING THE FLETCHER-REEVES FORM   | */
/*     |            OF THE CONJUGATE GRADIENT METHOD            | */
/*     |            WITH (OR WITHOUT) PRECONDITIONING           | */
/*     |                                                        | */
/*     |    INPUT:                                              | */
/*     |                                                        | */
/*     |         X     --ARRAY CONTAINING STARTING GUESS        | */
/*     |                                                        | */
/*     |         STEP  --STARTING GUESS FOR MINIMIZER IN DIREC- | */
/*     |                 TION OF NEGATIVE GRADIENT DURING FIRST | */
/*     |                 ITERATION (E. G. STEP=1) WHEN STEP=0,  | */
/*     |                 THE PROGRAM SELECTS A STARTING GUESS   | */
/*     |                                                        | */
/*     |         T     --COMPUTING TOLERANCE (ITERATIONS STOP   | */
/*     |                 WHEN MAX-NORM OF GRADIENT .LE. T)      | */
/*     |                                                        | */
/*     |         LIMIT --MAXIMUM NUMBER OF ITERATIONS           | */
/*     |                                                        | */
/*     |         N     --NUMBER OF UNKNOWNS                     | */
/*     |                                                        | */
/*     |         M     --NUMBER OF ITERATIONS UNTIL THE SEARCH  | */
/*     |                 DIRECTIONS ARE RENORMALIZED ALONG THE  | */
/*     |                 NEGATIVE GRADIENT (TYPICALLY, M = N)   | */
/*     |                                                        | */
/*     |         VALUE --NAME OF COST EVALUATION FUNC. ROUTINE  | */
/*     |                 (EXTERNAL IN MAIN PROGRAM)             | */
/*     |                 VALUE(X) IS VALUE OF COST AT X         | */
/*     |                                                        | */
/*     |         GRAD  --NAME OF GRADIENT EVALUATION SUBROUTINE | */
/*     |                 (EXTERNAL IN MAIN PROGRAM)             | */
/*     |                 GRAD(G,X) PUTS IN G THE GRADIENT AT X  | */
/*     |                                                        | */
/*     |         BOTH  --NAME SUBROUTINE TO EVALUATE BOTH COST  | */
/*     |                 AND ITS GRADIENT (EXTERNAL IN MAIN     | */
/*     |                 PROGRAM) BOTH(V,G,X) PUTS THE VALUE IN | */
/*     |                 V AND THE GRADIENT IN G FOR THE POINT X| */
/*     |                                                        | */
/*     |         PRE   --NAME OF PRECONDITIONING SUBROUTINE     | */
/*     |                 (EXTERNAL IN MAIN PROGRAM)             | */
/*     |                 PRE(Y,Z) APPLIES THE PRECONDITIONER TO | */
/*     |                 Z, STORING THE RESULT IN Y.            | */
/*     |                 IF PRECONDITIONING NOT USED SET Y = Z  | */
/*     |                                                        | */
/*     |         H     --WORK ARRAY (LENGTH AT LEAST 3N)        | */
/*     |                                                        | */
/*     |    OUTPUT:                                             | */
/*     |                                                        | */
/*     |         X     --MINIMIZER                              | */
/*     |                                                        | */
/*     |         E     --MAX-NORM OF GRADIENT                   | */
/*     |                                                        | */
/*     |         IT    --NUMBER OF ITERATIONS PERFORMED         | */
/*     |                                                        | */
/*     |         STEP  --STEP SIZE ALONG SEARCH DIRECTION FOR   | */
/*     |                 FINAL ITERATION                        | */
/*     |                                                        | */
/*     |    BUILTIN FUNCTIONS: DABS,DEXP,IDINT,DLOG,DSQRT,DMAX1,| */
/*     |                         DMIN1,DSIGN                    | */
/*     |    PACKAGE ROUTINES: CUB,FD,FV,FVD,INS                 | */
/*     |________________________________________________________| */

/* Subroutine */ int cg_(x, e, it, step, t, limit, n, m, value, grad, both,
        pre, h)
doublereal *x, *e;
integer *it;
doublereal *step, *t;
integer *limit, *n, *m;
doublereal (*value) ();
/* Subroutine */ int (*grad) (), (*both) (), (*pre) ();
doublereal *h;
{
    /* Initialized data */

    static doublereal a1 = .1;
    static doublereal a2 = .9;
    static doublereal a3 = 5.;
    static doublereal a4 = .2;
    static doublereal a5 = 10.;
    static doublereal a6 = .9;
    static doublereal a7 = .3;

    /* System generated locals */
    integer h_dim1, h_offset, i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double log(), exp(), d_sign(), sqrt();
    /* integer s_wsle(), do_lio(), e_wsle();*/
    /* Subroutine */ /*int s_stop();*/

    /* Local variables */
    static doublereal a, b, c, d, f, g;
    static integer i, j, k, l;
    static doublereal p, q, r, s, v, w, y[50], z[50], a8, c0, c1, d0, f0, f1,
            l3, da, db, fa, fb, fc;
    extern doublereal fd_();
    static integer na, nb, nc, nd, iq;
    extern doublereal fv_();
    extern /* Subroutine */ int cub_(), fvd_(), ins_();

    /* Fortran I/O blocks */
    static cilist io___43 = { 0, 6, 0, 0, 0 };
    static cilist io___44 = { 0, 6, 0, 0, 0 };
    static cilist io___45 = { 0, 6, 0, 0, 0 };
    static cilist io___46 = { 0, 6, 0, 0, 0 };


    /* Parameter adjustments */
    h_dim1 = *n;
    h_offset = h_dim1 + 1;
    h -= h_offset;
    --x;

    /* Function Body */
    a8 = a3 + .01;
    *it = 0;
    (*both)(&f, &h[h_dim1 * 3 + 1], &x[1]);
    *e = (float)0.;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* L10: */
        if ((d__1 = h[i + h_dim1 * 3], abs(d__1)) > *e) {
            *e = (d__2 = h[i + h_dim1 * 3], abs(d__2));
        }
    }
    if (*e <= *t) {
        return 0;
    }
    l3 = (float)1. / log(a3);
    (*pre)(&h[(h_dim1 << 1) + 1], &h[h_dim1 * 3 + 1]);
    a = *step;
    if (a > 0.) {
        goto L30;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* L20: */
        if ((d__1 = x[i], abs(d__1)) > a) {
            a = (d__2 = x[i], abs(d__2));
        }
    }
    a = a * (float).01 / *e;
    if (a == 0.) {
        a = (float)1.;
    }
L30:
    g = (float)0.;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* L40: */
        g += h[i + (h_dim1 << 1)] * h[i + h_dim1 * 3];
    }
    if (g < 0.) {
        goto L620;
    }
L50:
    l = 0;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* L60: */
        h[i + h_dim1] = -h[i + (h_dim1 << 1)];
    }
    d = -g;
L70:
    fa = fv_(&a, &x[1], &h[h_offset], n, value);
    c0 = a;
    f0 = fa;
    j = 2;
    y[0] = (float)0.;
    z[0] = f;
    y[1] = a;
    z[1] = fa;
    v = a1 * d;
    w = a2 * d;
    iq = 0;
    if (fa <= f) {
        goto L80;
    }
    c = a;
    b = (float)0.;
    a = (float)0.;
    fc = fa;
    fb = f;
    fa = f;
    goto L90;
L80:
    c = (float)0.;
    b = (float)0.;
    fc = f;
    fb = f;
    iq = 1;
L90:
    na = 0;
    nb = 0;
    nc = 0;
    nd = 0;
    q = (d + (f - f0) / c0) / c0;
    if (q < 0.) {
        goto L110;
    }
    q = a;
L100:
    ++nd;
    if (nd > 25) {
        goto L610;
    }
    q = a3 * q;
    p = fv_(&q, &x[1], &h[h_offset], n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p - f < w * q) {
        goto L100;
    }
    goto L260;
L110:
    q = d * (float).5 / q;
    if (q < c0 * (float).01) {
        q = c0 * (float).01;
    }
    p = fv_(&q, &x[1], &h[h_offset], n, value);
    if (p <= f0) {
        goto L120;
    }
    f1 = f0;
    c1 = c0;
    f0 = p;
    c0 = q;
    goto L130;
L120:
    f1 = p;
    c1 = q;
L130:
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
L135:
    if (a == 0.) {
        goto L140;
    }
    if (fa - f >= v * a) {
        goto L160;
    }
    if (fa - f < w * a) {
        goto L210;
    }
    goto L280;
L140:
    q = c0;
    if (c1 < q) {
        q = c1;
    }
L150:
    ++na;
    if (na > 25) {
        goto L630;
    }
    q = a4 * q;
    p = fv_(&q, &x[1], &h[h_offset], n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p - f >= v * q) {
        goto L150;
    }
    goto L250;
L160:
    if (c0 > c1) {
        goto L200;
    }
    if (f0 - f > v * c0) {
        goto L180;
    }
    if (f0 - f >= w * c0) {
        goto L320;
    }
    if (c1 <= a5 * c0) {
        goto L320;
    }
    r = log(c1 / c0);
    s = (doublereal) (-((integer) (r * l3 + (float).999)));
    r = exp(r / s) * (float).999;
    q = c1;
L170:
    q *= r;
    if (q < c0) {
        goto L320;
    }
    p = fv_(&q, &x[1], &h[h_offset], n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    ++na;
    if (p - f > v * q) {
        goto L170;
    }
    goto L320;
L180:
    q = c0;
L190:
    ++na;
    if (na > 25) {
        goto L630;
    }
    q = a4 * q;
    p = fv_(&q, &x[1], &h[h_offset], n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p - f >= v * q) {
        goto L190;
    }
    goto L250;
L200:
    q = a;
    goto L190;
L210:
    if (c0 < c1) {
        goto L290;
    }
    if (f0 - f >= v * c0) {
        goto L230;
    }
    if (f0 - f >= w * c0) {
        goto L250;
    }
    q = c0;
L220:
    ++nd;
    if (nd > 25) {
        goto L610;
    }
    q = a3 * q;
    p = fv_(&q, &x[1], &h[h_offset], n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p - f < w * q) {
        goto L220;
    }
    goto L250;
L230:
    if (c0 <= a5 * c1) {
        goto L250;
    }
    r = log(c0 / c1);
    s = (doublereal) ((integer) (r * l3 + (float).999));
    r = exp(r / s) * (float)1.001;
    q = a;
L240:
    q *= r;
    if (q > c0) {
        goto L250;
    }
    ++nd;
    p = fv_(&q, &x[1], &h[h_offset], n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p - f < w * q) {
        goto L240;
    }
L250:
    if (iq == 1) {
        goto L320;
    }
L260:
    if (b == 0.) {
        goto L280;
    }
    if (c == 0.) {
        goto L270;
    }
    v = c - a;
    w = a - b;
    r = (float)1. / v;
    s = (float)1. / w;
    p = fc - fa;
    q = fb - fa;
    *e = p * r + q * s;
    d__1 = c - b;
    if (d_sign(e, &d__1) != *e) {
        goto L320;
    }
    if (*e == 0.) {
        goto L320;
    }
    q = p * r * w - q * s * v;
    q = a - q * (float).5 / *e;
    p = fv_(&q, &x[1], &h[h_offset], n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    goto L320;
L270:
    r = (float)1. / a;
    s = (float)1. / b;
    p = r * (fa - f) - d;
    q = s * (fb - f) - d;
    *e = a - b;
    v = (r * p - s * q) / *e;
    w = (a * q * s - b * p * r) / *e;
    v = w * w - v * (float)3. * d;
    if (v < 0.) {
        v = (float)0.;
    }
    v = sqrt(v);
    if (w + v == 0.) {
        goto L320;
    }
    q = -d / (w + v);
    if (q <= 0.) {
        goto L320;
    }
    p = fv_(&q, &x[1], &h[h_offset], n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    goto L320;
L280:
    if (iq == 1) {
        goto L320;
    }
    q = (d + (f - fa) / a) / a;
    if (q >= 0.) {
        goto L320;
    }
    q = d * (float).5 / q;
    p = fv_(&q, &x[1], &h[h_offset], n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    goto L320;
L290:
    if (f0 - f > v * c0) {
        goto L300;
    }
    if (f0 - f > w * c0) {
        goto L320;
    }
L300:
    q = a;
L310:
    ++nd;
    if (nd > 25) {
        goto L610;
    }
    q = a3 * q;
    p = fv_(&q, &x[1], &h[h_offset], n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p - f < w * q) {
        goto L310;
    }
    goto L250;
L320:
    da = fd_(&a, &x[1], &h[h_offset], n, grad);
    if (da > a6 * g) {
        goto L410;
    }
    if (da >= 0.) {
        goto L560;
    }
    r = a;
    q = (float)0.;
    i__1 = j;
    for (i = 1; i <= i__1; ++i) {
        if (y[i - 1] > a) {
            goto L370;
        }
        if (y[i - 1] <= q) {
            goto L330;
        }
        if (y[i - 1] == a) {
            goto L330;
        }
        q = y[i - 1];
L330:
        ;
    }
    if (a <= a8 * q) {
        goto L560;
    }
    q = a;
L340:
    ++nd;
    if (nd > 25) {
        goto L610;
    }
    q = a3 * q;
    p = fv_(&q, &x[1], &h[h_offset], n, value);
    f1 = fa;
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p < f1) {
        goto L340;
    }
    if (a > r) {
        goto L360;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* L350: */
        h[i + (h_dim1 << 1)] = x[i] + a * h[i + h_dim1];
    }
    goto L560;
L360:
    da = fd_(&a, &x[1], &h[h_offset], n, grad);
    if (da > a6 * g) {
        goto L410;
    }
    goto L560;
L370:
    q = y[i - 1];
    i__1 = j;
    for (k = i; k <= i__1; ++k) {
        if (y[k - 1] <= a) {
            goto L380;
        }
        if (y[k - 1] < q) {
            q = y[k - 1];
        }
L380:
        ;
    }
    if (q <= a5 * a) {
        goto L560;
    }
    f0 = log(q / a);
    s = (doublereal) ((integer) (f0 * l3 + (float).999));
    f0 = exp(f0 / s) * (float)1.001;
    s = a;
L390:
    s *= f0;
    if (s >= q) {
        goto L320;
    }
    p = fv_(&s, &x[1], &h[h_offset], n, value);
    f1 = fa;
    ins_(&s, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p < f1) {
        goto L390;
    }
    if (a > r) {
        goto L320;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* L400: */
        h[i + (h_dim1 << 1)] = x[i] + a * h[i + h_dim1];
    }
    goto L560;
L410:
    b = (float)0.;
    k = 1;
    i = k;
L420:
    ++i;
    if (i > j) {
        goto L430;
    }
    if (y[i - 1] >= a) {
        goto L420;
    }
    if (y[i - 1] < b) {
        goto L420;
    }
    b = y[i - 1];
    k = i;
    goto L420;
L430:
    fb = z[k - 1];
    db = d;
    if (b != 0.) {
        db = fd_(&b, &x[1], &h[h_offset], n, grad);
    }
/* L440: */
    w = (d__1 = b - a, abs(d__1)) * (float)2.;
    cub_(&c, &a, &b, &fa, &fb, &da, &db);
    nc = 1;
    goto L480;
L450:
    w *= (float).5;
    if (w < (d__1 = c0 - c, abs(d__1))) {
        goto L550;
    }
    if (c0 < c) {
        goto L460;
    }
    if (d0 >= d) {
        goto L470;
    }
    goto L550;
L460:
    if (d0 > d) {
        goto L550;
    }
L470:
    cub_(&c, &c, &c0, &f, &f0, &d, &d0);
    ++nc;
    if (nc > 30) {
        goto L600;
    }
L480:
    r = max(a,b);
    s = min(a,b);
    if (c > r) {
        goto L490;
    }
    if (c > s) {
        goto L500;
    }
    c = s + (s - c);
    s = (a + b) * (float).5;
    if (c > s) {
        c = s;
    }
    goto L500;
L490:
    c = r - (c - r);
    s = (a + b) * (float).5;
    if (c < s) {
        c = s;
    }
L500:
    c0 = a;
    f0 = fa;
    d0 = da;
    fvd_(&f, &d, &c, &x[1], &h[h_offset], n, both);
    if (f < fa) {
        goto L510;
    }
    b = c;
    fb = f;
    db = d;
    goto L450;
L510:
    if (c < a) {
        goto L540;
    }
    if (d < 0.) {
        goto L530;
    }
L520:
    b = a;
    fb = fa;
    db = da;
L530:
    a = c;
    fa = f;
    da = d;
    if (d > a6 * g) {
        goto L450;
    }
    goto L560;
L540:
    if (d < 0.) {
        goto L520;
    }
    goto L530;
L550:
    c = (a + b) * (float).5;
    ++nb;
    w = (d__1 = b - a, abs(d__1));
    goto L500;
L560:
    *e = (float)0.;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        if ((d__1 = h[i + h_dim1 * 3], abs(d__1)) > *e) {
            *e = (d__2 = h[i + h_dim1 * 3], abs(d__2));
        }
/* L570: */
        x[i] = h[i + (h_dim1 << 1)];
    }
    ++(*it);
    if (*e <= *t) {
        goto L660;
    }
    if (*it >= *limit) {
        goto L660;
    }
    f = fa;
    d = da;
    a = a7 * a;
    (*pre)(&h[(h_dim1 << 1) + 1], &h[h_dim1 * 3 + 1]);
    r = (float)0.;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* L580: */
        r += h[i + (h_dim1 << 1)] * h[i + h_dim1 * 3];
    }
    if (r < 0.) {
        goto L620;
    }
    s = r / g;
    g = r;
    ++l;
    if (l >= *m) {
        goto L50;
    }
    d = (float)0.;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        h[i + h_dim1] = -h[i + (h_dim1 << 1)] + s * h[i + h_dim1];
/* L590: */
        d += h[i + h_dim1] * h[i + h_dim1 * 3];
    }
    goto L70;
L600:
    if (d < g) {
        goto L560;
    }
/*     s_wsle(&io___43); */
/*     do_lio(&c__9, &c__1, "UNABLE TO OBTAIN DESCENT DIRECTION", 34L); */
/*     e_wsle(); */
    printf("UNABLE TO OBTAIN DESCENT DIRECTION\n"); assert(0);
/*     s_stop("", 0L); */
L610:
/*     s_wsle(&io___44); */
/*     do_lio(&c__9, &c__1, "THE FUNCTION DECREASES WITH NO MINIMUM", 38L); */
/*     e_wsle(); */
    printf("THE FUNCTION DECREASES WITH NO MINIMUM\n"); assert(0);
/*     s_stop("", 0L); */
L620:
/*     s_wsle(&io___45); */
/*     do_lio(&c__9, &c__1, "PRECONDITIONER NOT POSITIVE DEFINITE", 36L); */
/*     e_wsle(); */
    printf("PRECONDITIONER NOT POSITIVE DEFINITE\n"); assert(0);
/*     s_stop("", 0L); */
L630:
/* Computing 25th power */
    d__1 = a3, d__2 = d__1, d__1 *= d__1, d__1 *= d__1, d__1 *= d__1, d__2 *=
            d__1;
    q *= d__2 * (d__1 * d__1);
    nd = 0;
L640:
    ++nd;
    if (nd > 25) {
        goto L650;
    }
    q = a3 * q;
    p = fv_(&q, &x[1], &h[h_offset], n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p - f > v * q) {
        goto L640;
    }
    goto L135;
L650:
/*     s_wsle(&io___46); */
/*     do_lio(&c__9, &c__1, "UNABLE TO SATISFY ARMIJO CONDITION", 34L); */
/*     e_wsle(); */
    printf("UNABLE TO SATISFY ARMIJO CONDITION\n");
    return 0;
L660:
    *step = a;
    return 0;
} /* cg_ */

doublereal fv_(a, x, h, n, value)
doublereal *a, *x, *h;
integer *n;
doublereal (*value) ();
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i;

    /* Parameter adjustments */
    h_dim1 = *n;
    h_offset = h_dim1 + 1;
    h -= h_offset;
    --x;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* L10: */
        h[i + (h_dim1 << 1)] = x[i] + *a * h[i + h_dim1];
    }
    ret_val = (*value)(&h[(h_dim1 << 1) + 1]);
    return ret_val;
} /* fv_ */

doublereal fd_(a, x, h, n, grad)
doublereal *a, *x, *h;
integer *n;
/* Subroutine */ int (*grad) ();
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1;
    doublereal ret_val;

    /* Local variables */
    static doublereal d;
    static integer i;

    /* Parameter adjustments */
    h_dim1 = *n;
    h_offset = h_dim1 + 1;
    h -= h_offset;
    --x;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* L10: */
        h[i + (h_dim1 << 1)] = x[i] + *a * h[i + h_dim1];
    }
    (*grad)(&h[h_dim1 * 3 + 1], &h[(h_dim1 << 1) + 1]);
    d = (float)0.;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* L20: */
        d += h[i + h_dim1] * h[i + h_dim1 * 3];
    }
    ret_val = d;
    return ret_val;
} /* fd_ */

/* Subroutine */ int fvd_(v, d, a, x, h, n, both)
doublereal *v, *d, *a, *x, *h;
integer *n;
/* Subroutine */ int (*both) ();
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1;

    /* Local variables */
    static integer i;

    /* Parameter adjustments */
    h_dim1 = *n;
    h_offset = h_dim1 + 1;
    h -= h_offset;
    --x;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* L10: */
        h[i + (h_dim1 << 1)] = x[i] + *a * h[i + h_dim1];
    }
    (*both)(v, &h[h_dim1 * 3 + 1], &h[(h_dim1 << 1) + 1]);
    *d = (float)0.;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* L20: */
        *d += h[i + h_dim1] * h[i + h_dim1 * 3];
    }
    return 0;
} /* fvd_ */

/* Subroutine */ int cub_(x, a, b, c, d, e, f)
doublereal *x, *a, *b, *c, *d, *e, *f;
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt(), d_sign();

    /* Local variables */
    static doublereal g, v, w, y, z;

    g = *b - *a;
    if (g == 0.) {
        goto L50;
    }
    v = *e + *f - (*d - *c) * 3 / g;
    w = v * v - *e * *f;
    if (w < 0.) {
        w = (float)0.;
    }
    d__1 = sqrt(w);
    w = d_sign(&d__1, &g);
    y = *e + v;
    z = *f + v;
    if (d_sign(&y, &g) != y) {
        goto L30;
    }
    if (d_sign(&z, &g) != z) {
        goto L20;
    }
    if (z == 0.) {
        goto L20;
    }
L10:
    *x = *b - g * *f / (z + w);
    return 0;
L20:
    if (*c < *d) {
        *x = *a;
    }
    if (*c >= *d) {
        *x = *b;
    }
    return 0;
L30:
    if (d_sign(&z, &g) != z) {
        goto L40;
    }
    if (abs(*e) > abs(*f)) {
        goto L10;
    }
L40:
    *x = *a + g * *e / (y - w);
    return 0;
L50:
    *x = *a;
    return 0;
} /* cub_ */

/* Subroutine */ int ins_(s, f, a, b, c, fa, fb, fc, j, y, z)
doublereal *s, *f, *a, *b, *c, *fa, *fb, *fc;
integer *j;
doublereal *y, *z;
{
    /* Parameter adjustments */
    --z;
    --y;

    /* Function Body */
    ++(*j);
    y[*j] = *s;
    z[*j] = *f;
    if (*f <= *fa) {
        goto L20;
    }
    if (*f <= *fb) {
        goto L10;
    }
    if (*f > *fc) {
        return 0;
    }
    *c = *s;
    *fc = *f;
    return 0;
L10:
    *c = *b;
    *b = *s;
    *fc = *fb;
    *fb = *f;
    return 0;
L20:
    *c = *b;
    *b = *a;
    *a = *s;
    *fc = *fb;
    *fb = *fa;
    *fa = *f;
    return 0;
} /* ins_ */


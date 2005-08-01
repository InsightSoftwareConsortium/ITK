#include "f2c.h"
#include "netlib.h"
#include <assert.h>
#include <stdio.h>
extern double log(double), exp(double), sqrt(double); /* #include <math.h> */

static doublereal fv_(doublereal *a, doublereal *x, doublereal *h, const integer *n, doublereal (*value)(doublereal*));
static doublereal fd_(doublereal *a, doublereal *x, doublereal *h, const integer *n, void (*grad)(doublereal*,doublereal*));
static void fvd_(doublereal *v, doublereal *d, doublereal *a, doublereal *x, doublereal *h, const integer *n,
                 void (*both)(doublereal*,doublereal*,doublereal*));
static void cub_(doublereal *x, doublereal *a, doublereal *b, doublereal *c, doublereal *d, doublereal *e, doublereal *f);
static void ins_(doublereal *s, doublereal *f, doublereal *a, doublereal *b, doublereal *c,
                 doublereal *fa, doublereal *fb, doublereal *fc, integer *j, doublereal *y, doublereal *z);

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

#ifdef DEBUG
/* Table of constant values */
static integer c__9 = 9;
static integer c__1 = 1;
#endif

/*      ________________________________________________________  */
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

/* Subroutine */ void cg_(x, e, it, step, t, limit, n, m, value, grad, both, pre, h)
doublereal *x, *e;
integer *it;
doublereal *step;
const doublereal *t;
const integer *limit, *n, *m;
doublereal (*value) (doublereal*);
void (*grad) (doublereal*,doublereal*);
void (*both) (doublereal*,doublereal*,doublereal*);
void (*pre) (doublereal*,doublereal*);
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
    doublereal d__1;

    /* Local variables */
    static doublereal a, b, c, d, f, g;
    static integer i, j, k, l;
    static doublereal p, q, r, s, v, w, y[50], z[50], a8, c0, c1, d0, f0, f1, l3, da, db, fa, fb, fc;
    static integer na, nb, nc, nd, iq;

#ifdef DEBUG
    /* Fortran I/O blocks */
    static cilist io___43 = { 0, 6, 0, 0, 0 };
    static cilist io___44 = { 0, 6, 0, 0, 0 };
    static cilist io___45 = { 0, 6, 0, 0, 0 };
    static cilist io___46 = { 0, 6, 0, 0, 0 };
#endif

    a8 = a3 + .01;
    *it = 0;
    (*both)(&f, &h[*n * 2], x);
    *e = 0.f;
    for (i = 0; i < *n; ++i) {
        if (abs(h[i + *n * 2]) > *e) {
            *e = abs(h[i + *n * 2]);
        }
    }
    if (*e <= *t) {
        return;
    }
    l3 = 1.f / log(a3);
    (*pre)(&h[*n], &h[*n * 2]);
    a = *step;
    if (a <= 0.) {
        for (i = 0; i < *n; ++i) {
            if (abs(x[i]) > a) {
                a = abs(x[i]);
            }
        }
        a *= .01f / *e;
        if (a == 0.) {
            a = 1.f;
        }
    }
    g = 0.f;
    for (i = 0; i < *n; ++i) {
        g += h[i + *n] * h[i + *n * 2];
    }
    if (g < 0.) {
        goto L620;
    }
L50:
    l = 0;
    for (i = 0; i < *n; ++i) {
        h[i] = -h[i+*n];
    }
    d = -g;
L70:
    fa = fv_(&a, x, h, n, value);
    c0 = a;
    f0 = fa;
    j = 2;
    y[0] = 0.f;
    z[0] = f;
    y[1] = a;
    z[1] = fa;
    v = a1 * d;
    w = a2 * d;
    iq = 0;
    if (fa > f) {
        c = a; b = 0.f; a = 0.f;
        fc = fa; fb = f; fa = f;
    }
    else {
        c = 0.f; b = 0.f;
        fc = f; fb = f;
        iq = 1;
    }
    na = 0; nb = 0; nc = 0; nd = 0;
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
    q *= a3;
    p = fv_(&q, x, h, n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p - f < w * q) {
        goto L100;
    }
    goto L260;
L110:
    q = d * .5f / q;
    if (q < c0 * .01f) {
        q = c0 * .01f;
    }
    p = fv_(&q, x, h, n, value);
    if (p > f0) {
        f1 = f0; c1 = c0;
        f0 = p; c0 = q;
    }
    else {
        f1 = p; c1 = q;
    }
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
    q *= a4;
    p = fv_(&q, x, h, n, value);
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
    s = (doublereal) (-((integer) (r * l3 + .999f)));
    r = exp(r / s) * .999f;
    q = c1;
L170:
    q *= r;
    if (q < c0) {
        goto L320;
    }
    p = fv_(&q, x, h, n, value);
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
    q *= a4;
    p = fv_(&q, x, h, n, value);
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
    q *= a3;
    p = fv_(&q, x, h, n, value);
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
    s = (doublereal) ((integer) (r * l3 + .999f));
    r = exp(r / s) * 1.001f;
    q = a;
L240:
    q *= r;
    if (q > c0) {
        goto L250;
    }
    ++nd;
    p = fv_(&q, x, h, n, value);
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
    r = 1.f / v;
    s = 1.f / w;
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
    q = a - q * .5f / *e;
    p = fv_(&q, x, h, n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    goto L320;
L270:
    r = 1.f / a;
    s = 1.f / b;
    p = r * (fa - f) - d;
    q = s * (fb - f) - d;
    *e = a - b;
    v = (r * p - s * q) / *e;
    w = (a * q * s - b * p * r) / *e;
    v = w * w - v * 3.f * d;
    if (v < 0.) {
        v = 0.f;
    }
    v = sqrt(v);
    if (w + v == 0.) {
        goto L320;
    }
    q = -d / (w + v);
    if (q <= 0.) {
        goto L320;
    }
    p = fv_(&q, x, h, n, value);
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
    q = d * .5f / q;
    p = fv_(&q, x, h, n, value);
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
    q *= a3;
    p = fv_(&q, x, h, n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p - f < w * q) {
        goto L310;
    }
    goto L250;
L320:
    da = fd_(&a, x, h, n, grad);
    if (da > a6 * g) {
        goto L410;
    }
    if (da >= 0.) {
        goto L560;
    }
    r = a;
    q = 0.f;
    for (i = 0; i < j; ++i) {
        if (y[i] > a) {
            goto L370;
        }
        if (y[i] <= q || y[i] == a) {
            continue; /* next i */
        }
        q = y[i];
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
    q *= a3;
    p = fv_(&q, x, h, n, value);
    f1 = fa;
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p < f1) {
        goto L340;
    }
    if (a > r) {
        goto L360;
    }
    for (i = 0; i < *n; ++i) {
        h[i+*n] = x[i] + a * h[i];
    }
    goto L560;
L360:
    da = fd_(&a, x, h, n, grad);
    if (da > a6 * g) {
        goto L410;
    }
    goto L560;
L370:
    q = y[i];
    for (k = i; k < j; ++k) {
        if (y[k] <= a) {
            continue; /* next k */
        }
        if (y[k] < q) {
            q = y[k];
        }
    }
    if (q <= a5 * a) {
        goto L560;
    }
    f0 = log(q / a);
    s = (doublereal) ((integer) (f0 * l3 + .999f));
    f0 = exp(f0 / s) * 1.001f;
    s = a;
L390:
    s *= f0;
    if (s >= q) {
        goto L320;
    }
    p = fv_(&s, x, h, n, value);
    f1 = fa;
    ins_(&s, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p < f1) {
        goto L390;
    }
    if (a > r) {
        goto L320;
    }
    for (i = 0; i < *n; ++i) {
        h[i+*n] = x[i] + a * h[i];
    }
    goto L560;
L410:
    b = 0.f;
    k = 0;
    i = k;
L420:
    ++i;
    if (i+1 > j) {
        goto L430;
    }
    if (y[i] >= a) {
        goto L420;
    }
    if (y[i] < b) {
        goto L420;
    }
    b = y[i];
    k = i;
    goto L420;
L430:
    fb = z[k];
    db = d;
    if (b != 0.) {
        db = fd_(&b, x, h, n, grad);
    }
    w = abs(b - a) * 2.f;
    cub_(&c, &a, &b, &fa, &fb, &da, &db);
    nc = 1;
    goto L480;
L450:
    w *= .5f;
    if (w < abs(c0 - c)) {
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
    s = (a + b) * .5f;
    if (c > s) {
        c = s;
    }
    goto L500;
L490:
    c = r - (c - r);
    s = (a + b) * .5f;
    if (c < s) {
        c = s;
    }
L500:
    c0 = a;
    f0 = fa;
    d0 = da;
    fvd_(&f, &d, &c, x, h, n, both);
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
    c = (a + b) * .5f;
    ++nb;
    w = abs(b - a);
    goto L500;
L560:
    *e = 0.f;
    for (i = 0; i < *n; ++i) {
        if (abs(h[i+*n*2]) > *e) {
            *e = abs(h[i+*n*2]);
        }
        x[i] = h[i+*n];
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
    a *= a7;
    (*pre)(&h[*n], &h[*n*2]);
    r = 0.f;
    for (i = 0; i < *n; ++i) {
        r += h[i+*n] * h[i+*n*2];
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
    d = 0.f;
    for (i = 0; i < *n; ++i) {
        h[i] = -h[i+*n] + s * h[i];
        d += h[i] * h[i+*n*2];
    }
    goto L70;
L600:
    if (d < g) {
        goto L560;
    }
#ifdef DEBUG
    s_wsle(&io___43);
    do_lio(&c__9, &c__1, "UNABLE TO OBTAIN DESCENT DIRECTION", 34L);
    e_wsle();
#endif
    printf("UNABLE TO OBTAIN DESCENT DIRECTION\n"); assert(0);
/*     s_stop("", 0L); */
L610:
#ifdef DEBUG
    s_wsle(&io___44);
    do_lio(&c__9, &c__1, "THE FUNCTION DECREASES WITH NO MINIMUM", 38L);
    e_wsle();
#endif
    printf("THE FUNCTION DECREASES WITH NO MINIMUM\n"); assert(0);
/*     s_stop("", 0L); */
L620:
#ifdef DEBUG
    s_wsle(&io___45);
    do_lio(&c__9, &c__1, "PRECONDITIONER NOT POSITIVE DEFINITE", 36L);
    e_wsle();
#endif
    printf("PRECONDITIONER NOT POSITIVE DEFINITE\n"); assert(0);
/*     s_stop("", 0L); */
L630:
    /* Computing 25th power */
    d__1 = a3, d__1 *= d__1, d__1 *= d__1, d__1 *= d__1,
    q *= a3 * d__1 * d__1 * d__1;
    nd = 0;
L640:
    ++nd;
    if (nd > 25) {
        goto L650;
    }
    q *= a3;
    p = fv_(&q, x, h, n, value);
    ins_(&q, &p, &a, &b, &c, &fa, &fb, &fc, &j, y, z);
    if (p - f > v * q) {
        goto L640;
    }
    goto L135;
L650:
#ifdef DEBUG
    s_wsle(&io___46);
    do_lio(&c__9, &c__1, "UNABLE TO SATISFY ARMIJO CONDITION", 34L);
    e_wsle();
#endif
    printf("UNABLE TO SATISFY ARMIJO CONDITION\n");
    return;
L660:
    *step = a;
} /* cg_ */

static doublereal fv_(a, x, h, n, value)
doublereal *a, *x, *h;
const integer *n;
doublereal (*value) (doublereal*);
{
    /* Local variables */
    static integer i;

    for (i = 0; i < *n; ++i) {
        h[i+*n] = x[i] + *a * h[i];
    }
    return (*value)(&h[*n]);
} /* fv_ */

static doublereal fd_(a, x, h, n, grad)
doublereal *a, *x, *h;
const integer *n;
void (*grad) (doublereal*,doublereal*);
{
    /* Local variables */
    static doublereal d;
    static integer i;

    for (i = 0; i < *n; ++i) {
        h[i+*n] = x[i] + *a * h[i];
    }
    (*grad)(&h[*n*2], &h[*n]);
    d = 0.f;
    for (i = 0; i < *n; ++i) {
        d += h[i] * h[i+*n*2];
    }
    return d;
} /* fd_ */

/* Subroutine */
static void fvd_(v, d, a, x, h, n, both)
doublereal *v, *d, *a, *x, *h;
const integer *n;
/* Subroutine */ void (*both) (doublereal*,doublereal*,doublereal*);
{
    /* Local variables */
    static integer i;

    for (i = 0; i < *n; ++i) {
        h[i+*n] = x[i] + *a * h[i];
    }
    (*both)(v, &h[*n*2], &h[*n]);
    *d = 0.f;
    for (i = 0; i < *n; ++i) {
        *d += h[i] * h[i+*n*2];
    }
    return;
} /* fvd_ */

/* Subroutine */
static void cub_(x, a, b, c, d, e, f)
doublereal *x, *a, *b, *c, *d, *e, *f;
{
    /* Local variables */
    static doublereal g, v, w, y, z;

    g = *b - *a;
    if (g == 0.) {
        goto L50;
    }
    v = *e + *f - (*d - *c) * 3 / g;
    w = v * v - *e * *f;
    if (w < 0.) {
        w = 0.f;
    }
    w = sqrt(w);
    w = d_sign(&w, &g);
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
    return;
L20:
    if (*c < *d) {
        *x = *a;
    }
    if (*c >= *d) {
        *x = *b;
    }
    return;
L30:
    if (d_sign(&z, &g) != z) {
        goto L40;
    }
    if (abs(*e) > abs(*f)) {
        goto L10;
    }
L40:
    *x = *a + g * *e / (y - w);
    return;
L50:
    *x = *a;
    return;
} /* cub_ */

/* Subroutine */
static void ins_(s, f, a, b, c, fa, fb, fc, j, y, z)
doublereal *s, *f, *a, *b, *c, *fa, *fb, *fc;
integer *j;
doublereal *y, *z;
{
    y[*j] = *s;
    z[*j] = *f;
    ++(*j);
    if (*f <= *fa) {
        *c = *b; *b = *a; *a = *s;
        *fc = *fb; *fb = *fa; *fa = *f;
        return;
    }
    if (*f <= *fb) {
        *c = *b; *b = *s;
        *fc = *fb; *fb = *f;
        return;
    }
    if (*f > *fc) {
        return;
    }
    *c = *s;
    *fc = *f;
} /* ins_ */

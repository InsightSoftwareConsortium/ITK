/* napack/cg.f -- translated by f2c (version 20050501).
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

#include <assert.h>
#include <stdio.h>

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

/*<       SUBROUTINE CG(X,E,IT,STEP,T,LIMIT,N,M,VALUE,GRAD,BOTH,PRE,H) >*/
/* Subroutine */ int cg_(doublereal *x, doublereal *e, integer *it,
        doublereal *step, doublereal *t, integer *limit, integer *n, integer *
        m,
        double (*value)(double*,void*),
        void (*grad)(double*,double*,void*),
        void (*both)(double*,double*,double*,void*),
        void (*pre)(double*,double*,void*),
        doublereal *h__,
        void* userdata,
        integer* error_code)
{
    /* Initialized data */

    static doublereal a1 = .1; /* constant */
    static doublereal a2 = .9; /* constant */
    static doublereal a3 = 5.; /* constant */
    static doublereal a4 = .2; /* constant */
    static doublereal a5 = 10.; /* constant */
    static doublereal a6 = .9; /* constant */
    static doublereal a7 = .3; /* constant */

    /* System generated locals */
    integer h_dim1, h_offset, i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double log(doublereal), exp(doublereal), d_sign(doublereal *, doublereal *
            ), sqrt(doublereal);
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen),
            e_wsle();
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    doublereal a, b, c__, d__, f, g;
    integer i__, j, k, l=0;
    doublereal p, q, r__, s, v=0, w=0, y[54], z__[54], a8,
               c0, c1=0, d0, f0, f1, l3, da, db, fa, fb, fc;
    extern doublereal fd_(doublereal *, doublereal *, doublereal *, integer *,
                          void (*grad)(double*,double*,void*), void*);
    integer na=0, nb, nc, nd, iq=0;
    extern doublereal fv_(doublereal *, doublereal *, doublereal *, integer *,
                          double (*value)(double*,void*), void*);
    extern /* Subroutine */ int cub_(doublereal *, doublereal *, doublereal *,
             doublereal *, doublereal *, doublereal *, doublereal *), fvd_(
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *, integer *, void (*)(double*,double*,double*,void*), void*),
            ins_(doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *, doublereal *, integer *, doublereal *, doublereal *)
            ;

/*<       INTEGER I,IT,J,K,L,LIMIT,M,N,NA,NB,NC,ND >*/
/*<       REAL*8 H(N,1),X(1),Y(54),Z(54),A1,A2,A3,A4,A5,A6,A7,A8,A,B,C,C0,C1 >*/
/*<       REAL*8 D,D0,DA,DB,E,F,F0,F1,FA,FB,FC,G,L3,P,Q,R,S,STEP,T,V,W >*/
/*<       REAL*8 FV,FD,VALUE >*/
/*<       EXTERNAL BOTH,GRAD,PRE,VALUE >*/
/*<       DATA A1/.1D0/,A2/.9D0/,A3/5.D0/,A4/.2D0/,A5/10.D0/,A6/.9D0/ >*/
    /* Parameter adjustments */
    --x;
    h_dim1 = *n;
    h_offset = 1 + h_dim1;
    h__ -= h_offset;

    /* Initialize to no error.  */
    if(error_code)
      {
      *error_code = 0;
      }

    /* Function Body */
/*<       DATA A7/.3D0/ >*/
/*<       A8 = A3 + .01D0 >*/
    a8 = a3 + .01;
/*<       IT = 0 >*/
    *it = 0;
/*<       CALL BOTH(F,H(1,3),X) >*/
    (*both)(&f, &h__[h_dim1 * 3 + 1], &x[1], userdata);
/*<       E = 0. >*/
    *e = (float)0.;
/*<       DO 10 I = 1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 10         IF ( DABS(H(I,3)) .GT. E ) E = DABS(H(I,3)) >*/
/* L10: */
        if ((d__1 = h__[i__ + h_dim1 * 3], abs(d__1)) > *e) {
            *e = (d__2 = h__[i__ + h_dim1 * 3], abs(d__2));
        }
    }
/*<       IF ( E .LE. T ) RETURN >*/
    if (*e <= *t) {
        return 0;
    }
/*<       L3 = 1./DLOG(A3) >*/
    l3 = (float)1. / log(a3);
/*<       CALL PRE(H(1,2),H(1,3)) >*/
    (*pre)(&h__[(h_dim1 << 1) + 1], &h__[h_dim1 * 3 + 1], userdata);
/*<       A = STEP >*/
    a = *step;
/*<       IF ( A .GT. 0. ) GOTO 30 >*/
    if (a > (float)0.) {
        goto L30;
    }
/*<       DO 20 I = 1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 20         IF ( DABS(X(I)) .GT. A ) A = DABS(X(I)) >*/
/* L20: */
        if ((d__1 = x[i__], abs(d__1)) > a) {
            a = (d__2 = x[i__], abs(d__2));
        }
    }
/*<       A = .01*A/E >*/
    a = a * (float).01 / *e;
/*<       IF ( A .EQ. 0. ) A = 1. >*/
    if (a == (float)0.) {
        a = (float)1.;
    }
/*< 30    G = 0. >*/
L30:
    g = (float)0.;
/*<       DO 40 I = 1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 40         G = G + H(I,2)*H(I,3) >*/
/* L40: */
        g += h__[i__ + (h_dim1 << 1)] * h__[i__ + h_dim1 * 3];
    }
/*<       IF ( G .LT. 0. ) GOTO 620 >*/
    if (g < (float)0.) {
        goto L620;
    }
/*< 50    L = 0 >*/
L50:
    l = 0;
/*<       DO 60 I = 1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 60         H(I,1) = -H(I,2) >*/
/* L60: */
        h__[i__ + h_dim1] = -h__[i__ + (h_dim1 << 1)];
    }
/*<       D = -G >*/
    d__ = -g;
/*< 70    FA = FV(A,X,H,N,VALUE) >*/
L70:
    fa = fv_(&a, &x[1], &h__[h_offset], n, value, userdata);
/*<       C0 = A >*/
    c0 = a;
/*<       F0 = FA >*/
    f0 = fa;
/*<       J = 2 >*/
    j = 2;
/*<       Y(1) = 0. >*/
    y[0] = (float)0.;
/*<       Z(1) = F >*/
    z__[0] = f;
/*<       Y(2) = A >*/
    y[1] = a;
/*<       Z(2) = FA >*/
    z__[1] = fa;
/*<       V = A1*D >*/
    v = a1 * d__;
/*<       W = A2*D >*/
    w = a2 * d__;
/*<       IQ = 0 >*/
    iq = 0;
/*<       IF ( FA .LE. F ) GOTO 80 >*/
    if (fa <= f) {
        goto L80;
    }
/*<       C = A >*/
    c__ = a;
/*<       B = 0. >*/
    b = (float)0.;
/*<       A = 0. >*/
    a = (float)0.;
/*<       FC = FA >*/
    fc = fa;
/*<       FB = F >*/
    fb = f;
/*<       FA = F >*/
    fa = f;
/*<       GOTO 90 >*/
    goto L90;
/*< 80    C = 0. >*/
L80:
    c__ = (float)0.;
/*<       B = 0. >*/
    b = (float)0.;
/*<       FC = F >*/
    fc = f;
/*<       FB = F >*/
    fb = f;
/*<       IQ = 1 >*/
    iq = 1;
/*< 90    NA = 0 >*/
L90:
    na = 0;
/*<       NB = 0 >*/
    nb = 0;
/*<       NC = 0 >*/
//    nc = 0;
/*<       ND = 0 >*/
    nd = 0;
/*<       Q = (D+(F-F0)/C0)/C0 >*/
    q = (d__ + (f - f0) / c0) / c0;
/*<       IF ( Q .LT. 0. ) GOTO 110 >*/
    if (q < (float)0.) {
        goto L110;
    }
/*<       Q = A >*/
    q = a;
/*< 100   ND = ND + 1 >*/
L100:
    ++nd;
/*<       IF ( ND .GT. 25 ) GOTO 610 >*/
    if (nd > 25) {
        goto L610;
    }
/*<       Q = A3*Q >*/
    q = a3 * q;
/*<       P = FV(Q,X,H,N,VALUE) >*/
    p = fv_(&q, &x[1], &h__[h_offset], n, value, userdata);
/*<       CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
    ins_(&q, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*<       IF ( P-F .LT. W*Q ) GOTO 100 >*/
    if (p - f < w * q) {
        goto L100;
    }
/*<       GOTO 260 >*/
    goto L260;
/*< 110   Q = .5*D/Q >*/
L110:
    q = d__ * (float).5 / q;
/*<       IF ( Q .LT. .01*C0 ) Q = .01*C0 >*/
    if (q < c0 * (float).01) {
        q = c0 * (float).01;
    }
/*<       P = FV(Q,X,H,N,VALUE) >*/
    p = fv_(&q, &x[1], &h__[h_offset], n, value, userdata);
/*<       IF ( P .LE. F0 ) GOTO 120 >*/
    if (p <= f0) {
        goto L120;
    }
/*<       F1 = F0 >*/
//    f1 = f0;
/*<       C1 = C0 >*/
    c1 = c0;
/*<       F0 = P >*/
    f0 = p;
/*<       C0 = Q >*/
    c0 = q;
/*<       GOTO 130 >*/
    goto L130;
/*< 120   F1 = P >*/
L120:
//    f1 = p;
/*<       C1 = Q >*/
    c1 = q;
/*< 130   CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
L130:
    ins_(&q, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*< 135   IF ( A .EQ. 0. ) GOTO 140 >*/
L135:
    if (a == (float)0.) {
        goto L140;
    }
/*<       IF ( FA-F .GE. V*A ) GOTO 160 >*/
    if (fa - f >= v * a) {
        goto L160;
    }
/*<       IF ( FA-F .LT. W*A ) GOTO 210 >*/
    if (fa - f < w * a) {
        goto L210;
    }
/*<       GOTO 280 >*/
    goto L280;
/*< 140   Q = C0 >*/
L140:
    q = c0;
/*<       IF ( C1 .LT. Q ) Q = C1 >*/
    if (c1 < q) {
        q = c1;
    }
/*< 150   NA = NA + 1 >*/
L150:
    ++na;
/*<       IF ( NA .GT. 25 ) GOTO 630 >*/
    if (na > 25) {
        goto L630;
    }
/*<       Q = A4*Q >*/
    q = a4 * q;
/*<       P = FV(Q,X,H,N,VALUE) >*/
    p = fv_(&q, &x[1], &h__[h_offset], n, value, userdata);
/*<       CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
    ins_(&q, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*<       IF ( P-F .GE. V*Q ) GOTO 150 >*/
    if (p - f >= v * q) {
        goto L150;
    }
/*<       GOTO 250 >*/
    goto L250;
/*< 160   IF ( C0 .GT. C1 ) GOTO 200 >*/
L160:
    if (c0 > c1) {
        goto L200;
    }
/*<       IF ( F0-F .GT. V*C0 ) GOTO 180 >*/
    if (f0 - f > v * c0) {
        goto L180;
    }
/*<       IF ( F0-F .GE. W*C0 ) GOTO 320 >*/
    if (f0 - f >= w * c0) {
        goto L320;
    }
/*<       IF ( C1 .LE. A5*C0 ) GOTO 320 >*/
    if (c1 <= a5 * c0) {
        goto L320;
    }
/*<       R = DLOG(C1/C0) >*/
    r__ = log(c1 / c0);
/*<       S = -IDINT(R*L3+.999) >*/
    s = (doublereal) (-((integer) (r__ * l3 + (float).999)));
/*<       R = .999*DEXP(R/S) >*/
    r__ = exp(r__ / s) * (float).999;
/*<       Q = C1 >*/
    q = c1;
/*< 170   Q = Q*R >*/
L170:
    q *= r__;
/*<       IF ( Q .LT. C0 ) GOTO 320 >*/
    if (q < c0) {
        goto L320;
    }
/*<       P = FV(Q,X,H,N,VALUE) >*/
    p = fv_(&q, &x[1], &h__[h_offset], n, value, userdata);
/*<       CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
    ins_(&q, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*<       NA = NA + 1 >*/
    ++na;
/*<       IF ( P-F .GT. V*Q ) GOTO 170 >*/
    if (p - f > v * q) {
        goto L170;
    }
/*<       GOTO 320 >*/
    goto L320;
/*< 180   Q = C0 >*/
L180:
    q = c0;
/*< 190   NA = NA + 1 >*/
L190:
    ++na;
/*<       IF ( NA .GT. 25 ) GOTO 630 >*/
    if (na > 25) {
        goto L630;
    }
/*<       Q = A4*Q >*/
    q = a4 * q;
/*<       P = FV(Q,X,H,N,VALUE) >*/
    p = fv_(&q, &x[1], &h__[h_offset], n, value, userdata);
/*<       CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
    ins_(&q, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*<       IF ( P-F .GE. V*Q ) GOTO 190 >*/
    if (p - f >= v * q) {
        goto L190;
    }
/*<       GOTO 250 >*/
    goto L250;
/*< 200   Q = A >*/
L200:
    q = a;
/*<       GOTO 190 >*/
    goto L190;
/*< 210   IF ( C0 .LT. C1 ) GOTO 290 >*/
L210:
    if (c0 < c1) {
        goto L290;
    }
/*<       IF ( F0-F .GE. V*C0 ) GOTO 230 >*/
    if (f0 - f >= v * c0) {
        goto L230;
    }
/*<       IF ( F0-F .GE. W*C0 ) GOTO 250 >*/
    if (f0 - f >= w * c0) {
        goto L250;
    }
/*<       Q = C0 >*/
    q = c0;
/*< 220   ND = ND  + 1 >*/
L220:
    ++nd;
/*<       IF ( ND .GT. 25 ) GOTO 610 >*/
    if (nd > 25) {
        goto L610;
    }
/*<       Q = A3*Q >*/
    q = a3 * q;
/*<       P = FV(Q,X,H,N,VALUE) >*/
    p = fv_(&q, &x[1], &h__[h_offset], n, value, userdata);
/*<       CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
    ins_(&q, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*<       IF ( P-F .LT. W*Q ) GOTO 220 >*/
    if (p - f < w * q) {
        goto L220;
    }
/*<       GOTO 250 >*/
    goto L250;
/*< 230   IF ( C0 .LE. A5*C1 ) GOTO 250 >*/
L230:
    if (c0 <= a5 * c1) {
        goto L250;
    }
/*<       R = DLOG(C0/C1) >*/
    r__ = log(c0 / c1);
/*<       S = IDINT(R*L3+.999) >*/
    s = (doublereal) ((integer) (r__ * l3 + (float).999));
/*<       R = 1.001*DEXP(R/S) >*/
    r__ = exp(r__ / s) * (float)1.001;
/*<       Q = A >*/
    q = a;
/*< 240   Q = Q*R >*/
L240:
    q *= r__;
/*<       IF ( Q .GT. C0 ) GOTO 250 >*/
    if (q > c0) {
        goto L250;
    }
/*<       ND = ND + 1 >*/
    ++nd;
/*<       P = FV(Q,X,H,N,VALUE) >*/
    p = fv_(&q, &x[1], &h__[h_offset], n, value, userdata);
/*<       CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
    ins_(&q, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*<       IF ( P-F .LT. W*Q ) GOTO 240 >*/
    if (p - f < w * q) {
        goto L240;
    }
/*< 250   IF ( IQ .EQ. 1 ) GOTO 320 >*/
L250:
    if (iq == 1) {
        goto L320;
    }
/*< 260   IF ( B .EQ. 0. ) GOTO 280 >*/
L260:
    if (b == (float)0.) {
        goto L280;
    }
/*<       IF ( C .EQ. 0. ) GOTO 270 >*/
    if (c__ == (float)0.) {
        goto L270;
    }
/*<       V = C - A >*/
    v = c__ - a;
/*<       W = A - B >*/
    w = a - b;
/*<       R = 1./V >*/
    r__ = (float)1. / v;
/*<       S = 1./W >*/
    s = (float)1. / w;
/*<       P = FC - FA >*/
    p = fc - fa;
/*<       Q = FB - FA >*/
    q = fb - fa;
/*<       E = P*R + Q*S >*/
    *e = p * r__ + q * s;
/*<       IF ( DSIGN(E,C-B) .NE. E ) GOTO 320 >*/
    d__1 = c__ - b;
    if (d_sign(e, &d__1) != *e) {
        goto L320;
    }
/*<       IF ( E .EQ. 0. ) GOTO 320 >*/
    if (*e == (float)0.) {
        goto L320;
    }
/*<       Q = (P*R)*W - (Q*S)*V >*/
    q = p * r__ * w - q * s * v;
/*<       Q = A - .5*Q/E >*/
    q = a - q * (float).5 / *e;
/*<       P = FV(Q,X,H,N,VALUE) >*/
    p = fv_(&q, &x[1], &h__[h_offset], n, value, userdata);
/*<       CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
    ins_(&q, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*<       GOTO 320 >*/
    goto L320;
/*< 270   R = 1./A >*/
L270:
    r__ = (float)1. / a;
/*<       S = 1./B >*/
    s = (float)1. / b;
/*<       P = R*(FA-F) - D >*/
    p = r__ * (fa - f) - d__;
/*<       Q = S*(FB-F) - D >*/
    q = s * (fb - f) - d__;
/*<       E = A - B >*/
    *e = a - b;
/*<       V = (R*P-S*Q)/E >*/
    v = (r__ * p - s * q) / *e;
/*<       W = (A*Q*S-B*P*R)/E >*/
    w = (a * q * s - b * p * r__) / *e;
/*<       V = W*W-3.*V*D >*/
    v = w * w - v * (float)3. * d__;
/*<       IF ( V .LT. 0. ) V = 0. >*/
    if (v < (float)0.) {
        v = (float)0.;
    }
/*<       V = DSQRT(V) >*/
    v = sqrt(v);
/*<       IF ( W+V .EQ. 0. ) GOTO 320 >*/
    if (w + v == (float)0.) {
        goto L320;
    }
/*<       Q = -D/(W+V) >*/
    q = -d__ / (w + v);
/*<       IF ( Q .LE. 0. ) GOTO 320 >*/
    if (q <= (float)0.) {
        goto L320;
    }
/*<       P = FV(Q,X,H,N,VALUE) >*/
    p = fv_(&q, &x[1], &h__[h_offset], n, value, userdata);
/*<       CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
    ins_(&q, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*<       GOTO 320 >*/
    goto L320;
/*< 280   IF ( IQ .EQ. 1 ) GOTO  320 >*/
L280:
    if (iq == 1) {
        goto L320;
    }
/*<       Q = (D+(F-FA)/A)/A >*/
    q = (d__ + (f - fa) / a) / a;
/*<       IF ( Q .GE. 0. ) GOTO 320 >*/
    if (q >= (float)0.) {
        goto L320;
    }
/*<       Q = .5*D/Q >*/
    q = d__ * (float).5 / q;
/*<       P = FV(Q,X,H,N,VALUE) >*/
    p = fv_(&q, &x[1], &h__[h_offset], n, value, userdata);
/*<       CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
    ins_(&q, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*<       GOTO 320 >*/
    goto L320;
/*< 290   IF ( F0-F .GT. V*C0 ) GOTO 300 >*/
L290:
    if (f0 - f > v * c0) {
        goto L300;
    }
/*<       IF ( F0-F .GT. W*C0 ) GOTO 320 >*/
    if (f0 - f > w * c0) {
        goto L320;
    }
/*< 300   Q = A >*/
L300:
    q = a;
/*< 310   ND = ND + 1 >*/
L310:
    ++nd;
/*<       IF ( ND .GT. 25 ) GOTO 610 >*/
    if (nd > 25) {
        goto L610;
    }
/*<       Q = A3*Q >*/
    q = a3 * q;
/*<       P = FV(Q,X,H,N,VALUE) >*/
    p = fv_(&q, &x[1], &h__[h_offset], n, value, userdata);
/*<       CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
    ins_(&q, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*<       IF ( P-F .LT. W*Q ) GOTO 310 >*/
    if (p - f < w * q) {
        goto L310;
    }
/*<       GOTO 250 >*/
    goto L250;
/*< 320   DA = FD(A,X,H,N,GRAD) >*/
L320:
    da = fd_(&a, &x[1], &h__[h_offset], n, grad, userdata);
/*<       IF ( DA .GT. A6*G ) GOTO 410 >*/
    if (da > a6 * g) {
        goto L410;
    }
/*<       IF ( DA .GE. 0. ) GOTO 560 >*/
    if (da >= (float)0.) {
        goto L560;
    }
/*<       R = A >*/
    r__ = a;
/*<       Q = 0. >*/
    q = (float)0.;
/*<       DO 330 I = 1,J >*/
    i__1 = j;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<            IF ( Y(I) .GT. A ) GOTO 370 >*/
        if (y[i__ - 1] > a) {
            goto L370;
        }
/*<            IF ( Y(I) .LE. Q ) GOTO 330 >*/
        if (y[i__ - 1] <= q) {
            goto L330;
        }
/*<            IF ( Y(I) .EQ. A ) GOTO 330 >*/
        if (y[i__ - 1] == a) {
            goto L330;
        }
/*<            Q = Y(I) >*/
        q = y[i__ - 1];
/*< 330   CONTINUE >*/
L330:
        ;
    }
/*<       IF ( A .LE. A8*Q ) GOTO 560 >*/
    if (a <= a8 * q) {
        goto L560;
    }
/*<       Q = A >*/
    q = a;
/*< 340   ND = ND + 1 >*/
L340:
    ++nd;
/*<       IF ( ND .GT. 25 ) GOTO 610 >*/
    if (nd > 25) {
        goto L610;
    }
/*<       Q = A3*Q >*/
    q = a3 * q;
/*<       P = FV(Q,X,H,N,VALUE) >*/
    p = fv_(&q, &x[1], &h__[h_offset], n, value, userdata);
/*<       F1 = FA >*/
    f1 = fa;
/*<       CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
    ins_(&q, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*<       IF ( P .LT. F1 ) GOTO 340 >*/
    if (p < f1) {
        goto L340;
    }
/*<       IF ( A .GT. R ) GOTO 360 >*/
    if (a > r__) {
        goto L360;
    }
/*<       DO 350 I = 1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 350        H(I,2) = X(I) + A*H(I,1) >*/
/* L350: */
        h__[i__ + (h_dim1 << 1)] = x[i__] + a * h__[i__ + h_dim1];
    }
/*<       GOTO 560 >*/
    goto L560;
/*< 360   DA = FD(A,X,H,N,GRAD) >*/
L360:
    da = fd_(&a, &x[1], &h__[h_offset], n, grad, userdata);
/*<       IF ( DA .GT. A6*G ) GOTO 410 >*/
    if (da > a6 * g) {
        goto L410;
    }
/*<       GOTO 560 >*/
    goto L560;
/*< 370   Q = Y(I) >*/
L370:
    q = y[i__ - 1];
/*<       DO 380 K = I,J >*/
    i__1 = j;
    for (k = i__; k <= i__1; ++k) {
/*<            IF ( Y(K) .LE. A ) GOTO 380 >*/
        if (y[k - 1] <= a) {
            goto L380;
        }
/*<            IF ( Y(K) .LT. Q ) Q = Y(K) >*/
        if (y[k - 1] < q) {
            q = y[k - 1];
        }
/*< 380   CONTINUE >*/
L380:
        ;
    }
/*<       IF ( Q .LE. A5*A ) GOTO 560 >*/
    if (q <= a5 * a) {
        goto L560;
    }
/*<       F0 = DLOG(Q/A) >*/
    f0 = log(q / a);
/*<       S = IDINT(F0*L3+.999) >*/
    s = (doublereal) ((integer) (f0 * l3 + (float).999));
/*<       F0 = 1.001*DEXP(F0/S) >*/
    f0 = exp(f0 / s) * (float)1.001;
/*<       S = A >*/
    s = a;
/*< 390   S = S*F0 >*/
L390:
    s *= f0;
/*<       IF ( S .GE. Q ) GOTO 320 >*/
    if (s >= q) {
        goto L320;
    }
/*<       P = FV(S,X,H,N,VALUE) >*/
    p = fv_(&s, &x[1], &h__[h_offset], n, value, userdata);
/*<       F1 = FA >*/
    f1 = fa;
/*<       CALL INS(S,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
    ins_(&s, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*<       IF ( P .LT. F1 ) GOTO 390 >*/
    if (p < f1) {
        goto L390;
    }
/*<       IF ( A .GT. R ) GOTO 320 >*/
    if (a > r__) {
        goto L320;
    }
/*<       DO 400 I = 1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 400        H(I,2) = X(I) + A*H(I,1) >*/
/* L400: */
        h__[i__ + (h_dim1 << 1)] = x[i__] + a * h__[i__ + h_dim1];
    }
/*<       GOTO 560 >*/
    goto L560;
/*< 410   B = 0. >*/
L410:
    b = (float)0.;
/*<       K = 1 >*/
    k = 1;
/*<       I = K >*/
    i__ = k;
/*< 420   I = I + 1 >*/
L420:
    ++i__;
/*<       IF ( I .GT. J ) GOTO 430 >*/
    if (i__ > j) {
        goto L430;
    }
/*<       IF ( Y(I) .GE. A ) GOTO 420 >*/
    if (y[i__ - 1] >= a) {
        goto L420;
    }
/*<       IF ( Y(I) .LT. B ) GOTO 420 >*/
    if (y[i__ - 1] < b) {
        goto L420;
    }
/*<       B = Y(I) >*/
    b = y[i__ - 1];
/*<       K = I >*/
    k = i__;
/*<       GOTO 420 >*/
    goto L420;
/*< 430   FB = Z(K) >*/
L430:
    fb = z__[k - 1];
/*<       DB = D >*/
    db = d__;
/*<       IF ( B .NE. 0. ) DB = FD(B,X,H,N,GRAD) >*/
    if (b != (float)0.) {
        db = fd_(&b, &x[1], &h__[h_offset], n, grad, userdata);
    }
/*< 440   W = 2.*DABS(B-A) >*/
/* L440: */
    w = (d__1 = b - a, abs(d__1)) * (float)2.;
/*<       CALL CUB(C,A,B,FA,FB,DA,DB) >*/
    cub_(&c__, &a, &b, &fa, &fb, &da, &db);
/*<       NC = 1 >*/
    nc = 1;
/*<       GOTO 480 >*/
    goto L480;
/*< 450   W = .5*W >*/
L450:
    w *= (float).5;
/*<       IF ( W .LT. DABS(C0-C) ) GOTO 550 >*/
    if (w < (d__1 = c0 - c__, abs(d__1))) {
        goto L550;
    }
/*<       IF ( C0 .LT. C ) GOTO 460 >*/
    if (c0 < c__) {
        goto L460;
    }
/*<       IF ( D0 .GE. D ) GOTO 470 >*/
    if (d0 >= d__) {
        goto L470;
    }
/*<       GOTO 550 >*/
    goto L550;
/*< 460   IF ( D0 .GT. D ) GOTO 550 >*/
L460:
    if (d0 > d__) {
        goto L550;
    }
/*< 470   CALL CUB(C,C,C0,F,F0,D,D0) >*/
L470:
    cub_(&c__, &c__, &c0, &f, &f0, &d__, &d0);
/*<       NC = NC + 1 >*/
    ++nc;
/*<       IF ( NC .GT. 30 ) GOTO 600 >*/
    if (nc > 30) {
        goto L600;
    }
/*< 480   R = DMAX1(A,B) >*/
L480:
    r__ = max(a,b);
/*<       S = DMIN1(A,B) >*/
    s = min(a,b);
/*<       IF ( C .GT. R ) GOTO 490 >*/
    if (c__ > r__) {
        goto L490;
    }
/*<       IF ( C .GT. S ) GOTO 500 >*/
    if (c__ > s) {
        goto L500;
    }
/*<       C = S + (S-C) >*/
    c__ = s + (s - c__);
/*<       S = .5*(A+B) >*/
    s = (a + b) * (float).5;
/*<       IF ( C .GT. S ) C = S >*/
    if (c__ > s) {
        c__ = s;
    }
/*<       GOTO 500 >*/
    goto L500;
/*< 490   C = R - (C-R) >*/
L490:
    c__ = r__ - (c__ - r__);
/*<       S = .5*(A+B) >*/
    s = (a + b) * (float).5;
/*<       IF ( C .LT. S ) C = S >*/
    if (c__ < s) {
        c__ = s;
    }
/*< 500   C0 = A >*/
L500:
    c0 = a;
/*<       F0 = FA >*/
    f0 = fa;
/*<       D0 = DA >*/
    d0 = da;
/*<       CALL FVD(F,D,C,X,H,N,BOTH) >*/
    fvd_(&f, &d__, &c__, &x[1], &h__[h_offset], n, both, userdata);
/*<       IF ( F .LT. FA ) GOTO 510 >*/
    if (f < fa) {
        goto L510;
    }
/*<       B = C >*/
    b = c__;
/*<       FB = F >*/
    fb = f;
/*<       DB = D >*/
    db = d__;
/*<       GOTO 450 >*/
    goto L450;
/*< 510   IF ( C .LT. A ) GOTO 540 >*/
L510:
    if (c__ < a) {
        goto L540;
    }
/*<       IF ( D .LT. 0. ) GOTO 530 >*/
    if (d__ < (float)0.) {
        goto L530;
    }
/*< 520   B = A >*/
L520:
    b = a;
/*<       FB = FA >*/
    fb = fa;
/*<       DB = DA >*/
    db = da;
/*< 530   A = C >*/
L530:
    a = c__;
/*<       FA = F >*/
    fa = f;
/*<       DA = D >*/
    da = d__;
/*<       IF ( D .GT. A6*G ) GOTO 450 >*/
    if (d__ > a6 * g) {
        goto L450;
    }
/*<       GOTO 560 >*/
    goto L560;
/*< 540   IF ( D .LT. 0. ) GOTO 520 >*/
L540:
    if (d__ < (float)0.) {
        goto L520;
    }
/*<       GOTO 530 >*/
    goto L530;
/*< 550   C = .5*(A+B) >*/
L550:
    c__ = (a + b) * (float).5;
/*<       NB = NB + 1 >*/
    ++nb;
/*<       W = DABS(B-A) >*/
    w = (d__1 = b - a, abs(d__1));
/*<       GOTO 500 >*/
    goto L500;
/*< 560   E = 0. >*/
L560:
    *e = (float)0.;
/*<       DO 570 I = 1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<            IF ( DABS(H(I,3)) .GT. E ) E = DABS(H(I,3)) >*/
        if ((d__1 = h__[i__ + h_dim1 * 3], abs(d__1)) > *e) {
            *e = (d__2 = h__[i__ + h_dim1 * 3], abs(d__2));
        }
/*< 570        X(I) = H(I,2) >*/
/* L570: */
        x[i__] = h__[i__ + (h_dim1 << 1)];
    }
/*<       IT = IT + 1 >*/
    ++(*it);
/*<       IF ( E .LE. T ) GOTO 660 >*/
    if (*e <= *t) {
        goto L660;
    }
/*<       IF ( IT .GE. LIMIT ) GOTO 660 >*/
    if (*it >= *limit) {
        goto L660;
    }
/*<       F = FA >*/
    f = fa;
/*<       D = DA >*/
    d__ = da;
/*<       A = A7*A >*/
    a = a7 * a;
/*<       CALL PRE(H(1,2),H(1,3)) >*/
    (*pre)(&h__[(h_dim1 << 1) + 1], &h__[h_dim1 * 3 + 1], userdata);
/*<       R = 0. >*/
    r__ = (float)0.;
/*<       DO 580 I = 1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 580        R = R + H(I,2)*H(I,3) >*/
/* L580: */
        r__ += h__[i__ + (h_dim1 << 1)] * h__[i__ + h_dim1 * 3];
    }
/*<       IF ( R .LT. 0. ) GOTO 620 >*/
    if (r__ < (float)0.) {
        goto L620;
    }
/*<       S = R/G >*/
    s = r__ / g;
/*<       G = R >*/
    g = r__;
/*<       L = L + 1 >*/
    ++l;
/*<       IF ( L .GE. M ) GOTO 50 >*/
    if (l >= *m) {
        goto L50;
    }
/*<       D = 0. >*/
    d__ = (float)0.;
/*<       DO 590 I = 1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<            H(I,1) = -H(I,2) + S*H(I,1) >*/
        h__[i__ + h_dim1] = -h__[i__ + (h_dim1 << 1)] + s * h__[i__ + h_dim1];
/*< 590        D = D + H(I,1)*H(I,3) >*/
/* L590: */
        d__ += h__[i__ + h_dim1] * h__[i__ + h_dim1 * 3];
    }
/*<       GOTO 70 >*/
    goto L70;
/*< 600   IF ( D .LT. G ) GOTO 560 >*/
L600:
    if (d__ < g) {
        goto L560;
    }
/*<       WRITE(6,*) 'UNABLE TO OBTAIN DESCENT DIRECTION' >*/
    if(error_code)
      {
      *error_code = 1;
      return 0;
      }
    else
      {
      printf("UNABLE TO OBTAIN DESCENT DIRECTION\n");
      }
/*<       STOP >*/
    /*assert(0);*/
    return 0;
/*< 610   WRITE(6,*) 'THE FUNCTION DECREASES WITH NO MINIMUM' >*/
L610:
    if(error_code)
      {
      *error_code = 2;
      }
    else
      {
      printf("THE FUNCTION DECREASES WITH NO MINIMUM\n");
      }
/*<       STOP >*/
    /*assert(0);*/
    return 0;
/*< 620   WRITE(6,*) 'PRECONDITIONER NOT POSITIVE DEFINITE' >*/
L620:
    if(error_code)
      {
      *error_code = 3;
      return 0;
      }
    else
      {
      printf("PRECONDITIONER NOT POSITIVE DEFINITE\n");
      }
/*<       STOP >*/
    /*assert(0);*/
    return 0;
/*< 630   Q = Q*A3**25 >*/
L630:
/* Computing 25th power */
    d__1 = a3, d__2 = d__1, d__1 *= d__1, d__1 *= d__1, d__1 *= d__1, d__2 *=
            d__1;
    q *= d__2 * (d__1 * d__1);
/*<       ND = 0 >*/
    nd = 0;
/*< 640   ND = ND + 1 >*/
L640:
    ++nd;
/*<       IF ( ND .GT. 25 ) GOTO 650 >*/
    if (nd > 25) {
        goto L650;
    }
/*<       Q = A3*Q >*/
    q = a3 * q;
/*<       P = FV(Q,X,H,N,VALUE) >*/
    p = fv_(&q, &x[1], &h__[h_offset], n, value, userdata);
/*<       CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z) >*/
    ins_(&q, &p, &a, &b, &c__, &fa, &fb, &fc, &j, y, z__);
/*<       IF ( P-F .GT. V*Q ) GOTO 640 >*/
    if (p - f > v * q) {
        goto L640;
    }
/*<       GOTO 135 >*/
    goto L135;
/*< 650   WRITE(6,*) 'UNABLE TO SATISFY ARMIJO CONDITION' >*/
L650:
    if(error_code)
      {
      *error_code = 4;
      }
    else
      {
      printf("UNABLE TO SATISFY ARMIJO CONDITION\n");
      }
/*<       RETURN >*/
    return 0;
/*< 660   STEP = A >*/
L660:
    *step = a;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* cg_ */

/*<       DOUBLE PRECISION FUNCTION FV(A,X,H,N,VALUE) >*/
doublereal fv_(doublereal *a, doublereal *x, doublereal *h__, integer *n,
               double (*value)(double*,void*), void* userdata)
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1;
    doublereal ret_val;

    /* Local variables */
    integer i__;

/*<       REAL*8 H(N,1),X(1),A,VALUE >*/
/*<       EXTERNAL VALUE >*/
/*<       DO 10 I = 1 , N >*/
    /* Parameter adjustments */
    --x;
    h_dim1 = *n;
    h_offset = 1 + h_dim1;
    h__ -= h_offset;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 10         H(I,2) = X(I) + A*H(I,1) >*/
/* L10: */
        h__[i__ + (h_dim1 << 1)] = x[i__] + *a * h__[i__ + h_dim1];
    }
/*<       FV = VALUE(H(1,2)) >*/
    ret_val = (*value)(&h__[(h_dim1 << 1) + 1], userdata);
/*<       RETURN >*/
    return ret_val;
/*<       END >*/
} /* fv_ */

/*<       DOUBLE PRECISION FUNCTION FD(A,X,H,N,GRAD) >*/
doublereal fd_(doublereal *a, doublereal *x, doublereal *h__, integer *n,
        void (*grad)(double*,double*,void*), void* userdata)
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1;
    doublereal ret_val;

    /* Local variables */
    doublereal d__;
    integer i__;

/*<       REAL*8 H(N,1),X(1),A,D >*/
/*<       EXTERNAL GRAD >*/
/*<       DO 10 I = 1 , N >*/
    /* Parameter adjustments */
    --x;
    h_dim1 = *n;
    h_offset = 1 + h_dim1;
    h__ -= h_offset;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 10         H(I,2) = X(I) + A*H(I,1) >*/
/* L10: */
        h__[i__ + (h_dim1 << 1)] = x[i__] + *a * h__[i__ + h_dim1];
    }
/*<       CALL GRAD(H(1,3),H(1,2)) >*/
    (*grad)(&h__[h_dim1 * 3 + 1], &h__[(h_dim1 << 1) + 1], userdata);
/*<       D = 0. >*/
    d__ = (float)0.;
/*<       DO 20 I = 1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 20         D = D + H(I,1)*H(I,3) >*/
/* L20: */
        d__ += h__[i__ + h_dim1] * h__[i__ + h_dim1 * 3];
    }
/*<       FD = D >*/
    ret_val = d__;
/*<       RETURN >*/
    return ret_val;
/*<       END >*/
} /* fd_ */

/*<       SUBROUTINE FVD(V,D,A,X,H,N,BOTH) >*/
/* Subroutine */ int fvd_(doublereal *v, doublereal *d__, doublereal *a,
                          doublereal *x, doublereal *h__, integer *n,
                          void (*both)(double*,double*,double*,void*),
                          void* userdata)
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1;

    /* Local variables */
    integer i__;

/*<       REAL*8 H(N,1),X(1),A,D,V >*/
/*<       EXTERNAL BOTH >*/
/*<       DO 10 I = 1 , N >*/
    /* Parameter adjustments */
    --x;
    h_dim1 = *n;
    h_offset = 1 + h_dim1;
    h__ -= h_offset;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 10         H(I,2) = X(I) + A*H(I,1) >*/
/* L10: */
        h__[i__ + (h_dim1 << 1)] = x[i__] + *a * h__[i__ + h_dim1];
    }
/*<       CALL BOTH(V,H(1,3),H(1,2)) >*/
    (*both)(v, &h__[h_dim1 * 3 + 1], &h__[(h_dim1 << 1) + 1], userdata);
/*<       D = 0. >*/
    *d__ = (float)0.;
/*<       DO 20 I = 1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 20         D = D + H(I,1)*H(I,3) >*/
/* L20: */
        *d__ += h__[i__ + h_dim1] * h__[i__ + h_dim1 * 3];
    }
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* fvd_ */

/*<       SUBROUTINE CUB(X,A,B,C,D,E,F) >*/
/* Subroutine */ int cub_(doublereal *x, doublereal *a, doublereal *b,
        doublereal *c__, doublereal *d__, doublereal *e, doublereal *f)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal g, v, w, y, z__;

/*<       REAL*8 A,B,C,D,E,F,G,V,W,X,Y,Z >*/
/*<       G = B - A >*/
    g = *b - *a;
/*<       IF ( G .EQ. 0. ) GOTO 50 >*/
    if (g == (float)0.) {
        goto L50;
    }
/*<       V = E + F - 3*(D-C)/G >*/
    v = *e + *f - (*d__ - *c__) * 3 / g;
/*<       W = V*V-E*F >*/
    w = v * v - *e * *f;
/*<       IF ( W .LT. 0. ) W = 0. >*/
    if (w < (float)0.) {
        w = (float)0.;
    }
/*<       W = DSIGN(DSQRT(W),G) >*/
    d__1 = sqrt(w);
    w = d_sign(&d__1, &g);
/*<       Y = E + V >*/
    y = *e + v;
/*<       Z = F + V >*/
    z__ = *f + v;
/*<       IF ( DSIGN(Y,G) .NE. Y ) GOTO 30 >*/
    if (d_sign(&y, &g) != y) {
        goto L30;
    }
/*<       IF ( DSIGN(Z,G) .NE. Z ) GOTO 20 >*/
    if (d_sign(&z__, &g) != z__) {
        goto L20;
    }
/*<       IF ( Z .EQ. 0. ) GOTO 20 >*/
    if (z__ == (float)0.) {
        goto L20;
    }
/*< 10    X = B - G*F/(Z+W) >*/
L10:
    *x = *b - g * *f / (z__ + w);
/*<       RETURN >*/
    return 0;
/*< 20    IF ( C .LT. D ) X = A >*/
L20:
    if (*c__ < *d__) {
        *x = *a;
    }
/*<       IF ( C .GE. D ) X = B >*/
    if (*c__ >= *d__) {
        *x = *b;
    }
/*<       RETURN >*/
    return 0;
/*< 30    IF ( DSIGN(Z,G) .NE. Z ) GOTO 40 >*/
L30:
    if (d_sign(&z__, &g) != z__) {
        goto L40;
    }
/*<       IF ( DABS(E) .GT. DABS(F) ) GOTO 10 >*/
    if (abs(*e) > abs(*f)) {
        goto L10;
    }
/*< 40    X = A + G*E/(Y-W) >*/
L40:
    *x = *a + g * *e / (y - w);
/*<       RETURN >*/
    return 0;
/*< 50    X = A >*/
L50:
    *x = *a;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* cub_ */

/*<       SUBROUTINE INS(S,F,A,B,C,FA,FB,FC,J,Y,Z) >*/
/* Subroutine */ int ins_(doublereal *s, doublereal *f, doublereal *a,
        doublereal *b, doublereal *c__, doublereal *fa, doublereal *fb,
        doublereal *fc, integer *j, doublereal *y, doublereal *z__)
{
/*<       REAL*8 A,B,C,F,FA,FB,FC,S,Y(1),Z(1) >*/
/*<       INTEGER J >*/
/*<       J = J + 1 >*/
    /* Parameter adjustments */
    --z__;
    --y;

    /* Function Body */
    ++(*j);
/*<       Y(J) = S >*/
    y[*j] = *s;
/*<       Z(J) = F >*/
    z__[*j] = *f;
/*<       IF ( F .LE. FA ) GOTO 20 >*/
    if (*f <= *fa) {
        goto L20;
    }
/*<       IF ( F .LE. FB ) GOTO 10 >*/
    if (*f <= *fb) {
        goto L10;
    }
/*<       IF ( F .GT. FC ) RETURN >*/
    if (*f > *fc) {
        return 0;
    }
/*<       C = S >*/
    *c__ = *s;
/*<       FC = F >*/
    *fc = *f;
/*<       RETURN >*/
    return 0;
/*< 10    C = B >*/
L10:
    *c__ = *b;
/*<       B = S >*/
    *b = *s;
/*<       FC = FB >*/
    *fc = *fb;
/*<       FB = F >*/
    *fb = *f;
/*<       RETURN >*/
    return 0;
/*< 20    C = B >*/
L20:
    *c__ = *b;
/*<       B = A >*/
    *b = *a;
/*<       A = S >*/
    *a = *s;
/*<       FC = FB >*/
    *fc = *fb;
/*<       FB = FA >*/
    *fb = *fa;
/*<       FA = F >*/
    *fa = *f;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* ins_ */

#ifdef __cplusplus
        }
#endif

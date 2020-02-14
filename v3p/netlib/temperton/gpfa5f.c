/* temperton/gpfa5f.f -- translated by f2c (version 20050501).
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

static integer c__5 = 5;

/*     fortran version of *gpfa5* - */
/*     radix-5 section of self-sorting, in-place, */
/*        generalized pfa */

/* ------------------------------------------------------------------- */

/*<       subroutine gpfa5f(a,b,trigs,inc,jump,n,mm,lot,isign) >*/
/* Subroutine */ int gpfa5f_(real *a, real *b, real *trigs, integer *inc,
        integer *jump, integer *n, integer *mm, integer *lot, integer *isign)
{
    /* Initialized data */

    static real sin36 = (float).587785252292473; /* constant */
    static real sin72 = (float).951056516295154; /* constant */
    static real qrt5 = (float).559016994374947; /* constant */
    static integer lvr = 128; /* constant */

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8, i__9, i__10;

    /* Builtin functions */
    integer pow_ii(integer *, integer *);

    /* Local variables */
    integer j, k, l, m;
    real s, c1, c2, c3;
    integer n5;
    real t1, t2, t3, t4, t5, t6, t7, t8, t9, u1, u2, u3, u4, u5, u6, u7, u8,
            u9;
    integer ja, jb, la, jc, jd, nb, je, jf, jg, jh;
    real t10, t11, u10, u11, ax, bx;
    integer mh, kk, ll, ji, jj, jk, mu, nu, jl, jm, jn, jo, jp, jq, jr, js,
            jt, ju, jv, jw, jx, jy;
    real co1=0, co2=0, co3=0, co4=0, si1=0, si2=0, si3=0, si4=0,
      aja, ajb, ajc, ajd, aje, bjb,
            bje, bjc, bjd, bja, ajf, ajk, bjf, bjk, ajg, ajj, ajh, aji, ajl,
            ajq, bjg, bjj, bjh, bji, bjl, bjq, ajo, ajm, ajn, ajr, ajw, bjo,
            bjm, bjn, bjr, bjw, ajt, ajs, ajx, ajp, bjt, bjs, bjx, bjp, ajv,
            ajy, aju, bjv, bjy, bju;
    integer inq, ink, jjj, ninc, left, nvex, ipass, nblox, jstep, laincl,
            jstepl, istart, jstepx;

/*<       real a(*), b(*), trigs(*) >*/
/*<       integer inc, jump, n, mm, lot, isign >*/
/*<       real s, ax, bx, c1, c2, c3 >*/
/*<       real t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11 >*/
/*<       real u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11 >*/
/*<       real co1, co2, co3, co4, si1, si2, si3, si4 >*/
/*<       real aja, ajb, ajc, ajd, aje, bjb, bje, bjc >*/
/*<       real bjd, bja, ajf, ajk, bjf, bjk, ajg, ajj >*/
/*<       real ajh, aji, ajl, ajq, bjg, bjj, bjh, bji >*/
/*<       real bjl, bjq, ajo, ajm, ajn, ajr, ajw, bjo >*/
/*<       real bjm, bjn, bjr, bjw, ajt, ajs, ajx, ajp >*/
/*<       real bjt, bjs, bjx, bjp, ajv, ajy, aju, bjv >*/
/*<       real bjy, bju >*/
/*<    >*/
    /* Parameter adjustments */
    --trigs;
    --b;
    --a;

    /* Function Body */
/*<       data lvr/128/ >*/

/*     *************************************************************** */
/*     *                                                             * */
/*     *  N.B. LVR = LENGTH OF VECTOR REGISTERS, SET TO 128 FOR C90. * */
/*     *  RESET TO 64 FOR OTHER CRAY MACHINES, OR TO ANY LARGE VALUE * */
/*     *  (GREATER THAN OR EQUAL TO LOT) FOR A SCALAR COMPUTER.      * */
/*     *                                                             * */
/*     *************************************************************** */

/*<       n5 = 5 ** mm >*/
    n5 = pow_ii(&c__5, mm);
/*<       inq = n / n5 >*/
    inq = *n / n5;
/*<       jstepx = (n5-n) * inc >*/
    jstepx = (n5 - *n) * *inc;
/*<       ninc = n * inc >*/
    ninc = *n * *inc;
/*<       ink = inc * inq >*/
    ink = *inc * inq;
/*<       mu = mod(inq,5) >*/
    mu = inq % 5;
/*<       if (isign.eq.-1) mu = 5 - mu >*/
    if (*isign == -1) {
        mu = 5 - mu;
    }

/*<       m = mm >*/
    m = *mm;
/*<       mh = (m+1)/2 >*/
    mh = (m + 1) / 2;
/*<       s = float(isign) >*/
//    s = (real) (*isign);
/*<       c1 = qrt5 >*/
    c1 = qrt5;
/*<       c2 = sin72 >*/
    c2 = sin72;
/*<       c3 = sin36 >*/
    c3 = sin36;
/*<       if (mu.eq.2.or.mu.eq.3) then >*/
    if (mu == 2 || mu == 3) {
/*<          c1 = -c1 >*/
        c1 = -c1;
/*<          c2 = sin36 >*/
        c2 = sin36;
/*<          c3 = sin72 >*/
        c3 = sin72;
/*<       endif >*/
    }
/*<       if (mu.eq.3.or.mu.eq.4) c2 = -c2 >*/
    if (mu == 3 || mu == 4) {
        c2 = -c2;
    }
/*<       if (mu.eq.2.or.mu.eq.4) c3 = -c3 >*/
    if (mu == 2 || mu == 4) {
        c3 = -c3;
    }

/*<       nblox = 1 + (lot-1)/lvr >*/
    nblox = (*lot - 1) / lvr + 1;
/*<       left = lot >*/
    left = *lot;
/*<       s = float(isign) >*/
    s = (real) (*isign);
/*<       istart = 1 >*/
    istart = 1;

/*  loop on blocks of lvr transforms */
/*  -------------------------------- */
/*<       do 500 nb = 1 , nblox >*/
    i__1 = nblox;
    for (nb = 1; nb <= i__1; ++nb) {

/*<       if (left.le.lvr) then >*/
        if (left <= lvr) {
/*<          nvex = left >*/
            nvex = left;
/*<       else if (left.lt.(2*lvr)) then >*/
        } else if (left < lvr << 1) {
/*<          nvex = left/2 >*/
            nvex = left / 2;
/*<          nvex = nvex + mod(nvex,2) >*/
            nvex += nvex % 2;
/*<       else >*/
        } else {
/*<          nvex = lvr >*/
            nvex = lvr;
/*<       endif >*/
        }
/*<       left = left - nvex >*/
        left -= nvex;

/*<       la = 1 >*/
        la = 1;

/*  loop on type I radix-5 passes */
/*  ----------------------------- */
/*<       do 160 ipass = 1 , mh >*/
        i__2 = mh;
        for (ipass = 1; ipass <= i__2; ++ipass) {
/*<       jstep = (n*inc) / (5*la) >*/
            jstep = *n * *inc / (la * 5);
/*<       jstepl = jstep - ninc >*/
            jstepl = jstep - ninc;
/*<       kk = 0 >*/
            kk = 0;

/*  loop on k */
/*  --------- */
/*<       do 150 k = 0 , jstep-ink , ink >*/
            i__3 = jstep - ink;
            i__4 = ink;
            for (k = 0; i__4 < 0 ? k >= i__3 : k <= i__3; k += i__4) {

/*<       if (k.gt.0) then >*/
                if (k > 0) {
/*<       co1 = trigs(kk+1) >*/
                    co1 = trigs[kk + 1];
/*<       si1 = s*trigs(kk+2) >*/
                    si1 = s * trigs[kk + 2];
/*<       co2 = trigs(2*kk+1) >*/
                    co2 = trigs[(kk << 1) + 1];
/*<       si2 = s*trigs(2*kk+2) >*/
                    si2 = s * trigs[(kk << 1) + 2];
/*<       co3 = trigs(3*kk+1) >*/
                    co3 = trigs[kk * 3 + 1];
/*<       si3 = s*trigs(3*kk+2) >*/
                    si3 = s * trigs[kk * 3 + 2];
/*<       co4 = trigs(4*kk+1) >*/
                    co4 = trigs[(kk << 2) + 1];
/*<       si4 = s*trigs(4*kk+2) >*/
                    si4 = s * trigs[(kk << 2) + 2];
/*<       endif >*/
                }

/*  loop along transform */
/*  -------------------- */
/*<       do 140 jjj = k , (n-1)*inc , 5*jstep >*/
                i__5 = (*n - 1) * *inc;
                i__6 = jstep * 5;
                for (jjj = k; i__6 < 0 ? jjj >= i__5 : jjj <= i__5; jjj +=
                        i__6) {
/*<       ja = istart + jjj >*/
                    ja = istart + jjj;

/*     "transverse" loop */
/*     ----------------- */
/*<       do 135 nu = 1 , inq >*/
                    i__7 = inq;
                    for (nu = 1; nu <= i__7; ++nu) {
/*<       jb = ja + jstepl >*/
                        jb = ja + jstepl;
/*<       if (jb.lt.istart) jb = jb + ninc >*/
                        if (jb < istart) {
                            jb += ninc;
                        }
/*<       jc = jb + jstepl >*/
                        jc = jb + jstepl;
/*<       if (jc.lt.istart) jc = jc + ninc >*/
                        if (jc < istart) {
                            jc += ninc;
                        }
/*<       jd = jc + jstepl >*/
                        jd = jc + jstepl;
/*<       if (jd.lt.istart) jd = jd + ninc >*/
                        if (jd < istart) {
                            jd += ninc;
                        }
/*<       je = jd + jstepl >*/
                        je = jd + jstepl;
/*<       if (je.lt.istart) je = je + ninc >*/
                        if (je < istart) {
                            je += ninc;
                        }
/*<       j = 0 >*/
                        j = 0;

/*  loop across transforms */
/*  ---------------------- */
/*<       if (k.eq.0) then >*/
                        if (k == 0) {

/* dir$ ivdep, shortloop */
/*<       do 110 l = 1 , nvex >*/
                            i__8 = nvex;
                            for (l = 1; l <= i__8; ++l) {
/*<       ajb = a(jb+j) >*/
                                ajb = a[jb + j];
/*<       aje = a(je+j) >*/
                                aje = a[je + j];
/*<       t1 = ajb + aje >*/
                                t1 = ajb + aje;
/*<       ajc = a(jc+j) >*/
                                ajc = a[jc + j];
/*<       ajd = a(jd+j) >*/
                                ajd = a[jd + j];
/*<       t2 = ajc + ajd >*/
                                t2 = ajc + ajd;
/*<       t3 = ajb - aje >*/
                                t3 = ajb - aje;
/*<       t4 = ajc - ajd >*/
                                t4 = ajc - ajd;
/*<       t5 = t1 + t2 >*/
                                t5 = t1 + t2;
/*<       t6 = c1 * ( t1 - t2 ) >*/
                                t6 = c1 * (t1 - t2);
/*<       aja = a(ja+j) >*/
                                aja = a[ja + j];
/*<       t7 = aja - 0.25 * t5 >*/
                                t7 = aja - t5 * (float).25;
/*<       a(ja+j) = aja + t5 >*/
                                a[ja + j] = aja + t5;
/*<       t8 = t7 + t6 >*/
                                t8 = t7 + t6;
/*<       t9 = t7 - t6 >*/
                                t9 = t7 - t6;
/*<       t10 = c3 * t3 - c2 * t4 >*/
                                t10 = c3 * t3 - c2 * t4;
/*<       t11 = c2 * t3 + c3 * t4 >*/
                                t11 = c2 * t3 + c3 * t4;
/*<       bjb = b(jb+j) >*/
                                bjb = b[jb + j];
/*<       bje = b(je+j) >*/
                                bje = b[je + j];
/*<       u1 = bjb + bje >*/
                                u1 = bjb + bje;
/*<       bjc = b(jc+j) >*/
                                bjc = b[jc + j];
/*<       bjd = b(jd+j) >*/
                                bjd = b[jd + j];
/*<       u2 = bjc + bjd >*/
                                u2 = bjc + bjd;
/*<       u3 = bjb - bje >*/
                                u3 = bjb - bje;
/*<       u4 = bjc - bjd >*/
                                u4 = bjc - bjd;
/*<       u5 = u1 + u2 >*/
                                u5 = u1 + u2;
/*<       u6 = c1 * ( u1 - u2 ) >*/
                                u6 = c1 * (u1 - u2);
/*<       bja = b(ja+j) >*/
                                bja = b[ja + j];
/*<       u7 = bja - 0.25 * u5 >*/
                                u7 = bja - u5 * (float).25;
/*<       b(ja+j) = bja + u5 >*/
                                b[ja + j] = bja + u5;
/*<       u8 = u7 + u6 >*/
                                u8 = u7 + u6;
/*<       u9 = u7 - u6 >*/
                                u9 = u7 - u6;
/*<       u10 = c3 * u3 - c2 * u4 >*/
                                u10 = c3 * u3 - c2 * u4;
/*<       u11 = c2 * u3 + c3 * u4 >*/
                                u11 = c2 * u3 + c3 * u4;
/*<       a(jb+j) = t8 - u11 >*/
                                a[jb + j] = t8 - u11;
/*<       b(jb+j) = u8 + t11 >*/
                                b[jb + j] = u8 + t11;
/*<       a(je+j) = t8 + u11 >*/
                                a[je + j] = t8 + u11;
/*<       b(je+j) = u8 - t11 >*/
                                b[je + j] = u8 - t11;
/*<       a(jc+j) = t9 - u10 >*/
                                a[jc + j] = t9 - u10;
/*<       b(jc+j) = u9 + t10 >*/
                                b[jc + j] = u9 + t10;
/*<       a(jd+j) = t9 + u10 >*/
                                a[jd + j] = t9 + u10;
/*<       b(jd+j) = u9 - t10 >*/
                                b[jd + j] = u9 - t10;
/*<       j = j + jump >*/
                                j += *jump;
/*<   110 continue >*/
/* L110: */
                            }

/*<       else >*/
                        } else {

/* dir$ ivdep,shortloop */
/*<       do 130 l = 1 , nvex >*/
                            i__8 = nvex;
                            for (l = 1; l <= i__8; ++l) {
/*<       ajb = a(jb+j) >*/
                                ajb = a[jb + j];
/*<       aje = a(je+j) >*/
                                aje = a[je + j];
/*<       t1 = ajb + aje >*/
                                t1 = ajb + aje;
/*<       ajc = a(jc+j) >*/
                                ajc = a[jc + j];
/*<       ajd = a(jd+j) >*/
                                ajd = a[jd + j];
/*<       t2 = ajc + ajd >*/
                                t2 = ajc + ajd;
/*<       t3 = ajb - aje >*/
                                t3 = ajb - aje;
/*<       t4 = ajc - ajd >*/
                                t4 = ajc - ajd;
/*<       t5 = t1 + t2 >*/
                                t5 = t1 + t2;
/*<       t6 = c1 * ( t1 - t2 ) >*/
                                t6 = c1 * (t1 - t2);
/*<       aja = a(ja+j) >*/
                                aja = a[ja + j];
/*<       t7 = aja - 0.25 * t5 >*/
                                t7 = aja - t5 * (float).25;
/*<       a(ja+j) = aja + t5 >*/
                                a[ja + j] = aja + t5;
/*<       t8 = t7 + t6 >*/
                                t8 = t7 + t6;
/*<       t9 = t7 - t6 >*/
                                t9 = t7 - t6;
/*<       t10 = c3 * t3 - c2 * t4 >*/
                                t10 = c3 * t3 - c2 * t4;
/*<       t11 = c2 * t3 + c3 * t4 >*/
                                t11 = c2 * t3 + c3 * t4;
/*<       bjb = b(jb+j) >*/
                                bjb = b[jb + j];
/*<       bje = b(je+j) >*/
                                bje = b[je + j];
/*<       u1 = bjb + bje >*/
                                u1 = bjb + bje;
/*<       bjc = b(jc+j) >*/
                                bjc = b[jc + j];
/*<       bjd = b(jd+j) >*/
                                bjd = b[jd + j];
/*<       u2 = bjc + bjd >*/
                                u2 = bjc + bjd;
/*<       u3 = bjb - bje >*/
                                u3 = bjb - bje;
/*<       u4 = bjc - bjd >*/
                                u4 = bjc - bjd;
/*<       u5 = u1 + u2 >*/
                                u5 = u1 + u2;
/*<       u6 = c1 * ( u1 - u2 ) >*/
                                u6 = c1 * (u1 - u2);
/*<       bja = b(ja+j) >*/
                                bja = b[ja + j];
/*<       u7 = bja - 0.25 * u5 >*/
                                u7 = bja - u5 * (float).25;
/*<       b(ja+j) = bja + u5 >*/
                                b[ja + j] = bja + u5;
/*<       u8 = u7 + u6 >*/
                                u8 = u7 + u6;
/*<       u9 = u7 - u6 >*/
                                u9 = u7 - u6;
/*<       u10 = c3 * u3 - c2 * u4 >*/
                                u10 = c3 * u3 - c2 * u4;
/*<       u11 = c2 * u3 + c3 * u4 >*/
                                u11 = c2 * u3 + c3 * u4;
/*<       a(jb+j) = co1*(t8-u11) - si1*(u8+t11) >*/
                                a[jb + j] = co1 * (t8 - u11) - si1 * (u8 +
                                        t11);
/*<       b(jb+j) = si1*(t8-u11) + co1*(u8+t11) >*/
                                b[jb + j] = si1 * (t8 - u11) + co1 * (u8 +
                                        t11);
/*<       a(je+j) = co4*(t8+u11) - si4*(u8-t11) >*/
                                a[je + j] = co4 * (t8 + u11) - si4 * (u8 -
                                        t11);
/*<       b(je+j) = si4*(t8+u11) + co4*(u8-t11) >*/
                                b[je + j] = si4 * (t8 + u11) + co4 * (u8 -
                                        t11);
/*<       a(jc+j) = co2*(t9-u10) - si2*(u9+t10) >*/
                                a[jc + j] = co2 * (t9 - u10) - si2 * (u9 +
                                        t10);
/*<       b(jc+j) = si2*(t9-u10) + co2*(u9+t10) >*/
                                b[jc + j] = si2 * (t9 - u10) + co2 * (u9 +
                                        t10);
/*<       a(jd+j) = co3*(t9+u10) - si3*(u9-t10) >*/
                                a[jd + j] = co3 * (t9 + u10) - si3 * (u9 -
                                        t10);
/*<       b(jd+j) = si3*(t9+u10) + co3*(u9-t10) >*/
                                b[jd + j] = si3 * (t9 + u10) + co3 * (u9 -
                                        t10);
/*<       j = j + jump >*/
                                j += *jump;
/*<   130 continue >*/
/* L130: */
                            }

/*<       endif >*/
                        }

/* -----( end of loop across transforms ) */

/*<       ja = ja + jstepx >*/
                        ja += jstepx;
/*<       if (ja.lt.istart) ja = ja + ninc >*/
                        if (ja < istart) {
                            ja += ninc;
                        }
/*<   135 continue >*/
/* L135: */
                    }
/*<   140 continue >*/
/* L140: */
                }
/* -----( end of loop along transforms ) */
/*<       kk = kk + 2*la >*/
                kk += la << 1;
/*<   150 continue >*/
/* L150: */
            }
/* -----( end of loop on nonzero k ) */
/*<       la = 5*la >*/
            la *= 5;
/*<   160 continue >*/
/* L160: */
        }
/* -----( end of loop on type I radix-5 passes) */

/*<       if (n.eq.5) go to 490 >*/
        if (*n == 5) {
            goto L490;
        }

/*  loop on type II radix-5 passes */
/*  ------------------------------ */
/*<   400 continue >*/
/* L400: */

/*<       do 480 ipass = mh+1 , m >*/
        i__2 = m;
        for (ipass = mh + 1; ipass <= i__2; ++ipass) {
/*<       jstep = (n*inc) / (5*la) >*/
            jstep = *n * *inc / (la * 5);
/*<       jstepl = jstep - ninc >*/
            jstepl = jstep - ninc;
/*<       laincl = la * ink - ninc >*/
            laincl = la * ink - ninc;
/*<       kk = 0 >*/
            kk = 0;

/*     loop on k */
/*     --------- */
/*<       do 470 k = 0 , jstep-ink , ink >*/
            i__4 = jstep - ink;
            i__3 = ink;
            for (k = 0; i__3 < 0 ? k >= i__4 : k <= i__4; k += i__3) {

/*<       if (k.gt.0) then >*/
                if (k > 0) {
/*<       co1 = trigs(kk+1) >*/
                    co1 = trigs[kk + 1];
/*<       si1 = s*trigs(kk+2) >*/
                    si1 = s * trigs[kk + 2];
/*<       co2 = trigs(2*kk+1) >*/
                    co2 = trigs[(kk << 1) + 1];
/*<       si2 = s*trigs(2*kk+2) >*/
                    si2 = s * trigs[(kk << 1) + 2];
/*<       co3 = trigs(3*kk+1) >*/
                    co3 = trigs[kk * 3 + 1];
/*<       si3 = s*trigs(3*kk+2) >*/
                    si3 = s * trigs[kk * 3 + 2];
/*<       co4 = trigs(4*kk+1) >*/
                    co4 = trigs[(kk << 2) + 1];
/*<       si4 = s*trigs(4*kk+2) >*/
                    si4 = s * trigs[(kk << 2) + 2];
/*<       endif >*/
                }

/*  double loop along first transform in block */
/*  ------------------------------------------ */
/*<       do 460 ll = k , (la-1)*ink , 5*jstep >*/
                i__6 = (la - 1) * ink;
                i__5 = jstep * 5;
                for (ll = k; i__5 < 0 ? ll >= i__6 : ll <= i__6; ll += i__5) {

/*<       do 450 jjj = ll , (n-1)*inc , 5*la*ink >*/
                    i__7 = (*n - 1) * *inc;
                    i__8 = la * 5 * ink;
                    for (jjj = ll; i__8 < 0 ? jjj >= i__7 : jjj <= i__7; jjj
                            += i__8) {
/*<       ja = istart + jjj >*/
                        ja = istart + jjj;

/*     "transverse" loop */
/*     ----------------- */
/*<       do 445 nu = 1 , inq >*/
                        i__9 = inq;
                        for (nu = 1; nu <= i__9; ++nu) {
/*<       jb = ja + jstepl >*/
                            jb = ja + jstepl;
/*<       if (jb.lt.istart) jb = jb + ninc >*/
                            if (jb < istart) {
                                jb += ninc;
                            }
/*<       jc = jb + jstepl >*/
                            jc = jb + jstepl;
/*<       if (jc.lt.istart) jc = jc + ninc >*/
                            if (jc < istart) {
                                jc += ninc;
                            }
/*<       jd = jc + jstepl >*/
                            jd = jc + jstepl;
/*<       if (jd.lt.istart) jd = jd + ninc >*/
                            if (jd < istart) {
                                jd += ninc;
                            }
/*<       je = jd + jstepl >*/
                            je = jd + jstepl;
/*<       if (je.lt.istart) je = je + ninc >*/
                            if (je < istart) {
                                je += ninc;
                            }
/*<       jf = ja + laincl >*/
                            jf = ja + laincl;
/*<       if (jf.lt.istart) jf = jf + ninc >*/
                            if (jf < istart) {
                                jf += ninc;
                            }
/*<       jg = jf + jstepl >*/
                            jg = jf + jstepl;
/*<       if (jg.lt.istart) jg = jg + ninc >*/
                            if (jg < istart) {
                                jg += ninc;
                            }
/*<       jh = jg + jstepl >*/
                            jh = jg + jstepl;
/*<       if (jh.lt.istart) jh = jh + ninc >*/
                            if (jh < istart) {
                                jh += ninc;
                            }
/*<       ji = jh + jstepl >*/
                            ji = jh + jstepl;
/*<       if (ji.lt.istart) ji = ji + ninc >*/
                            if (ji < istart) {
                                ji += ninc;
                            }
/*<       jj = ji + jstepl >*/
                            jj = ji + jstepl;
/*<       if (jj.lt.istart) jj = jj + ninc >*/
                            if (jj < istart) {
                                jj += ninc;
                            }
/*<       jk = jf + laincl >*/
                            jk = jf + laincl;
/*<       if (jk.lt.istart) jk = jk + ninc >*/
                            if (jk < istart) {
                                jk += ninc;
                            }
/*<       jl = jk + jstepl >*/
                            jl = jk + jstepl;
/*<       if (jl.lt.istart) jl = jl + ninc >*/
                            if (jl < istart) {
                                jl += ninc;
                            }
/*<       jm = jl + jstepl >*/
                            jm = jl + jstepl;
/*<       if (jm.lt.istart) jm = jm + ninc >*/
                            if (jm < istart) {
                                jm += ninc;
                            }
/*<       jn = jm + jstepl >*/
                            jn = jm + jstepl;
/*<       if (jn.lt.istart) jn = jn + ninc >*/
                            if (jn < istart) {
                                jn += ninc;
                            }
/*<       jo = jn + jstepl >*/
                            jo = jn + jstepl;
/*<       if (jo.lt.istart) jo = jo + ninc >*/
                            if (jo < istart) {
                                jo += ninc;
                            }
/*<       jp = jk + laincl >*/
                            jp = jk + laincl;
/*<       if (jp.lt.istart) jp = jp + ninc >*/
                            if (jp < istart) {
                                jp += ninc;
                            }
/*<       jq = jp + jstepl >*/
                            jq = jp + jstepl;
/*<       if (jq.lt.istart) jq = jq + ninc >*/
                            if (jq < istart) {
                                jq += ninc;
                            }
/*<       jr = jq + jstepl >*/
                            jr = jq + jstepl;
/*<       if (jr.lt.istart) jr = jr + ninc >*/
                            if (jr < istart) {
                                jr += ninc;
                            }
/*<       js = jr + jstepl >*/
                            js = jr + jstepl;
/*<       if (js.lt.istart) js = js + ninc >*/
                            if (js < istart) {
                                js += ninc;
                            }
/*<       jt = js + jstepl >*/
                            jt = js + jstepl;
/*<       if (jt.lt.istart) jt = jt + ninc >*/
                            if (jt < istart) {
                                jt += ninc;
                            }
/*<       ju = jp + laincl >*/
                            ju = jp + laincl;
/*<       if (ju.lt.istart) ju = ju + ninc >*/
                            if (ju < istart) {
                                ju += ninc;
                            }
/*<       jv = ju + jstepl >*/
                            jv = ju + jstepl;
/*<       if (jv.lt.istart) jv = jv + ninc >*/
                            if (jv < istart) {
                                jv += ninc;
                            }
/*<       jw = jv + jstepl >*/
                            jw = jv + jstepl;
/*<       if (jw.lt.istart) jw = jw + ninc >*/
                            if (jw < istart) {
                                jw += ninc;
                            }
/*<       jx = jw + jstepl >*/
                            jx = jw + jstepl;
/*<       if (jx.lt.istart) jx = jx + ninc >*/
                            if (jx < istart) {
                                jx += ninc;
                            }
/*<       jy = jx + jstepl >*/
                            jy = jx + jstepl;
/*<       if (jy.lt.istart) jy = jy + ninc >*/
                            if (jy < istart) {
                                jy += ninc;
                            }
/*<       j = 0 >*/
                            j = 0;

/*  loop across transforms */
/*  ---------------------- */
/*<       if (k.eq.0) then >*/
                            if (k == 0) {

/* dir$ ivdep, shortloop */
/*<       do 410 l = 1 , nvex >*/
                                i__10 = nvex;
                                for (l = 1; l <= i__10; ++l) {
/*<       ajb = a(jb+j) >*/
                                    ajb = a[jb + j];
/*<       aje = a(je+j) >*/
                                    aje = a[je + j];
/*<       t1 = ajb + aje >*/
                                    t1 = ajb + aje;
/*<       ajc = a(jc+j) >*/
                                    ajc = a[jc + j];
/*<       ajd = a(jd+j) >*/
                                    ajd = a[jd + j];
/*<       t2 = ajc + ajd >*/
                                    t2 = ajc + ajd;
/*<       t3 = ajb - aje >*/
                                    t3 = ajb - aje;
/*<       t4 = ajc - ajd >*/
                                    t4 = ajc - ajd;
/*<       ajf = a(jf+j) >*/
                                    ajf = a[jf + j];
/*<       ajb =  ajf >*/
                                    ajb = ajf;
/*<       t5 = t1 + t2 >*/
                                    t5 = t1 + t2;
/*<       t6 = c1 * ( t1 - t2 ) >*/
                                    t6 = c1 * (t1 - t2);
/*<       aja = a(ja+j) >*/
                                    aja = a[ja + j];
/*<       t7 = aja - 0.25 * t5 >*/
                                    t7 = aja - t5 * (float).25;
/*<       a(ja+j) = aja + t5 >*/
                                    a[ja + j] = aja + t5;
/*<       t8 = t7 + t6 >*/
                                    t8 = t7 + t6;
/*<       t9 = t7 - t6 >*/
                                    t9 = t7 - t6;
/*<       ajk = a(jk+j) >*/
                                    ajk = a[jk + j];
/*<       ajc =  ajk >*/
                                    ajc = ajk;
/*<       t10 = c3 * t3 - c2 * t4 >*/
                                    t10 = c3 * t3 - c2 * t4;
/*<       t11 = c2 * t3 + c3 * t4 >*/
                                    t11 = c2 * t3 + c3 * t4;
/*<       bjb = b(jb+j) >*/
                                    bjb = b[jb + j];
/*<       bje = b(je+j) >*/
                                    bje = b[je + j];
/*<       u1 = bjb + bje >*/
                                    u1 = bjb + bje;
/*<       bjc = b(jc+j) >*/
                                    bjc = b[jc + j];
/*<       bjd = b(jd+j) >*/
                                    bjd = b[jd + j];
/*<       u2 = bjc + bjd >*/
                                    u2 = bjc + bjd;
/*<       u3 = bjb - bje >*/
                                    u3 = bjb - bje;
/*<       u4 = bjc - bjd >*/
                                    u4 = bjc - bjd;
/*<       bjf = b(jf+j) >*/
                                    bjf = b[jf + j];
/*<       bjb =  bjf >*/
                                    bjb = bjf;
/*<       u5 = u1 + u2 >*/
                                    u5 = u1 + u2;
/*<       u6 = c1 * ( u1 - u2 ) >*/
                                    u6 = c1 * (u1 - u2);
/*<       bja = b(ja+j) >*/
                                    bja = b[ja + j];
/*<       u7 = bja - 0.25 * u5 >*/
                                    u7 = bja - u5 * (float).25;
/*<       b(ja+j) = bja + u5 >*/
                                    b[ja + j] = bja + u5;
/*<       u8 = u7 + u6 >*/
                                    u8 = u7 + u6;
/*<       u9 = u7 - u6 >*/
                                    u9 = u7 - u6;
/*<       bjk = b(jk+j) >*/
                                    bjk = b[jk + j];
/*<       bjc =  bjk >*/
                                    bjc = bjk;
/*<       u10 = c3 * u3 - c2 * u4 >*/
                                    u10 = c3 * u3 - c2 * u4;
/*<       u11 = c2 * u3 + c3 * u4 >*/
                                    u11 = c2 * u3 + c3 * u4;
/*<       a(jf+j) = t8 - u11 >*/
                                    a[jf + j] = t8 - u11;
/*<       b(jf+j) = u8 + t11 >*/
                                    b[jf + j] = u8 + t11;
/*<       aje =  t8 + u11 >*/
                                    aje = t8 + u11;
/*<       bje =  u8 - t11 >*/
                                    bje = u8 - t11;
/*<       a(jk+j) = t9 - u10 >*/
                                    a[jk + j] = t9 - u10;
/*<       b(jk+j) = u9 + t10 >*/
                                    b[jk + j] = u9 + t10;
/*<       ajd =  t9 + u10 >*/
                                    ajd = t9 + u10;
/*<       bjd =  u9 - t10 >*/
                                    bjd = u9 - t10;
/* ---------------------- */
/*<       ajg = a(jg+j) >*/
                                    ajg = a[jg + j];
/*<       ajj = a(jj+j) >*/
                                    ajj = a[jj + j];
/*<       t1 = ajg + ajj >*/
                                    t1 = ajg + ajj;
/*<       ajh = a(jh+j) >*/
                                    ajh = a[jh + j];
/*<       aji = a(ji+j) >*/
                                    aji = a[ji + j];
/*<       t2 = ajh + aji >*/
                                    t2 = ajh + aji;
/*<       t3 = ajg - ajj >*/
                                    t3 = ajg - ajj;
/*<       t4 = ajh - aji >*/
                                    t4 = ajh - aji;
/*<       ajl = a(jl+j) >*/
                                    ajl = a[jl + j];
/*<       ajh =  ajl >*/
                                    ajh = ajl;
/*<       t5 = t1 + t2 >*/
                                    t5 = t1 + t2;
/*<       t6 = c1 * ( t1 - t2 ) >*/
                                    t6 = c1 * (t1 - t2);
/*<       t7 = ajb - 0.25 * t5 >*/
                                    t7 = ajb - t5 * (float).25;
/*<       a(jb+j) = ajb + t5 >*/
                                    a[jb + j] = ajb + t5;
/*<       t8 = t7 + t6 >*/
                                    t8 = t7 + t6;
/*<       t9 = t7 - t6 >*/
                                    t9 = t7 - t6;
/*<       ajq = a(jq+j) >*/
                                    ajq = a[jq + j];
/*<       aji =  ajq >*/
                                    aji = ajq;
/*<       t10 = c3 * t3 - c2 * t4 >*/
                                    t10 = c3 * t3 - c2 * t4;
/*<       t11 = c2 * t3 + c3 * t4 >*/
                                    t11 = c2 * t3 + c3 * t4;
/*<       bjg = b(jg+j) >*/
                                    bjg = b[jg + j];
/*<       bjj = b(jj+j) >*/
                                    bjj = b[jj + j];
/*<       u1 = bjg + bjj >*/
                                    u1 = bjg + bjj;
/*<       bjh = b(jh+j) >*/
                                    bjh = b[jh + j];
/*<       bji = b(ji+j) >*/
                                    bji = b[ji + j];
/*<       u2 = bjh + bji >*/
                                    u2 = bjh + bji;
/*<       u3 = bjg - bjj >*/
                                    u3 = bjg - bjj;
/*<       u4 = bjh - bji >*/
                                    u4 = bjh - bji;
/*<       bjl = b(jl+j) >*/
                                    bjl = b[jl + j];
/*<       bjh =  bjl >*/
                                    bjh = bjl;
/*<       u5 = u1 + u2 >*/
                                    u5 = u1 + u2;
/*<       u6 = c1 * ( u1 - u2 ) >*/
                                    u6 = c1 * (u1 - u2);
/*<       u7 = bjb - 0.25 * u5 >*/
                                    u7 = bjb - u5 * (float).25;
/*<       b(jb+j) = bjb + u5 >*/
                                    b[jb + j] = bjb + u5;
/*<       u8 = u7 + u6 >*/
                                    u8 = u7 + u6;
/*<       u9 = u7 - u6 >*/
                                    u9 = u7 - u6;
/*<       bjq = b(jq+j) >*/
                                    bjq = b[jq + j];
/*<       bji =  bjq >*/
                                    bji = bjq;
/*<       u10 = c3 * u3 - c2 * u4 >*/
                                    u10 = c3 * u3 - c2 * u4;
/*<       u11 = c2 * u3 + c3 * u4 >*/
                                    u11 = c2 * u3 + c3 * u4;
/*<       a(jg+j) = t8 - u11 >*/
                                    a[jg + j] = t8 - u11;
/*<       b(jg+j) = u8 + t11 >*/
                                    b[jg + j] = u8 + t11;
/*<       ajj =  t8 + u11 >*/
                                    ajj = t8 + u11;
/*<       bjj =  u8 - t11 >*/
                                    bjj = u8 - t11;
/*<       a(jl+j) = t9 - u10 >*/
                                    a[jl + j] = t9 - u10;
/*<       b(jl+j) = u9 + t10 >*/
                                    b[jl + j] = u9 + t10;
/*<       a(jq+j) = t9 + u10 >*/
                                    a[jq + j] = t9 + u10;
/*<       b(jq+j) = u9 - t10 >*/
                                    b[jq + j] = u9 - t10;
/* ---------------------- */
/*<       ajo = a(jo+j) >*/
                                    ajo = a[jo + j];
/*<       t1 = ajh + ajo >*/
                                    t1 = ajh + ajo;
/*<       ajm = a(jm+j) >*/
                                    ajm = a[jm + j];
/*<       ajn = a(jn+j) >*/
                                    ajn = a[jn + j];
/*<       t2 = ajm + ajn >*/
                                    t2 = ajm + ajn;
/*<       t3 = ajh - ajo >*/
                                    t3 = ajh - ajo;
/*<       t4 = ajm - ajn >*/
                                    t4 = ajm - ajn;
/*<       ajr = a(jr+j) >*/
                                    ajr = a[jr + j];
/*<       ajn =  ajr >*/
                                    ajn = ajr;
/*<       t5 = t1 + t2 >*/
                                    t5 = t1 + t2;
/*<       t6 = c1 * ( t1 - t2 ) >*/
                                    t6 = c1 * (t1 - t2);
/*<       t7 = ajc - 0.25 * t5 >*/
                                    t7 = ajc - t5 * (float).25;
/*<       a(jc+j) = ajc + t5 >*/
                                    a[jc + j] = ajc + t5;
/*<       t8 = t7 + t6 >*/
                                    t8 = t7 + t6;
/*<       t9 = t7 - t6 >*/
                                    t9 = t7 - t6;
/*<       ajw = a(jw+j) >*/
                                    ajw = a[jw + j];
/*<       ajo =  ajw >*/
                                    ajo = ajw;
/*<       t10 = c3 * t3 - c2 * t4 >*/
                                    t10 = c3 * t3 - c2 * t4;
/*<       t11 = c2 * t3 + c3 * t4 >*/
                                    t11 = c2 * t3 + c3 * t4;
/*<       bjo = b(jo+j) >*/
                                    bjo = b[jo + j];
/*<       u1 = bjh + bjo >*/
                                    u1 = bjh + bjo;
/*<       bjm = b(jm+j) >*/
                                    bjm = b[jm + j];
/*<       bjn = b(jn+j) >*/
                                    bjn = b[jn + j];
/*<       u2 = bjm + bjn >*/
                                    u2 = bjm + bjn;
/*<       u3 = bjh - bjo >*/
                                    u3 = bjh - bjo;
/*<       u4 = bjm - bjn >*/
                                    u4 = bjm - bjn;
/*<       bjr = b(jr+j) >*/
                                    bjr = b[jr + j];
/*<       bjn =  bjr >*/
                                    bjn = bjr;
/*<       u5 = u1 + u2 >*/
                                    u5 = u1 + u2;
/*<       u6 = c1 * ( u1 - u2 ) >*/
                                    u6 = c1 * (u1 - u2);
/*<       u7 = bjc - 0.25 * u5 >*/
                                    u7 = bjc - u5 * (float).25;
/*<       b(jc+j) = bjc + u5 >*/
                                    b[jc + j] = bjc + u5;
/*<       u8 = u7 + u6 >*/
                                    u8 = u7 + u6;
/*<       u9 = u7 - u6 >*/
                                    u9 = u7 - u6;
/*<       bjw = b(jw+j) >*/
                                    bjw = b[jw + j];
/*<       bjo =  bjw >*/
                                    bjo = bjw;
/*<       u10 = c3 * u3 - c2 * u4 >*/
                                    u10 = c3 * u3 - c2 * u4;
/*<       u11 = c2 * u3 + c3 * u4 >*/
                                    u11 = c2 * u3 + c3 * u4;
/*<       a(jh+j) = t8 - u11 >*/
                                    a[jh + j] = t8 - u11;
/*<       b(jh+j) = u8 + t11 >*/
                                    b[jh + j] = u8 + t11;
/*<       a(jw+j) = t8 + u11 >*/
                                    a[jw + j] = t8 + u11;
/*<       b(jw+j) = u8 - t11 >*/
                                    b[jw + j] = u8 - t11;
/*<       a(jm+j) = t9 - u10 >*/
                                    a[jm + j] = t9 - u10;
/*<       b(jm+j) = u9 + t10 >*/
                                    b[jm + j] = u9 + t10;
/*<       a(jr+j) = t9 + u10 >*/
                                    a[jr + j] = t9 + u10;
/*<       b(jr+j) = u9 - t10 >*/
                                    b[jr + j] = u9 - t10;
/* ---------------------- */
/*<       ajt = a(jt+j) >*/
                                    ajt = a[jt + j];
/*<       t1 = aji + ajt >*/
                                    t1 = aji + ajt;
/*<       ajs = a(js+j) >*/
                                    ajs = a[js + j];
/*<       t2 = ajn + ajs >*/
                                    t2 = ajn + ajs;
/*<       t3 = aji - ajt >*/
                                    t3 = aji - ajt;
/*<       t4 = ajn - ajs >*/
                                    t4 = ajn - ajs;
/*<       ajx = a(jx+j) >*/
                                    ajx = a[jx + j];
/*<       ajt =  ajx >*/
                                    ajt = ajx;
/*<       t5 = t1 + t2 >*/
                                    t5 = t1 + t2;
/*<       t6 = c1 * ( t1 - t2 ) >*/
                                    t6 = c1 * (t1 - t2);
/*<       ajp = a(jp+j) >*/
                                    ajp = a[jp + j];
/*<       t7 = ajp - 0.25 * t5 >*/
                                    t7 = ajp - t5 * (float).25;
/*<       ax = ajp + t5 >*/
                                    ax = ajp + t5;
/*<       t8 = t7 + t6 >*/
                                    t8 = t7 + t6;
/*<       t9 = t7 - t6 >*/
                                    t9 = t7 - t6;
/*<       a(jp+j) = ajd >*/
                                    a[jp + j] = ajd;
/*<       t10 = c3 * t3 - c2 * t4 >*/
                                    t10 = c3 * t3 - c2 * t4;
/*<       t11 = c2 * t3 + c3 * t4 >*/
                                    t11 = c2 * t3 + c3 * t4;
/*<       a(jd+j) = ax >*/
                                    a[jd + j] = ax;
/*<       bjt = b(jt+j) >*/
                                    bjt = b[jt + j];
/*<       u1 = bji + bjt >*/
                                    u1 = bji + bjt;
/*<       bjs = b(js+j) >*/
                                    bjs = b[js + j];
/*<       u2 = bjn + bjs >*/
                                    u2 = bjn + bjs;
/*<       u3 = bji - bjt >*/
                                    u3 = bji - bjt;
/*<       u4 = bjn - bjs >*/
                                    u4 = bjn - bjs;
/*<       bjx = b(jx+j) >*/
                                    bjx = b[jx + j];
/*<       bjt =  bjx >*/
                                    bjt = bjx;
/*<       u5 = u1 + u2 >*/
                                    u5 = u1 + u2;
/*<       u6 = c1 * ( u1 - u2 ) >*/
                                    u6 = c1 * (u1 - u2);
/*<       bjp = b(jp+j) >*/
                                    bjp = b[jp + j];
/*<       u7 = bjp - 0.25 * u5 >*/
                                    u7 = bjp - u5 * (float).25;
/*<       bx = bjp + u5 >*/
                                    bx = bjp + u5;
/*<       u8 = u7 + u6 >*/
                                    u8 = u7 + u6;
/*<       u9 = u7 - u6 >*/
                                    u9 = u7 - u6;
/*<       b(jp+j) = bjd >*/
                                    b[jp + j] = bjd;
/*<       u10 = c3 * u3 - c2 * u4 >*/
                                    u10 = c3 * u3 - c2 * u4;
/*<       u11 = c2 * u3 + c3 * u4 >*/
                                    u11 = c2 * u3 + c3 * u4;
/*<       b(jd+j) = bx >*/
                                    b[jd + j] = bx;
/*<       a(ji+j) = t8 - u11 >*/
                                    a[ji + j] = t8 - u11;
/*<       b(ji+j) = u8 + t11 >*/
                                    b[ji + j] = u8 + t11;
/*<       a(jx+j) = t8 + u11 >*/
                                    a[jx + j] = t8 + u11;
/*<       b(jx+j) = u8 - t11 >*/
                                    b[jx + j] = u8 - t11;
/*<       a(jn+j) = t9 - u10 >*/
                                    a[jn + j] = t9 - u10;
/*<       b(jn+j) = u9 + t10 >*/
                                    b[jn + j] = u9 + t10;
/*<       a(js+j) = t9 + u10 >*/
                                    a[js + j] = t9 + u10;
/*<       b(js+j) = u9 - t10 >*/
                                    b[js + j] = u9 - t10;
/* ---------------------- */
/*<       ajv = a(jv+j) >*/
                                    ajv = a[jv + j];
/*<       ajy = a(jy+j) >*/
                                    ajy = a[jy + j];
/*<       t1 = ajv + ajy >*/
                                    t1 = ajv + ajy;
/*<       t2 = ajo + ajt >*/
                                    t2 = ajo + ajt;
/*<       t3 = ajv - ajy >*/
                                    t3 = ajv - ajy;
/*<       t4 = ajo - ajt >*/
                                    t4 = ajo - ajt;
/*<       a(jv+j) = ajj >*/
                                    a[jv + j] = ajj;
/*<       t5 = t1 + t2 >*/
                                    t5 = t1 + t2;
/*<       t6 = c1 * ( t1 - t2 ) >*/
                                    t6 = c1 * (t1 - t2);
/*<       aju = a(ju+j) >*/
                                    aju = a[ju + j];
/*<       t7 = aju - 0.25 * t5 >*/
                                    t7 = aju - t5 * (float).25;
/*<       ax = aju + t5 >*/
                                    ax = aju + t5;
/*<       t8 = t7 + t6 >*/
                                    t8 = t7 + t6;
/*<       t9 = t7 - t6 >*/
                                    t9 = t7 - t6;
/*<       a(ju+j) = aje >*/
                                    a[ju + j] = aje;
/*<       t10 = c3 * t3 - c2 * t4 >*/
                                    t10 = c3 * t3 - c2 * t4;
/*<       t11 = c2 * t3 + c3 * t4 >*/
                                    t11 = c2 * t3 + c3 * t4;
/*<       a(je+j) = ax >*/
                                    a[je + j] = ax;
/*<       bjv = b(jv+j) >*/
                                    bjv = b[jv + j];
/*<       bjy = b(jy+j) >*/
                                    bjy = b[jy + j];
/*<       u1 = bjv + bjy >*/
                                    u1 = bjv + bjy;
/*<       u2 = bjo + bjt >*/
                                    u2 = bjo + bjt;
/*<       u3 = bjv - bjy >*/
                                    u3 = bjv - bjy;
/*<       u4 = bjo - bjt >*/
                                    u4 = bjo - bjt;
/*<       b(jv+j) = bjj >*/
                                    b[jv + j] = bjj;
/*<       u5 = u1 + u2 >*/
                                    u5 = u1 + u2;
/*<       u6 = c1 * ( u1 - u2 ) >*/
                                    u6 = c1 * (u1 - u2);
/*<       bju = b(ju+j) >*/
                                    bju = b[ju + j];
/*<       u7 = bju - 0.25 * u5 >*/
                                    u7 = bju - u5 * (float).25;
/*<       bx = bju + u5 >*/
                                    bx = bju + u5;
/*<       u8 = u7 + u6 >*/
                                    u8 = u7 + u6;
/*<       u9 = u7 - u6 >*/
                                    u9 = u7 - u6;
/*<       b(ju+j) = bje >*/
                                    b[ju + j] = bje;
/*<       u10 = c3 * u3 - c2 * u4 >*/
                                    u10 = c3 * u3 - c2 * u4;
/*<       u11 = c2 * u3 + c3 * u4 >*/
                                    u11 = c2 * u3 + c3 * u4;
/*<       b(je+j) = bx >*/
                                    b[je + j] = bx;
/*<       a(jj+j) = t8 - u11 >*/
                                    a[jj + j] = t8 - u11;
/*<       b(jj+j) = u8 + t11 >*/
                                    b[jj + j] = u8 + t11;
/*<       a(jy+j) = t8 + u11 >*/
                                    a[jy + j] = t8 + u11;
/*<       b(jy+j) = u8 - t11 >*/
                                    b[jy + j] = u8 - t11;
/*<       a(jo+j) = t9 - u10 >*/
                                    a[jo + j] = t9 - u10;
/*<       b(jo+j) = u9 + t10 >*/
                                    b[jo + j] = u9 + t10;
/*<       a(jt+j) = t9 + u10 >*/
                                    a[jt + j] = t9 + u10;
/*<       b(jt+j) = u9 - t10 >*/
                                    b[jt + j] = u9 - t10;
/*<       j = j + jump >*/
                                    j += *jump;
/*<   410 continue >*/
/* L410: */
                                }

/*<       else >*/
                            } else {

/* dir$ ivdep, shortloop */
/*<       do 440 l = 1 , nvex >*/
                                i__10 = nvex;
                                for (l = 1; l <= i__10; ++l) {
/*<       ajb = a(jb+j) >*/
                                    ajb = a[jb + j];
/*<       aje = a(je+j) >*/
                                    aje = a[je + j];
/*<       t1 = ajb + aje >*/
                                    t1 = ajb + aje;
/*<       ajc = a(jc+j) >*/
                                    ajc = a[jc + j];
/*<       ajd = a(jd+j) >*/
                                    ajd = a[jd + j];
/*<       t2 = ajc + ajd >*/
                                    t2 = ajc + ajd;
/*<       t3 = ajb - aje >*/
                                    t3 = ajb - aje;
/*<       t4 = ajc - ajd >*/
                                    t4 = ajc - ajd;
/*<       ajf = a(jf+j) >*/
                                    ajf = a[jf + j];
/*<       ajb =  ajf >*/
                                    ajb = ajf;
/*<       t5 = t1 + t2 >*/
                                    t5 = t1 + t2;
/*<       t6 = c1 * ( t1 - t2 ) >*/
                                    t6 = c1 * (t1 - t2);
/*<       aja = a(ja+j) >*/
                                    aja = a[ja + j];
/*<       t7 = aja - 0.25 * t5 >*/
                                    t7 = aja - t5 * (float).25;
/*<       a(ja+j) = aja + t5 >*/
                                    a[ja + j] = aja + t5;
/*<       t8 = t7 + t6 >*/
                                    t8 = t7 + t6;
/*<       t9 = t7 - t6 >*/
                                    t9 = t7 - t6;
/*<       ajk = a(jk+j) >*/
                                    ajk = a[jk + j];
/*<       ajc =  ajk >*/
                                    ajc = ajk;
/*<       t10 = c3 * t3 - c2 * t4 >*/
                                    t10 = c3 * t3 - c2 * t4;
/*<       t11 = c2 * t3 + c3 * t4 >*/
                                    t11 = c2 * t3 + c3 * t4;
/*<       bjb = b(jb+j) >*/
                                    bjb = b[jb + j];
/*<       bje = b(je+j) >*/
                                    bje = b[je + j];
/*<       u1 = bjb + bje >*/
                                    u1 = bjb + bje;
/*<       bjc = b(jc+j) >*/
                                    bjc = b[jc + j];
/*<       bjd = b(jd+j) >*/
                                    bjd = b[jd + j];
/*<       u2 = bjc + bjd >*/
                                    u2 = bjc + bjd;
/*<       u3 = bjb - bje >*/
                                    u3 = bjb - bje;
/*<       u4 = bjc - bjd >*/
                                    u4 = bjc - bjd;
/*<       bjf = b(jf+j) >*/
                                    bjf = b[jf + j];
/*<       bjb =  bjf >*/
                                    bjb = bjf;
/*<       u5 = u1 + u2 >*/
                                    u5 = u1 + u2;
/*<       u6 = c1 * ( u1 - u2 ) >*/
                                    u6 = c1 * (u1 - u2);
/*<       bja = b(ja+j) >*/
                                    bja = b[ja + j];
/*<       u7 = bja - 0.25 * u5 >*/
                                    u7 = bja - u5 * (float).25;
/*<       b(ja+j) = bja + u5 >*/
                                    b[ja + j] = bja + u5;
/*<       u8 = u7 + u6 >*/
                                    u8 = u7 + u6;
/*<       u9 = u7 - u6 >*/
                                    u9 = u7 - u6;
/*<       bjk = b(jk+j) >*/
                                    bjk = b[jk + j];
/*<       bjc =  bjk >*/
                                    bjc = bjk;
/*<       u10 = c3 * u3 - c2 * u4 >*/
                                    u10 = c3 * u3 - c2 * u4;
/*<       u11 = c2 * u3 + c3 * u4 >*/
                                    u11 = c2 * u3 + c3 * u4;
/*<       a(jf+j) = co1*(t8-u11) - si1*(u8+t11) >*/
                                    a[jf + j] = co1 * (t8 - u11) - si1 * (u8
                                            + t11);
/*<       b(jf+j) = si1*(t8-u11) + co1*(u8+t11) >*/
                                    b[jf + j] = si1 * (t8 - u11) + co1 * (u8
                                            + t11);
/*<       aje =  co4*(t8+u11) - si4*(u8-t11) >*/
                                    aje = co4 * (t8 + u11) - si4 * (u8 - t11);
/*<       bje =  si4*(t8+u11) + co4*(u8-t11) >*/
                                    bje = si4 * (t8 + u11) + co4 * (u8 - t11);
/*<       a(jk+j) = co2*(t9-u10) - si2*(u9+t10) >*/
                                    a[jk + j] = co2 * (t9 - u10) - si2 * (u9
                                            + t10);
/*<       b(jk+j) = si2*(t9-u10) + co2*(u9+t10) >*/
                                    b[jk + j] = si2 * (t9 - u10) + co2 * (u9
                                            + t10);
/*<       ajd =  co3*(t9+u10) - si3*(u9-t10) >*/
                                    ajd = co3 * (t9 + u10) - si3 * (u9 - t10);
/*<       bjd =  si3*(t9+u10) + co3*(u9-t10) >*/
                                    bjd = si3 * (t9 + u10) + co3 * (u9 - t10);
/* ---------------------- */
/*<       ajg = a(jg+j) >*/
                                    ajg = a[jg + j];
/*<       ajj = a(jj+j) >*/
                                    ajj = a[jj + j];
/*<       t1 = ajg + ajj >*/
                                    t1 = ajg + ajj;
/*<       ajh = a(jh+j) >*/
                                    ajh = a[jh + j];
/*<       aji = a(ji+j) >*/
                                    aji = a[ji + j];
/*<       t2 = ajh + aji >*/
                                    t2 = ajh + aji;
/*<       t3 = ajg - ajj >*/
                                    t3 = ajg - ajj;
/*<       t4 = ajh - aji >*/
                                    t4 = ajh - aji;
/*<       ajl = a(jl+j) >*/
                                    ajl = a[jl + j];
/*<       ajh =  ajl >*/
                                    ajh = ajl;
/*<       t5 = t1 + t2 >*/
                                    t5 = t1 + t2;
/*<       t6 = c1 * ( t1 - t2 ) >*/
                                    t6 = c1 * (t1 - t2);
/*<       t7 = ajb - 0.25 * t5 >*/
                                    t7 = ajb - t5 * (float).25;
/*<       a(jb+j) = ajb + t5 >*/
                                    a[jb + j] = ajb + t5;
/*<       t8 = t7 + t6 >*/
                                    t8 = t7 + t6;
/*<       t9 = t7 - t6 >*/
                                    t9 = t7 - t6;
/*<       ajq = a(jq+j) >*/
                                    ajq = a[jq + j];
/*<       aji =  ajq >*/
                                    aji = ajq;
/*<       t10 = c3 * t3 - c2 * t4 >*/
                                    t10 = c3 * t3 - c2 * t4;
/*<       t11 = c2 * t3 + c3 * t4 >*/
                                    t11 = c2 * t3 + c3 * t4;
/*<       bjg = b(jg+j) >*/
                                    bjg = b[jg + j];
/*<       bjj = b(jj+j) >*/
                                    bjj = b[jj + j];
/*<       u1 = bjg + bjj >*/
                                    u1 = bjg + bjj;
/*<       bjh = b(jh+j) >*/
                                    bjh = b[jh + j];
/*<       bji = b(ji+j) >*/
                                    bji = b[ji + j];
/*<       u2 = bjh + bji >*/
                                    u2 = bjh + bji;
/*<       u3 = bjg - bjj >*/
                                    u3 = bjg - bjj;
/*<       u4 = bjh - bji >*/
                                    u4 = bjh - bji;
/*<       bjl = b(jl+j) >*/
                                    bjl = b[jl + j];
/*<       bjh =  bjl >*/
                                    bjh = bjl;
/*<       u5 = u1 + u2 >*/
                                    u5 = u1 + u2;
/*<       u6 = c1 * ( u1 - u2 ) >*/
                                    u6 = c1 * (u1 - u2);
/*<       u7 = bjb - 0.25 * u5 >*/
                                    u7 = bjb - u5 * (float).25;
/*<       b(jb+j) = bjb + u5 >*/
                                    b[jb + j] = bjb + u5;
/*<       u8 = u7 + u6 >*/
                                    u8 = u7 + u6;
/*<       u9 = u7 - u6 >*/
                                    u9 = u7 - u6;
/*<       bjq = b(jq+j) >*/
                                    bjq = b[jq + j];
/*<       bji =  bjq >*/
                                    bji = bjq;
/*<       u10 = c3 * u3 - c2 * u4 >*/
                                    u10 = c3 * u3 - c2 * u4;
/*<       u11 = c2 * u3 + c3 * u4 >*/
                                    u11 = c2 * u3 + c3 * u4;
/*<       a(jg+j) = co1*(t8-u11) - si1*(u8+t11) >*/
                                    a[jg + j] = co1 * (t8 - u11) - si1 * (u8
                                            + t11);
/*<       b(jg+j) = si1*(t8-u11) + co1*(u8+t11) >*/
                                    b[jg + j] = si1 * (t8 - u11) + co1 * (u8
                                            + t11);
/*<       ajj =  co4*(t8+u11) - si4*(u8-t11) >*/
                                    ajj = co4 * (t8 + u11) - si4 * (u8 - t11);
/*<       bjj =  si4*(t8+u11) + co4*(u8-t11) >*/
                                    bjj = si4 * (t8 + u11) + co4 * (u8 - t11);
/*<       a(jl+j) = co2*(t9-u10) - si2*(u9+t10) >*/
                                    a[jl + j] = co2 * (t9 - u10) - si2 * (u9
                                            + t10);
/*<       b(jl+j) = si2*(t9-u10) + co2*(u9+t10) >*/
                                    b[jl + j] = si2 * (t9 - u10) + co2 * (u9
                                            + t10);
/*<       a(jq+j) = co3*(t9+u10) - si3*(u9-t10) >*/
                                    a[jq + j] = co3 * (t9 + u10) - si3 * (u9
                                            - t10);
/*<       b(jq+j) = si3*(t9+u10) + co3*(u9-t10) >*/
                                    b[jq + j] = si3 * (t9 + u10) + co3 * (u9
                                            - t10);
/* ---------------------- */
/*<       ajo = a(jo+j) >*/
                                    ajo = a[jo + j];
/*<       t1 = ajh + ajo >*/
                                    t1 = ajh + ajo;
/*<       ajm = a(jm+j) >*/
                                    ajm = a[jm + j];
/*<       ajn = a(jn+j) >*/
                                    ajn = a[jn + j];
/*<       t2 = ajm + ajn >*/
                                    t2 = ajm + ajn;
/*<       t3 = ajh - ajo >*/
                                    t3 = ajh - ajo;
/*<       t4 = ajm - ajn >*/
                                    t4 = ajm - ajn;
/*<       ajr = a(jr+j) >*/
                                    ajr = a[jr + j];
/*<       ajn =  ajr >*/
                                    ajn = ajr;
/*<       t5 = t1 + t2 >*/
                                    t5 = t1 + t2;
/*<       t6 = c1 * ( t1 - t2 ) >*/
                                    t6 = c1 * (t1 - t2);
/*<       t7 = ajc - 0.25 * t5 >*/
                                    t7 = ajc - t5 * (float).25;
/*<       a(jc+j) = ajc + t5 >*/
                                    a[jc + j] = ajc + t5;
/*<       t8 = t7 + t6 >*/
                                    t8 = t7 + t6;
/*<       t9 = t7 - t6 >*/
                                    t9 = t7 - t6;
/*<       ajw = a(jw+j) >*/
                                    ajw = a[jw + j];
/*<       ajo =  ajw >*/
                                    ajo = ajw;
/*<       t10 = c3 * t3 - c2 * t4 >*/
                                    t10 = c3 * t3 - c2 * t4;
/*<       t11 = c2 * t3 + c3 * t4 >*/
                                    t11 = c2 * t3 + c3 * t4;
/*<       bjo = b(jo+j) >*/
                                    bjo = b[jo + j];
/*<       u1 = bjh + bjo >*/
                                    u1 = bjh + bjo;
/*<       bjm = b(jm+j) >*/
                                    bjm = b[jm + j];
/*<       bjn = b(jn+j) >*/
                                    bjn = b[jn + j];
/*<       u2 = bjm + bjn >*/
                                    u2 = bjm + bjn;
/*<       u3 = bjh - bjo >*/
                                    u3 = bjh - bjo;
/*<       u4 = bjm - bjn >*/
                                    u4 = bjm - bjn;
/*<       bjr = b(jr+j) >*/
                                    bjr = b[jr + j];
/*<       bjn =  bjr >*/
                                    bjn = bjr;
/*<       u5 = u1 + u2 >*/
                                    u5 = u1 + u2;
/*<       u6 = c1 * ( u1 - u2 ) >*/
                                    u6 = c1 * (u1 - u2);
/*<       u7 = bjc - 0.25 * u5 >*/
                                    u7 = bjc - u5 * (float).25;
/*<       b(jc+j) = bjc + u5 >*/
                                    b[jc + j] = bjc + u5;
/*<       u8 = u7 + u6 >*/
                                    u8 = u7 + u6;
/*<       u9 = u7 - u6 >*/
                                    u9 = u7 - u6;
/*<       bjw = b(jw+j) >*/
                                    bjw = b[jw + j];
/*<       bjo =  bjw >*/
                                    bjo = bjw;
/*<       u10 = c3 * u3 - c2 * u4 >*/
                                    u10 = c3 * u3 - c2 * u4;
/*<       u11 = c2 * u3 + c3 * u4 >*/
                                    u11 = c2 * u3 + c3 * u4;
/*<       a(jh+j) = co1*(t8-u11) - si1*(u8+t11) >*/
                                    a[jh + j] = co1 * (t8 - u11) - si1 * (u8
                                            + t11);
/*<       b(jh+j) = si1*(t8-u11) + co1*(u8+t11) >*/
                                    b[jh + j] = si1 * (t8 - u11) + co1 * (u8
                                            + t11);
/*<       a(jw+j) = co4*(t8+u11) - si4*(u8-t11) >*/
                                    a[jw + j] = co4 * (t8 + u11) - si4 * (u8
                                            - t11);
/*<       b(jw+j) = si4*(t8+u11) + co4*(u8-t11) >*/
                                    b[jw + j] = si4 * (t8 + u11) + co4 * (u8
                                            - t11);
/*<       a(jm+j) = co2*(t9-u10) - si2*(u9+t10) >*/
                                    a[jm + j] = co2 * (t9 - u10) - si2 * (u9
                                            + t10);
/*<       b(jm+j) = si2*(t9-u10) + co2*(u9+t10) >*/
                                    b[jm + j] = si2 * (t9 - u10) + co2 * (u9
                                            + t10);
/*<       a(jr+j) = co3*(t9+u10) - si3*(u9-t10) >*/
                                    a[jr + j] = co3 * (t9 + u10) - si3 * (u9
                                            - t10);
/*<       b(jr+j) = si3*(t9+u10) + co3*(u9-t10) >*/
                                    b[jr + j] = si3 * (t9 + u10) + co3 * (u9
                                            - t10);
/* ---------------------- */
/*<       ajt = a(jt+j) >*/
                                    ajt = a[jt + j];
/*<       t1 = aji + ajt >*/
                                    t1 = aji + ajt;
/*<       ajs = a(js+j) >*/
                                    ajs = a[js + j];
/*<       t2 = ajn + ajs >*/
                                    t2 = ajn + ajs;
/*<       t3 = aji - ajt >*/
                                    t3 = aji - ajt;
/*<       t4 = ajn - ajs >*/
                                    t4 = ajn - ajs;
/*<       ajx = a(jx+j) >*/
                                    ajx = a[jx + j];
/*<       ajt =  ajx >*/
                                    ajt = ajx;
/*<       t5 = t1 + t2 >*/
                                    t5 = t1 + t2;
/*<       t6 = c1 * ( t1 - t2 ) >*/
                                    t6 = c1 * (t1 - t2);
/*<       ajp = a(jp+j) >*/
                                    ajp = a[jp + j];
/*<       t7 = ajp - 0.25 * t5 >*/
                                    t7 = ajp - t5 * (float).25;
/*<       ax = ajp + t5 >*/
                                    ax = ajp + t5;
/*<       t8 = t7 + t6 >*/
                                    t8 = t7 + t6;
/*<       t9 = t7 - t6 >*/
                                    t9 = t7 - t6;
/*<       a(jp+j) = ajd >*/
                                    a[jp + j] = ajd;
/*<       t10 = c3 * t3 - c2 * t4 >*/
                                    t10 = c3 * t3 - c2 * t4;
/*<       t11 = c2 * t3 + c3 * t4 >*/
                                    t11 = c2 * t3 + c3 * t4;
/*<       a(jd+j) = ax >*/
                                    a[jd + j] = ax;
/*<       bjt = b(jt+j) >*/
                                    bjt = b[jt + j];
/*<       u1 = bji + bjt >*/
                                    u1 = bji + bjt;
/*<       bjs = b(js+j) >*/
                                    bjs = b[js + j];
/*<       u2 = bjn + bjs >*/
                                    u2 = bjn + bjs;
/*<       u3 = bji - bjt >*/
                                    u3 = bji - bjt;
/*<       u4 = bjn - bjs >*/
                                    u4 = bjn - bjs;
/*<       bjx = b(jx+j) >*/
                                    bjx = b[jx + j];
/*<       bjt =  bjx >*/
                                    bjt = bjx;
/*<       u5 = u1 + u2 >*/
                                    u5 = u1 + u2;
/*<       u6 = c1 * ( u1 - u2 ) >*/
                                    u6 = c1 * (u1 - u2);
/*<       bjp = b(jp+j) >*/
                                    bjp = b[jp + j];
/*<       u7 = bjp - 0.25 * u5 >*/
                                    u7 = bjp - u5 * (float).25;
/*<       bx = bjp + u5 >*/
                                    bx = bjp + u5;
/*<       u8 = u7 + u6 >*/
                                    u8 = u7 + u6;
/*<       u9 = u7 - u6 >*/
                                    u9 = u7 - u6;
/*<       b(jp+j) = bjd >*/
                                    b[jp + j] = bjd;
/*<       u10 = c3 * u3 - c2 * u4 >*/
                                    u10 = c3 * u3 - c2 * u4;
/*<       u11 = c2 * u3 + c3 * u4 >*/
                                    u11 = c2 * u3 + c3 * u4;
/*<       b(jd+j) = bx >*/
                                    b[jd + j] = bx;
/*<       a(ji+j) = co1*(t8-u11) - si1*(u8+t11) >*/
                                    a[ji + j] = co1 * (t8 - u11) - si1 * (u8
                                            + t11);
/*<       b(ji+j) = si1*(t8-u11) + co1*(u8+t11) >*/
                                    b[ji + j] = si1 * (t8 - u11) + co1 * (u8
                                            + t11);
/*<       a(jx+j) = co4*(t8+u11) - si4*(u8-t11) >*/
                                    a[jx + j] = co4 * (t8 + u11) - si4 * (u8
                                            - t11);
/*<       b(jx+j) = si4*(t8+u11) + co4*(u8-t11) >*/
                                    b[jx + j] = si4 * (t8 + u11) + co4 * (u8
                                            - t11);
/*<       a(jn+j) = co2*(t9-u10) - si2*(u9+t10) >*/
                                    a[jn + j] = co2 * (t9 - u10) - si2 * (u9
                                            + t10);
/*<       b(jn+j) = si2*(t9-u10) + co2*(u9+t10) >*/
                                    b[jn + j] = si2 * (t9 - u10) + co2 * (u9
                                            + t10);
/*<       a(js+j) = co3*(t9+u10) - si3*(u9-t10) >*/
                                    a[js + j] = co3 * (t9 + u10) - si3 * (u9
                                            - t10);
/*<       b(js+j) = si3*(t9+u10) + co3*(u9-t10) >*/
                                    b[js + j] = si3 * (t9 + u10) + co3 * (u9
                                            - t10);
/* ---------------------- */
/*<       ajv = a(jv+j) >*/
                                    ajv = a[jv + j];
/*<       ajy = a(jy+j) >*/
                                    ajy = a[jy + j];
/*<       t1 = ajv + ajy >*/
                                    t1 = ajv + ajy;
/*<       t2 = ajo + ajt >*/
                                    t2 = ajo + ajt;
/*<       t3 = ajv - ajy >*/
                                    t3 = ajv - ajy;
/*<       t4 = ajo - ajt >*/
                                    t4 = ajo - ajt;
/*<       a(jv+j) = ajj >*/
                                    a[jv + j] = ajj;
/*<       t5 = t1 + t2 >*/
                                    t5 = t1 + t2;
/*<       t6 = c1 * ( t1 - t2 ) >*/
                                    t6 = c1 * (t1 - t2);
/*<       aju = a(ju+j) >*/
                                    aju = a[ju + j];
/*<       t7 = aju - 0.25 * t5 >*/
                                    t7 = aju - t5 * (float).25;
/*<       ax = aju + t5 >*/
                                    ax = aju + t5;
/*<       t8 = t7 + t6 >*/
                                    t8 = t7 + t6;
/*<       t9 = t7 - t6 >*/
                                    t9 = t7 - t6;
/*<       a(ju+j) = aje >*/
                                    a[ju + j] = aje;
/*<       t10 = c3 * t3 - c2 * t4 >*/
                                    t10 = c3 * t3 - c2 * t4;
/*<       t11 = c2 * t3 + c3 * t4 >*/
                                    t11 = c2 * t3 + c3 * t4;
/*<       a(je+j) = ax >*/
                                    a[je + j] = ax;
/*<       bjv = b(jv+j) >*/
                                    bjv = b[jv + j];
/*<       bjy = b(jy+j) >*/
                                    bjy = b[jy + j];
/*<       u1 = bjv + bjy >*/
                                    u1 = bjv + bjy;
/*<       u2 = bjo + bjt >*/
                                    u2 = bjo + bjt;
/*<       u3 = bjv - bjy >*/
                                    u3 = bjv - bjy;
/*<       u4 = bjo - bjt >*/
                                    u4 = bjo - bjt;
/*<       b(jv+j) = bjj >*/
                                    b[jv + j] = bjj;
/*<       u5 = u1 + u2 >*/
                                    u5 = u1 + u2;
/*<       u6 = c1 * ( u1 - u2 ) >*/
                                    u6 = c1 * (u1 - u2);
/*<       bju = b(ju+j) >*/
                                    bju = b[ju + j];
/*<       u7 = bju - 0.25 * u5 >*/
                                    u7 = bju - u5 * (float).25;
/*<       bx = bju + u5 >*/
                                    bx = bju + u5;
/*<       u8 = u7 + u6 >*/
                                    u8 = u7 + u6;
/*<       u9 = u7 - u6 >*/
                                    u9 = u7 - u6;
/*<       b(ju+j) = bje >*/
                                    b[ju + j] = bje;
/*<       u10 = c3 * u3 - c2 * u4 >*/
                                    u10 = c3 * u3 - c2 * u4;
/*<       u11 = c2 * u3 + c3 * u4 >*/
                                    u11 = c2 * u3 + c3 * u4;
/*<       b(je+j) = bx >*/
                                    b[je + j] = bx;
/*<       a(jj+j) = co1*(t8-u11) - si1*(u8+t11) >*/
                                    a[jj + j] = co1 * (t8 - u11) - si1 * (u8
                                            + t11);
/*<       b(jj+j) = si1*(t8-u11) + co1*(u8+t11) >*/
                                    b[jj + j] = si1 * (t8 - u11) + co1 * (u8
                                            + t11);
/*<       a(jy+j) = co4*(t8+u11) - si4*(u8-t11) >*/
                                    a[jy + j] = co4 * (t8 + u11) - si4 * (u8
                                            - t11);
/*<       b(jy+j) = si4*(t8+u11) + co4*(u8-t11) >*/
                                    b[jy + j] = si4 * (t8 + u11) + co4 * (u8
                                            - t11);
/*<       a(jo+j) = co2*(t9-u10) - si2*(u9+t10) >*/
                                    a[jo + j] = co2 * (t9 - u10) - si2 * (u9
                                            + t10);
/*<       b(jo+j) = si2*(t9-u10) + co2*(u9+t10) >*/
                                    b[jo + j] = si2 * (t9 - u10) + co2 * (u9
                                            + t10);
/*<       a(jt+j) = co3*(t9+u10) - si3*(u9-t10) >*/
                                    a[jt + j] = co3 * (t9 + u10) - si3 * (u9
                                            - t10);
/*<       b(jt+j) = si3*(t9+u10) + co3*(u9-t10) >*/
                                    b[jt + j] = si3 * (t9 + u10) + co3 * (u9
                                            - t10);
/*<       j = j + jump >*/
                                    j += *jump;
/*<   440 continue >*/
/* L440: */
                                }

/*<       endif >*/
                            }

/* -----(end of loop across transforms) */

/*<       ja = ja + jstepx >*/
                            ja += jstepx;
/*<       if (ja.lt.istart) ja = ja + ninc >*/
                            if (ja < istart) {
                                ja += ninc;
                            }
/*<   445 continue >*/
/* L445: */
                        }
/*<   450 continue >*/
/* L450: */
                    }
/*<   460 continue >*/
/* L460: */
                }
/* -----( end of double loop for this k ) */
/*<       kk = kk + 2*la >*/
                kk += la << 1;
/*<   470 continue >*/
/* L470: */
            }
/* -----( end of loop over values of k ) */
/*<       la = 5*la >*/
            la *= 5;
/*<   480 continue >*/
/* L480: */
        }
/* -----( end of loop on type II radix-5 passes ) */
/* -----( nvex transforms completed) */
/*<   490 continue >*/
L490:
/*<       istart = istart + nvex * jump >*/
        istart += nvex * *jump;
/*<   500 continue >*/
/* L500: */
    }
/* -----( end of loop on blocks of transforms ) */

/*<       return >*/
    return 0;
/*<       end >*/
} /* gpfa5f_ */

#ifdef __cplusplus
        }
#endif

/* temperton/gpfa3f.f -- translated by f2c (version 20050501).
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

static integer c__3 = 3;

/*     fortran version of *gpfa3* - */
/*     radix-3 section of self-sorting, in-place */
/*        generalized PFA */

/* ------------------------------------------------------------------- */

/*<       subroutine gpfa3f(a,b,trigs,inc,jump,n,mm,lot,isign) >*/
/* Subroutine */ int gpfa3f_(real *a, real *b, real *trigs, integer *inc,
        integer *jump, integer *n, integer *mm, integer *lot, integer *isign)
{
    /* Initialized data */

    static real sin60 = (float).866025403784437; /* constant */
    static integer lvr = 128; /* constant */

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8, i__9, i__10;

    /* Builtin functions */
    integer pow_ii(integer *, integer *);

    /* Local variables */
    integer j, k, l, m;
    real s, c1;
    integer n3;
    real t1, t2, t3, u1, u2, u3;
    integer ja, jb, la, jc, jd, nb, je, jf, jg, jh, mh, kk, ji, ll, mu, nu;
    real co1, co2, si1, si2, aja, ajb, ajc, bjb, bjc, bja, ajd, bjd, aje, ajf,
             ajh, bje, bjf, bjh, aji, ajg, bji, bjg;
    integer jjj, ink, inq, ninc, left, nvex, ipass, nblox, jstep, laincl,
            jstepl, istart, jstepx;

/*<       real a(*), b(*), trigs(*) >*/
/*<       integer inc, jump, n, mm, lot, isign >*/
/*<       real s, c1, t1, t2, t3, u1, u2, u3, co1, co2 >*/
/*<       real si1, si2, aja, ajb, ajc, bjb, bjc, bja, ajd, bjd >*/
/*<       real aje, ajf, ajh, bje, bjf, bjh, aji, ajg, bji, bjg >*/
/*<       data sin60/0.866025403784437/ >*/
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

/*<       n3 = 3**mm >*/
    n3 = pow_ii(&c__3, mm);
/*<       inq = n/n3 >*/
    inq = *n / n3;
/*<       jstepx = (n3-n) * inc >*/
    jstepx = (n3 - *n) * *inc;
/*<       ninc = n * inc >*/
    ninc = *n * *inc;
/*<       ink = inc * inq >*/
    ink = *inc * inq;
/*<       mu = mod(inq,3) >*/
    mu = inq % 3;
/*<       if (isign.eq.-1) mu = 3-mu >*/
    if (*isign == -1) {
        mu = 3 - mu;
    }
/*<       m = mm >*/
    m = *mm;
/*<       mh = (m+1)/2 >*/
    mh = (m + 1) / 2;
/*<       s = float(isign) >*/
//    s = (real) (*isign);
/*<       c1 = sin60 >*/
    c1 = sin60;
/*<       if (mu.eq.2) c1 = -c1 >*/
    if (mu == 2) {
        c1 = -c1;
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

/*  loop on type I radix-3 passes */
/*  ----------------------------- */
/*<       do 160 ipass = 1 , mh >*/
        i__2 = mh;
        for (ipass = 1; ipass <= i__2; ++ipass) {
/*<       jstep = (n*inc) / (3*la) >*/
            jstep = *n * *inc / (la * 3);
/*<       jstepl = jstep - ninc >*/
            jstepl = jstep - ninc;

/*  k = 0 loop (no twiddle factors) */
/*  ------------------------------- */
/*<       do 120 jjj = 0 , (n-1)*inc , 3*jstep >*/
            i__3 = (*n - 1) * *inc;
            i__4 = jstep * 3;
            for (jjj = 0; i__4 < 0 ? jjj >= i__3 : jjj <= i__3; jjj += i__4) {
/*<       ja = istart + jjj >*/
                ja = istart + jjj;

/*  "transverse" loop */
/*  ----------------- */
/*<       do 115 nu = 1 , inq >*/
                i__5 = inq;
                for (nu = 1; nu <= i__5; ++nu) {
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
/*<       j = 0 >*/
                    j = 0;

/*  loop across transforms */
/*  ---------------------- */
/* dir$ ivdep, shortloop */
/*<       do 110 l = 1 , nvex >*/
                    i__6 = nvex;
                    for (l = 1; l <= i__6; ++l) {
/*<       ajb = a(jb+j) >*/
                        ajb = a[jb + j];
/*<       ajc = a(jc+j) >*/
                        ajc = a[jc + j];
/*<       t1 = ajb + ajc >*/
                        t1 = ajb + ajc;
/*<       aja = a(ja+j) >*/
                        aja = a[ja + j];
/*<       t2 = aja - 0.5 * t1 >*/
                        t2 = aja - t1 * (float).5;
/*<       t3 = c1 * ( ajb - ajc ) >*/
                        t3 = c1 * (ajb - ajc);
/*<       bjb = b(jb+j) >*/
                        bjb = b[jb + j];
/*<       bjc = b(jc+j) >*/
                        bjc = b[jc + j];
/*<       u1 = bjb + bjc >*/
                        u1 = bjb + bjc;
/*<       bja = b(ja+j) >*/
                        bja = b[ja + j];
/*<       u2 = bja - 0.5 * u1 >*/
                        u2 = bja - u1 * (float).5;
/*<       u3 = c1 * ( bjb - bjc ) >*/
                        u3 = c1 * (bjb - bjc);
/*<       a(ja+j) = aja + t1 >*/
                        a[ja + j] = aja + t1;
/*<       b(ja+j) = bja + u1 >*/
                        b[ja + j] = bja + u1;
/*<       a(jb+j) = t2 - u3 >*/
                        a[jb + j] = t2 - u3;
/*<       b(jb+j) = u2 + t3 >*/
                        b[jb + j] = u2 + t3;
/*<       a(jc+j) = t2 + u3 >*/
                        a[jc + j] = t2 + u3;
/*<       b(jc+j) = u2 - t3 >*/
                        b[jc + j] = u2 - t3;
/*<       j = j + jump >*/
                        j += *jump;
/*<   110 continue >*/
/* L110: */
                    }
/*<       ja = ja + jstepx >*/
                    ja += jstepx;
/*<       if (ja.lt.istart) ja = ja + ninc >*/
                    if (ja < istart) {
                        ja += ninc;
                    }
/*<   115 continue >*/
/* L115: */
                }
/*<   120 continue >*/
/* L120: */
            }

/*  finished if n3 = 3 */
/*  ------------------ */
/*<       if (n3.eq.3) go to 490 >*/
            if (n3 == 3) {
                goto L490;
            }
/*<       kk = 2 * la >*/
            kk = la << 1;

/*  loop on nonzero k */
/*  ----------------- */
/*<       do 150 k = ink , jstep-ink , ink >*/
            i__4 = jstep - ink;
            i__3 = ink;
            for (k = ink; i__3 < 0 ? k >= i__4 : k <= i__4; k += i__3) {
/*<       co1 = trigs(kk+1) >*/
                co1 = trigs[kk + 1];
/*<       si1 = s*trigs(kk+2) >*/
                si1 = s * trigs[kk + 2];
/*<       co2 = trigs(2*kk+1) >*/
                co2 = trigs[(kk << 1) + 1];
/*<       si2 = s*trigs(2*kk+2) >*/
                si2 = s * trigs[(kk << 1) + 2];

/*  loop along transform */
/*  -------------------- */
/*<       do 140 jjj = k , (n-1)*inc , 3*jstep >*/
                i__5 = (*n - 1) * *inc;
                i__6 = jstep * 3;
                for (jjj = k; i__6 < 0 ? jjj >= i__5 : jjj <= i__5; jjj +=
                        i__6) {
/*<       ja = istart + jjj >*/
                    ja = istart + jjj;

/*  "transverse" loop */
/*  ----------------- */
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
/*<       j = 0 >*/
                        j = 0;

/*  loop across transforms */
/*  ---------------------- */
/* dir$ ivdep,shortloop */
/*<       do 130 l = 1 , nvex >*/
                        i__8 = nvex;
                        for (l = 1; l <= i__8; ++l) {
/*<       ajb = a(jb+j) >*/
                            ajb = a[jb + j];
/*<       ajc = a(jc+j) >*/
                            ajc = a[jc + j];
/*<       t1 = ajb + ajc >*/
                            t1 = ajb + ajc;
/*<       aja = a(ja+j) >*/
                            aja = a[ja + j];
/*<       t2 = aja - 0.5 * t1 >*/
                            t2 = aja - t1 * (float).5;
/*<       t3 = c1 * ( ajb - ajc ) >*/
                            t3 = c1 * (ajb - ajc);
/*<       bjb = b(jb+j) >*/
                            bjb = b[jb + j];
/*<       bjc = b(jc+j) >*/
                            bjc = b[jc + j];
/*<       u1 = bjb + bjc >*/
                            u1 = bjb + bjc;
/*<       bja = b(ja+j) >*/
                            bja = b[ja + j];
/*<       u2 = bja - 0.5 * u1 >*/
                            u2 = bja - u1 * (float).5;
/*<       u3 = c1 * ( bjb - bjc ) >*/
                            u3 = c1 * (bjb - bjc);
/*<       a(ja+j) = aja + t1 >*/
                            a[ja + j] = aja + t1;
/*<       b(ja+j) = bja + u1 >*/
                            b[ja + j] = bja + u1;
/*<       a(jb+j) = co1*(t2-u3) - si1*(u2+t3) >*/
                            a[jb + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
/*<       b(jb+j) = si1*(t2-u3) + co1*(u2+t3) >*/
                            b[jb + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
/*<       a(jc+j) = co2*(t2+u3) - si2*(u2-t3) >*/
                            a[jc + j] = co2 * (t2 + u3) - si2 * (u2 - t3);
/*<       b(jc+j) = si2*(t2+u3) + co2*(u2-t3) >*/
                            b[jc + j] = si2 * (t2 + u3) + co2 * (u2 - t3);
/*<       j = j + jump >*/
                            j += *jump;
/*<   130 continue >*/
/* L130: */
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
/*<       la = 3*la >*/
            la *= 3;
/*<   160 continue >*/
/* L160: */
        }
/* -----( end of loop on type I radix-3 passes) */

/*  loop on type II radix-3 passes */
/*  ------------------------------ */
/*<   400 continue >*/
/* L400: */

/*<       do 480 ipass = mh+1 , m >*/
        i__2 = m;
        for (ipass = mh + 1; ipass <= i__2; ++ipass) {
/*<       jstep = (n*inc) / (3*la) >*/
            jstep = *n * *inc / (la * 3);
/*<       jstepl = jstep - ninc >*/
            jstepl = jstep - ninc;
/*<       laincl = la*ink - ninc >*/
            laincl = la * ink - ninc;

/*  k=0 loop (no twiddle factors) */
/*  ----------------------------- */
/*<       do 430 ll = 0 , (la-1)*ink , 3*jstep >*/
            i__3 = (la - 1) * ink;
            i__4 = jstep * 3;
            for (ll = 0; i__4 < 0 ? ll >= i__3 : ll <= i__3; ll += i__4) {

/*<       do 420 jjj = ll , (n-1)*inc , 3*la*ink >*/
                i__6 = (*n - 1) * *inc;
                i__5 = la * 3 * ink;
                for (jjj = ll; i__5 < 0 ? jjj >= i__6 : jjj <= i__6; jjj +=
                        i__5) {
/*<       ja = istart + jjj >*/
                    ja = istart + jjj;

/*  "transverse" loop */
/*  ----------------- */
/*<       do 415 nu = 1 , inq >*/
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
/*<       jd = ja + laincl >*/
                        jd = ja + laincl;
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
/*<       jf = je + jstepl >*/
                        jf = je + jstepl;
/*<       if (jf.lt.istart) jf = jf + ninc >*/
                        if (jf < istart) {
                            jf += ninc;
                        }
/*<       jg = jd + laincl >*/
                        jg = jd + laincl;
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
/*<       j = 0 >*/
                        j = 0;

/*  loop across transforms */
/*  ---------------------- */
/* dir$ ivdep, shortloop */
/*<       do 410 l = 1 , nvex >*/
                        i__8 = nvex;
                        for (l = 1; l <= i__8; ++l) {
/*<       ajb = a(jb+j) >*/
                            ajb = a[jb + j];
/*<       ajc = a(jc+j) >*/
                            ajc = a[jc + j];
/*<       t1 = ajb + ajc >*/
                            t1 = ajb + ajc;
/*<       aja = a(ja+j) >*/
                            aja = a[ja + j];
/*<       t2 = aja - 0.5 * t1 >*/
                            t2 = aja - t1 * (float).5;
/*<       t3 = c1 * ( ajb - ajc ) >*/
                            t3 = c1 * (ajb - ajc);
/*<       ajd = a(jd+j) >*/
                            ajd = a[jd + j];
/*<       ajb =  ajd >*/
                            ajb = ajd;
/*<       bjb = b(jb+j) >*/
                            bjb = b[jb + j];
/*<       bjc = b(jc+j) >*/
                            bjc = b[jc + j];
/*<       u1 = bjb + bjc >*/
                            u1 = bjb + bjc;
/*<       bja = b(ja+j) >*/
                            bja = b[ja + j];
/*<       u2 = bja - 0.5 * u1 >*/
                            u2 = bja - u1 * (float).5;
/*<       u3 = c1 * ( bjb - bjc ) >*/
                            u3 = c1 * (bjb - bjc);
/*<       bjd = b(jd+j) >*/
                            bjd = b[jd + j];
/*<       bjb =  bjd >*/
                            bjb = bjd;
/*<       a(ja+j) = aja + t1 >*/
                            a[ja + j] = aja + t1;
/*<       b(ja+j) = bja + u1 >*/
                            b[ja + j] = bja + u1;
/*<       a(jd+j) = t2 - u3 >*/
                            a[jd + j] = t2 - u3;
/*<       b(jd+j) = u2 + t3 >*/
                            b[jd + j] = u2 + t3;
/*<       ajc =  t2 + u3 >*/
                            ajc = t2 + u3;
/*<       bjc =  u2 - t3 >*/
                            bjc = u2 - t3;
/* ---------------------- */
/*<       aje = a(je+j) >*/
                            aje = a[je + j];
/*<       ajf = a(jf+j) >*/
                            ajf = a[jf + j];
/*<       t1 = aje + ajf >*/
                            t1 = aje + ajf;
/*<       t2 = ajb - 0.5 * t1 >*/
                            t2 = ajb - t1 * (float).5;
/*<       t3 = c1 * ( aje - ajf ) >*/
                            t3 = c1 * (aje - ajf);
/*<       ajh = a(jh+j) >*/
                            ajh = a[jh + j];
/*<       ajf =  ajh >*/
                            ajf = ajh;
/*<       bje = b(je+j) >*/
                            bje = b[je + j];
/*<       bjf = b(jf+j) >*/
                            bjf = b[jf + j];
/*<       u1 = bje + bjf >*/
                            u1 = bje + bjf;
/*<       u2 = bjb - 0.5 * u1 >*/
                            u2 = bjb - u1 * (float).5;
/*<       u3 = c1 * ( bje - bjf ) >*/
                            u3 = c1 * (bje - bjf);
/*<       bjh = b(jh+j) >*/
                            bjh = b[jh + j];
/*<       bjf =  bjh >*/
                            bjf = bjh;
/*<       a(jb+j) = ajb + t1 >*/
                            a[jb + j] = ajb + t1;
/*<       b(jb+j) = bjb + u1 >*/
                            b[jb + j] = bjb + u1;
/*<       a(je+j) = t2 - u3 >*/
                            a[je + j] = t2 - u3;
/*<       b(je+j) = u2 + t3 >*/
                            b[je + j] = u2 + t3;
/*<       a(jh+j) = t2 + u3 >*/
                            a[jh + j] = t2 + u3;
/*<       b(jh+j) = u2 - t3 >*/
                            b[jh + j] = u2 - t3;
/* ---------------------- */
/*<       aji = a(ji+j) >*/
                            aji = a[ji + j];
/*<       t1 = ajf + aji >*/
                            t1 = ajf + aji;
/*<       ajg = a(jg+j) >*/
                            ajg = a[jg + j];
/*<       t2 = ajg - 0.5 * t1 >*/
                            t2 = ajg - t1 * (float).5;
/*<       t3 = c1 * ( ajf - aji ) >*/
                            t3 = c1 * (ajf - aji);
/*<       t1 = ajg + t1 >*/
                            t1 = ajg + t1;
/*<       a(jg+j) = ajc >*/
                            a[jg + j] = ajc;
/*<       bji = b(ji+j) >*/
                            bji = b[ji + j];
/*<       u1 = bjf + bji >*/
                            u1 = bjf + bji;
/*<       bjg = b(jg+j) >*/
                            bjg = b[jg + j];
/*<       u2 = bjg - 0.5 * u1 >*/
                            u2 = bjg - u1 * (float).5;
/*<       u3 = c1 * ( bjf - bji ) >*/
                            u3 = c1 * (bjf - bji);
/*<       u1 = bjg + u1 >*/
                            u1 = bjg + u1;
/*<       b(jg+j) = bjc >*/
                            b[jg + j] = bjc;
/*<       a(jc+j) = t1 >*/
                            a[jc + j] = t1;
/*<       b(jc+j) = u1 >*/
                            b[jc + j] = u1;
/*<       a(jf+j) = t2 - u3 >*/
                            a[jf + j] = t2 - u3;
/*<       b(jf+j) = u2 + t3 >*/
                            b[jf + j] = u2 + t3;
/*<       a(ji+j) = t2 + u3 >*/
                            a[ji + j] = t2 + u3;
/*<       b(ji+j) = u2 - t3 >*/
                            b[ji + j] = u2 - t3;
/*<       j = j + jump >*/
                            j += *jump;
/*<   410 continue >*/
/* L410: */
                        }
/* -----( end of loop across transforms ) */
/*<       ja = ja + jstepx >*/
                        ja += jstepx;
/*<       if (ja.lt.istart) ja = ja + ninc >*/
                        if (ja < istart) {
                            ja += ninc;
                        }
/*<   415 continue >*/
/* L415: */
                    }
/*<   420 continue >*/
/* L420: */
                }
/*<   430 continue >*/
/* L430: */
            }
/* -----( end of double loop for k=0 ) */

/*  finished if last pass */
/*  --------------------- */
/*<       if (ipass.eq.m) go to 490 >*/
            if (ipass == m) {
                goto L490;
            }

/*<       kk = 2*la >*/
            kk = la << 1;

/*     loop on nonzero k */
/*     ----------------- */
/*<       do 470 k = ink , jstep-ink , ink >*/
            i__4 = jstep - ink;
            i__3 = ink;
            for (k = ink; i__3 < 0 ? k >= i__4 : k <= i__4; k += i__3) {
/*<       co1 = trigs(kk+1) >*/
                co1 = trigs[kk + 1];
/*<       si1 = s*trigs(kk+2) >*/
                si1 = s * trigs[kk + 2];
/*<       co2 = trigs(2*kk+1) >*/
                co2 = trigs[(kk << 1) + 1];
/*<       si2 = s*trigs(2*kk+2) >*/
                si2 = s * trigs[(kk << 1) + 2];

/*  double loop along first transform in block */
/*  ------------------------------------------ */
/*<       do 460 ll = k , (la-1)*ink , 3*jstep >*/
                i__5 = (la - 1) * ink;
                i__6 = jstep * 3;
                for (ll = k; i__6 < 0 ? ll >= i__5 : ll <= i__5; ll += i__6) {

/*<       do 450 jjj = ll , (n-1)*inc , 3*la*ink >*/
                    i__7 = (*n - 1) * *inc;
                    i__8 = la * 3 * ink;
                    for (jjj = ll; i__8 < 0 ? jjj >= i__7 : jjj <= i__7; jjj
                            += i__8) {
/*<       ja = istart + jjj >*/
                        ja = istart + jjj;

/*  "transverse" loop */
/*  ----------------- */
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
/*<       jd = ja + laincl >*/
                            jd = ja + laincl;
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
/*<       jf = je + jstepl >*/
                            jf = je + jstepl;
/*<       if (jf.lt.istart) jf = jf + ninc >*/
                            if (jf < istart) {
                                jf += ninc;
                            }
/*<       jg = jd + laincl >*/
                            jg = jd + laincl;
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
/*<       j = 0 >*/
                            j = 0;

/*  loop across transforms */
/*  ---------------------- */
/* dir$ ivdep, shortloop */
/*<       do 440 l = 1 , nvex >*/
                            i__10 = nvex;
                            for (l = 1; l <= i__10; ++l) {
/*<       ajb = a(jb+j) >*/
                                ajb = a[jb + j];
/*<       ajc = a(jc+j) >*/
                                ajc = a[jc + j];
/*<       t1 = ajb + ajc >*/
                                t1 = ajb + ajc;
/*<       aja = a(ja+j) >*/
                                aja = a[ja + j];
/*<       t2 = aja - 0.5 * t1 >*/
                                t2 = aja - t1 * (float).5;
/*<       t3 = c1 * ( ajb - ajc ) >*/
                                t3 = c1 * (ajb - ajc);
/*<       ajd = a(jd+j) >*/
                                ajd = a[jd + j];
/*<       ajb =  ajd >*/
                                ajb = ajd;
/*<       bjb = b(jb+j) >*/
                                bjb = b[jb + j];
/*<       bjc = b(jc+j) >*/
                                bjc = b[jc + j];
/*<       u1 = bjb + bjc >*/
                                u1 = bjb + bjc;
/*<       bja = b(ja+j) >*/
                                bja = b[ja + j];
/*<       u2 = bja - 0.5 * u1 >*/
                                u2 = bja - u1 * (float).5;
/*<       u3 = c1 * ( bjb - bjc ) >*/
                                u3 = c1 * (bjb - bjc);
/*<       bjd = b(jd+j) >*/
                                bjd = b[jd + j];
/*<       bjb =  bjd >*/
                                bjb = bjd;
/*<       a(ja+j) = aja + t1 >*/
                                a[ja + j] = aja + t1;
/*<       b(ja+j) = bja + u1 >*/
                                b[ja + j] = bja + u1;
/*<       a(jd+j) = co1*(t2-u3) - si1*(u2+t3) >*/
                                a[jd + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
/*<       b(jd+j) = si1*(t2-u3) + co1*(u2+t3) >*/
                                b[jd + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
/*<       ajc =  co2*(t2+u3) - si2*(u2-t3) >*/
                                ajc = co2 * (t2 + u3) - si2 * (u2 - t3);
/*<       bjc =  si2*(t2+u3) + co2*(u2-t3) >*/
                                bjc = si2 * (t2 + u3) + co2 * (u2 - t3);
/* ---------------------- */
/*<       aje = a(je+j) >*/
                                aje = a[je + j];
/*<       ajf = a(jf+j) >*/
                                ajf = a[jf + j];
/*<       t1 = aje + ajf >*/
                                t1 = aje + ajf;
/*<       t2 = ajb - 0.5 * t1 >*/
                                t2 = ajb - t1 * (float).5;
/*<       t3 = c1 * ( aje - ajf ) >*/
                                t3 = c1 * (aje - ajf);
/*<       ajh = a(jh+j) >*/
                                ajh = a[jh + j];
/*<       ajf =  ajh >*/
                                ajf = ajh;
/*<       bje = b(je+j) >*/
                                bje = b[je + j];
/*<       bjf = b(jf+j) >*/
                                bjf = b[jf + j];
/*<       u1 = bje + bjf >*/
                                u1 = bje + bjf;
/*<       u2 = bjb - 0.5 * u1 >*/
                                u2 = bjb - u1 * (float).5;
/*<       u3 = c1 * ( bje - bjf ) >*/
                                u3 = c1 * (bje - bjf);
/*<       bjh = b(jh+j) >*/
                                bjh = b[jh + j];
/*<       bjf =  bjh >*/
                                bjf = bjh;
/*<       a(jb+j) = ajb + t1 >*/
                                a[jb + j] = ajb + t1;
/*<       b(jb+j) = bjb + u1 >*/
                                b[jb + j] = bjb + u1;
/*<       a(je+j) = co1*(t2-u3) - si1*(u2+t3) >*/
                                a[je + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
/*<       b(je+j) = si1*(t2-u3) + co1*(u2+t3) >*/
                                b[je + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
/*<       a(jh+j) = co2*(t2+u3) - si2*(u2-t3) >*/
                                a[jh + j] = co2 * (t2 + u3) - si2 * (u2 - t3);
/*<       b(jh+j) = si2*(t2+u3) + co2*(u2-t3) >*/
                                b[jh + j] = si2 * (t2 + u3) + co2 * (u2 - t3);
/* ---------------------- */
/*<       aji = a(ji+j) >*/
                                aji = a[ji + j];
/*<       t1 = ajf + aji >*/
                                t1 = ajf + aji;
/*<       ajg = a(jg+j) >*/
                                ajg = a[jg + j];
/*<       t2 = ajg - 0.5 * t1 >*/
                                t2 = ajg - t1 * (float).5;
/*<       t3 = c1 * ( ajf - aji ) >*/
                                t3 = c1 * (ajf - aji);
/*<       t1 = ajg + t1 >*/
                                t1 = ajg + t1;
/*<       a(jg+j) = ajc >*/
                                a[jg + j] = ajc;
/*<       bji = b(ji+j) >*/
                                bji = b[ji + j];
/*<       u1 = bjf + bji >*/
                                u1 = bjf + bji;
/*<       bjg = b(jg+j) >*/
                                bjg = b[jg + j];
/*<       u2 = bjg - 0.5 * u1 >*/
                                u2 = bjg - u1 * (float).5;
/*<       u3 = c1 * ( bjf - bji ) >*/
                                u3 = c1 * (bjf - bji);
/*<       u1 = bjg + u1 >*/
                                u1 = bjg + u1;
/*<       b(jg+j) = bjc >*/
                                b[jg + j] = bjc;
/*<       a(jc+j) = t1 >*/
                                a[jc + j] = t1;
/*<       b(jc+j) = u1 >*/
                                b[jc + j] = u1;
/*<       a(jf+j) = co1*(t2-u3) - si1*(u2+t3) >*/
                                a[jf + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
/*<       b(jf+j) = si1*(t2-u3) + co1*(u2+t3) >*/
                                b[jf + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
/*<       a(ji+j) = co2*(t2+u3) - si2*(u2-t3) >*/
                                a[ji + j] = co2 * (t2 + u3) - si2 * (u2 - t3);
/*<       b(ji+j) = si2*(t2+u3) + co2*(u2-t3) >*/
                                b[ji + j] = si2 * (t2 + u3) + co2 * (u2 - t3);
/*<       j = j + jump >*/
                                j += *jump;
/*<   440 continue >*/
/* L440: */
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
/*<       la = 3*la >*/
            la *= 3;
/*<   480 continue >*/
/* L480: */
        }
/* -----( end of loop on type II radix-3 passes ) */
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
} /* gpfa3f_ */

#ifdef __cplusplus
        }
#endif

/* temperton/gpfa2f.f -- translated by f2c (version 20050501).
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

static integer c__2 = 2;

/*     fortran version of *gpfa2* - */
/*     radix-2 section of self-sorting, in-place, generalized pfa */
/*     central radix-2 and radix-8 passes included */
/*      so that transform length can be any power of 2 */

/* ------------------------------------------------------------------- */

/*<       subroutine gpfa2f(a,b,trigs,inc,jump,n,mm,lot,isign) >*/
/* Subroutine */ int gpfa2f_(real *a, real *b, real *trigs, integer *inc,
        integer *jump, integer *n, integer *mm, integer *lot, integer *isign)
{
    /* Initialized data */

    static integer lvr = 1024; /* constant */

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8, i__9, i__10;

    /* Builtin functions */
    integer pow_ii(integer *, integer *);
    double sqrt(doublereal);

    /* Local variables */
    integer j, k, l, m=0;
    real s, c1, c2, c3;
    integer m2, n2;
    real t0, t1, t2, t3, u0, u1, u2, u3;
    integer m8, ja, jb, la, jc, jd, nb, je, jf, jg, jh, mh, kk, ji, ll, jj,
            jk, jl, jm, jn, jo, jp, mu, nu;
    real ss, co1, co2, co3, co4, co5, co6, co7, si1, si2, si3, si4, si5, si6,
            si7, aja, ajb, ajc, ajd, bja, bjc, bjb, bjd, aje, ajg, ajf, ajh,
            bje, bjg, bjf, bjh, aji, bjm, ajj, bjj, ajk, ajl, bji, bjk, ajo,
            bjl, bjo, ajm, ajn, ajp, bjn, bjp;
    integer inq, ink, jjj, ninc, left, nvex, ipass, nblox, jstep, laincl,
            jstepl, istart, jstepx;

/*<       real a(*), b(*), trigs(*) >*/
/*<       integer inc, jump, n, mm, lot, isign >*/
/*<       real s, ss, c1, c2, c3, t0, t1, t2, t3, u0, u1, u2, u3 >*/
/*<       real co1, co2, co3, co4, co5, co6, co7 >*/
/*<       real si1, si2, si3, si4, si5, si6, si7 >*/
/*<       real aja, ajb, ajc, ajd, bja, bjc, bjb, bjd >*/
/*<       real aje, ajg, ajf, ajh, bje, bjg, bjf, bjh, aji >*/
/*<       real bjm, ajj, bjj, ajk, ajl, bji, bjk >*/
/*<       real ajo, bjl, bjo, ajm, ajn, ajp, bjn, bjp >*/
/*<       data lvr/1024/ >*/
    /* Parameter adjustments */
    --trigs;
    --b;
    --a;

    /* Function Body */

/*     *************************************************************** */
/*     *                                                             * */
/*     *  N.B. LVR = LENGTH OF VECTOR REGISTERS, SET TO 128 FOR C90. * */
/*     *  RESET TO 64 FOR OTHER CRAY MACHINES, OR TO ANY LARGE VALUE * */
/*     *  (GREATER THAN OR EQUAL TO LOT) FOR A SCALAR COMPUTER.      * */
/*     *                                                             * */
/*     *************************************************************** */

/*<       n2 = 2**mm >*/
    n2 = pow_ii(&c__2, mm);
/*<       inq = n/n2 >*/
    inq = *n / n2;
/*<       jstepx = (n2-n) * inc >*/
    jstepx = (n2 - *n) * *inc;
/*<       ninc = n * inc >*/
    ninc = *n * *inc;
/*<       ink = inc * inq >*/
    ink = *inc * inq;

/*<       m2 = 0 >*/
    m2 = 0;
/*<       m8 = 0 >*/
    m8 = 0;
/*<       if (mod(mm,2).eq.0) then >*/
    if (*mm % 2 == 0) {
/*<          m = mm/2 >*/
        m = *mm / 2;
/*<       else if (mod(mm,4).eq.1) then >*/
    } else if (*mm % 4 == 1) {
/*<          m = (mm-1)/2 >*/
        m = (*mm - 1) / 2;
/*<          m2 = 1 >*/
        m2 = 1;
/*<       else if (mod(mm,4).eq.3) then >*/
    } else if (*mm % 4 == 3) {
/*<          m = (mm-3)/2 >*/
        m = (*mm - 3) / 2;
/*<          m8 = 1 >*/
        m8 = 1;
/*<       endif >*/
    }
/*<       mh = (m+1)/2 >*/
    mh = (m + 1) / 2;

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

/*  loop on type I radix-4 passes */
/*  ----------------------------- */
/*<       mu = mod(inq,4) >*/
        mu = inq % 4;
/*<       if (isign.eq.-1) mu = 4 - mu >*/
        if (*isign == -1) {
            mu = 4 - mu;
        }
/*<       ss = 1.0 >*/
        ss = 1.f;
/*<       if (mu.eq.3) ss = -1.0 >*/
        if (mu == 3) {
            ss = -1.f;
        }

/*<       if (mh.eq.0) go to 200 >*/
        if (mh == 0) {
            goto L200;
        }

/*<       do 160 ipass = 1 , mh >*/
        i__2 = mh;
        for (ipass = 1; ipass <= i__2; ++ipass) {
/*<       jstep = (n*inc) / (4*la) >*/
            jstep = *n * *inc / (la << 2);
/*<       jstepl = jstep - ninc >*/
            jstepl = jstep - ninc;

/*  k = 0 loop (no twiddle factors) */
/*  ------------------------------- */
/*<       do 120 jjj = 0 , (n-1)*inc , 4*jstep >*/
            i__3 = (*n - 1) * *inc;
            i__4 = jstep << 2;
            for (jjj = 0; i__4 < 0 ? jjj >= i__3 : jjj <= i__3; jjj += i__4) {
/*<       ja = istart + jjj >*/
                ja = istart + jjj;

/*     "transverse" loop */
/*     ----------------- */
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
/*<       jd = jc + jstepl >*/
                    jd = jc + jstepl;
/*<       if (jd.lt.istart) jd = jd + ninc >*/
                    if (jd < istart) {
                        jd += ninc;
                    }
/*<       j = 0 >*/
                    j = 0;

/*  loop across transforms */
/*  ---------------------- */
/* dir$ ivdep, shortloop */
/*<       do 110 l = 1 , nvex >*/
                    i__6 = nvex;
                    for (l = 1; l <= i__6; ++l) {
/*<       aja = a(ja+j) >*/
                        aja = a[ja + j];
/*<       ajc = a(jc+j) >*/
                        ajc = a[jc + j];
/*<       t0 = aja + ajc >*/
                        t0 = aja + ajc;
/*<       t2 = aja - ajc >*/
                        t2 = aja - ajc;
/*<       ajb = a(jb+j) >*/
                        ajb = a[jb + j];
/*<       ajd = a(jd+j) >*/
                        ajd = a[jd + j];
/*<       t1 = ajb + ajd >*/
                        t1 = ajb + ajd;
/*<       t3 = ss * ( ajb - ajd ) >*/
                        t3 = ss * (ajb - ajd);
/*<       bja = b(ja+j) >*/
                        bja = b[ja + j];
/*<       bjc = b(jc+j) >*/
                        bjc = b[jc + j];
/*<       u0 = bja + bjc >*/
                        u0 = bja + bjc;
/*<       u2 = bja - bjc >*/
                        u2 = bja - bjc;
/*<       bjb = b(jb+j) >*/
                        bjb = b[jb + j];
/*<       bjd = b(jd+j) >*/
                        bjd = b[jd + j];
/*<       u1 = bjb + bjd >*/
                        u1 = bjb + bjd;
/*<       u3 = ss * ( bjb - bjd ) >*/
                        u3 = ss * (bjb - bjd);
/*<       a(ja+j) = t0 + t1 >*/
                        a[ja + j] = t0 + t1;
/*<       a(jc+j) = t0 - t1 >*/
                        a[jc + j] = t0 - t1;
/*<       b(ja+j) = u0 + u1 >*/
                        b[ja + j] = u0 + u1;
/*<       b(jc+j) = u0 - u1 >*/
                        b[jc + j] = u0 - u1;
/*<       a(jb+j) = t2 - u3 >*/
                        a[jb + j] = t2 - u3;
/*<       a(jd+j) = t2 + u3 >*/
                        a[jd + j] = t2 + u3;
/*<       b(jb+j) = u2 + t3 >*/
                        b[jb + j] = u2 + t3;
/*<       b(jd+j) = u2 - t3 >*/
                        b[jd + j] = u2 - t3;
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

/*  finished if n2 = 4 */
/*  ------------------ */
/*<       if (n2.eq.4) go to 490 >*/
            if (n2 == 4) {
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
/*<       co3 = trigs(3*kk+1) >*/
                co3 = trigs[kk * 3 + 1];
/*<       si3 = s*trigs(3*kk+2) >*/
                si3 = s * trigs[kk * 3 + 2];

/*  loop along transform */
/*  -------------------- */
/*<       do 140 jjj = k , (n-1)*inc , 4*jstep >*/
                i__5 = (*n - 1) * *inc;
                i__6 = jstep << 2;
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
/*<       j = 0 >*/
                        j = 0;

/*  loop across transforms */
/*  ---------------------- */
/* dir$ ivdep,shortloop */
/*<       do 130 l = 1 , nvex >*/
                        i__8 = nvex;
                        for (l = 1; l <= i__8; ++l) {
/*<       aja = a(ja+j) >*/
                            aja = a[ja + j];
/*<       ajc = a(jc+j) >*/
                            ajc = a[jc + j];
/*<       t0 = aja + ajc >*/
                            t0 = aja + ajc;
/*<       t2 = aja - ajc >*/
                            t2 = aja - ajc;
/*<       ajb = a(jb+j) >*/
                            ajb = a[jb + j];
/*<       ajd = a(jd+j) >*/
                            ajd = a[jd + j];
/*<       t1 = ajb + ajd >*/
                            t1 = ajb + ajd;
/*<       t3 = ss * ( ajb - ajd ) >*/
                            t3 = ss * (ajb - ajd);
/*<       bja = b(ja+j) >*/
                            bja = b[ja + j];
/*<       bjc = b(jc+j) >*/
                            bjc = b[jc + j];
/*<       u0 = bja + bjc >*/
                            u0 = bja + bjc;
/*<       u2 = bja - bjc >*/
                            u2 = bja - bjc;
/*<       bjb = b(jb+j) >*/
                            bjb = b[jb + j];
/*<       bjd = b(jd+j) >*/
                            bjd = b[jd + j];
/*<       u1 = bjb + bjd >*/
                            u1 = bjb + bjd;
/*<       u3 = ss * ( bjb - bjd ) >*/
                            u3 = ss * (bjb - bjd);
/*<       a(ja+j) = t0 + t1 >*/
                            a[ja + j] = t0 + t1;
/*<       b(ja+j) = u0 + u1 >*/
                            b[ja + j] = u0 + u1;
/*<       a(jb+j) = co1*(t2-u3) - si1*(u2+t3) >*/
                            a[jb + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
/*<       b(jb+j) = si1*(t2-u3) + co1*(u2+t3) >*/
                            b[jb + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
/*<       a(jc+j) = co2*(t0-t1) - si2*(u0-u1) >*/
                            a[jc + j] = co2 * (t0 - t1) - si2 * (u0 - u1);
/*<       b(jc+j) = si2*(t0-t1) + co2*(u0-u1) >*/
                            b[jc + j] = si2 * (t0 - t1) + co2 * (u0 - u1);
/*<       a(jd+j) = co3*(t2+u3) - si3*(u2-t3) >*/
                            a[jd + j] = co3 * (t2 + u3) - si3 * (u2 - t3);
/*<       b(jd+j) = si3*(t2+u3) + co3*(u2-t3) >*/
                            b[jd + j] = si3 * (t2 + u3) + co3 * (u2 - t3);
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
/*<       la = 4*la >*/
            la <<= 2;
/*<   160 continue >*/
/* L160: */
        }
/* -----( end of loop on type I radix-4 passes) */

/*  central radix-2 pass */
/*  -------------------- */
/*<   200 continue >*/
L200:
/*<       if (m2.eq.0) go to 300 >*/
        if (m2 == 0) {
            goto L300;
        }

/*<       jstep = (n*inc) / (2*la) >*/
        jstep = *n * *inc / (la << 1);
/*<       jstepl = jstep - ninc >*/
        jstepl = jstep - ninc;

/*  k=0 loop (no twiddle factors) */
/*  ----------------------------- */
/*<       do 220 jjj = 0 , (n-1)*inc , 2*jstep >*/
        i__2 = (*n - 1) * *inc;
        i__3 = jstep << 1;
        for (jjj = 0; i__3 < 0 ? jjj >= i__2 : jjj <= i__2; jjj += i__3) {
/*<       ja = istart + jjj >*/
            ja = istart + jjj;

/*     "transverse" loop */
/*     ----------------- */
/*<       do 215 nu = 1 , inq >*/
            i__4 = inq;
            for (nu = 1; nu <= i__4; ++nu) {
/*<       jb = ja + jstepl >*/
                jb = ja + jstepl;
/*<       if (jb.lt.istart) jb = jb + ninc >*/
                if (jb < istart) {
                    jb += ninc;
                }
/*<       j = 0 >*/
                j = 0;

/*  loop across transforms */
/*  ---------------------- */
/* dir$ ivdep, shortloop */
/*<       do 210 l = 1 , nvex >*/
                i__6 = nvex;
                for (l = 1; l <= i__6; ++l) {
/*<       aja = a(ja+j) >*/
                    aja = a[ja + j];
/*<       ajb = a(jb+j) >*/
                    ajb = a[jb + j];
/*<       t0 = aja - ajb >*/
                    t0 = aja - ajb;
/*<       a(ja+j) = aja + ajb >*/
                    a[ja + j] = aja + ajb;
/*<       a(jb+j) = t0 >*/
                    a[jb + j] = t0;
/*<       bja = b(ja+j) >*/
                    bja = b[ja + j];
/*<       bjb = b(jb+j) >*/
                    bjb = b[jb + j];
/*<       u0 = bja - bjb >*/
                    u0 = bja - bjb;
/*<       b(ja+j) = bja + bjb >*/
                    b[ja + j] = bja + bjb;
/*<       b(jb+j) = u0 >*/
                    b[jb + j] = u0;
/*<       j = j + jump >*/
                    j += *jump;
/*<   210 continue >*/
/* L210: */
                }
/* -----(end of loop across transforms) */
/*<       ja = ja + jstepx >*/
                ja += jstepx;
/*<       if (ja.lt.istart) ja = ja + ninc >*/
                if (ja < istart) {
                    ja += ninc;
                }
/*<   215 continue >*/
/* L215: */
            }
/*<   220 continue >*/
/* L220: */
        }

/*  finished if n2=2 */
/*  ---------------- */
/*<       if (n2.eq.2) go to 490 >*/
        if (n2 == 2) {
            goto L490;
        }

/*<       kk = 2 * la >*/
        kk = la << 1;

/*  loop on nonzero k */
/*  ----------------- */
/*<       do 260 k = ink , jstep - ink , ink >*/
        i__3 = jstep - ink;
        i__2 = ink;
        for (k = ink; i__2 < 0 ? k >= i__3 : k <= i__3; k += i__2) {
/*<       co1 = trigs(kk+1) >*/
            co1 = trigs[kk + 1];
/*<       si1 = s*trigs(kk+2) >*/
            si1 = s * trigs[kk + 2];

/*  loop along transforms */
/*  --------------------- */
/*<       do 250 jjj = k , (n-1)*inc , 2*jstep >*/
            i__4 = (*n - 1) * *inc;
            i__6 = jstep << 1;
            for (jjj = k; i__6 < 0 ? jjj >= i__4 : jjj <= i__4; jjj += i__6) {
/*<       ja = istart + jjj >*/
                ja = istart + jjj;

/*     "transverse" loop */
/*     ----------------- */
/*<       do 245 nu = 1 , inq >*/
                i__5 = inq;
                for (nu = 1; nu <= i__5; ++nu) {
/*<       jb = ja + jstepl >*/
                    jb = ja + jstepl;
/*<       if (jb.lt.istart) jb = jb + ninc >*/
                    if (jb < istart) {
                        jb += ninc;
                    }
/*<       j = 0 >*/
                    j = 0;

/*  loop across transforms */
/*  ---------------------- */
/*<       if (kk.eq.n2/2) then >*/
                    if (kk == n2 / 2) {
/* dir$ ivdep, shortloop */
/*<       do 230 l = 1 , nvex >*/
                        i__7 = nvex;
                        for (l = 1; l <= i__7; ++l) {
/*<       aja = a(ja+j) >*/
                            aja = a[ja + j];
/*<       ajb = a(jb+j) >*/
                            ajb = a[jb + j];
/*<       t0 = ss * ( aja - ajb ) >*/
                            t0 = ss * (aja - ajb);
/*<       a(ja+j) = aja + ajb >*/
                            a[ja + j] = aja + ajb;
/*<       bjb = b(jb+j) >*/
                            bjb = b[jb + j];
/*<       bja = b(ja+j) >*/
                            bja = b[ja + j];
/*<       a(jb+j) = ss * ( bjb - bja ) >*/
                            a[jb + j] = ss * (bjb - bja);
/*<       b(ja+j) = bja + bjb >*/
                            b[ja + j] = bja + bjb;
/*<       b(jb+j) = t0 >*/
                            b[jb + j] = t0;
/*<       j = j + jump >*/
                            j += *jump;
/*<   230 continue >*/
/* L230: */
                        }

/*<       else >*/
                    } else {

/* dir$ ivdep, shortloop */
/*<       do 240 l = 1 , nvex >*/
                        i__7 = nvex;
                        for (l = 1; l <= i__7; ++l) {
/*<       aja = a(ja+j) >*/
                            aja = a[ja + j];
/*<       ajb = a(jb+j) >*/
                            ajb = a[jb + j];
/*<       t0 = aja - ajb >*/
                            t0 = aja - ajb;
/*<       a(ja+j) = aja + ajb >*/
                            a[ja + j] = aja + ajb;
/*<       bja = b(ja+j) >*/
                            bja = b[ja + j];
/*<       bjb = b(jb+j) >*/
                            bjb = b[jb + j];
/*<       u0 = bja - bjb >*/
                            u0 = bja - bjb;
/*<       b(ja+j) = bja + bjb >*/
                            b[ja + j] = bja + bjb;
/*<       a(jb+j) = co1*t0 - si1*u0 >*/
                            a[jb + j] = co1 * t0 - si1 * u0;
/*<       b(jb+j) = si1*t0 + co1*u0 >*/
                            b[jb + j] = si1 * t0 + co1 * u0;
/*<       j = j + jump >*/
                            j += *jump;
/*<   240 continue >*/
/* L240: */
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
/*<   245 continue >*/
/* L245: */
                }
/*<   250 continue >*/
/* L250: */
            }
/* -----(end of loop along transforms) */
/*<       kk = kk + 2 * la >*/
            kk += la << 1;
/*<   260 continue >*/
/* L260: */
        }
/* -----(end of loop on nonzero k) */
/* -----(end of radix-2 pass) */

/*<       la = 2 * la >*/
        la <<= 1;
/*<       go to 400 >*/
        goto L400;

/*  central radix-8 pass */
/*  -------------------- */
/*<   300 continue >*/
L300:
/*<       if (m8.eq.0) go to 400 >*/
        if (m8 == 0) {
            goto L400;
        }
/*<       jstep = (n*inc) / (8*la) >*/
        jstep = *n * *inc / (la << 3);
/*<       jstepl = jstep - ninc >*/
        jstepl = jstep - ninc;
/*<       mu = mod(inq,8) >*/
        mu = inq % 8;
/*<       if (isign.eq.-1) mu = 8 - mu >*/
        if (*isign == -1) {
            mu = 8 - mu;
        }
/*<       c1 = 1.0 >*/
        c1 = 1.f;
/*<       if (mu.eq.3.or.mu.eq.7) c1 = -1.0 >*/
        if (mu == 3 || mu == 7) {
            c1 = -1.f;
        }
/*<       c2 = sqrt(0.5) >*/
        c2 = sqrt(.5f);
/*<       if (mu.eq.3.or.mu.eq.5) c2 = -c2 >*/
        if (mu == 3 || mu == 5) {
            c2 = -c2;
        }
/*<       c3 = c1 * c2 >*/
        c3 = c1 * c2;

/*  stage 1 */
/*  ------- */
/*<       do 320 k = 0 , jstep - ink , ink >*/
        i__2 = jstep - ink;
        i__3 = ink;
        for (k = 0; i__3 < 0 ? k >= i__2 : k <= i__2; k += i__3) {
/*<       do 315 jjj = k , (n-1)*inc , 8*jstep >*/
            i__6 = (*n - 1) * *inc;
            i__4 = jstep << 3;
            for (jjj = k; i__4 < 0 ? jjj >= i__6 : jjj <= i__6; jjj += i__4) {
/*<       ja = istart + jjj >*/
                ja = istart + jjj;

/*     "transverse" loop */
/*     ----------------- */
/*<       do 312 nu = 1 , inq >*/
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
/*<       jf = je + jstepl >*/
                    jf = je + jstepl;
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
/*<       j = 0 >*/
                    j = 0;
/* dir$ ivdep, shortloop */
/*<       do 310 l = 1 , nvex >*/
                    i__7 = nvex;
                    for (l = 1; l <= i__7; ++l) {
/*<       aja = a(ja+j) >*/
                        aja = a[ja + j];
/*<       aje = a(je+j) >*/
                        aje = a[je + j];
/*<       t0 = aja - aje >*/
                        t0 = aja - aje;
/*<       a(ja+j) = aja + aje >*/
                        a[ja + j] = aja + aje;
/*<       ajc = a(jc+j) >*/
                        ajc = a[jc + j];
/*<       ajg = a(jg+j) >*/
                        ajg = a[jg + j];
/*<       t1 = c1 * ( ajc - ajg ) >*/
                        t1 = c1 * (ajc - ajg);
/*<       a(je+j) = ajc + ajg >*/
                        a[je + j] = ajc + ajg;
/*<       ajb = a(jb+j) >*/
                        ajb = a[jb + j];
/*<       ajf = a(jf+j) >*/
                        ajf = a[jf + j];
/*<       t2 = ajb - ajf >*/
                        t2 = ajb - ajf;
/*<       a(jc+j) = ajb + ajf >*/
                        a[jc + j] = ajb + ajf;
/*<       ajd = a(jd+j) >*/
                        ajd = a[jd + j];
/*<       ajh = a(jh+j) >*/
                        ajh = a[jh + j];
/*<       t3 = ajd - ajh >*/
                        t3 = ajd - ajh;
/*<       a(jg+j) = ajd + ajh >*/
                        a[jg + j] = ajd + ajh;
/*<       a(jb+j) = t0 >*/
                        a[jb + j] = t0;
/*<       a(jf+j) = t1 >*/
                        a[jf + j] = t1;
/*<       a(jd+j) = c2 * ( t2 - t3 ) >*/
                        a[jd + j] = c2 * (t2 - t3);
/*<       a(jh+j) = c3 * ( t2 + t3 ) >*/
                        a[jh + j] = c3 * (t2 + t3);
/*<       bja = b(ja+j) >*/
                        bja = b[ja + j];
/*<       bje = b(je+j) >*/
                        bje = b[je + j];
/*<       u0 = bja - bje >*/
                        u0 = bja - bje;
/*<       b(ja+j) = bja + bje >*/
                        b[ja + j] = bja + bje;
/*<       bjc = b(jc+j) >*/
                        bjc = b[jc + j];
/*<       bjg = b(jg+j) >*/
                        bjg = b[jg + j];
/*<       u1 = c1 * ( bjc - bjg ) >*/
                        u1 = c1 * (bjc - bjg);
/*<       b(je+j) = bjc + bjg >*/
                        b[je + j] = bjc + bjg;
/*<       bjb = b(jb+j) >*/
                        bjb = b[jb + j];
/*<       bjf = b(jf+j) >*/
                        bjf = b[jf + j];
/*<       u2 = bjb - bjf >*/
                        u2 = bjb - bjf;
/*<       b(jc+j) = bjb + bjf >*/
                        b[jc + j] = bjb + bjf;
/*<       bjd = b(jd+j) >*/
                        bjd = b[jd + j];
/*<       bjh = b(jh+j) >*/
                        bjh = b[jh + j];
/*<       u3 = bjd - bjh >*/
                        u3 = bjd - bjh;
/*<       b(jg+j) = bjd + bjh >*/
                        b[jg + j] = bjd + bjh;
/*<       b(jb+j) = u0 >*/
                        b[jb + j] = u0;
/*<       b(jf+j) = u1 >*/
                        b[jf + j] = u1;
/*<       b(jd+j) = c2 * ( u2 - u3 ) >*/
                        b[jd + j] = c2 * (u2 - u3);
/*<       b(jh+j) = c3 * ( u2 + u3 ) >*/
                        b[jh + j] = c3 * (u2 + u3);
/*<       j = j + jump >*/
                        j += *jump;
/*<   310 continue >*/
/* L310: */
                    }
/*<       ja = ja + jstepx >*/
                    ja += jstepx;
/*<       if (ja.lt.istart) ja = ja + ninc >*/
                    if (ja < istart) {
                        ja += ninc;
                    }
/*<   312 continue >*/
/* L312: */
                }
/*<   315 continue >*/
/* L315: */
            }
/*<   320 continue >*/
/* L320: */
        }

/*  stage 2 */
/*  ------- */

/*  k=0 (no twiddle factors) */
/*  ------------------------ */
/*<       do 330 jjj = 0 , (n-1)*inc , 8*jstep >*/
        i__3 = (*n - 1) * *inc;
        i__2 = jstep << 3;
        for (jjj = 0; i__2 < 0 ? jjj >= i__3 : jjj <= i__3; jjj += i__2) {
/*<       ja = istart + jjj >*/
            ja = istart + jjj;

/*     "transverse" loop */
/*     ----------------- */
/*<       do 328 nu = 1 , inq >*/
            i__4 = inq;
            for (nu = 1; nu <= i__4; ++nu) {
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
/*<       jf = je + jstepl >*/
                jf = je + jstepl;
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
/*<       j = 0 >*/
                j = 0;
/* dir$ ivdep, shortloop */
/*<       do 325 l = 1 , nvex >*/
                i__6 = nvex;
                for (l = 1; l <= i__6; ++l) {
/*<       aja = a(ja+j) >*/
                    aja = a[ja + j];
/*<       aje = a(je+j) >*/
                    aje = a[je + j];
/*<       t0 = aja + aje >*/
                    t0 = aja + aje;
/*<       t2 = aja - aje >*/
                    t2 = aja - aje;
/*<       ajc = a(jc+j) >*/
                    ajc = a[jc + j];
/*<       ajg = a(jg+j) >*/
                    ajg = a[jg + j];
/*<       t1 = ajc + ajg >*/
                    t1 = ajc + ajg;
/*<       t3 = c1 * ( ajc - ajg ) >*/
                    t3 = c1 * (ajc - ajg);
/*<       bja = b(ja+j) >*/
                    bja = b[ja + j];
/*<       bje = b(je+j) >*/
                    bje = b[je + j];
/*<       u0 = bja + bje >*/
                    u0 = bja + bje;
/*<       u2 = bja - bje >*/
                    u2 = bja - bje;
/*<       bjc = b(jc+j) >*/
                    bjc = b[jc + j];
/*<       bjg = b(jg+j) >*/
                    bjg = b[jg + j];
/*<       u1 = bjc + bjg >*/
                    u1 = bjc + bjg;
/*<       u3 = c1 * ( bjc - bjg ) >*/
                    u3 = c1 * (bjc - bjg);
/*<       a(ja+j) = t0 + t1 >*/
                    a[ja + j] = t0 + t1;
/*<       a(je+j) = t0 - t1 >*/
                    a[je + j] = t0 - t1;
/*<       b(ja+j) = u0 + u1 >*/
                    b[ja + j] = u0 + u1;
/*<       b(je+j) = u0 - u1 >*/
                    b[je + j] = u0 - u1;
/*<       a(jc+j) = t2 - u3 >*/
                    a[jc + j] = t2 - u3;
/*<       a(jg+j) = t2 + u3 >*/
                    a[jg + j] = t2 + u3;
/*<       b(jc+j) = u2 + t3 >*/
                    b[jc + j] = u2 + t3;
/*<       b(jg+j) = u2 - t3 >*/
                    b[jg + j] = u2 - t3;
/*<       ajb = a(jb+j) >*/
                    ajb = a[jb + j];
/*<       ajd = a(jd+j) >*/
                    ajd = a[jd + j];
/*<       t0 = ajb + ajd >*/
                    t0 = ajb + ajd;
/*<       t2 = ajb - ajd >*/
                    t2 = ajb - ajd;
/*<       ajf = a(jf+j) >*/
                    ajf = a[jf + j];
/*<       ajh = a(jh+j) >*/
                    ajh = a[jh + j];
/*<       t1 = ajf - ajh >*/
                    t1 = ajf - ajh;
/*<       t3 = ajf + ajh >*/
                    t3 = ajf + ajh;
/*<       bjb = b(jb+j) >*/
                    bjb = b[jb + j];
/*<       bjd = b(jd+j) >*/
                    bjd = b[jd + j];
/*<       u0 = bjb + bjd >*/
                    u0 = bjb + bjd;
/*<       u2 = bjb - bjd >*/
                    u2 = bjb - bjd;
/*<       bjf = b(jf+j) >*/
                    bjf = b[jf + j];
/*<       bjh = b(jh+j) >*/
                    bjh = b[jh + j];
/*<       u1 = bjf - bjh >*/
                    u1 = bjf - bjh;
/*<       u3 = bjf + bjh >*/
                    u3 = bjf + bjh;
/*<       a(jb+j) = t0 - u3 >*/
                    a[jb + j] = t0 - u3;
/*<       a(jh+j) = t0 + u3 >*/
                    a[jh + j] = t0 + u3;
/*<       b(jb+j) = u0 + t3 >*/
                    b[jb + j] = u0 + t3;
/*<       b(jh+j) = u0 - t3 >*/
                    b[jh + j] = u0 - t3;
/*<       a(jd+j) = t2 + u1 >*/
                    a[jd + j] = t2 + u1;
/*<       a(jf+j) = t2 - u1 >*/
                    a[jf + j] = t2 - u1;
/*<       b(jd+j) = u2 - t1 >*/
                    b[jd + j] = u2 - t1;
/*<       b(jf+j) = u2 + t1 >*/
                    b[jf + j] = u2 + t1;
/*<       j = j + jump >*/
                    j += *jump;
/*<   325 continue >*/
/* L325: */
                }
/*<       ja = ja + jstepx >*/
                ja += jstepx;
/*<       if (ja.lt.istart) ja = ja + ninc >*/
                if (ja < istart) {
                    ja += ninc;
                }
/*<   328 continue >*/
/* L328: */
            }
/*<   330 continue >*/
/* L330: */
        }

/*<       if (n2.eq.8) go to 490 >*/
        if (n2 == 8) {
            goto L490;
        }

/*  loop on nonzero k */
/*  ----------------- */
/*<       kk = 2 * la >*/
        kk = la << 1;

/*<       do 350 k = ink , jstep - ink , ink >*/
        i__2 = jstep - ink;
        i__3 = ink;
        for (k = ink; i__3 < 0 ? k >= i__2 : k <= i__2; k += i__3) {

/*<       co1 = trigs(kk+1) >*/
            co1 = trigs[kk + 1];
/*<       si1 = s * trigs(kk+2) >*/
            si1 = s * trigs[kk + 2];
/*<       co2 = trigs(2*kk+1) >*/
            co2 = trigs[(kk << 1) + 1];
/*<       si2 = s * trigs(2*kk+2) >*/
            si2 = s * trigs[(kk << 1) + 2];
/*<       co3 = trigs(3*kk+1) >*/
            co3 = trigs[kk * 3 + 1];
/*<       si3 = s * trigs(3*kk+2) >*/
            si3 = s * trigs[kk * 3 + 2];
/*<       co4 = trigs(4*kk+1) >*/
            co4 = trigs[(kk << 2) + 1];
/*<       si4 = s * trigs(4*kk+2) >*/
            si4 = s * trigs[(kk << 2) + 2];
/*<       co5 = trigs(5*kk+1) >*/
            co5 = trigs[kk * 5 + 1];
/*<       si5 = s * trigs(5*kk+2) >*/
            si5 = s * trigs[kk * 5 + 2];
/*<       co6 = trigs(6*kk+1) >*/
            co6 = trigs[kk * 6 + 1];
/*<       si6 = s * trigs(6*kk+2) >*/
            si6 = s * trigs[kk * 6 + 2];
/*<       co7 = trigs(7*kk+1) >*/
            co7 = trigs[kk * 7 + 1];
/*<       si7 = s * trigs(7*kk+2) >*/
            si7 = s * trigs[kk * 7 + 2];

/*<       do 345 jjj = k , (n-1)*inc , 8*jstep >*/
            i__4 = (*n - 1) * *inc;
            i__6 = jstep << 3;
            for (jjj = k; i__6 < 0 ? jjj >= i__4 : jjj <= i__4; jjj += i__6) {
/*<       ja = istart + jjj >*/
                ja = istart + jjj;

/*     "transverse" loop */
/*     ----------------- */
/*<       do 342 nu = 1 , inq >*/
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
/*<       jf = je + jstepl >*/
                    jf = je + jstepl;
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
/*<       j = 0 >*/
                    j = 0;
/* dir$ ivdep, shortloop */
/*<       do 340 l = 1 , nvex >*/
                    i__7 = nvex;
                    for (l = 1; l <= i__7; ++l) {
/*<       aja = a(ja+j) >*/
                        aja = a[ja + j];
/*<       aje = a(je+j) >*/
                        aje = a[je + j];
/*<       t0 = aja + aje >*/
                        t0 = aja + aje;
/*<       t2 = aja - aje >*/
                        t2 = aja - aje;
/*<       ajc = a(jc+j) >*/
                        ajc = a[jc + j];
/*<       ajg = a(jg+j) >*/
                        ajg = a[jg + j];
/*<       t1 = ajc + ajg >*/
                        t1 = ajc + ajg;
/*<       t3 = c1 * ( ajc - ajg ) >*/
                        t3 = c1 * (ajc - ajg);
/*<       bja = b(ja+j) >*/
                        bja = b[ja + j];
/*<       bje = b(je+j) >*/
                        bje = b[je + j];
/*<       u0 = bja + bje >*/
                        u0 = bja + bje;
/*<       u2 = bja - bje >*/
                        u2 = bja - bje;
/*<       bjc = b(jc+j) >*/
                        bjc = b[jc + j];
/*<       bjg = b(jg+j) >*/
                        bjg = b[jg + j];
/*<       u1 = bjc + bjg >*/
                        u1 = bjc + bjg;
/*<       u3 = c1 * ( bjc - bjg ) >*/
                        u3 = c1 * (bjc - bjg);
/*<       a(ja+j) = t0 + t1 >*/
                        a[ja + j] = t0 + t1;
/*<       b(ja+j) = u0 + u1 >*/
                        b[ja + j] = u0 + u1;
/*<       a(je+j) = co4*(t0-t1) - si4*(u0-u1) >*/
                        a[je + j] = co4 * (t0 - t1) - si4 * (u0 - u1);
/*<       b(je+j) = si4*(t0-t1) + co4*(u0-u1) >*/
                        b[je + j] = si4 * (t0 - t1) + co4 * (u0 - u1);
/*<       a(jc+j) = co2*(t2-u3) - si2*(u2+t3) >*/
                        a[jc + j] = co2 * (t2 - u3) - si2 * (u2 + t3);
/*<       b(jc+j) = si2*(t2-u3) + co2*(u2+t3) >*/
                        b[jc + j] = si2 * (t2 - u3) + co2 * (u2 + t3);
/*<       a(jg+j) = co6*(t2+u3) - si6*(u2-t3) >*/
                        a[jg + j] = co6 * (t2 + u3) - si6 * (u2 - t3);
/*<       b(jg+j) = si6*(t2+u3) + co6*(u2-t3) >*/
                        b[jg + j] = si6 * (t2 + u3) + co6 * (u2 - t3);
/*<       ajb = a(jb+j) >*/
                        ajb = a[jb + j];
/*<       ajd = a(jd+j) >*/
                        ajd = a[jd + j];
/*<       t0 = ajb + ajd >*/
                        t0 = ajb + ajd;
/*<       t2 = ajb - ajd >*/
                        t2 = ajb - ajd;
/*<       ajf = a(jf+j) >*/
                        ajf = a[jf + j];
/*<       ajh = a(jh+j) >*/
                        ajh = a[jh + j];
/*<       t1 = ajf - ajh >*/
                        t1 = ajf - ajh;
/*<       t3 = ajf + ajh >*/
                        t3 = ajf + ajh;
/*<       bjb = b(jb+j) >*/
                        bjb = b[jb + j];
/*<       bjd = b(jd+j) >*/
                        bjd = b[jd + j];
/*<       u0 = bjb + bjd >*/
                        u0 = bjb + bjd;
/*<       u2 = bjb - bjd >*/
                        u2 = bjb - bjd;
/*<       bjf = b(jf+j) >*/
                        bjf = b[jf + j];
/*<       bjh = b(jh+j) >*/
                        bjh = b[jh + j];
/*<       u1 = bjf - bjh >*/
                        u1 = bjf - bjh;
/*<       u3 = bjf + bjh >*/
                        u3 = bjf + bjh;
/*<       a(jb+j) = co1*(t0-u3) - si1*(u0+t3) >*/
                        a[jb + j] = co1 * (t0 - u3) - si1 * (u0 + t3);
/*<       b(jb+j) = si1*(t0-u3) + co1*(u0+t3) >*/
                        b[jb + j] = si1 * (t0 - u3) + co1 * (u0 + t3);
/*<       a(jh+j) = co7*(t0+u3) - si7*(u0-t3) >*/
                        a[jh + j] = co7 * (t0 + u3) - si7 * (u0 - t3);
/*<       b(jh+j) = si7*(t0+u3) + co7*(u0-t3) >*/
                        b[jh + j] = si7 * (t0 + u3) + co7 * (u0 - t3);
/*<       a(jd+j) = co3*(t2+u1) - si3*(u2-t1) >*/
                        a[jd + j] = co3 * (t2 + u1) - si3 * (u2 - t1);
/*<       b(jd+j) = si3*(t2+u1) + co3*(u2-t1) >*/
                        b[jd + j] = si3 * (t2 + u1) + co3 * (u2 - t1);
/*<       a(jf+j) = co5*(t2-u1) - si5*(u2+t1) >*/
                        a[jf + j] = co5 * (t2 - u1) - si5 * (u2 + t1);
/*<       b(jf+j) = si5*(t2-u1) + co5*(u2+t1) >*/
                        b[jf + j] = si5 * (t2 - u1) + co5 * (u2 + t1);
/*<       j = j + jump >*/
                        j += *jump;
/*<   340 continue >*/
/* L340: */
                    }
/*<       ja = ja + jstepx >*/
                    ja += jstepx;
/*<       if (ja.lt.istart) ja = ja + ninc >*/
                    if (ja < istart) {
                        ja += ninc;
                    }
/*<   342 continue >*/
/* L342: */
                }
/*<   345 continue >*/
/* L345: */
            }
/*<       kk = kk + 2 * la >*/
            kk += la << 1;
/*<   350 continue >*/
/* L350: */
        }

/*<       la = 8 * la >*/
        la <<= 3;

/*  loop on type II radix-4 passes */
/*  ------------------------------ */
/*<   400 continue >*/
L400:
/*<       mu = mod(inq,4) >*/
        mu = inq % 4;
/*<       if (isign.eq.-1) mu = 4 - mu >*/
        if (*isign == -1) {
            mu = 4 - mu;
        }
/*<       ss = 1.0 >*/
        ss = 1.f;
/*<       if (mu.eq.3) ss = -1.0 >*/
        if (mu == 3) {
            ss = -1.f;
        }

/*<       do 480 ipass = mh+1 , m >*/
        i__3 = m;
        for (ipass = mh + 1; ipass <= i__3; ++ipass) {
/*<       jstep = (n*inc) / (4*la) >*/
            jstep = *n * *inc / (la << 2);
/*<       jstepl = jstep - ninc >*/
            jstepl = jstep - ninc;
/*<       laincl = la * ink - ninc >*/
            laincl = la * ink - ninc;

/*  k=0 loop (no twiddle factors) */
/*  ----------------------------- */
/*<       do 430 ll = 0 , (la-1)*ink , 4*jstep >*/
            i__2 = (la - 1) * ink;
            i__6 = jstep << 2;
            for (ll = 0; i__6 < 0 ? ll >= i__2 : ll <= i__2; ll += i__6) {

/*<       do 420 jjj = ll , (n-1)*inc , 4*la*ink >*/
                i__4 = (*n - 1) * *inc;
                i__5 = (la << 2) * ink;
                for (jjj = ll; i__5 < 0 ? jjj >= i__4 : jjj <= i__4; jjj +=
                        i__5) {
/*<       ja = istart + jjj >*/
                    ja = istart + jjj;

/*     "transverse" loop */
/*     ----------------- */
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
/*<       jd = jc + jstepl >*/
                        jd = jc + jstepl;
/*<       if (jd.lt.istart) jd = jd + ninc >*/
                        if (jd < istart) {
                            jd += ninc;
                        }
/*<       je = ja + laincl >*/
                        je = ja + laincl;
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
/*<       ji = je + laincl >*/
                        ji = je + laincl;
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
/*<       jk = jj + jstepl >*/
                        jk = jj + jstepl;
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
/*<       jm = ji + laincl >*/
                        jm = ji + laincl;
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
/*<       jp = jo + jstepl >*/
                        jp = jo + jstepl;
/*<       if (jp.lt.istart) jp = jp + ninc >*/
                        if (jp < istart) {
                            jp += ninc;
                        }
/*<       j = 0 >*/
                        j = 0;

/*  loop across transforms */
/*  ---------------------- */
/* dir$ ivdep, shortloop */
/*<       do 410 l = 1 , nvex >*/
                        i__8 = nvex;
                        for (l = 1; l <= i__8; ++l) {
/*<       aja = a(ja+j) >*/
                            aja = a[ja + j];
/*<       ajc = a(jc+j) >*/
                            ajc = a[jc + j];
/*<       t0 = aja + ajc >*/
                            t0 = aja + ajc;
/*<       t2 = aja - ajc >*/
                            t2 = aja - ajc;
/*<       ajb = a(jb+j) >*/
                            ajb = a[jb + j];
/*<       ajd = a(jd+j) >*/
                            ajd = a[jd + j];
/*<       t1 = ajb + ajd >*/
                            t1 = ajb + ajd;
/*<       t3 = ss * ( ajb - ajd ) >*/
                            t3 = ss * (ajb - ajd);
/*<       aji = a(ji+j) >*/
                            aji = a[ji + j];
/*<       ajc =  aji >*/
                            ajc = aji;
/*<       bja = b(ja+j) >*/
                            bja = b[ja + j];
/*<       bjc = b(jc+j) >*/
                            bjc = b[jc + j];
/*<       u0 = bja + bjc >*/
                            u0 = bja + bjc;
/*<       u2 = bja - bjc >*/
                            u2 = bja - bjc;
/*<       bjb = b(jb+j) >*/
                            bjb = b[jb + j];
/*<       bjd = b(jd+j) >*/
                            bjd = b[jd + j];
/*<       u1 = bjb + bjd >*/
                            u1 = bjb + bjd;
/*<       u3 = ss * ( bjb - bjd ) >*/
                            u3 = ss * (bjb - bjd);
/*<       aje = a(je+j) >*/
                            aje = a[je + j];
/*<       ajb =  aje >*/
                            ajb = aje;
/*<       a(ja+j) = t0 + t1 >*/
                            a[ja + j] = t0 + t1;
/*<       a(ji+j) = t0 - t1 >*/
                            a[ji + j] = t0 - t1;
/*<       b(ja+j) = u0 + u1 >*/
                            b[ja + j] = u0 + u1;
/*<       bjc =  u0 - u1 >*/
                            bjc = u0 - u1;
/*<       bjm = b(jm+j) >*/
                            bjm = b[jm + j];
/*<       bjd =  bjm >*/
                            bjd = bjm;
/*<       a(je+j) = t2 - u3 >*/
                            a[je + j] = t2 - u3;
/*<       ajd =  t2 + u3 >*/
                            ajd = t2 + u3;
/*<       bjb =  u2 + t3 >*/
                            bjb = u2 + t3;
/*<       b(jm+j) = u2 - t3 >*/
                            b[jm + j] = u2 - t3;
/* ---------------------- */
/*<       ajg = a(jg+j) >*/
                            ajg = a[jg + j];
/*<       t0 = ajb + ajg >*/
                            t0 = ajb + ajg;
/*<       t2 = ajb - ajg >*/
                            t2 = ajb - ajg;
/*<       ajf = a(jf+j) >*/
                            ajf = a[jf + j];
/*<       ajh = a(jh+j) >*/
                            ajh = a[jh + j];
/*<       t1 = ajf + ajh >*/
                            t1 = ajf + ajh;
/*<       t3 = ss * ( ajf - ajh ) >*/
                            t3 = ss * (ajf - ajh);
/*<       ajj = a(jj+j) >*/
                            ajj = a[jj + j];
/*<       ajg =  ajj >*/
                            ajg = ajj;
/*<       bje = b(je+j) >*/
                            bje = b[je + j];
/*<       bjg = b(jg+j) >*/
                            bjg = b[jg + j];
/*<       u0 = bje + bjg >*/
                            u0 = bje + bjg;
/*<       u2 = bje - bjg >*/
                            u2 = bje - bjg;
/*<       bjf = b(jf+j) >*/
                            bjf = b[jf + j];
/*<       bjh = b(jh+j) >*/
                            bjh = b[jh + j];
/*<       u1 = bjf + bjh >*/
                            u1 = bjf + bjh;
/*<       u3 = ss * ( bjf - bjh ) >*/
                            u3 = ss * (bjf - bjh);
/*<       b(je+j) = bjb >*/
                            b[je + j] = bjb;
/*<       a(jb+j) = t0 + t1 >*/
                            a[jb + j] = t0 + t1;
/*<       a(jj+j) = t0 - t1 >*/
                            a[jj + j] = t0 - t1;
/*<       bjj = b(jj+j) >*/
                            bjj = b[jj + j];
/*<       bjg =  bjj >*/
                            bjg = bjj;
/*<       b(jb+j) = u0 + u1 >*/
                            b[jb + j] = u0 + u1;
/*<       b(jj+j) = u0 - u1 >*/
                            b[jj + j] = u0 - u1;
/*<       a(jf+j) = t2 - u3 >*/
                            a[jf + j] = t2 - u3;
/*<       ajh =  t2 + u3 >*/
                            ajh = t2 + u3;
/*<       b(jf+j) = u2 + t3 >*/
                            b[jf + j] = u2 + t3;
/*<       bjh =  u2 - t3 >*/
                            bjh = u2 - t3;
/* ---------------------- */
/*<       ajk = a(jk+j) >*/
                            ajk = a[jk + j];
/*<       t0 = ajc + ajk >*/
                            t0 = ajc + ajk;
/*<       t2 = ajc - ajk >*/
                            t2 = ajc - ajk;
/*<       ajl = a(jl+j) >*/
                            ajl = a[jl + j];
/*<       t1 = ajg + ajl >*/
                            t1 = ajg + ajl;
/*<       t3 = ss * ( ajg - ajl ) >*/
                            t3 = ss * (ajg - ajl);
/*<       bji = b(ji+j) >*/
                            bji = b[ji + j];
/*<       bjk = b(jk+j) >*/
                            bjk = b[jk + j];
/*<       u0 = bji + bjk >*/
                            u0 = bji + bjk;
/*<       u2 = bji - bjk >*/
                            u2 = bji - bjk;
/*<       ajo = a(jo+j) >*/
                            ajo = a[jo + j];
/*<       ajl =  ajo >*/
                            ajl = ajo;
/*<       bjl = b(jl+j) >*/
                            bjl = b[jl + j];
/*<       u1 = bjg + bjl >*/
                            u1 = bjg + bjl;
/*<       u3 = ss * ( bjg - bjl ) >*/
                            u3 = ss * (bjg - bjl);
/*<       b(ji+j) = bjc >*/
                            b[ji + j] = bjc;
/*<       a(jc+j) = t0 + t1 >*/
                            a[jc + j] = t0 + t1;
/*<       a(jk+j) = t0 - t1 >*/
                            a[jk + j] = t0 - t1;
/*<       bjo = b(jo+j) >*/
                            bjo = b[jo + j];
/*<       bjl =  bjo >*/
                            bjl = bjo;
/*<       b(jc+j) = u0 + u1 >*/
                            b[jc + j] = u0 + u1;
/*<       b(jk+j) = u0 - u1 >*/
                            b[jk + j] = u0 - u1;
/*<       a(jg+j) = t2 - u3 >*/
                            a[jg + j] = t2 - u3;
/*<       a(jo+j) = t2 + u3 >*/
                            a[jo + j] = t2 + u3;
/*<       b(jg+j) = u2 + t3 >*/
                            b[jg + j] = u2 + t3;
/*<       b(jo+j) = u2 - t3 >*/
                            b[jo + j] = u2 - t3;
/* ---------------------- */
/*<       ajm = a(jm+j) >*/
                            ajm = a[jm + j];
/*<       t0 = ajm + ajl >*/
                            t0 = ajm + ajl;
/*<       t2 = ajm - ajl >*/
                            t2 = ajm - ajl;
/*<       ajn = a(jn+j) >*/
                            ajn = a[jn + j];
/*<       ajp = a(jp+j) >*/
                            ajp = a[jp + j];
/*<       t1 = ajn + ajp >*/
                            t1 = ajn + ajp;
/*<       t3 = ss * ( ajn - ajp ) >*/
                            t3 = ss * (ajn - ajp);
/*<       a(jm+j) = ajd >*/
                            a[jm + j] = ajd;
/*<       u0 = bjd + bjl >*/
                            u0 = bjd + bjl;
/*<       u2 = bjd - bjl >*/
                            u2 = bjd - bjl;
/*<       bjn = b(jn+j) >*/
                            bjn = b[jn + j];
/*<       bjp = b(jp+j) >*/
                            bjp = b[jp + j];
/*<       u1 = bjn + bjp >*/
                            u1 = bjn + bjp;
/*<       u3 = ss * ( bjn - bjp ) >*/
                            u3 = ss * (bjn - bjp);
/*<       a(jn+j) = ajh >*/
                            a[jn + j] = ajh;
/*<       a(jd+j) = t0 + t1 >*/
                            a[jd + j] = t0 + t1;
/*<       a(jl+j) = t0 - t1 >*/
                            a[jl + j] = t0 - t1;
/*<       b(jd+j) = u0 + u1 >*/
                            b[jd + j] = u0 + u1;
/*<       b(jl+j) = u0 - u1 >*/
                            b[jl + j] = u0 - u1;
/*<       b(jn+j) = bjh >*/
                            b[jn + j] = bjh;
/*<       a(jh+j) = t2 - u3 >*/
                            a[jh + j] = t2 - u3;
/*<       a(jp+j) = t2 + u3 >*/
                            a[jp + j] = t2 + u3;
/*<       b(jh+j) = u2 + t3 >*/
                            b[jh + j] = u2 + t3;
/*<       b(jp+j) = u2 - t3 >*/
                            b[jp + j] = u2 - t3;
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
            i__6 = jstep - ink;
            i__2 = ink;
            for (k = ink; i__2 < 0 ? k >= i__6 : k <= i__6; k += i__2) {
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

/*  double loop along first transform in block */
/*  ------------------------------------------ */
/*<       do 460 ll = k , (la-1)*ink , 4*jstep >*/
                i__5 = (la - 1) * ink;
                i__4 = jstep << 2;
                for (ll = k; i__4 < 0 ? ll >= i__5 : ll <= i__5; ll += i__4) {

/*<       do 450 jjj = ll , (n-1)*inc , 4*la*ink >*/
                    i__7 = (*n - 1) * *inc;
                    i__8 = (la << 2) * ink;
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
/*<       je = ja + laincl >*/
                            je = ja + laincl;
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
/*<       ji = je + laincl >*/
                            ji = je + laincl;
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
/*<       jk = jj + jstepl >*/
                            jk = jj + jstepl;
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
/*<       jm = ji + laincl >*/
                            jm = ji + laincl;
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
/*<       jp = jo + jstepl >*/
                            jp = jo + jstepl;
/*<       if (jp.lt.istart) jp = jp + ninc >*/
                            if (jp < istart) {
                                jp += ninc;
                            }
/*<       j = 0 >*/
                            j = 0;

/*  loop across transforms */
/*  ---------------------- */
/* dir$ ivdep, shortloop */
/*<       do 440 l = 1 , nvex >*/
                            i__10 = nvex;
                            for (l = 1; l <= i__10; ++l) {
/*<       aja = a(ja+j) >*/
                                aja = a[ja + j];
/*<       ajc = a(jc+j) >*/
                                ajc = a[jc + j];
/*<       t0 = aja + ajc >*/
                                t0 = aja + ajc;
/*<       t2 = aja - ajc >*/
                                t2 = aja - ajc;
/*<       ajb = a(jb+j) >*/
                                ajb = a[jb + j];
/*<       ajd = a(jd+j) >*/
                                ajd = a[jd + j];
/*<       t1 = ajb + ajd >*/
                                t1 = ajb + ajd;
/*<       t3 = ss * ( ajb - ajd ) >*/
                                t3 = ss * (ajb - ajd);
/*<       aji = a(ji+j) >*/
                                aji = a[ji + j];
/*<       ajc =  aji >*/
                                ajc = aji;
/*<       bja = b(ja+j) >*/
                                bja = b[ja + j];
/*<       bjc = b(jc+j) >*/
                                bjc = b[jc + j];
/*<       u0 = bja + bjc >*/
                                u0 = bja + bjc;
/*<       u2 = bja - bjc >*/
                                u2 = bja - bjc;
/*<       bjb = b(jb+j) >*/
                                bjb = b[jb + j];
/*<       bjd = b(jd+j) >*/
                                bjd = b[jd + j];
/*<       u1 = bjb + bjd >*/
                                u1 = bjb + bjd;
/*<       u3 = ss * ( bjb - bjd ) >*/
                                u3 = ss * (bjb - bjd);
/*<       aje = a(je+j) >*/
                                aje = a[je + j];
/*<       ajb =  aje >*/
                                ajb = aje;
/*<       a(ja+j) = t0 + t1 >*/
                                a[ja + j] = t0 + t1;
/*<       b(ja+j) = u0 + u1 >*/
                                b[ja + j] = u0 + u1;
/*<       a(je+j) = co1*(t2-u3) - si1*(u2+t3) >*/
                                a[je + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
/*<       bjb =  si1*(t2-u3) + co1*(u2+t3) >*/
                                bjb = si1 * (t2 - u3) + co1 * (u2 + t3);
/*<       bjm = b(jm+j) >*/
                                bjm = b[jm + j];
/*<       bjd =  bjm >*/
                                bjd = bjm;
/*<       a(ji+j) = co2*(t0-t1) - si2*(u0-u1) >*/
                                a[ji + j] = co2 * (t0 - t1) - si2 * (u0 - u1);
/*<       bjc =  si2*(t0-t1) + co2*(u0-u1) >*/
                                bjc = si2 * (t0 - t1) + co2 * (u0 - u1);
/*<       ajd =  co3*(t2+u3) - si3*(u2-t3) >*/
                                ajd = co3 * (t2 + u3) - si3 * (u2 - t3);
/*<       b(jm+j) = si3*(t2+u3) + co3*(u2-t3) >*/
                                b[jm + j] = si3 * (t2 + u3) + co3 * (u2 - t3);
/* ---------------------------------------- */
/*<       ajg = a(jg+j) >*/
                                ajg = a[jg + j];
/*<       t0 = ajb + ajg >*/
                                t0 = ajb + ajg;
/*<       t2 = ajb - ajg >*/
                                t2 = ajb - ajg;
/*<       ajf = a(jf+j) >*/
                                ajf = a[jf + j];
/*<       ajh = a(jh+j) >*/
                                ajh = a[jh + j];
/*<       t1 = ajf + ajh >*/
                                t1 = ajf + ajh;
/*<       t3 = ss * ( ajf - ajh ) >*/
                                t3 = ss * (ajf - ajh);
/*<       ajj = a(jj+j) >*/
                                ajj = a[jj + j];
/*<       ajg =  ajj >*/
                                ajg = ajj;
/*<       bje = b(je+j) >*/
                                bje = b[je + j];
/*<       bjg = b(jg+j) >*/
                                bjg = b[jg + j];
/*<       u0 = bje + bjg >*/
                                u0 = bje + bjg;
/*<       u2 = bje - bjg >*/
                                u2 = bje - bjg;
/*<       bjf = b(jf+j) >*/
                                bjf = b[jf + j];
/*<       bjh = b(jh+j) >*/
                                bjh = b[jh + j];
/*<       u1 = bjf + bjh >*/
                                u1 = bjf + bjh;
/*<       u3 = ss * ( bjf - bjh ) >*/
                                u3 = ss * (bjf - bjh);
/*<       b(je+j) = bjb >*/
                                b[je + j] = bjb;
/*<       a(jb+j) = t0 + t1 >*/
                                a[jb + j] = t0 + t1;
/*<       b(jb+j) = u0 + u1 >*/
                                b[jb + j] = u0 + u1;
/*<       bjj = b(jj+j) >*/
                                bjj = b[jj + j];
/*<       bjg =  bjj >*/
                                bjg = bjj;
/*<       a(jf+j) = co1*(t2-u3) - si1*(u2+t3) >*/
                                a[jf + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
/*<       b(jf+j) = si1*(t2-u3) + co1*(u2+t3) >*/
                                b[jf + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
/*<       a(jj+j) = co2*(t0-t1) - si2*(u0-u1) >*/
                                a[jj + j] = co2 * (t0 - t1) - si2 * (u0 - u1);
/*<       b(jj+j) = si2*(t0-t1) + co2*(u0-u1) >*/
                                b[jj + j] = si2 * (t0 - t1) + co2 * (u0 - u1);
/*<       ajh =  co3*(t2+u3) - si3*(u2-t3) >*/
                                ajh = co3 * (t2 + u3) - si3 * (u2 - t3);
/*<       bjh =  si3*(t2+u3) + co3*(u2-t3) >*/
                                bjh = si3 * (t2 + u3) + co3 * (u2 - t3);
/* ---------------------------------------- */
/*<       ajk = a(jk+j) >*/
                                ajk = a[jk + j];
/*<       t0 = ajc + ajk >*/
                                t0 = ajc + ajk;
/*<       t2 = ajc - ajk >*/
                                t2 = ajc - ajk;
/*<       ajl = a(jl+j) >*/
                                ajl = a[jl + j];
/*<       t1 = ajg + ajl >*/
                                t1 = ajg + ajl;
/*<       t3 = ss * ( ajg - ajl ) >*/
                                t3 = ss * (ajg - ajl);
/*<       bji = b(ji+j) >*/
                                bji = b[ji + j];
/*<       bjk = b(jk+j) >*/
                                bjk = b[jk + j];
/*<       u0 = bji + bjk >*/
                                u0 = bji + bjk;
/*<       u2 = bji - bjk >*/
                                u2 = bji - bjk;
/*<       ajo = a(jo+j) >*/
                                ajo = a[jo + j];
/*<       ajl =  ajo >*/
                                ajl = ajo;
/*<       bjl = b(jl+j) >*/
                                bjl = b[jl + j];
/*<       u1 = bjg + bjl >*/
                                u1 = bjg + bjl;
/*<       u3 = ss * ( bjg - bjl ) >*/
                                u3 = ss * (bjg - bjl);
/*<       b(ji+j) = bjc >*/
                                b[ji + j] = bjc;
/*<       a(jc+j) = t0 + t1 >*/
                                a[jc + j] = t0 + t1;
/*<       b(jc+j) = u0 + u1 >*/
                                b[jc + j] = u0 + u1;
/*<       bjo = b(jo+j) >*/
                                bjo = b[jo + j];
/*<       bjl =  bjo >*/
                                bjl = bjo;
/*<       a(jg+j) = co1*(t2-u3) - si1*(u2+t3) >*/
                                a[jg + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
/*<       b(jg+j) = si1*(t2-u3) + co1*(u2+t3) >*/
                                b[jg + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
/*<       a(jk+j) = co2*(t0-t1) - si2*(u0-u1) >*/
                                a[jk + j] = co2 * (t0 - t1) - si2 * (u0 - u1);
/*<       b(jk+j) = si2*(t0-t1) + co2*(u0-u1) >*/
                                b[jk + j] = si2 * (t0 - t1) + co2 * (u0 - u1);
/*<       a(jo+j) = co3*(t2+u3) - si3*(u2-t3) >*/
                                a[jo + j] = co3 * (t2 + u3) - si3 * (u2 - t3);
/*<       b(jo+j) = si3*(t2+u3) + co3*(u2-t3) >*/
                                b[jo + j] = si3 * (t2 + u3) + co3 * (u2 - t3);
/* ---------------------------------------- */
/*<       ajm = a(jm+j) >*/
                                ajm = a[jm + j];
/*<       t0 = ajm + ajl >*/
                                t0 = ajm + ajl;
/*<       t2 = ajm - ajl >*/
                                t2 = ajm - ajl;
/*<       ajn = a(jn+j) >*/
                                ajn = a[jn + j];
/*<       ajp = a(jp+j) >*/
                                ajp = a[jp + j];
/*<       t1 = ajn + ajp >*/
                                t1 = ajn + ajp;
/*<       t3 = ss * ( ajn - ajp ) >*/
                                t3 = ss * (ajn - ajp);
/*<       a(jm+j) = ajd >*/
                                a[jm + j] = ajd;
/*<       u0 = bjd + bjl >*/
                                u0 = bjd + bjl;
/*<       u2 = bjd - bjl >*/
                                u2 = bjd - bjl;
/*<       a(jn+j) = ajh >*/
                                a[jn + j] = ajh;
/*<       bjn = b(jn+j) >*/
                                bjn = b[jn + j];
/*<       bjp = b(jp+j) >*/
                                bjp = b[jp + j];
/*<       u1 = bjn + bjp >*/
                                u1 = bjn + bjp;
/*<       u3 = ss * ( bjn - bjp ) >*/
                                u3 = ss * (bjn - bjp);
/*<       b(jn+j) = bjh >*/
                                b[jn + j] = bjh;
/*<       a(jd+j) = t0 + t1 >*/
                                a[jd + j] = t0 + t1;
/*<       b(jd+j) = u0 + u1 >*/
                                b[jd + j] = u0 + u1;
/*<       a(jh+j) = co1*(t2-u3) - si1*(u2+t3) >*/
                                a[jh + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
/*<       b(jh+j) = si1*(t2-u3) + co1*(u2+t3) >*/
                                b[jh + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
/*<       a(jl+j) = co2*(t0-t1) - si2*(u0-u1) >*/
                                a[jl + j] = co2 * (t0 - t1) - si2 * (u0 - u1);
/*<       b(jl+j) = si2*(t0-t1) + co2*(u0-u1) >*/
                                b[jl + j] = si2 * (t0 - t1) + co2 * (u0 - u1);
/*<       a(jp+j) = co3*(t2+u3) - si3*(u2-t3) >*/
                                a[jp + j] = co3 * (t2 + u3) - si3 * (u2 - t3);
/*<       b(jp+j) = si3*(t2+u3) + co3*(u2-t3) >*/
                                b[jp + j] = si3 * (t2 + u3) + co3 * (u2 - t3);
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
/*<       la = 4*la >*/
            la <<= 2;
/*<   480 continue >*/
/* L480: */
        }
/* -----( end of loop on type II radix-4 passes ) */
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
} /* gpfa2f_ */

#ifdef __cplusplus
        }
#endif

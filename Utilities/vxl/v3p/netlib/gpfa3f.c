#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__3 = 3;

/*     fortran version of *gpfa3* - */
/*     radix-3 section of self-sorting, in-place */
/*        generalized PFA */

/* ------------------------------------------------------------------- */

/* Subroutine */ void gpfa3f_(real *a, real *b, const real *trigs,
        const integer *inc, const integer *jump, const integer *n, const integer *mm,
        const integer *lot, const integer *isign)
{
    /* Initialized data */
    static real sin60 = .866025403784437f;
    static integer lvr = 128;

    /* System generated locals */
    integer i__3, i__4, i__5, i__6, i__7, i__8;

    /* Local variables */
    static integer ninc, left, nvex, j, k, l, m;
    static real s;
    static integer ipass, nblox;
    static real c1;
    static integer jstep, n3;
    static real t1, t2, t3, u1, u2, u3;
    static integer ja, jb, jc, jd, je, jf, jg, jh, ji, la, nb, mh, kk, ll, mu, nu, laincl, jstepl;
    static real co1, co2;
    static integer istart, jstepx;
    static real si1, si2, aja, ajb, ajc, bjb, bjc, bja, ajd, bjd, aje, ajf, ajh, bje, bjf, bjh, aji, ajg, bji;
    static integer jjj;
    static real bjg;
    static integer ink, inq;

/*     *************************************************************** */
/*     *                                                             * */
/*     *  N.B. LVR = LENGTH OF VECTOR REGISTERS, SET TO 128 FOR C90. * */
/*     *  RESET TO 64 FOR OTHER CRAY MACHINES, OR TO ANY LARGE VALUE * */
/*     *  (GREATER THAN OR EQUAL TO LOT) FOR A SCALAR COMPUTER.      * */
/*     *                                                             * */
/*     *************************************************************** */

    n3 = pow_ii(&c__3, mm);
    inq = *n / n3;
    jstepx = (n3 - *n) * *inc;
    ninc = *n * *inc;
    ink = *inc * inq;
    mu = inq % 3;
    if (*isign == -1) {
        mu = 3 - mu;
    }
    m = *mm;
    mh = (m + 1) / 2;
    s = (real) (*isign);
    c1 = sin60;
    if (mu == 2) {
        c1 = -c1;
    }

    nblox = (*lot - 1) / lvr + 1;
    left = *lot;
    s = (real) (*isign);
    istart = 0;

/*  loop on blocks of lvr transforms */
/*  -------------------------------- */
    for (nb = 1; nb <= nblox; ++nb) {

        if (left <= lvr) {
            nvex = left;
        } else if (left < lvr << 1) {
            nvex = left / 2;
            nvex += nvex % 2;
        } else {
            nvex = lvr;
        }
        left -= nvex;

        la = 1;

/*  loop on type I radix-3 passes */
/*  ----------------------------- */
        for (ipass = 0; ipass < mh; ++ipass) {
            jstep = *n * *inc / (la * 3);
            jstepl = jstep - ninc;

/*  k = 0 loop (no twiddle factors) */
/*  ------------------------------- */
            i__3 = (*n - 1) * *inc;
            i__4 = jstep * 3;
            for (jjj = 0; i__4 < 0 ? jjj >= i__3 : jjj <= i__3; jjj += i__4) {
                ja = istart + jjj;

/*  "transverse" loop */
/*  ----------------- */
                for (nu = 1; nu <= inq; ++nu) {
                    jb = ja + jstepl;
                    if (jb < istart) {
                        jb += ninc;
                    }
                    jc = jb + jstepl;
                    if (jc < istart) {
                        jc += ninc;
                    }
                    j = 0;

/*  loop across transforms */
/*  ---------------------- */
/* dir$ ivdep, shortloop */
                    for (l = 1; l <= nvex; ++l) {
                        ajb = a[jb + j];
                        ajc = a[jc + j];
                        t1 = ajb + ajc;
                        aja = a[ja + j];
                        t2 = aja - t1 * .5f;
                        t3 = c1 * (ajb - ajc);
                        bjb = b[jb + j];
                        bjc = b[jc + j];
                        u1 = bjb + bjc;
                        bja = b[ja + j];
                        u2 = bja - u1 * .5f;
                        u3 = c1 * (bjb - bjc);
                        a[ja + j] = aja + t1;
                        b[ja + j] = bja + u1;
                        a[jb + j] = t2 - u3;
                        b[jb + j] = u2 + t3;
                        a[jc + j] = t2 + u3;
                        b[jc + j] = u2 - t3;
                        j += *jump;
                    }
                    ja += jstepx;
                    if (ja < istart) {
                        ja += ninc;
                    }
                }
            }

/*  finished if n3 = 3 */
/*  ------------------ */
            if (n3 == 3) {
                goto L490;
            }
            kk = la << 1;

/*  loop on nonzero k */
/*  ----------------- */
            i__4 = jstep - ink;
            for (k = ink; ink < 0 ? k >= i__4 : k <= i__4; k += ink) {
                co1 = trigs[kk];     si1 = s * trigs[kk + 1];
                co2 = trigs[kk * 2]; si2 = s * trigs[kk * 2 + 1];

/*  loop along transform */
/*  -------------------- */
                i__5 = (*n - 1) * *inc;
                i__6 = jstep * 3;
                for (jjj = k; i__6 < 0 ? jjj >= i__5 : jjj <= i__5; jjj += i__6) {
                    ja = istart + jjj;

/*  "transverse" loop */
/*  ----------------- */
                    for (nu = 1; nu <= inq; ++nu) {
                        jb = ja + jstepl;
                        if (jb < istart) {
                            jb += ninc;
                        }
                        jc = jb + jstepl;
                        if (jc < istart) {
                            jc += ninc;
                        }
                        j = 0;

/*  loop across transforms */
/*  ---------------------- */
/* dir$ ivdep,shortloop */
                        for (l = 1; l <= nvex; ++l) {
                            ajb = a[jb + j];
                            ajc = a[jc + j];
                            t1 = ajb + ajc;
                            aja = a[ja + j];
                            t2 = aja - t1 * .5f;
                            t3 = c1 * (ajb - ajc);
                            bjb = b[jb + j];
                            bjc = b[jc + j];
                            u1 = bjb + bjc;
                            bja = b[ja + j];
                            u2 = bja - u1 * .5f;
                            u3 = c1 * (bjb - bjc);
                            a[ja + j] = aja + t1;
                            b[ja + j] = bja + u1;
                            a[jb + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
                            b[jb + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
                            a[jc + j] = co2 * (t2 + u3) - si2 * (u2 - t3);
                            b[jc + j] = si2 * (t2 + u3) + co2 * (u2 - t3);
                            j += *jump;
                        }
                        ja += jstepx;
                        if (ja < istart) {
                            ja += ninc;
                        }
                    }
                }
                kk += la << 1;
            }
            la *= 3;
        }

/*  loop on type II radix-3 passes */
/*  ------------------------------ */

        for (ipass = mh; ipass < m; ++ipass) {
            jstep = *n * *inc / (la * 3);
            jstepl = jstep - ninc;
            laincl = la * ink - ninc;

/*  k=0 loop (no twiddle factors) */
/*  ----------------------------- */
            i__3 = (la - 1) * ink;
            i__4 = jstep * 3;
            for (ll = 0; i__4 < 0 ? ll >= i__3 : ll <= i__3; ll += i__4) {

                i__6 = (*n - 1) * *inc;
                i__5 = la * 3 * ink;
                for (jjj = ll; i__5 < 0 ? jjj >= i__6 : jjj <= i__6; jjj += i__5) {
                    ja = istart + jjj;

/*  "transverse" loop */
/*  ----------------- */
                    for (nu = 1; nu <= inq; ++nu) {
                        jb = ja + jstepl;
                        if (jb < istart) {
                            jb += ninc;
                        }
                        jc = jb + jstepl;
                        if (jc < istart) {
                            jc += ninc;
                        }
                        jd = ja + laincl;
                        if (jd < istart) {
                            jd += ninc;
                        }
                        je = jd + jstepl;
                        if (je < istart) {
                            je += ninc;
                        }
                        jf = je + jstepl;
                        if (jf < istart) {
                            jf += ninc;
                        }
                        jg = jd + laincl;
                        if (jg < istart) {
                            jg += ninc;
                        }
                        jh = jg + jstepl;
                        if (jh < istart) {
                            jh += ninc;
                        }
                        ji = jh + jstepl;
                        if (ji < istart) {
                            ji += ninc;
                        }
                        j = 0;

/*  loop across transforms */
/*  ---------------------- */
/* dir$ ivdep, shortloop */
                        for (l = 1; l <= nvex; ++l) {
                            ajb = a[jb + j];
                            ajc = a[jc + j];
                            t1 = ajb + ajc;
                            aja = a[ja + j];
                            t2 = aja - t1 * .5f;
                            t3 = c1 * (ajb - ajc);
                            ajd = a[jd + j];
                            ajb = ajd;
                            bjb = b[jb + j];
                            bjc = b[jc + j];
                            u1 = bjb + bjc;
                            bja = b[ja + j];
                            u2 = bja - u1 * .5f;
                            u3 = c1 * (bjb - bjc);
                            bjd = b[jd + j];
                            bjb = bjd;
                            a[ja + j] = aja + t1;
                            b[ja + j] = bja + u1;
                            a[jd + j] = t2 - u3;
                            b[jd + j] = u2 + t3;
                            ajc = t2 + u3;
                            bjc = u2 - t3;
/* ---------------------- */
                            aje = a[je + j];
                            ajf = a[jf + j];
                            t1 = aje + ajf;
                            t2 = ajb - t1 * .5f;
                            t3 = c1 * (aje - ajf);
                            ajh = a[jh + j];
                            ajf = ajh;
                            bje = b[je + j];
                            bjf = b[jf + j];
                            u1 = bje + bjf;
                            u2 = bjb - u1 * .5f;
                            u3 = c1 * (bje - bjf);
                            bjh = b[jh + j];
                            bjf = bjh;
                            a[jb + j] = ajb + t1;
                            b[jb + j] = bjb + u1;
                            a[je + j] = t2 - u3;
                            b[je + j] = u2 + t3;
                            a[jh + j] = t2 + u3;
                            b[jh + j] = u2 - t3;
/* ---------------------- */
                            aji = a[ji + j];
                            t1 = ajf + aji;
                            ajg = a[jg + j];
                            t2 = ajg - t1 * .5f;
                            t3 = c1 * (ajf - aji);
                            t1 += ajg;
                            a[jg + j] = ajc;
                            bji = b[ji + j];
                            u1 = bjf + bji;
                            bjg = b[jg + j];
                            u2 = bjg - u1 * .5f;
                            u3 = c1 * (bjf - bji);
                            u1 += bjg;
                            b[jg + j] = bjc;
                            a[jc + j] = t1;
                            b[jc + j] = u1;
                            a[jf + j] = t2 - u3;
                            b[jf + j] = u2 + t3;
                            a[ji + j] = t2 + u3;
                            b[ji + j] = u2 - t3;
                            j += *jump;
                        }
                        ja += jstepx;
                        if (ja < istart) {
                            ja += ninc;
                        }
                    }
                }
            }

/*  finished if last pass */
/*  --------------------- */
            if (ipass == m-1) {
                goto L490;
            }

            kk = la << 1;

/*     loop on nonzero k */
/*     ----------------- */
            i__4 = jstep - ink;
            for (k = ink; ink < 0 ? k >= i__4 : k <= i__4; k += ink) {
                co1 = trigs[kk];     si1 = s * trigs[kk + 1];
                co2 = trigs[kk * 2]; si2 = s * trigs[kk * 2 + 1];

/*  double loop along first transform in block */
/*  ------------------------------------------ */
                i__5 = (la - 1) * ink;
                i__6 = jstep * 3;
                for (ll = k; i__6 < 0 ? ll >= i__5 : ll <= i__5; ll += i__6) {

                    i__7 = (*n - 1) * *inc;
                    i__8 = la * 3 * ink;
                    for (jjj = ll; i__8 < 0 ? jjj >= i__7 : jjj <= i__7; jjj += i__8) {
                        ja = istart + jjj;

/*  "transverse" loop */
/*  ----------------- */
                        for (nu = 1; nu <= inq; ++nu) {
                            jb = ja + jstepl;
                            if (jb < istart) {
                                jb += ninc;
                            }
                            jc = jb + jstepl;
                            if (jc < istart) {
                                jc += ninc;
                            }
                            jd = ja + laincl;
                            if (jd < istart) {
                                jd += ninc;
                            }
                            je = jd + jstepl;
                            if (je < istart) {
                                je += ninc;
                            }
                            jf = je + jstepl;
                            if (jf < istart) {
                                jf += ninc;
                            }
                            jg = jd + laincl;
                            if (jg < istart) {
                                jg += ninc;
                            }
                            jh = jg + jstepl;
                            if (jh < istart) {
                                jh += ninc;
                            }
                            ji = jh + jstepl;
                            if (ji < istart) {
                                ji += ninc;
                            }
                            j = 0;

/*  loop across transforms */
/*  ---------------------- */
/* dir$ ivdep, shortloop */
                            for (l = 1; l <= nvex; ++l) {
                                ajb = a[jb + j];
                                ajc = a[jc + j];
                                t1 = ajb + ajc;
                                aja = a[ja + j];
                                t2 = aja - t1 * .5f;
                                t3 = c1 * (ajb - ajc);
                                ajd = a[jd + j];
                                ajb = ajd;
                                bjb = b[jb + j];
                                bjc = b[jc + j];
                                u1 = bjb + bjc;
                                bja = b[ja + j];
                                u2 = bja - u1 * .5f;
                                u3 = c1 * (bjb - bjc);
                                bjd = b[jd + j];
                                bjb = bjd;
                                a[ja + j] = aja + t1;
                                b[ja + j] = bja + u1;
                                a[jd + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
                                b[jd + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
                                ajc = co2 * (t2 + u3) - si2 * (u2 - t3);
                                bjc = si2 * (t2 + u3) + co2 * (u2 - t3);
/* ---------------------- */
                                aje = a[je + j];
                                ajf = a[jf + j];
                                t1 = aje + ajf;
                                t2 = ajb - t1 * .5f;
                                t3 = c1 * (aje - ajf);
                                ajh = a[jh + j];
                                ajf = ajh;
                                bje = b[je + j];
                                bjf = b[jf + j];
                                u1 = bje + bjf;
                                u2 = bjb - u1 * .5f;
                                u3 = c1 * (bje - bjf);
                                bjh = b[jh + j];
                                bjf = bjh;
                                a[jb + j] = ajb + t1;
                                b[jb + j] = bjb + u1;
                                a[je + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
                                b[je + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
                                a[jh + j] = co2 * (t2 + u3) - si2 * (u2 - t3);
                                b[jh + j] = si2 * (t2 + u3) + co2 * (u2 - t3);
/* ---------------------- */
                                aji = a[ji + j];
                                t1 = ajf + aji;
                                ajg = a[jg + j];
                                t2 = ajg - t1 * .5f;
                                t3 = c1 * (ajf - aji);
                                t1 += ajg;
                                a[jg + j] = ajc;
                                bji = b[ji + j];
                                u1 = bjf + bji;
                                bjg = b[jg + j];
                                u2 = bjg - u1 * .5f;
                                u3 = c1 * (bjf - bji);
                                u1 += bjg;
                                b[jg + j] = bjc;
                                a[jc + j] = t1;
                                b[jc + j] = u1;
                                a[jf + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
                                b[jf + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
                                a[ji + j] = co2 * (t2 + u3) - si2 * (u2 - t3);
                                b[ji + j] = si2 * (t2 + u3) + co2 * (u2 - t3);
                                j += *jump;
                            }
                            ja += jstepx;
                            if (ja < istart) {
                                ja += ninc;
                            }
                        }
                    }
                }
                kk += la << 1;
            }
            la *= 3;
        }
L490:
        istart += nvex * *jump;
    }
} /* gpfa3f_ */

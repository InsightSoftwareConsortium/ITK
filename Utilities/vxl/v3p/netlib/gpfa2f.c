#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Table of constant values */
static integer c__2 = 2;

/*     fortran version of *gpfa2* -                                    */
/*     radix-2 section of self-sorting, in-place, generalized pfa      */
/*     central radix-2 and radix-8 passes included                     */
/*      so that transform length can be any power of 2                 */
/*                                                                     */
/* ------------------------------------------------------------------- */

/* Subroutine */ void gpfa2f_(real *a, real *b, const real *trigs, const integer *inc,
        const integer *jump, const integer *n, const integer *mm, const integer *lot, const integer *isign)
{
    /* Initialized data */
    static integer lvr = 1024;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer ninc, left, nvex, j, k, l, m;
    static real s;
    static integer ipass, nblox;
    static real c1;
    static integer jstep;
    static real c2, c3;
    static integer m2, n2;
    static real t0;
    static integer m8;
    static real t2, t1, t3, u0, u2, u1, u3;
    static integer ja, jb, jc, jd, je, jf, jg, jh, ji, jj, jk, jl, jm, jn, jo, jp, la, nb, mh, kk, ll, mu, nu, laincl;
    static real ss;
    static integer jstepl;
    static real co1, co2, co3;
    static integer istart;
    static real co4, co5, co6, co7;
    static integer jstepx;
    static real si1, si2, si3, si4, si5, si6, si7, aja, ajb, ajc, ajd, bja,
            bjc, bjb, bjd, aje, ajg, ajf, ajh, bje, bjg, bjf, bjh, aji;
    static integer jjj;
    static real bjm, ajj;
    static integer ink;
    static real bjj, ajk, ajl, bji, bjk;
    static integer inq;
    static real ajo, bjl, bjo, ajm, ajn, ajp, bjn, bjp;

/*     *************************************************************** */
/*     *                                                             * */
/*     *  N.B. LVR = LENGTH OF VECTOR REGISTERS, SET TO 128 FOR C90. * */
/*     *  RESET TO 64 FOR OTHER CRAY MACHINES, OR TO ANY LARGE VALUE * */
/*     *  (GREATER THAN OR EQUAL TO LOT) FOR A SCALAR COMPUTER.      * */
/*     *                                                             * */
/*     *************************************************************** */

    n2 = pow_ii(&c__2, mm);
    inq = *n / n2;
    jstepx = (n2 - *n) * *inc;
    ninc = *n * *inc;
    ink = *inc * inq;

    m2 = 0;
    m8 = 0;
    if (*mm % 2 == 0) {
        m = *mm / 2;
    } else if (*mm % 4 == 1) {
        m = (*mm - 1) / 2;
        m2 = 1;
    } else if (*mm % 4 == 3) {
        m = (*mm - 3) / 2;
        m8 = 1;
    }
    mh = (m + 1) / 2;

    nblox = (*lot - 1) / lvr + 1;
    left = *lot;
    s = (real) (*isign);
    istart = 0;

/*  loop on blocks of lvr transforms */
    for (nb = 0; nb < nblox; ++nb) {

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

/*  loop on type I radix-4 passes */
        mu = inq % 4;
        if (*isign == -1) {
            mu = 4 - mu;
        }
        ss = 1.f;
        if (mu == 3) {
            ss = -1.f;
        }

        if (mh == 0) {
            goto L200;
        }

        for (ipass = 0; ipass < mh; ++ipass) {
            jstep = *n * *inc / (la << 2);
            jstepl = jstep - ninc;

/*  k = 0 loop (no twiddle factors) */
            i__1 = jstep << 2;
            for (jjj = 0; i__1 < 0 ? jjj >= (*n - 1) * *inc : jjj <= (*n - 1) * *inc; jjj += i__1) {
                ja = istart + jjj;

/*     "transverse" loop */
                for (nu = 0; nu < inq; ++nu) {
                    jb = ja + jstepl;
                    if (jb < istart) {
                        jb += ninc;
                    }
                    jc = jb + jstepl;
                    if (jc < istart) {
                        jc += ninc;
                    }
                    jd = jc + jstepl;
                    if (jd < istart) {
                        jd += ninc;
                    }
                    j = 0;

/*  loop across transforms */
/* dir$ ivdep, shortloop */
                    for (l = 0; l < nvex; ++l) {
                        aja = a[ja + j];
                        ajc = a[jc + j];
                        t0 = aja + ajc;
                        t2 = aja - ajc;
                        ajb = a[jb + j];
                        ajd = a[jd + j];
                        t1 = ajb + ajd;
                        t3 = ss * (ajb - ajd);
                        bja = b[ja + j];
                        bjc = b[jc + j];
                        u0 = bja + bjc;
                        u2 = bja - bjc;
                        bjb = b[jb + j];
                        bjd = b[jd + j];
                        u1 = bjb + bjd;
                        u3 = ss * (bjb - bjd);
                        a[ja + j] = t0 + t1;
                        a[jc + j] = t0 - t1;
                        b[ja + j] = u0 + u1;
                        b[jc + j] = u0 - u1;
                        a[jb + j] = t2 - u3;
                        a[jd + j] = t2 + u3;
                        b[jb + j] = u2 + t3;
                        b[jd + j] = u2 - t3;
                        j += *jump;
                    }
                    ja += jstepx;
                    if (ja < istart) {
                        ja += ninc;
                    }
                }
            }

/*  finished if n2 = 4 */
            if (n2 == 4) {
                goto L490;
            }
            kk = la << 1;

/*  loop on nonzero k */
            for (k = ink; ink < 0 ? k >= jstep - ink : k <= jstep - ink; k += ink) {
                co1 = trigs[kk];     si1 = s * trigs[kk + 1];
                co2 = trigs[kk * 2]; si2 = s * trigs[kk * 2 + 1];
                co3 = trigs[kk * 3]; si3 = s * trigs[kk * 3 + 1];

/*  loop along transform */
                i__1 = jstep << 2;
                for (jjj = k; i__1 < 0 ? jjj >= (*n - 1) * *inc : jjj <= (*n - 1) * *inc; jjj += i__1) {
                    ja = istart + jjj;

/*     "transverse" loop */
                    for (nu = 0; nu < inq; ++nu) {
                        jb = ja + jstepl;
                        if (jb < istart) {
                            jb += ninc;
                        }
                        jc = jb + jstepl;
                        if (jc < istart) {
                            jc += ninc;
                        }
                        jd = jc + jstepl;
                        if (jd < istart) {
                            jd += ninc;
                        }
                        j = 0;

/*  loop across transforms */
/* dir$ ivdep,shortloop */
                        for (l = 0; l < nvex; ++l) {
                            aja = a[ja + j];
                            ajc = a[jc + j];
                            t0 = aja + ajc;
                            t2 = aja - ajc;
                            ajb = a[jb + j];
                            ajd = a[jd + j];
                            t1 = ajb + ajd;
                            t3 = ss * (ajb - ajd);
                            bja = b[ja + j];
                            bjc = b[jc + j];
                            u0 = bja + bjc;
                            u2 = bja - bjc;
                            bjb = b[jb + j];
                            bjd = b[jd + j];
                            u1 = bjb + bjd;
                            u3 = ss * (bjb - bjd);
                            a[ja + j] = t0 + t1;
                            b[ja + j] = u0 + u1;
                            a[jb + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
                            b[jb + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
                            a[jc + j] = co2 * (t0 - t1) - si2 * (u0 - u1);
                            b[jc + j] = si2 * (t0 - t1) + co2 * (u0 - u1);
                            a[jd + j] = co3 * (t2 + u3) - si3 * (u2 - t3);
                            b[jd + j] = si3 * (t2 + u3) + co3 * (u2 - t3);
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
            la <<= 2;
        }

/*  central radix-2 pass */
L200:
        if (m2 == 0) {
            goto L300;
        }

        jstep = *n * *inc / (la << 1);
        jstepl = jstep - ninc;

/*  k=0 loop (no twiddle factors) */
        for (jjj = 0; jjj <= (*n - 1) * *inc; jjj += (jstep<<1)) {
            ja = istart + jjj;

/*     "transverse" loop */
            for (nu = 0; nu < inq; ++nu) {
                jb = ja + jstepl;
                if (jb < istart) {
                    jb += ninc;
                }
                j = 0;

/*  loop across transforms */
/* dir$ ivdep, shortloop */
                for (l = 0; l < nvex; ++l) {
                    aja = a[ja + j];
                    ajb = a[jb + j];
                    t0 = aja - ajb;
                    a[ja + j] = aja + ajb;
                    a[jb + j] = t0;
                    bja = b[ja + j];
                    bjb = b[jb + j];
                    u0 = bja - bjb;
                    b[ja + j] = bja + bjb;
                    b[jb + j] = u0;
                    j += *jump;
                }
                ja += jstepx;
                if (ja < istart) {
                    ja += ninc;
                }
            }
        }

/*  finished if n2=2 */
        if (n2 == 2) {
            goto L490;
        }

        kk = la << 1;

/*  loop on nonzero k */
        for (k = ink; ink < 0 ? k >= jstep - ink : k <= jstep - ink; k += ink) {
            co1 = trigs[kk];
            si1 = s * trigs[kk + 1];

/*  loop along transforms */
            i__1 = jstep << 1;
            for (jjj = k; i__1 < 0 ? jjj >= (*n - 1) * *inc : jjj <= (*n - 1) * *inc; jjj += i__1) {
                ja = istart + jjj;

/*     "transverse" loop */
                for (nu = 0; nu < inq; ++nu) {
                    jb = ja + jstepl;
                    if (jb < istart) {
                        jb += ninc;
                    }
                    j = 0;

/*  loop across transforms */
                    if (kk == n2 / 2) {
/* dir$ ivdep, shortloop */
                        for (l = 0; l < nvex; ++l) {
                            aja = a[ja + j];
                            ajb = a[jb + j];
                            t0 = ss * (aja - ajb);
                            a[ja + j] = aja + ajb;
                            bjb = b[jb + j];
                            bja = b[ja + j];
                            a[jb + j] = ss * (bjb - bja);
                            b[ja + j] = bja + bjb;
                            b[jb + j] = t0;
                            j += *jump;
                        }
                    } else {

/* dir$ ivdep, shortloop */
                        for (l = 0; l < nvex; ++l) {
                            aja = a[ja + j];
                            ajb = a[jb + j];
                            t0 = aja - ajb;
                            a[ja + j] = aja + ajb;
                            bja = b[ja + j];
                            bjb = b[jb + j];
                            u0 = bja - bjb;
                            b[ja + j] = bja + bjb;
                            a[jb + j] = co1 * t0 - si1 * u0;
                            b[jb + j] = si1 * t0 + co1 * u0;
                            j += *jump;
                        }
                    }

                    ja += jstepx;
                    if (ja < istart) {
                        ja += ninc;
                    }
                }
            }
            kk += la << 1;
        }

        la <<= 1;
        goto L400;

/*  central radix-8 pass */
L300:
        if (m8 == 0) {
            goto L400;
        }
        jstep = *n * *inc / (la << 3);
        jstepl = jstep - ninc;
        mu = inq % 8;
        if (*isign == -1) {
            mu = 8 - mu;
        }
        c1 = 1.f;
        if (mu == 3 || mu == 7) {
            c1 = -1.f;
        }
        c2 = sqrtf(.5f);
        if (mu == 3 || mu == 5) {
            c2 = -c2;
        }
        c3 = c1 * c2;

/*  stage 1 */
        for (k = 0; ink < 0 ? k >= jstep - ink : k <= jstep - ink; k += ink) {
            i__1 = jstep << 3;
            for (jjj = k; i__1 < 0 ? jjj >= (*n - 1) * *inc : jjj <= (*n - 1) * *inc; jjj += i__1) {
                ja = istart + jjj;

/*     "transverse" loop */
                for (nu = 0; nu < inq; ++nu) {
                    jb = ja + jstepl;
                    if (jb < istart) {
                        jb += ninc;
                    }
                    jc = jb + jstepl;
                    if (jc < istart) {
                        jc += ninc;
                    }
                    jd = jc + jstepl;
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
                    jg = jf + jstepl;
                    if (jg < istart) {
                        jg += ninc;
                    }
                    jh = jg + jstepl;
                    if (jh < istart) {
                        jh += ninc;
                    }
                    j = 0;
/* dir$ ivdep, shortloop */
                    for (l = 0; l < nvex; ++l) {
                        aja = a[ja + j];
                        aje = a[je + j];
                        t0 = aja - aje;
                        a[ja + j] = aja + aje;
                        ajc = a[jc + j];
                        ajg = a[jg + j];
                        t1 = c1 * (ajc - ajg);
                        a[je + j] = ajc + ajg;
                        ajb = a[jb + j];
                        ajf = a[jf + j];
                        t2 = ajb - ajf;
                        a[jc + j] = ajb + ajf;
                        ajd = a[jd + j];
                        ajh = a[jh + j];
                        t3 = ajd - ajh;
                        a[jg + j] = ajd + ajh;
                        a[jb + j] = t0;
                        a[jf + j] = t1;
                        a[jd + j] = c2 * (t2 - t3);
                        a[jh + j] = c3 * (t2 + t3);
                        bja = b[ja + j];
                        bje = b[je + j];
                        u0 = bja - bje;
                        b[ja + j] = bja + bje;
                        bjc = b[jc + j];
                        bjg = b[jg + j];
                        u1 = c1 * (bjc - bjg);
                        b[je + j] = bjc + bjg;
                        bjb = b[jb + j];
                        bjf = b[jf + j];
                        u2 = bjb - bjf;
                        b[jc + j] = bjb + bjf;
                        bjd = b[jd + j];
                        bjh = b[jh + j];
                        u3 = bjd - bjh;
                        b[jg + j] = bjd + bjh;
                        b[jb + j] = u0;
                        b[jf + j] = u1;
                        b[jd + j] = c2 * (u2 - u3);
                        b[jh + j] = c3 * (u2 + u3);
                        j += *jump;
                    }
                    ja += jstepx;
                    if (ja < istart) {
                        ja += ninc;
                    }
                }
            }
        }

/*  stage 2 */

/*  k=0 (no twiddle factors) */
        i__1 = jstep << 3;
        for (jjj = 0; i__1 < 0 ? jjj >= (*n - 1) * *inc : jjj <= (*n - 1) * *inc; jjj += i__1) {
            ja = istart + jjj;

/*     "transverse" loop */
            for (nu = 0; nu < inq; ++nu) {
                jb = ja + jstepl;
                if (jb < istart) {
                    jb += ninc;
                }
                jc = jb + jstepl;
                if (jc < istart) {
                    jc += ninc;
                }
                jd = jc + jstepl;
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
                jg = jf + jstepl;
                if (jg < istart) {
                    jg += ninc;
                }
                jh = jg + jstepl;
                if (jh < istart) {
                    jh += ninc;
                }
                j = 0;
/* dir$ ivdep, shortloop */
                for (l = 0; l < nvex; ++l) {
                    aja = a[ja + j];
                    aje = a[je + j];
                    t0 = aja + aje;
                    t2 = aja - aje;
                    ajc = a[jc + j];
                    ajg = a[jg + j];
                    t1 = ajc + ajg;
                    t3 = c1 * (ajc - ajg);
                    bja = b[ja + j];
                    bje = b[je + j];
                    u0 = bja + bje;
                    u2 = bja - bje;
                    bjc = b[jc + j];
                    bjg = b[jg + j];
                    u1 = bjc + bjg;
                    u3 = c1 * (bjc - bjg);
                    a[ja + j] = t0 + t1;
                    a[je + j] = t0 - t1;
                    b[ja + j] = u0 + u1;
                    b[je + j] = u0 - u1;
                    a[jc + j] = t2 - u3;
                    a[jg + j] = t2 + u3;
                    b[jc + j] = u2 + t3;
                    b[jg + j] = u2 - t3;
                    ajb = a[jb + j];
                    ajd = a[jd + j];
                    t0 = ajb + ajd;
                    t2 = ajb - ajd;
                    ajf = a[jf + j];
                    ajh = a[jh + j];
                    t1 = ajf - ajh;
                    t3 = ajf + ajh;
                    bjb = b[jb + j];
                    bjd = b[jd + j];
                    u0 = bjb + bjd;
                    u2 = bjb - bjd;
                    bjf = b[jf + j];
                    bjh = b[jh + j];
                    u1 = bjf - bjh;
                    u3 = bjf + bjh;
                    a[jb + j] = t0 - u3;
                    a[jh + j] = t0 + u3;
                    b[jb + j] = u0 + t3;
                    b[jh + j] = u0 - t3;
                    a[jd + j] = t2 + u1;
                    a[jf + j] = t2 - u1;
                    b[jd + j] = u2 - t1;
                    b[jf + j] = u2 + t1;
                    j += *jump;
                }
                ja += jstepx;
                if (ja < istart) {
                    ja += ninc;
                }
            }
        }

        if (n2 == 8) {
            goto L490;
        }

/*  loop on nonzero k */
        kk = la << 1;

        for (k = ink; ink < 0 ? k >= jstep - ink : k <= jstep - ink; k += ink) {
            co1 = trigs[kk];     si1 = s * trigs[kk + 1];
            co2 = trigs[kk * 2]; si2 = s * trigs[kk * 2 + 1];
            co3 = trigs[kk * 3]; si3 = s * trigs[kk * 3 + 1];
            co4 = trigs[kk * 4]; si4 = s * trigs[kk * 4 + 1];
            co5 = trigs[kk * 5]; si5 = s * trigs[kk * 5 + 1];
            co6 = trigs[kk * 6]; si6 = s * trigs[kk * 6 + 1];
            co7 = trigs[kk * 7]; si7 = s * trigs[kk * 7 + 1];

            i__1 = jstep << 3;
            for (jjj = k; i__1 < 0 ? jjj >= (*n - 1) * *inc : jjj <= (*n - 1) * *inc; jjj += i__1) {
                ja = istart + jjj;

/*     "transverse" loop */
                for (nu = 0; nu < inq; ++nu) {
                    jb = ja + jstepl;
                    if (jb < istart) {
                        jb += ninc;
                    }
                    jc = jb + jstepl;
                    if (jc < istart) {
                        jc += ninc;
                    }
                    jd = jc + jstepl;
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
                    jg = jf + jstepl;
                    if (jg < istart) {
                        jg += ninc;
                    }
                    jh = jg + jstepl;
                    if (jh < istart) {
                        jh += ninc;
                    }
                    j = 0;
/* dir$ ivdep, shortloop */
                    for (l = 0; l < nvex; ++l) {
                        aja = a[ja + j];
                        aje = a[je + j];
                        t0 = aja + aje;
                        t2 = aja - aje;
                        ajc = a[jc + j];
                        ajg = a[jg + j];
                        t1 = ajc + ajg;
                        t3 = c1 * (ajc - ajg);
                        bja = b[ja + j];
                        bje = b[je + j];
                        u0 = bja + bje;
                        u2 = bja - bje;
                        bjc = b[jc + j];
                        bjg = b[jg + j];
                        u1 = bjc + bjg;
                        u3 = c1 * (bjc - bjg);
                        a[ja + j] = t0 + t1;
                        b[ja + j] = u0 + u1;
                        a[je + j] = co4 * (t0 - t1) - si4 * (u0 - u1);
                        b[je + j] = si4 * (t0 - t1) + co4 * (u0 - u1);
                        a[jc + j] = co2 * (t2 - u3) - si2 * (u2 + t3);
                        b[jc + j] = si2 * (t2 - u3) + co2 * (u2 + t3);
                        a[jg + j] = co6 * (t2 + u3) - si6 * (u2 - t3);
                        b[jg + j] = si6 * (t2 + u3) + co6 * (u2 - t3);
                        ajb = a[jb + j];
                        ajd = a[jd + j];
                        t0 = ajb + ajd;
                        t2 = ajb - ajd;
                        ajf = a[jf + j];
                        ajh = a[jh + j];
                        t1 = ajf - ajh;
                        t3 = ajf + ajh;
                        bjb = b[jb + j];
                        bjd = b[jd + j];
                        u0 = bjb + bjd;
                        u2 = bjb - bjd;
                        bjf = b[jf + j];
                        bjh = b[jh + j];
                        u1 = bjf - bjh;
                        u3 = bjf + bjh;
                        a[jb + j] = co1 * (t0 - u3) - si1 * (u0 + t3);
                        b[jb + j] = si1 * (t0 - u3) + co1 * (u0 + t3);
                        a[jh + j] = co7 * (t0 + u3) - si7 * (u0 - t3);
                        b[jh + j] = si7 * (t0 + u3) + co7 * (u0 - t3);
                        a[jd + j] = co3 * (t2 + u1) - si3 * (u2 - t1);
                        b[jd + j] = si3 * (t2 + u1) + co3 * (u2 - t1);
                        a[jf + j] = co5 * (t2 - u1) - si5 * (u2 + t1);
                        b[jf + j] = si5 * (t2 - u1) + co5 * (u2 + t1);
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

        la <<= 3;

/*  loop on type II radix-4 passes */
L400:
        mu = inq % 4;
        if (*isign == -1) {
            mu = 4 - mu;
        }
        ss = 1.f;
        if (mu == 3) {
            ss = -1.f;
        }

        for (ipass = mh; ipass < m; ++ipass) {
            jstep = *n * *inc / (la << 2);
            jstepl = jstep - ninc;
            laincl = la * ink - ninc;

/*  k=0 loop (no twiddle factors) */
            i__1 = jstep << 2;
            for (ll = 0; i__1 < 0 ? ll >= (la - 1) * ink : ll <= (la - 1) * ink; ll += i__1) {

                i__1 = (la << 2) * ink;
                for (jjj = ll; i__1 < 0 ? jjj >= (*n - 1) * *inc : jjj <= (*n - 1) * *inc; jjj += i__1) {
                    ja = istart + jjj;

/*     "transverse" loop */
                    for (nu = 0; nu < inq; ++nu) {
                        jb = ja + jstepl;
                        if (jb < istart) {
                            jb += ninc;
                        }
                        jc = jb + jstepl;
                        if (jc < istart) {
                            jc += ninc;
                        }
                        jd = jc + jstepl;
                        if (jd < istart) {
                            jd += ninc;
                        }
                        je = ja + laincl;
                        if (je < istart) {
                            je += ninc;
                        }
                        jf = je + jstepl;
                        if (jf < istart) {
                            jf += ninc;
                        }
                        jg = jf + jstepl;
                        if (jg < istart) {
                            jg += ninc;
                        }
                        jh = jg + jstepl;
                        if (jh < istart) {
                            jh += ninc;
                        }
                        ji = je + laincl;
                        if (ji < istart) {
                            ji += ninc;
                        }
                        jj = ji + jstepl;
                        if (jj < istart) {
                            jj += ninc;
                        }
                        jk = jj + jstepl;
                        if (jk < istart) {
                            jk += ninc;
                        }
                        jl = jk + jstepl;
                        if (jl < istart) {
                            jl += ninc;
                        }
                        jm = ji + laincl;
                        if (jm < istart) {
                            jm += ninc;
                        }
                        jn = jm + jstepl;
                        if (jn < istart) {
                            jn += ninc;
                        }
                        jo = jn + jstepl;
                        if (jo < istart) {
                            jo += ninc;
                        }
                        jp = jo + jstepl;
                        if (jp < istart) {
                            jp += ninc;
                        }
                        j = 0;

/*  loop across transforms */
                        for (l = 0; l < nvex; ++l) {
                            aja = a[ja + j];
                            ajc = a[jc + j];
                            t0 = aja + ajc;
                            t2 = aja - ajc;
                            ajb = a[jb + j];
                            ajd = a[jd + j];
                            t1 = ajb + ajd;
                            t3 = ss * (ajb - ajd);
                            aji = a[ji + j];
                            ajc = aji;
                            bja = b[ja + j];
                            bjc = b[jc + j];
                            u0 = bja + bjc;
                            u2 = bja - bjc;
                            bjb = b[jb + j];
                            bjd = b[jd + j];
                            u1 = bjb + bjd;
                            u3 = ss * (bjb - bjd);
                            aje = a[je + j];
                            ajb = aje;
                            a[ja + j] = t0 + t1;
                            a[ji + j] = t0 - t1;
                            b[ja + j] = u0 + u1;
                            bjc = u0 - u1;
                            bjm = b[jm + j];
                            bjd = bjm;
                            a[je + j] = t2 - u3;
                            ajd = t2 + u3;
                            bjb = u2 + t3;
                            b[jm + j] = u2 - t3;

                            ajg = a[jg + j];
                            t0 = ajb + ajg;
                            t2 = ajb - ajg;
                            ajf = a[jf + j];
                            ajh = a[jh + j];
                            t1 = ajf + ajh;
                            t3 = ss * (ajf - ajh);
                            ajj = a[jj + j];
                            ajg = ajj;
                            bje = b[je + j];
                            bjg = b[jg + j];
                            u0 = bje + bjg;
                            u2 = bje - bjg;
                            bjf = b[jf + j];
                            bjh = b[jh + j];
                            u1 = bjf + bjh;
                            u3 = ss * (bjf - bjh);
                            b[je + j] = bjb;
                            a[jb + j] = t0 + t1;
                            a[jj + j] = t0 - t1;
                            bjj = b[jj + j];
                            bjg = bjj;
                            b[jb + j] = u0 + u1;
                            b[jj + j] = u0 - u1;
                            a[jf + j] = t2 - u3;
                            ajh = t2 + u3;
                            b[jf + j] = u2 + t3;
                            bjh = u2 - t3;

                            ajk = a[jk + j];
                            t0 = ajc + ajk;
                            t2 = ajc - ajk;
                            ajl = a[jl + j];
                            t1 = ajg + ajl;
                            t3 = ss * (ajg - ajl);
                            bji = b[ji + j];
                            bjk = b[jk + j];
                            u0 = bji + bjk;
                            u2 = bji - bjk;
                            ajo = a[jo + j];
                            ajl = ajo;
                            bjl = b[jl + j];
                            u1 = bjg + bjl;
                            u3 = ss * (bjg - bjl);
                            b[ji + j] = bjc;
                            a[jc + j] = t0 + t1;
                            a[jk + j] = t0 - t1;
                            bjo = b[jo + j];
                            bjl = bjo;
                            b[jc + j] = u0 + u1;
                            b[jk + j] = u0 - u1;
                            a[jg + j] = t2 - u3;
                            a[jo + j] = t2 + u3;
                            b[jg + j] = u2 + t3;
                            b[jo + j] = u2 - t3;

                            ajm = a[jm + j];
                            t0 = ajm + ajl;
                            t2 = ajm - ajl;
                            ajn = a[jn + j];
                            ajp = a[jp + j];
                            t1 = ajn + ajp;
                            t3 = ss * (ajn - ajp);
                            a[jm + j] = ajd;
                            u0 = bjd + bjl;
                            u2 = bjd - bjl;
                            bjn = b[jn + j];
                            bjp = b[jp + j];
                            u1 = bjn + bjp;
                            u3 = ss * (bjn - bjp);
                            a[jn + j] = ajh;
                            a[jd + j] = t0 + t1;
                            a[jl + j] = t0 - t1;
                            b[jd + j] = u0 + u1;
                            b[jl + j] = u0 - u1;
                            b[jn + j] = bjh;
                            a[jh + j] = t2 - u3;
                            a[jp + j] = t2 + u3;
                            b[jh + j] = u2 + t3;
                            b[jp + j] = u2 - t3;
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
            if (ipass == m-1) {
                goto L490;
            }

            kk = la << 1;

/*     loop on nonzero k */
            for (k = ink; ink < 0 ? k >= jstep - ink : k <= jstep - ink; k += ink) {
                co1 = trigs[kk];     si1 = s * trigs[kk + 1];
                co2 = trigs[kk * 2]; si2 = s * trigs[kk * 2 + 1];
                co3 = trigs[kk * 3]; si3 = s * trigs[kk * 3 + 1];

/*  double loop along first transform in block */
                i__1 = jstep << 2;
                for (ll = k; i__1 < 0 ? ll >= (la - 1) * ink : ll <= (la - 1) * ink; ll += i__1) {

                    i__1 = (la << 2) * ink;
                    for (jjj = ll; i__1 < 0 ? jjj >= (*n - 1) * *inc : jjj <= (*n - 1) * *inc; jjj += i__1) {
                        ja = istart + jjj;

/*     "transverse" loop */
                        for (nu = 0; nu < inq; ++nu) {
                            jb = ja + jstepl;
                            if (jb < istart) {
                                jb += ninc;
                            }
                            jc = jb + jstepl;
                            if (jc < istart) {
                                jc += ninc;
                            }
                            jd = jc + jstepl;
                            if (jd < istart) {
                                jd += ninc;
                            }
                            je = ja + laincl;
                            if (je < istart) {
                                je += ninc;
                            }
                            jf = je + jstepl;
                            if (jf < istart) {
                                jf += ninc;
                            }
                            jg = jf + jstepl;
                            if (jg < istart) {
                                jg += ninc;
                            }
                            jh = jg + jstepl;
                            if (jh < istart) {
                                jh += ninc;
                            }
                            ji = je + laincl;
                            if (ji < istart) {
                                ji += ninc;
                            }
                            jj = ji + jstepl;
                            if (jj < istart) {
                                jj += ninc;
                            }
                            jk = jj + jstepl;
                            if (jk < istart) {
                                jk += ninc;
                            }
                            jl = jk + jstepl;
                            if (jl < istart) {
                                jl += ninc;
                            }
                            jm = ji + laincl;
                            if (jm < istart) {
                                jm += ninc;
                            }
                            jn = jm + jstepl;
                            if (jn < istart) {
                                jn += ninc;
                            }
                            jo = jn + jstepl;
                            if (jo < istart) {
                                jo += ninc;
                            }
                            jp = jo + jstepl;
                            if (jp < istart) {
                                jp += ninc;
                            }
                            j = 0;

/*  loop across transforms */
/* dir$ ivdep, shortloop */
                            for (l = 0; l < nvex; ++l) {
                                aja = a[ja + j];
                                ajc = a[jc + j];
                                t0 = aja + ajc;
                                t2 = aja - ajc;
                                ajb = a[jb + j];
                                ajd = a[jd + j];
                                t1 = ajb + ajd;
                                t3 = ss * (ajb - ajd);
                                aji = a[ji + j];
                                ajc = aji;
                                bja = b[ja + j];
                                bjc = b[jc + j];
                                u0 = bja + bjc;
                                u2 = bja - bjc;
                                bjb = b[jb + j];
                                bjd = b[jd + j];
                                u1 = bjb + bjd;
                                u3 = ss * (bjb - bjd);
                                aje = a[je + j];
                                ajb = aje;
                                a[ja + j] = t0 + t1;
                                b[ja + j] = u0 + u1;
                                a[je + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
                                bjb = si1 * (t2 - u3) + co1 * (u2 + t3);
                                bjm = b[jm + j];
                                bjd = bjm;
                                a[ji + j] = co2 * (t0 - t1) - si2 * (u0 - u1);
                                bjc = si2 * (t0 - t1) + co2 * (u0 - u1);
                                ajd = co3 * (t2 + u3) - si3 * (u2 - t3);
                                b[jm + j] = si3 * (t2 + u3) + co3 * (u2 - t3);

                                ajg = a[jg + j];
                                t0 = ajb + ajg;
                                t2 = ajb - ajg;
                                ajf = a[jf + j];
                                ajh = a[jh + j];
                                t1 = ajf + ajh;
                                t3 = ss * (ajf - ajh);
                                ajj = a[jj + j];
                                ajg = ajj;
                                bje = b[je + j];
                                bjg = b[jg + j];
                                u0 = bje + bjg;
                                u2 = bje - bjg;
                                bjf = b[jf + j];
                                bjh = b[jh + j];
                                u1 = bjf + bjh;
                                u3 = ss * (bjf - bjh);
                                b[je + j] = bjb;
                                a[jb + j] = t0 + t1;
                                b[jb + j] = u0 + u1;
                                bjj = b[jj + j];
                                bjg = bjj;
                                a[jf + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
                                b[jf + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
                                a[jj + j] = co2 * (t0 - t1) - si2 * (u0 - u1);
                                b[jj + j] = si2 * (t0 - t1) + co2 * (u0 - u1);
                                ajh = co3 * (t2 + u3) - si3 * (u2 - t3);
                                bjh = si3 * (t2 + u3) + co3 * (u2 - t3);

                                ajk = a[jk + j];
                                t0 = ajc + ajk;
                                t2 = ajc - ajk;
                                ajl = a[jl + j];
                                t1 = ajg + ajl;
                                t3 = ss * (ajg - ajl);
                                bji = b[ji + j];
                                bjk = b[jk + j];
                                u0 = bji + bjk;
                                u2 = bji - bjk;
                                ajo = a[jo + j];
                                ajl = ajo;
                                bjl = b[jl + j];
                                u1 = bjg + bjl;
                                u3 = ss * (bjg - bjl);
                                b[ji + j] = bjc;
                                a[jc + j] = t0 + t1;
                                b[jc + j] = u0 + u1;
                                bjo = b[jo + j];
                                bjl = bjo;
                                a[jg + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
                                b[jg + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
                                a[jk + j] = co2 * (t0 - t1) - si2 * (u0 - u1);
                                b[jk + j] = si2 * (t0 - t1) + co2 * (u0 - u1);
                                a[jo + j] = co3 * (t2 + u3) - si3 * (u2 - t3);
                                b[jo + j] = si3 * (t2 + u3) + co3 * (u2 - t3);

                                ajm = a[jm + j];
                                t0 = ajm + ajl;
                                t2 = ajm - ajl;
                                ajn = a[jn + j];
                                ajp = a[jp + j];
                                t1 = ajn + ajp;
                                t3 = ss * (ajn - ajp);
                                a[jm + j] = ajd;
                                u0 = bjd + bjl;
                                u2 = bjd - bjl;
                                a[jn + j] = ajh;
                                bjn = b[jn + j];
                                bjp = b[jp + j];
                                u1 = bjn + bjp;
                                u3 = ss * (bjn - bjp);
                                b[jn + j] = bjh;
                                a[jd + j] = t0 + t1;
                                b[jd + j] = u0 + u1;
                                a[jh + j] = co1 * (t2 - u3) - si1 * (u2 + t3);
                                b[jh + j] = si1 * (t2 - u3) + co1 * (u2 + t3);
                                a[jl + j] = co2 * (t0 - t1) - si2 * (u0 - u1);
                                b[jl + j] = si2 * (t0 - t1) + co2 * (u0 - u1);
                                a[jp + j] = co3 * (t2 + u3) - si3 * (u2 - t3);
                                b[jp + j] = si3 * (t2 + u3) + co3 * (u2 - t3);
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
            la <<= 2;
        }
L490:
        istart += nvex * *jump;
    }
} /* gpfa2f_ */

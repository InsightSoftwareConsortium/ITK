/* adaquad.f -- translated by f2c (version 20020621). */
#include "f2c.h"
extern /* Subroutine */
int srule_(E_fp, doublereal *, doublereal *, doublereal *, doublereal *);
extern /* Subroutine */
int refine_(U_fp, integer *, doublereal *, integer *, integer *);

/*     NUMERICAL METHODS: FORTRAN Programs, (c) John H. Mathews 1994 */
/*     To accompany the text: */
/*     NUMERICAL METHODS for Mathematics, Science and Engineering, 2nd Ed, 1992 */
/*     Prentice Hall, Englewood Cliffs, New Jersey, 07632, U.S.A. */
/*     This free software is complements of the author. */

/*     Algorithm 7.5 (Adaptive Quadrature Using Simpson's Rule). */
/*     Section 7.4, Adaptive Quadrature, Page 389 */

/* Subroutine */
int adaptquad_(E_fp f, doublereal *a, doublereal *b, doublereal *tol, doublereal *srmat, 
    doublereal *integral, doublereal *errbdd, integer *m, integer *state)
{
    /* Local variables */
    static integer j, k, n, iterating;
    static doublereal sum1, sum2;
    static doublereal srvec[11];

    /* Function Body */
    iterating = 0;
    srule_((E_fp)f, a, b, tol, srvec);
    for (k = 0; k < 11; ++k) {
        srmat[k * 101] = srvec[k];
    }
    *m = 1;
    *state = iterating;
    while (*state == iterating) {
        n = *m;
        for (j = n; j >= 1; --j) {
            refine_((U_fp)f, &j, srmat, m, state);
        }
    }
    sum1 = 0.f;
    sum2 = 0.f;
    for (j = 0; j < *m; ++j) {
        sum1 += srmat[j + 707];
        sum2 += dabs(srmat[j + 808]);
    }
    *integral = sum1;
    *errbdd = sum2;
    return 0;
} /* adaptquad_ */

/* Subroutine */
int refine_(U_fp f, integer *p, doublereal *srmat, integer *m, integer *state)
{
    /* System generated locals */
    integer pm1;

    /* Local variables */
    static doublereal a, b, c__;
    static integer j, k;
    static integer iterating;
    static doublereal err, tol, tol2;
    static doublereal check;
    static doublereal sr0vec[11], sr1vec[11], sr2vec[11];

    /* Function Body */
    iterating = 0;
    *state = 1;
    pm1 = *p - 1;
    for (k = 0; k < 11; ++k) {
        sr0vec[k] = srmat[pm1 + k * 101];
    }
    a = sr0vec[0];
    c__ = sr0vec[1];
    b = sr0vec[2];
/*  fa = sr0vec[3]; */
/*  fc = sr0vec[4]; */
/*  fb = sr0vec[5]; */
/*  s = sr0vec[6]; */
/*  s2 = sr0vec[7]; */
    err = sr0vec[8];
    tol = sr0vec[9];
    check = sr0vec[10];
    if (check == 1.f) {
        return 0;
    }
    tol2 = tol / 2;
    srule_((E_fp)f, &a, &c__, &tol2, sr1vec);
    srule_((E_fp)f, &c__, &b, &tol2, sr2vec);
    err = dabs(sr0vec[6] - sr1vec[6] - sr2vec[6]) / 10;
    if (err < tol) {
        sr0vec[10] = 1.f;
    }
    if (err < tol) {
        for (k = 0; k < 11; ++k) {
            srmat[pm1 + k * 101] = sr0vec[k];
        }
        srmat[pm1 + 707] = sr1vec[6] + sr2vec[6];
        srmat[pm1 + 808] = err;
    } else {
        for (j = *m; j >= pm1; --j) {
            for (k = 0; k < 11; ++k) {
                srmat[j + k * 101] = srmat[j - 1 + k * 101];
            }
        }
        ++(*m);
        for (k = 0; k < 11; ++k) {
            srmat[pm1 + k * 101] = sr1vec[k];
        }
        for (k = 0; k < 11; ++k) {
            srmat[*p + k * 101] = sr2vec[k];
        }
        *state = iterating;
    }
    return 0;
} /* refine_ */

/* Subroutine */
int srule_(E_fp f, doublereal *a, doublereal *b, doublereal *tol0, doublereal *srvec)
{
    static doublereal c__, h__, s, s2, fa, fb, fc, err, tol1, check;

    /* Function Body */
    h__ = (*b - *a) / 2;
    c__ = (*a + *b) / 2;
    fa = (*f)(a);
    fc = (*f)(&c__);
    fb = (*f)(b);
    s = h__ * ((*f)(a) + (*f)(&c__) * 4 + (*f)(b)) / 3;
    s2 = s;
    tol1 = *tol0;
    err = *tol0;
    check = 0.f;
    srvec[0] = *a;
    srvec[1] = c__;
    srvec[2] = *b;
    srvec[3] = fa;
    srvec[4] = fc;
    srvec[5] = fb;
    srvec[6] = s;
    srvec[7] = s2;
    srvec[8] = err;
    srvec[9] = tol1;
    srvec[10] = check;
    return 0;
} /* srule_ */

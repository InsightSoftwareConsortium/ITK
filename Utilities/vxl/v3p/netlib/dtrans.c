#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void dtrans_(a, m, n, mn, move, iwrk, iok)
doublereal *a;
const integer *m, *n, *mn;
integer *move, *iwrk, *iok;
{
    /* Local variables */
    static doublereal b, c, d;
    static integer i, j, k, i1, i2, im, i1c, i2c, ncount, ir0, ir1, ir2, kmi, max_;

/* ***** */
/*  ALGORITHM 380 - REVISED */
/* ***** */
/*  A IS A ONE-DIMENSIONAL ARRAY OF LENGTH MN=M*N, WHICH       */
/*  CONTAINS THE MXN MATRIX TO BE TRANSPOSED (STORED           */
/*  COLUMNWISE). MOVE IS A ONE-DIMENSIONAL ARRAY OF LENGTH IWRK */
/*  USED TO STORE INFORMATION TO SPEED UP THE PROCESS.  THE    */
/*  VALUE IWRK=(M+N)/2 IS RECOMMENDED. IOK INDICATES THE       */
/*  SUCCESS OR FAILURE OF THE ROUTINE.                         */
/*  NORMAL RETURN  IOK=0                                       */
/*  ERRORS         IOK=-1 ,MN NOT EQUAL TO M*N                 */
/*                 IOK=-2 ,IWRK NEGATIVE OR ZERO               */
/*                 IOK.GT.0, (SHOULD NEVER OCCUR),IN THIS CASE */
/*  WE SET IOK EQUAL TO THE FINAL VALUE OF I WHEN THE SEARCH   */
/*  IS COMPLETED BUT SOME LOOPS HAVE NOT BEEN MOVED            */
/*  NOTE * MOVE(I) WILL STAY ZERO FOR FIXED POINTS             */

/* CHECK ARGUMENTS AND INITIALIZE. */

    if (*m < 2 || *n < 2) {
        goto L120;
    }
    if (*mn != *m * *n) {
        goto L180;
    }
    if (*iwrk < 1) {
        goto L190;
    }
    if (*m == *n) {
        goto L130;
    }
    ncount = 2;
    k = *mn - 1;
    for (i = 0; i < *iwrk; ++i) {
        move[i] = 0;
    }
    if (*m < 3 || *n < 3) {
        goto L30;
    }
/* CALCULATE THE NUMBER OF FIXED POINTS, EUCLIDS ALGORITHM */
/* FOR GCD(M-1,N-1). */
    ir2 = *m - 1;
    ir1 = *n - 1;
L20:
    ir0 = ir2 % ir1;
    ir2 = ir1;
    ir1 = ir0;
    if (ir0 != 0) {
        goto L20;
    }
    ncount = ncount + ir2 - 1;
/* SET INITIAL VALUES FOR SEARCH */
L30:
    i = 0;
    im = *m;
/* AT LEAST ONE LOOP MUST BE RE-ARRANGED */
    goto L80;
/* SEARCH FOR LOOPS TO REARRANGE */
L40:
    ++i;
    max_ = k - i;
    if (i >= max_) {
        goto L160;
    }
    im += *m;
    if (im > k) {
        im -= k;
    }
    i2 = im;
    if (i+1 == i2) {
        goto L40;
    }
    if (i >= *iwrk) {
        goto L60;
    }
    if (move[i] == 0) {
        goto L80;
    }
    goto L40;
L50:
    i2 = *m * i1 - k * (i1 / *n);
L60:
    if (i2 <= i+1 || i2 >= max_) {
        goto L70;
    }
    i1 = i2;
    goto L50;
L70:
    if (i2 != i+1) {
        goto L40;
    }
/* REARRANGE THE ELEMENTS OF A LOOP AND ITS COMPANION LOOP */
L80:
    i1 = i + 1;
    kmi = k - i - 1;
    b = a[i1];
    i1c = kmi;
    c = a[i1c];
L90:
    i2 = *m * i1 - k * (i1 / *n);
    i2c = k - i2;
    if (i1 <= *iwrk) {
        move[i1-1] = 2;
    }
    if (i1c <= *iwrk) {
        move[i1c-1] = 2;
    }
    ncount += 2;
    if (i2 == i+1) {
        goto L110;
    }
    if (i2 == kmi) {
        goto L100;
    }
    a[i1] = a[i2];
    a[i1c] = a[i2c];
    i1 = i2;
    i1c = i2c;
    goto L90;
/* FINAL STORE AND TEST FOR FINISHED */
L100:
    d = b;
    b = c;
    c = d;
L110:
    a[i1] = b;
    a[i1c] = c;
    if (ncount < *mn) {
        goto L40;
    }
/* NORMAL RETURN */
L120:
    *iok = 0;
    return;
/* IF MATRIX IS SQUARE,EXCHANGE ELEMENTS A(I,J) AND A(J,I). */
L130:
    for (i = 0; i < *n; ++i) {
        for (j = i+1; j < *n; ++j) {
            i1 = i + j * *n;
            i2 = j + i * *m;
            b = a[i1]; a[i1] = a[i2]; a[i2] = b;
        }
    }
    goto L120;
/* ERROR RETURNS. */
L160:
    *iok = i+1;
L170:
    return;
L180:
    *iok = -1;
    goto L170;
L190:
    *iok = -2;
    goto L170;
} /* dtrans_ */

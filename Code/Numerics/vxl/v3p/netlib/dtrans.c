/* dtrans.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int dtrans_(a, m, n, mn, move, iwrk, iok)
doublereal *a;
integer *m, *n, *mn, *move, *iwrk, *iok;
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static doublereal b, c, d;
    static integer i, j, k, i1, i2, j1, n1, im, i1c, i2c, ncount, ir0, ir1,
            ir2, kmi, max_;

/* ***** */
/*  ALGORITHM 380 - REVISED */
/* ***** */
/*  A IS A ONE-DIMENSIONAL ARRAY OF LENGTH MN=M*N, WHICH */
/*  CONTAINS THE MXN MATRIX TO BE TRANSPOSED (STORED */
/*  COLUMWISE). MOVE IS A ONE-DIMENSIONAL ARRAY OF LENGTH IWRK */
/*  USED TO STORE INFORMATION TO SPEED UP THE PROCESS.  THE */
/*  VALUE IWRK=(M+N)/2 IS RECOMMENDED. IOK INDICATES THE */
/*  SUCCESS OR FAILURE OF THE ROUTINE. */
/*  NORMAL RETURN  IOK=0 */
/*  ERRORS         IOK=-1 ,MN NOT EQUAL TO M*N */
/*                 IOK=-2 ,IWRK NEGATIVE OR ZERO */
/*                 IOK.GT.0, (SHOULD NEVER OCCUR),IN THIS CASE */
/*  WE SET IOK EQUAL TO THE FINAL VALUE OF I WHEN THE SEARCH */
/*  IS COMPLETED BUT SOME LOOPS HAVE NOT BEEN MOVED */
/*  NOTE * MOVE(I) WILL STAY ZERO FOR FIXED POINTS */
/* CHECK ARGUMENTS AND INITIALIZE. */
    /* Parameter adjustments */
    --move;
    --a;

    /* Function Body */
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
    i__1 = *iwrk;
    for (i = 1; i <= i__1; ++i) {
        move[i] = 0;
/* L10: */
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
    i = 1;
    im = *m;
/* AT LEAST ONE LOOP MUST BE RE-ARRANGED */
    goto L80;
/* SEARCH FOR LOOPS TO REARRANGE */
L40:
    max_ = k - i;
    ++i;
    if (i > max_) {
        goto L160;
    }
    im += *m;
    if (im > k) {
        im -= k;
    }
    i2 = im;
    if (i == i2) {
        goto L40;
    }
    if (i > *iwrk) {
        goto L60;
    }
    if (move[i] == 0) {
        goto L80;
    }
    goto L40;
L50:
    i2 = *m * i1 - k * (i1 / *n);
L60:
    if (i2 <= i || i2 >= max_) {
        goto L70;
    }
    i1 = i2;
    goto L50;
L70:
    if (i2 != i) {
        goto L40;
    }
/* REARRANGE THE ELEMENTS OF A LOOP AND ITS COMPANION LOOP */
L80:
    i1 = i;
    kmi = k - i;
    b = a[i1 + 1];
    i1c = kmi;
    c = a[i1c + 1];
L90:
    i2 = *m * i1 - k * (i1 / *n);
    i2c = k - i2;
    if (i1 <= *iwrk) {
        move[i1] = 2;
    }
    if (i1c <= *iwrk) {
        move[i1c] = 2;
    }
    ncount += 2;
    if (i2 == i) {
        goto L110;
    }
    if (i2 == kmi) {
        goto L100;
    }
    a[i1 + 1] = a[i2 + 1];
    a[i1c + 1] = a[i2c + 1];
    i1 = i2;
    i1c = i2c;
    goto L90;
/* FINAL STORE AND TEST FOR FINISHED */
L100:
    d = b;
    b = c;
    c = d;
L110:
    a[i1 + 1] = b;
    a[i1c + 1] = c;
    if (ncount < *mn) {
        goto L40;
    }
/* NORMAL RETURN */
L120:
    *iok = 0;
    return 0;
/* IF MATRIX IS SQUARE,EXCHANGE ELEMENTS A(I,J) AND A(J,I). */
L130:
    n1 = *n - 1;
    i__1 = n1;
    for (i = 1; i <= i__1; ++i) {
        j1 = i + 1;
        i__2 = *n;
        for (j = j1; j <= i__2; ++j) {
            i1 = i + (j - 1) * *n;
            i2 = j + (i - 1) * *m;
            b = a[i1];
            a[i1] = a[i2];
            a[i2] = b;
/* L140: */
        }
/* L150: */
    }
    goto L120;
/* ERROR RETURNS. */
L160:
    *iok = i;
L170:
    return 0;
L180:
    *iok = -1;
    goto L170;
L190:
    *iok = -2;
    goto L170;
} /* dtrans_ */


/* blas/drotg.f -- translated by f2c (version 20050501).
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

static doublereal c_b4 = 1.;

/*<       subroutine drotg(da,db,c,s) >*/
/* Subroutine */ int drotg_(doublereal *da, doublereal *db, doublereal *c__,
        doublereal *s)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal r__, z__, roe, scale;


/*     construct givens plane rotation. */
/*     jack dongarra, linpack, 3/11/78. */

/*<       double precision da,db,c,s,roe,scale,r,z >*/

/*<       roe = db >*/
    roe = *db;
/*<       if( dabs(da) .gt. dabs(db) ) roe = da >*/
    if (abs(*da) > abs(*db)) {
        roe = *da;
    }
/*<       scale = dabs(da) + dabs(db) >*/
    scale = abs(*da) + abs(*db);
/*<       if( scale .ne. 0.0d0 ) go to 10 >*/
    if (scale != 0.) {
        goto L10;
    }
/*<          c = 1.0d0 >*/
    *c__ = 1.;
/*<          s = 0.0d0 >*/
    *s = 0.;
/*<          r = 0.0d0 >*/
    r__ = 0.;
/*<          z = 0.0d0 >*/
    z__ = 0.;
/*<          go to 20 >*/
    goto L20;
/*<    10 r = scale*dsqrt((da/scale)**2 + (db/scale)**2) >*/
L10:
/* Computing 2nd power */
    d__1 = *da / scale;
/* Computing 2nd power */
    d__2 = *db / scale;
    r__ = scale * sqrt(d__1 * d__1 + d__2 * d__2);
/*<       r = dsign(1.0d0,roe)*r >*/
    r__ = d_sign(&c_b4, &roe) * r__;
/*<       c = da/r >*/
    *c__ = *da / r__;
/*<       s = db/r >*/
    *s = *db / r__;
/*<       z = 1.0d0 >*/
    z__ = 1.;
/*<       if( dabs(da) .gt. dabs(db) ) z = s >*/
    if (abs(*da) > abs(*db)) {
        z__ = *s;
    }
/*<       if( dabs(db) .ge. dabs(da) .and. c .ne. 0.0d0 ) z = 1.0d0/c >*/
    if (abs(*db) >= abs(*da) && *c__ != 0.) {
        z__ = 1. / *c__;
    }
/*<    20 da = r >*/
L20:
    *da = r__;
/*<       db = z >*/
    *db = z__;
/*<       return >*/
    return 0;
/*<       end >*/
} /* drotg_ */

#ifdef __cplusplus
        }
#endif

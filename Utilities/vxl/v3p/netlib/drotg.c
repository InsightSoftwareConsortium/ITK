#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Table of constant values */
static doublereal c_b4 = 1.;

/* Subroutine */ void drotg_(da, db, c, s)
doublereal *da, *db, *c, *s;
{
    /* Local variables */
    static doublereal r, scale, z, roe;

/*     construct givens plane rotation.     */
/*     jack dongarra, linpack, 3/11/78.     */

    scale = abs(*da) + abs(*db);
    if (scale == 0.) {
        *c = 1.; *s = 0.;
        *da = *db = 0.;
    }
    else {
        roe = *db;
        if (abs(*da) > abs(*db)) {
            roe = *da;
        }
        r = *da / scale;
        z = *db / scale;
        r = scale * sqrt(r * r + z * z);
        r *= d_sign(&c_b4, &roe);
        *c = *da / r;
        *s = *db / r;
        z = 1.;
        if (abs(*da) > abs(*db)) {
            z = *s;
        }
        if (abs(*db) >= abs(*da) && *c != 0.) {
            z = 1. / *c;
        }
        *da = r;
        *db = z;
    }
} /* drotg_ */

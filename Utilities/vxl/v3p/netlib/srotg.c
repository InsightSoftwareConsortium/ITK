#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Table of constant values */
static real c_b4 = 1.f;

/* Subroutine */ void srotg_(sa, sb, c, s)
real *sa, *sb, *c, *s;
{
    /* Local variables */
    static real r, scale, z, roe;

/*     construct givens plane rotation.        */
/*     jack dongarra, linpack, 3/11/78.        */

    scale = abs(*sa) + abs(*sb);
    if (scale == 0.f) {
        *c = 1.f; *s = 0.f;
        *sa = *sb = 0.f;
    }
    else {
        roe = *sb;
        if (abs(*sa) > abs(*sb)) {
            roe = *sa;
        }
        r = *sa / scale;
        z = *sb / scale;
        r = scale * sqrtf(r * r + z * z);
        r *= r_sign(&c_b4, &roe);
        *c = *sa / r;
        *s = *sb / r;
        z = 1.f;
        if (abs(*sa) > abs(*sb)) {
            z = *s;
        }
        if (abs(*sb) >= abs(*sa) && *c != 0.f) {
            z = 1.f / *c;
        }
        *sa = r;
        *sb = z;
    }
} /* srotg_ */

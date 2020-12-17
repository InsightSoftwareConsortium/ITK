/* linpack/dcabs1.f -- translated by f2c (version 20050501).
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

/*<       double precision function dcabs1(z) >*/
doublereal dcabs1_(doublecomplex *z__)
{
    /* System generated locals */
    doublereal ret_val;
    doublecomplex equiv_0[1];

    /* Local variables */
#define t ((doublereal *)equiv_0)
#define zz (equiv_0)

/*<       double complex z,zz >*/
/*<       double precision t(2) >*/
/*<       equivalence (zz,t(1)) >*/
/*<       zz = z >*/
    zz->r = z__->r, zz->i = z__->i;
/*<       dcabs1 = dabs(t(1)) + dabs(t(2)) >*/
    ret_val = abs(t[0]) + abs(t[1]);
/*<       return >*/
    return ret_val;
/*<       end >*/
} /* dcabs1_ */

#undef zz
#undef t


#ifdef __cplusplus
        }
#endif

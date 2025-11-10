/* minpack/dpmpar.f -- translated by f2c (version 20050501).
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

/*<       double precision function dpmpar(i) >*/
doublereal dpmpar_(integer *i__)
{
    /* Initialized data */

    static struct { /* constant */
        doublereal e_1[3];
        doublereal fill_2[1];
        }
    equiv_2 =
      { {2.22044604926e-16, 2.22507385852e-308, 1.79769313485e308}, {0.0} };


    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
#define dmach ((doublereal *)&equiv_2)
#define minmag ((integer *)&equiv_2 + 2)
#define maxmag ((integer *)&equiv_2 + 4)
#define mcheps ((integer *)&equiv_2)

/*<       integer i >*/
/*     ********** */

/*     Function dpmpar */

/*     This function provides double precision machine parameters */
/*     when the appropriate set of data statements is activated (by */
/*     removing the c from column 1) and all other data statements are */
/*     rendered inactive. Most of the parameter values were obtained */
/*     from the corresponding Bell Laboratories Port Library function. */

/*     The function statement is */

/*       double precision function dpmpar(i) */

/*     where */

/*       i is an integer input variable set to 1, 2, or 3 which */
/*         selects the desired machine parameter. If the machine has */
/*         t base b digits and its smallest and largest exponents are */
/*         emin and emax, respectively, then these parameters are */

/*         dpmpar(1) = b**(1 - t), the machine precision, */

/*         dpmpar(2) = b**(emin - 1), the smallest magnitude, */

/*         dpmpar(3) = b**emax*(1 - b**(-t)), the largest magnitude. */

/*     Argonne National Laboratory. MINPACK Project. November 1996. */
/*     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More' */

/*     ********** */
/*<       integer mcheps(4) >*/
/*<       integer minmag(4) >*/
/*<       integer maxmag(4) >*/
/*<       double precision dmach(3) >*/
/*<       equivalence (dmach(1),mcheps(1)) >*/
/*<       equivalence (dmach(2),minmag(1)) >*/
/*<       equivalence (dmach(3),maxmag(1)) >*/

/*     Machine constants for the IBM 360/370 series, */
/*     the Amdahl 470/V6, the ICL 2900, the Itel AS/6, */
/*     the Xerox Sigma 5/7/9 and the Sel systems 85/86. */

/*     data mcheps(1),mcheps(2) / z34100000, z00000000 / */
/*     data minmag(1),minmag(2) / z00100000, z00000000 / */
/*     data maxmag(1),maxmag(2) / z7fffffff, zffffffff / */

/*     Machine constants for the Honeywell 600/6000 series. */

/*     data mcheps(1),mcheps(2) / o606400000000, o000000000000 / */
/*     data minmag(1),minmag(2) / o402400000000, o000000000000 / */
/*     data maxmag(1),maxmag(2) / o376777777777, o777777777777 / */

/*     Machine constants for the CDC 6000/7000 series. */

/*     data mcheps(1) / 15614000000000000000b / */
/*     data mcheps(2) / 15010000000000000000b / */

/*     data minmag(1) / 00604000000000000000b / */
/*     data minmag(2) / 00000000000000000000b / */

/*     data maxmag(1) / 37767777777777777777b / */
/*     data maxmag(2) / 37167777777777777777b / */

/*     Machine constants for the PDP-10 (KA processor). */

/*     data mcheps(1),mcheps(2) / "114400000000, "000000000000 / */
/*     data minmag(1),minmag(2) / "033400000000, "000000000000 / */
/*     data maxmag(1),maxmag(2) / "377777777777, "344777777777 / */

/*     Machine constants for the PDP-10 (KI processor). */

/*     data mcheps(1),mcheps(2) / "104400000000, "000000000000 / */
/*     data minmag(1),minmag(2) / "000400000000, "000000000000 / */
/*     data maxmag(1),maxmag(2) / "377777777777, "377777777777 / */

/*     Machine constants for the PDP-11. */

/*     data mcheps(1),mcheps(2) /   9472,      0 / */
/*     data mcheps(3),mcheps(4) /      0,      0 / */

/*     data minmag(1),minmag(2) /    128,      0 / */
/*     data minmag(3),minmag(4) /      0,      0 / */

/*     data maxmag(1),maxmag(2) /  32767,     -1 / */
/*     data maxmag(3),maxmag(4) /     -1,     -1 / */

/*     Machine constants for the Burroughs 6700/7700 systems. */

/*     data mcheps(1) / o1451000000000000 / */
/*     data mcheps(2) / o0000000000000000 / */

/*     data minmag(1) / o1771000000000000 / */
/*     data minmag(2) / o7770000000000000 / */

/*     data maxmag(1) / o0777777777777777 / */
/*     data maxmag(2) / o7777777777777777 / */

/*     Machine constants for the Burroughs 5700 system. */

/*     data mcheps(1) / o1451000000000000 / */
/*     data mcheps(2) / o0000000000000000 / */

/*     data minmag(1) / o1771000000000000 / */
/*     data minmag(2) / o0000000000000000 / */

/*     data maxmag(1) / o0777777777777777 / */
/*     data maxmag(2) / o0007777777777777 / */

/*     Machine constants for the Burroughs 1700 system. */

/*     data mcheps(1) / zcc6800000 / */
/*     data mcheps(2) / z000000000 / */

/*     data minmag(1) / zc00800000 / */
/*     data minmag(2) / z000000000 / */

/*     data maxmag(1) / zdffffffff / */
/*     data maxmag(2) / zfffffffff / */

/*     Machine constants for the Univac 1100 series. */

/*     data mcheps(1),mcheps(2) / o170640000000, o000000000000 / */
/*     data minmag(1),minmag(2) / o000040000000, o000000000000 / */
/*     data maxmag(1),maxmag(2) / o377777777777, o777777777777 / */

/*     Machine constants for the Data General Eclipse S/200. */

/*     Note - it may be appropriate to include the following card - */
/*     static dmach(3) */ /* constant */

/*     data minmag/20k,3*0/,maxmag/77777k,3*177777k/ */
/*     data mcheps/32020k,3*0/ */

/*     Machine constants for the Harris 220. */

/*     data mcheps(1),mcheps(2) / '20000000, '00000334 / */
/*     data minmag(1),minmag(2) / '20000000, '00000201 / */
/*     data maxmag(1),maxmag(2) / '37777777, '37777577 / */

/*     Machine constants for the Cray-1. */

/*     data mcheps(1) / 0376424000000000000000b / */
/*     data mcheps(2) / 0000000000000000000000b / */

/*     data minmag(1) / 0200034000000000000000b / */
/*     data minmag(2) / 0000000000000000000000b / */

/*     data maxmag(1) / 0577777777777777777777b / */
/*     data maxmag(2) / 0000007777777777777776b / */

/*     Machine constants for the Prime 400. */

/*     data mcheps(1),mcheps(2) / :10000000000, :00000000123 / */
/*     data minmag(1),minmag(2) / :10000000000, :00000100000 / */
/*     data maxmag(1),maxmag(2) / :17777777777, :37777677776 / */

/*     Machine constants for the VAX-11. */

/*     data mcheps(1),mcheps(2) /   9472,  0 / */
/*     data minmag(1),minmag(2) /    128,  0 / */
/*     data maxmag(1),maxmag(2) / -32769, -1 / */

/*     Machine constants for IEEE machines. */

/*<       data dmach(1) /2.22044604926d-16/ >*/
/*<       data dmach(2) /2.22507385852d-308/ >*/
/*<       data dmach(3) /1.79769313485d+308/ >*/

/*<       dpmpar = dmach(i) >*/
    ret_val = dmach[(0 + (0 + ((*i__ - 1) << 3))) / 8];
/*<       return >*/
    return ret_val;

/*     Last card of function dpmpar. */

/*<       end >*/
} /* dpmpar_ */

#undef mcheps
#undef maxmag
#undef minmag
#undef dmach


#ifdef __cplusplus
        }
#endif

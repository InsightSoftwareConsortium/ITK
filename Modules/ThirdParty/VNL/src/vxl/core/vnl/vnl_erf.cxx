// This is core/vnl/vnl_erf.cxx
#include "vnl_erf.h"
//:
// \file
// \brief Complete and incomplete gamma function approximations
// \author Tim Cootes
//   Translated from NETLIB/SPECFUN/erf by Ian Scott
//   Original SPECFUN fortran based on
//   the main computation evaluates near-minimax approximations
//   from "Rational Chebyshev approximations for the error function"
//   by W. J. Cody, Math. Comp., 1969, PP. 631-638.

double vnl_erfc(double x)
{
  // Initialized data

  const double thresh = .46875;
  const double xbig = 26.543;
  const double xhuge = 6.71e7;
  const double xmax = 2.53e307;
#if 0 // unused:
  const double xneg = -26.628;
  const double xsmall = 1.11e-16;
  const double xinf = 1.79e308;
#endif // 0

  const double c[9] = { .564188496988670089,8.88314979438837594,
    66.1191906371416295,298.635138197400131,881.95222124176909,
    1712.04761263407058,2051.07837782607147,1230.33935479799725,
    2.15311535474403846e-8 };
  const double d[8] = { 15.7449261107098347,117.693950891312499,
    537.181101862009858,1621.38957456669019,3290.79923573345963,
    4362.61909014324716,3439.36767414372164,1230.33935480374942 };
  const double p[6] = { .305326634961232344,.360344899949804439,
    .125781726111229246,.0160837851487422766,6.58749161529837803e-4,
    .0163153871373020978 };
  const double q[5] = { 2.56852019228982242,1.87295284992346047,
    .527905102951428412,.0605183413124413191,.00233520497626869185 };
  const double sqrpi = .56418958354775628695;


  // Local variables
  double xden, xnum, result;
  int i;
  double y, del, ysq;

  // ------------------------------------------------------------------

  // This packet evaluates  erf(x),  erfc(x),  and  exp(x*x)*erfc(x)
  //   for a real argument  x.  It contains three FUNCTION type
  //   subprograms: ERF, ERFC, and ERFCX (or DERF, DERFC, and DERFCX),
  //   and one SUBROUTINE type subprogram, CALERF.  The calling
  //   statements for the primary entries are:
  //
  //                   Y=ERF(X)     (or   Y=DERF(X)),
  //
  //                   Y=ERFC(X)    (or   Y=DERFC(X)),
  //   and
  //                   Y=ERFCX(X)   (or   Y=DERFCX(X)).
  //
  //   The routine  CALERF  is intended for internal packet use only,
  //   all computations within the packet being concentrated in this
  //   routine.  The function subprograms invoke  CALERF  with the
  //   statement
  //
  //          CALL CALERF(ARG,RESULT,JINT)
  //
  //   where the parameter usage is as follows
  //
  //      Function                     Parameters for CALERF
  //       call              ARG                  Result          JINT
  //
  //     ERF(ARG)      ANY REAL ARGUMENT         ERF(ARG)          0
  //     ERFC(ARG)     ABS(ARG) .LT. XBIG        ERFC(ARG)         1
  //     ERFCX(ARG)    XNEG .LT. ARG .LT. XMAX   ERFCX(ARG)        2
  //
  //   The main computation evaluates near-minimax approximations
  //   from "Rational Chebyshev approximations for the error function"
  //   by W. J. Cody, Math. Comp., 1969, PP. 631-638.  This
  //   transportable program uses rational functions that theoretically
  //   approximate  erf(x)  and  erfc(x)  to at least 18 significant
  //   decimal digits.  The accuracy achieved depends on the arithmetic
  //   system, the compiler, the intrinsic functions, and proper
  //   selection of the machine-dependent constants.
  //
  // *******************************************************************
  // *******************************************************************
  //
  // Explanation of machine-dependent constants
  //
  //   XMIN   = the smallest positive floating-point number.
  //   XINF   = the largest positive finite floating-point number.
  //   XNEG   = the largest negative argument acceptable to ERFCX;
  //            the negative of the solution to the equation
  //            2*exp(x*x) = XINF.
  //   XSMALL = argument below which erf(x) may be represented by
  //            2*x/sqrt(pi)  and above which  x*x  will not underflow.
  //            A conservative value is the largest machine number X
  //            such that   1.0 + X = 1.0   to machine precision.
  //   XBIG   = largest argument acceptable to ERFC;  solution to
  //            the equation:  W(x) * (1-0.5/x**2) = XMIN,  where
  //            W(x) = exp(-x*x)/[x*sqrt(pi)].
  //   XHUGE  = argument above which  1.0 - 1/(2*x*x) = 1.0  to
  //            machine precision.  A conservative value is
  //            1/[2*sqrt(XSMALL)]
  //   XMAX   = largest acceptable argument to ERFCX; the minimum
  //            of XINF and 1/[sqrt(pi)*XMIN].
  //
  //   Approximate values for some important machines are:
  //
  //                               XMIN       XINF        XNEG     XSMALL   XBIG   XHUGE    XMAX
  //
  //  CDC 7600           (S.P.)  3.13E-294   1.26E+322   -27.220  7.11E-1  25.922  8.39E+6  1.80X+293 5
  //  CRAY-1             (S.P.)  4.58E-2467  5.45E+2465  -75.345  7.11E-1  75.326  8.39E+6  5.45E+24655
  // IEEE(IBM/XT,SUN,..) (S.P.)  1.18E-38    3.40E+38     -9.382  5.96E-8   9.194  2.90E+3  4.79E+37
  // IEEE(IBM/XT,SUN,..) (D.P.)  2.23D-308   1.79D+308   -26.628  1.11D-1  26.543  6.71D+7  2.53D+307 6
  //  IBM 195            (D.P.)  5.40D-79    7.23E+75    -13.190  1.39D-1  13.306  1.90D+8  7.23E+75  7
  //  UNIVAC 1108        (D.P.)  2.78D-309   8.98D+307   -26.615  1.73D-1  26.582  5.37D+8  8.98D+307 8
  //  VAX D-Format       (D.P.)  2.94D-39    1.70D+38     -9.345  1.39D-1   9.269  1.90D+8  1.70D+38  7
  //  VAX G-Format       (D.P.)  5.56D-309   8.98D+307   -26.615  1.11D-1  26.569  6.71D+7  8.98D+307 6

  // *******************************************************************
  // *******************************************************************

  // Error returns
  //
  //  The program returns  ERFC = 0      for  ARG .GE. XBIG;
  //
  //                       ERFCX = XINF  for  ARG .LT. XNEG;
  //      and
  //                       ERFCX = 0     for  ARG .GE. XMAX.


  // Intrinsic functions required are:
  //
  //     ABS, AINT, EXP


  //  Author: W. J. Cody
  //          Mathematics and Computer Science Division
  //          Argonne National Laboratory
  //          Argonne, IL 60439
  //
  //  Latest modification: March 19, 1990

  y = vcl_abs(x);
  // ------------------------------------------------------------------
  //  Evaluate  erfc  for  |X| <= 0.46875
  // ------------------------------------------------------------------
  if (y <= thresh)
    return 1 - vnl_erf(x);

  // ------------------------------------------------------------------
  //  Evaluate  erfc  for 0.46875 <= |X| <= 4.0
  // ------------------------------------------------------------------
  else if (y <= 4.0)
  {
    xnum = c[8] * y;
    xden = y;
    for (i = 0; i < 7; ++i)
    {
      xnum = (xnum + c[i]) * y;
      xden = (xden + d[i]) * y;
    }
    result = (xnum + c[7]) / (xden + d[7]);
    ysq = vcl_floor(y * 16.0) / 16.0;
    del = (y - ysq) * (y + ysq);
    result = vcl_exp(-ysq * ysq) * vcl_exp(-del) * result;

    // ------------------------------------------------------------------
    //  Evaluate  erfc  for |X| > 4.0
    // ------------------------------------------------------------------
  }
  else
  {
    if (y >= xhuge)
    {
      if (y < xmax)
        result = sqrpi / y;
      else
        result = 0;
    }
    else if (y >= xbig)
      result = 0;
    else
    {
      ysq = 1.0 / (y * y);
      xnum = p[5] * ysq;
      xden = ysq;
      for (unsigned i = 0; i < 4; ++i)
      {
        xnum = (xnum + p[i]) * ysq;
        xden = (xden + q[i]) * ysq;
      }
      result = ysq * (xnum + p[4]) / (xden + q[4]);
      result = (sqrpi - result) / y;
      ysq = vcl_floor(y * 16.0) / 16.0;
      del = (y - ysq) * (y + ysq);
      result = vcl_exp(-ysq * ysq) * vcl_exp(-del) * result;
    }
  }
  // ------------------------------------------------------------------
  //  Fix up for negative argument, erf, etc.
  // ------------------------------------------------------------------
  if (x < 0.0)
    result = 2.0 - result;
  return result;
}

/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

/* FEM-private copy of the EISPACK pythag routine, renamed to itkfem_pythag_.
 * v3p_netlib drops eispack under ITK_FUTURE_LEGACY_REMOVE, but the itk::fem
 * ITPACK solver (dsrc2c.c) still needs it, so FEM owns a self-contained copy
 * in every configuration. */

#include "itkfem_eispack.h"
#ifdef __cplusplus
extern "C"
{
#endif
  typedef double doublereal;
#ifndef abs
#  define abs(x) ((x) >= 0 ? (x) : -(x))
#endif
#ifndef max
#  define max(a, b) ((a) >= (b) ? (a) : (b))
#endif
#ifndef min
#  define min(a, b) ((a) <= (b) ? (a) : (b))
#endif

  /*<       double precision function pythag(a,b) >*/
  doublereal
  itkfem_pythag_(doublereal * a, doublereal * b)
  {
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3;

    /* Local variables */
    doublereal p, r__, s, t, u;

    /*<       double precision a,b >*/

    /*     finds dsqrt(a**2+b**2) without overflow or destructive underflow */

    /*<       double precision p,r,s,t,u >*/
    /*<       p = dmax1(dabs(a),dabs(b)) >*/
    /* Computing MAX */
    d__1 = abs(*a), d__2 = abs(*b);
    p = max(d__1, d__2);
    /*<       if (p .eq. 0.0d0) go to 20 >*/
    if (p == 0.)
    {
      goto L20;
    }
    /*<       r = (dmin1(dabs(a),dabs(b))/p)**2 >*/
    /* Computing MIN */
    d__2 = abs(*a), d__3 = abs(*b);
    /* Computing 2nd power */
    d__1 = min(d__2, d__3) / p;
    r__ = d__1 * d__1;
  /*<    10 continue >*/
  L10:
    /*<          t = 4.0d0 + r >*/
    t = r__ + 4.;
    /*<          if (t .eq. 4.0d0) go to 20 >*/
    if (t == 4.)
    {
      goto L20;
    }
    /*<          s = r/t >*/
    s = r__ / t;
    /*<          u = 1.0d0 + 2.0d0*s >*/
    u = s * 2. + 1.;
    /*<          p = u*p >*/
    p = u * p;
    /*<          r = (s/u)**2 * r >*/
    /* Computing 2nd power */
    d__1 = s / u;
    r__ = d__1 * d__1 * r__;
    /*<       go to 10 >*/
    goto L10;
  /*<    20 pythag = p >*/
  L20:
    ret_val = p;
    /*<       return >*/
    return ret_val;
    /*<       end >*/
  } /* itkfem_pythag_ */

#ifdef __cplusplus
}
#endif

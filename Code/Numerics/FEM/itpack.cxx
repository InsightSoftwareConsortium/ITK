/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itpack.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/**
 * \file itpack.cxx
 * \brief Routines from ITPACK numerical library.
 *
 * In this file routines from ITPACK are defined in the itk::fem::itpack
 * namespace. This is done by calling the original ITPACK code
 * (file dsrc2c.c) which was converted from Fortran using f2c.
 * See "ftp://netlib.bell-labs.com/netlib/f2c" for more info.
 * 
 * Besides that we also define some functions that the ITPACK uses and should
 * normally be defined in f2c.lib. But since we don't have that library
 * we redefine these function here, so that ITPACK code is happy.
 *
 * ITPACK was converted with the following command line options:
 *    f2c -C++P dsrc2c.f
 *
 * This produced two files:
 *   - dsrc2c.c was converted to itpack_dsrc2c.c by removing the
 *     #include "f2c" and all "#ifdef __cplusplus extern C  #endif" parts.
 *   - dsrc2c.P was used to obtain function prototypes which are stored
 *     in itpack.h file.
 *
 * To use ITPACK in your ITK code, simply include the header "itpack.h",
 * and link to the corresponding library.
 *
 * \note All ITPACK functions reside in namespace itpack.
 */




/* Turn off warnings in f2c generated code */
#if defined(_MSC_VER)
#if defined(__ICL)
#pragma warning(disable: 239 264 1011 )
#else
#pragma warning(disable: 4101 4244 4554 4756 4723)
#endif
#endif

#include <math.h>



namespace itk {
namespace fem {
namespace itpack {

/**
 * \namespace itk::fem::itpack
 * \brief ITPACK numeric library is stored in this namespace.
 */




/*
 * Required includes and typedefs for code created with f2c
 */
#include "itpack_f2c.h"




/*
 * Functions that are called within itpack
 * They are all inline, to speed things up a bit.
 */

inline double pow_dd(doublereal *ap, doublereal *bp)
{
  return(pow(*ap, *bp) );
}

inline double pow_di(doublereal *ap, integer *bp)
{
double pow, x;
integer n;
unsigned long u;

pow = 1;
x = *ap;
n = *bp;

if(n != 0)
    {
    if(n < 0)
        {
        n = -n;
        x = 1/x;
        }
    for(u = n; ; )
        {
        if(u & 01)
            pow *= x;
        if(u >>= 1)
            x *= x;
        else
            break;
        }
    }
return(pow);
}

inline double d_lg10(doublereal *x)
{
return( 0.43429448190325182765 * log(*x) );
}

inline double sqrt(doublereal x)
{
  return( ::sqrt(x) );
}

inline double log(doublereal x)
{
  return( ::log(x) );
}

inline double d_sign(doublereal *a, doublereal *b)
{
double x;
x = (*a >= 0 ? *a : - *a);
return( *b >= 0 ? x : -x);
}

inline integer i_sign(integer *a, integer *b)
{
integer x;
x = (*a >= 0 ? *a : - *a);
return( *b >= 0 ? x : -x);
}


/*
 * The following couple of functions have something to
 * do with I/O. Since we don't want ITPACK to output
 * anything, the functions do nothing.
 */
inline integer do_fio(ftnint *number, char *ptr, ftnlen len)
{
  return 0;
}

inline integer e_wsfe(void)
{
  return 0;
}

inline integer s_wsfe(cilist *a)
{
  return 0;
}

inline doublereal etime_(float *tarray)
{
  tarray[0]=0.0;
  tarray[1]=0.0;
  return 0.0;
}




/*
 * Required macros for for code obtained with f2c
 */
#define TRUE_ (1)
#define FALSE_ (0)

#define max(a,b) ((a) >= (b) ? (a) : (b))
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define abs(x) ((x) >= 0 ? (x) : -(x))




/*
 * Original ITPACK code converted from Fortran with f2c
 */
#include "itpack_dsrc2c.c"




}}} // end namespace itk::fem::itpack

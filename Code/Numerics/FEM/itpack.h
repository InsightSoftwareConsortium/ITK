/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itpack.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itpack_h
#define __itpack_h

#include "itpack_f2c.h"



/** 
 * Declarations of functions from itpack
 * NOTE: variables names are consistent with itpack to prevent confusion
 * 
 * nn - order of linear system
 * ia - row pointers used in compressed row storage format
 * ja - column indices used in compressed row storage format
 * a - nonzero entries in compressed row storage format
 * rhs - right hand side of linear system
 * u - initial guess of solution.  holds solution on output
 * iwksp - vector of length 3*nn.  on output: holds permutation vector, its inverse, and integer workspace
 * nw - on input: length of wksp, on output: actual amount used
 * wksp - vector used for real working space.  must be at least nw in length
 * iparm - vector of length 12 used to initialize various parameters on input
 * rparm - vector of length 12 used to initialize various parameters on input
 * ierr - error flag, on output: zero=no error, non-zero=error condition
 * nz - number of non-zero entries in matrix
 * iwork - vector of length nz.  matrix creation workspace
 * i - matrix index
 * j - matrix index
 * value - matrix entry
 * mode - type of matrix building
 * level - error reporting flag
 * nout - error reporting location
 * ier - error flag for matrix building
 */

typedef integer ItpackReturn;

extern "C" 
{ 

  /* fill all entires of n-sized array u with value */ 
  ItpackReturn vfill_(integer *nn, doublereal *u, doublereal *value);
  
  /* initialize sparse matrix storage */
  ItpackReturn sbini_(integer *nn, integer *nz, integer *ia, integer *ja, doublereal *a, integer *iwork);

  /* insert entry into sparse matrix */
  ItpackReturn sbsij_(integer *nn, integer *nz, integer *ia, integer *ja, doublereal *a, integer *iwork, integer *i, integer*j, doublereal *value, 
          integer *mode, integer *level, integer *nout, integer *ier);

  /* convert sparse matrix into compressed row format needed for solving */
  ItpackReturn sbend_(integer *nn, integer *nz, integer *ia, integer *ja, doublereal *a, integer *iwork );

  /* convert compressed row matrix back to linked-list representation used for adding entires */
  ItpackReturn sbagn_(integer *nn, integer *nz, integer *ia, integer *ja, doublereal *a, integer *iwork, integer *level, integer *nout, integer *ier);

  /* obtain default parameters */
  ItpackReturn dfault_(integer *iparm, doublereal *rparm);

  /* solver : jacobian conjugate gradient */
  ItpackReturn jcg_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, 
       doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);

  /* solver : jacobian semi-iteration */
  ItpackReturn jsi_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, 
       doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);

  /* solver : successive overrelaxation */
  ItpackReturn sor_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, 
       doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);

  /* solver : symmetric successive overrelaxation conjugate gradient */
  ItpackReturn ssorcg_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, 
       doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);

  /* solver : symmetric successive overrelaxation semi-iteration */
  ItpackReturn ssorsi_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, 
       doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);

  /* solver : reduced system conjugate gradient */
  ItpackReturn rscg_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, 
       doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);

  /* solver : reduced system semi-iteration */
  ItpackReturn rssi_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, 
       doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);

  ItpackReturn sum3_( integer *n,
       doublereal *c1, doublereal *x1,
       doublereal *c2, doublereal *x2,
       doublereal *c3, doublereal *x3);

}

#endif // #ifndef __itpack_h

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

namespace itk {
namespace fem {
namespace itpack {


/**
 * \file itpack.h
 * \brief Declarations of functions from itpack.
 * 
 * The following are the common parameters that are used when calling
 * functions. Note: Variables names are consistent with itpack to prevent
 * confusion.
 * 
 * \param nn Order of linear system
 * \param ia Row pointers used in compressed row storage format
 * \param ja Column indices used in compressed row storage format
 * \param a Nonzero entries in compressed row storage format
 * \param rhs Right hand side of linear system
 * \param u Initial guess of solution.  holds solution on output
 * \param iwksp Vector of length 3*nn.  on output: holds permutation vector, its inverse, and integer workspace
 * \param nw On input: length of wksp, on output: actual amount used
 * \param wksp Vector used for real working space.  must be at least nw in length
 * \param iparm Vector of length 12 used to initialize various parameters on input
 * \param rparm Vector of length 12 used to initialize various parameters on input
 * \param ierr Error flag, on output: zero=no error, non-zero=error condition
 * \param nz Number of non-zero entries in matrix
 * \param iwork Vector of length nz.  matrix creation workspace
 * \param i Matrix index
 * \param j Matrix index
 * \param value Matrix entry
 * \param mode Type of matrix building
 * \param level Error reporting flag
 * \param nout Error reporting location
 * \param ier Error flag for matrix building
 */



/**
 * jacobian conjugate gradient
 * \param ia array of row offsets
 * \param ja array of column indices
 * \param a array of matrix values
 * \param rhs array of right hand side of system
 * \param u solution array
 * \param iwksp int array used by itpack for calculations
 * \param nw size of iwskp and wksp
 * \param wksp double array used by itpack for calculations
 * \param iparm array of 12 integer parameters
 * \param rparm array of 12 double parameters
 * \param ierr holds error flag on return
 */
extern int jcg_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);


/**
 * jacobian semi-iteration
 * \param ia array of row offsets
 * \param ja array of column indices
 * \param a array of matrix values
 * \param rhs array of right hand side of system
 * \param u solution array
 * \param iwksp int array used by itpack for calculations
 * \param nw size of iwskp and wksp
 * \param wksp double array used by itpack for calculations
 * \param iparm array of 12 integer parameters
 * \param rparm array of 12 double parameters
 * \param ierr holds error flag on return
 */
extern int jsi_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);


/**
 * successive overrelaxation
 * \param ia array of row offsets
 * \param ja array of column indices
 * \param a array of matrix values
 * \param rhs array of right hand side of system
 * \param u solution array
 * \param iwksp int array used by itpack for calculations
 * \param nw size of iwskp and wksp
 * \param wksp double array used by itpack for calculations
 * \param iparm array of 12 integer parameters
 * \param rparm array of 12 double parameters
 * \param ierr holds error flag on return
 */
extern int sor_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);


/**
 * symmetric successive overrelaxation conjugate gradient
 * \param ia array of row offsets
 * \param ja array of column indices
 * \param a array of matrix values
 * \param rhs array of right hand side of system
 * \param u solution array
 * \param iwksp int array used by itpack for calculations
 * \param nw size of iwskp and wksp
 * \param wksp double array used by itpack for calculations
 * \param iparm array of 12 integer parameters
 * \param rparm array of 12 double parameters
 * \param ierr holds error flag on return
 */
extern int ssorcg_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);


/** 
 * symmetric successive overrelaxation semi-iteration
 * \param ia array of row offsets
 * \param ja array of column indices
 * \param a array of matrix values
 * \param rhs array of right hand side of system
 * \param u solution array
 * \param iwksp int array used by itpack for calculations
 * \param nw size of iwskp and wksp
 * \param wksp double array used by itpack for calculations
 * \param iparm array of 12 integer parameters
 * \param rparm array of 12 double parameters
 * \param ierr holds error flag on return
 */
extern int ssorsi_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);


/**
 * reduced system conjugate gradient
 * \param ia array of row offsets
 * \param ja array of column indices
 * \param a array of matrix values
 * \param rhs array of right hand side of system
 * \param u solution array
 * \param iwksp int array used by itpack for calculations
 * \param nw size of iwskp and wksp
 * \param wksp double array used by itpack for calculations
 * \param iparm array of 12 integer parameters
 * \param rparm array of 12 double parameters
 * \param ierr holds error flag on return
 */
extern int rscg_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);


/**
 * reduced system semi-iteration
 * \param ia array of row offsets
 * \param ja array of column indices
 * \param a array of matrix values
 * \param rhs array of right hand side of system
 * \param u solution array
 * \param iwksp int array used by itpack for calculations
 * \param nw size of iwskp and wksp
 * \param wksp double array used by itpack for calculations
 * \param iparm array of 12 integer parameters
 * \param rparm array of 12 double parameters
 * \param ierr holds error flag on return
 */
extern int rssi_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr);


extern int itjcg_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *u1, doublereal *d__, doublereal *d1, doublereal *dtwd, doublereal *tri);
extern int itjsi_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, doublereal *u1, doublereal *d__, integer *icnt);
extern int itsor_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, doublereal *wk);
extern int itsrcg_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, doublereal *u1, doublereal *c__, doublereal *c1, doublereal *d__, doublereal *dl, doublereal *wk, doublereal *tri);
extern int itsrsi_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, doublereal *u1, doublereal *c__, doublereal *d__, doublereal *ctwd, doublereal *wk);
extern int itrscg_(integer *n, integer *nnb, integer *ia, integer *ja, doublereal *a, doublereal *ub, doublereal *ub1, doublereal *db, doublereal *db1, doublereal *wb, doublereal *tri);
extern int itrssi_(integer *n, integer *nnb, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *ub, doublereal *ub1, doublereal *db);
extern integer bisrch_(integer *n, integer *k, integer *l);
extern doublereal cheby_(doublereal *qa, doublereal *qt, doublereal *rrr, integer *ip, doublereal *cme, doublereal *sme);
extern int chgcon_(doublereal *tri, doublereal *gamold, doublereal *rhoold, integer *ibmth);
extern int chgsi_(doublereal *dtnrm, integer *ibmth);
extern logical chgsme_(doublereal *oldnrm, integer *icnt);
extern int daxpy_(integer *n, doublereal *da, doublereal *dx, integer *incx, doublereal *dy, integer *incy);
extern int dcopy_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy);
extern doublereal ddot_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy);
extern doublereal determ_(integer *n, doublereal *tri, doublereal *xlmda);


/**
  * Obtain default parameters
  * \param iparm array of 12 integer parameters
  * \param rparm array of 12 double parameters
  */
extern int dfault_(integer *iparm, doublereal *rparm);


extern int echall_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, integer *iparm, doublereal *rparm, integer *icall);
extern int echout_(integer *iparm, doublereal *rparm, integer *imthd);
extern doublereal eigvns_(integer *n, doublereal *tri, doublereal *d__, doublereal *e2, integer *ier);
extern doublereal eigvss_(integer *n, doublereal *tri, doublereal *start, doublereal *zeta, integer *itmax, integer *ier);
extern int eqrt1s_(doublereal *d__, doublereal *e2, integer *nn, integer *m, integer *isw, integer *ierr);
extern integer ipstr_(doublereal *omega);
extern int iterm_(integer *nn, doublereal *a, doublereal *u, doublereal *wk, integer *imthdd);
extern int ivfill_(integer *n, integer *iv, integer *ival);
extern int omeg_(doublereal *dnrm, integer *iflag);
extern logical omgchg_(integer *ndummy);
extern logical omgstr_(integer *ndummy);
extern int parcon_(doublereal *dtnrm, doublereal *c1, doublereal *c2, doublereal *c3, doublereal *c4, doublereal *gamold, doublereal *rhotmp, integer *ibmth);
extern int parsi_(doublereal *c1, doublereal *c2, doublereal *c3, integer *ibmth);
extern doublereal pbeta_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *v, doublereal *w1, doublereal *w2);
extern int pbsor_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *rhs);
extern int permat_(integer *nn, integer *ia, integer *ja, doublereal *a, integer *p, integer *newia, integer *isym, integer *level, integer *nout, integer *ierr);
extern int perror_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, doublereal *w, doublereal *digtt1, doublereal *digtt2, integer *idgtts);
extern int pervec_(integer *n, doublereal *v, integer *p);
extern int pfsor_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *rhs);
extern int pfsor1_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *rhs);
extern int pjac_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *rhs);
extern int pmult_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *w);
extern int prbndx_(integer *nn, integer *nblack, integer *ia, integer *ja, integer *p, integer *ip, integer *level, integer *nout, integer *ier);
extern int prsblk_(integer *nnb, integer *nnr, integer *ia, integer *ja, doublereal *a, doublereal *ur, doublereal *vb);
extern int prsred_(integer *nnb, integer *nnr, integer *ia, integer *ja, doublereal *a, doublereal *ub, doublereal *vr);
extern int pssor1_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *rhs, doublereal *fr, doublereal *br);
extern int pstop_(integer *n, doublereal *u, doublereal *dnrm, doublereal *ccon, integer *iflag, logical *q1);
extern doublereal pvtbv_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *v);
extern int qsort_(integer *nn, integer *key, doublereal *data, integer *error);


/**
 * Convert compressed row matrix back to linked-list representation used for adding entires
 * \param nn order of matrix
 * \param nz maximum number of non-zero values
 * \param ia array of row offsets
 * \param ja array of column indices
 * \param a array of matrix values
 * \param iwork workspace array used by itpack
 * \param i row index of value to add
 * \param j column index of value to add
 * \param value value to add
 * \param mode flag for type of adding to be done
 * \param level specifier for level of output
 * \param nout specifier for output
 * \param ier holds error flag on return
 */
extern int sbagn_(integer *n, integer *nz, integer *ia, integer *ja, doublereal *a, integer *iwork, integer *levell, integer *noutt, integer *ierr);
extern int sbelm_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, integer *iw, doublereal *rw, doublereal *tol, integer *isym, integer *level, integer *nout, integer *ier);


/**
 * Finalize matrix storage format
 * \param nn order of matrix
 * \param nz maximum number of non-zero values
 * \param ia array of row offsets
 * \param ja array of column indices
 * \param a array of matrix values
 * \param iwork workspace array used by itpack
 */
extern int sbend_(integer *n, integer *nz, integer *ia, integer *ja, doublereal *a, integer *iwork);


/**
 * Initialize sparse matrix storage 
 * \param nn order of matrix
 * \param nz maximum number of non-zero values
 * \param ia array of row offsets
 * \param ja array of column indices
 * \param a array of matrix values
 * \param i work workspace array used by itpack
 */
extern int sbini_(integer *n, integer *nz, integer *ia, integer *ja, doublereal *a, integer *iwork);


/**
 * Insert entry into sparse matrix 
 * \param nn order of matrix
 * \param nz maximum number of non-zero values
 * \param ia array of row offsets
 * \param ja array of column indices
 * \param a array of matrix values
 * \param iwork workspace array used by itpack
 * \param i row index of value to add
 * \param j column index of value to add
 * \param value value to add
 * \param mode flag for type of adding to be done
 * \param level specifier for level of output
 * \param nout specifier for output
 * \param ier holds error flag on return
 */
extern int sbsij_(integer *n, integer *nz, integer *ia, integer *ja, doublereal *a, integer *iwork, integer *ii, integer *jj, doublereal *vall, integer *mode, integer *levell, integer *noutt, integer *ierr);


extern int scal_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, doublereal *d__, integer *level, integer *nout, integer *ier);
extern int sum3_(integer *n, doublereal *c1, doublereal *x1, doublereal *c2, doublereal *x2, doublereal *c3, doublereal *x3);
extern doublereal tau_(integer *ii);
extern E_f timer_(real *timdmy);
extern logical tstchg_(integer *ibmth);
extern int unscal_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, doublereal *d__);
extern int vevmw_(integer *n, doublereal *v, doublereal *w);
extern int vevpw_(integer *n, doublereal *v, doublereal *w);


/** 
 * Fill all entires of nn-sized array u with value
 * \param nn size of array
 * \param u array
 * \param value value to fill array with
 */ 
extern int vfill_(integer *n, doublereal *v, doublereal *val);
extern int vout_(integer *n, doublereal *v, integer *iswt, integer *noutt);
extern int wevmw_(integer *n, doublereal *v, doublereal *w);
extern int zbrent_(integer *n, doublereal *tri, doublereal *eps, integer *nsig, doublereal *aa, doublereal *bb, integer *maxfnn, integer *ier);




}}} // end namespace itk::fem::itpack

#endif // #ifndef __itpack_h

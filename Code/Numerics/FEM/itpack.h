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
 * nn Order of linear system
 * a Row pointers used in compressed row storage format
 * m ja Column indices used in compressed row storage format
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
 * Jacobian conjugate gradient
 *
 * \param nn Order of linear system
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
 * Jacobian semi-iteration
 *
 * \param nn Order of linear system
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
 * Successive overrelaxation
 *
 * \param nn Order of linear system
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
 * Symmetric successive overrelaxation conjugate gradient
 *
 * \param nn Order of linear system
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
 * Symmetric successive overrelaxation semi-iteration
 *
 * \param nn Order of linear system
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
 * Reduced system conjugate gradient
 *
 * \param nn Order of linear system
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
 * Reduced system semi-iteration
 *
 * \param nn Order of linear system
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


/** 
 * Performs one iteration of the jacobian conjugate gradiant method
 * \param nn Order of linear system 
 * \param ia array of row offsets
 * \param ja array of column indices     
 * \param a array of matrix values
 * \param u input version of solution vector 
 * \param u1 value of solution vector at the end of the in-1 iteration, gets filled with newest estimate 
 * \param d__  solution vector, gets filled with pseudo-residual vector after in iterations 
 * \param d1 pseudo-residual vector after in-1 iterations, gets filled with newest pseudo-residual vector
 * \param dtwd used in computation of acceleration parameeter gamma and the pseudo-residual
 * \param tri stores the tridiagonal matrix associated with the eigenvalues of the conjugate gradient ploynomial 
 */
extern int itjcg_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *u1, doublereal *d__, doublereal *d1, doublereal *dtwd, doublereal *tri);

/** 
 * Performs one iteration of the jacobian semi-iteration method
 * \param nn Order of linear system 
 * \param ia array of row offsets
 * \param ja array of column indices     
 * \param a array of matrix values
 * \param rhs array of right hand side of linear system
 * \param u solution estimate at in iterations 
 * \param u1 value of solution vector at the end of the in-1 iteration, gets filled with newest estimate 
 * \param d__  solution vector, gets filled with pseudo-residual vector after in iterations 
 * \param icnt number of iterations since last change in solution estimate
 */
extern int itjsi_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, doublereal *u1, doublereal *d__, integer *icnt);


/** 
 * Performs one iteration of the successive overrelaxation method
 * \param nn Order of linear system 
 * \param ia array of row offsets
 * \param ja array of column indices    
 * \param a array of matrix values 
 * \param rhs array of right hand side of linear system
 * \param u solution estimate array
 * \param wk work array of length nn
 */
extern int itsor_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, doublereal *wk);


/** 
 * Performs one iteration of the symmetric successive overrelaxation conjugate gradient method
 * \param nn Order of linear system 
 * \param ia array of row offsets
 * \param ja array of column indices     
 * \param a array of matrix values
 * \param rhs array of right hand side of linear system
 * \param u solution estimate at in iterations,
 * \param u1 value of solution vector at the end of the in-1 iteration, gets filled with newest estimate 
 * \param c__  forward residual after in iterations
 * \param c1 forward residual after in-1 iterations, filled with in+1 values
 * \param d__ used to compute backward pseudo-residual for current iteration
 * \param dl used in computation of acceleration parameters
 * \param wk work array of length nn
 * \param tri stores the tridiagonal matrix associated with the eigenvalues of the conjugate gradient ploynomial 
 */
extern int itsrcg_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, doublereal *u1, doublereal *c__, doublereal *c1, doublereal *d__, doublereal *dl, doublereal *wk, doublereal *tri);


/** 
 * Performs one iteration of the symmetric successive overrelaxation semi-iteration method
 * \param nn Order of linear system 
 * \param ia array of row offsets
 * \param ja array of column indices     
 * \param a array of matrix values
 * \param rhs array of right hand side of linear system
 * \param u solution estimate at in-1 iterations
 * \param u1 value of solution vector at the end of the in-1 iteration, gets filled with newest estimate 
 * \param c__ used to compute forward pseudo-residual vector
 * \param d__ used to compute backward pseudo-residual vector 
 * \param ctwd used in computation of acceleration parameters
 * \param wk work array of length nn
 */
extern int itsrsi_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, doublereal *u1, doublereal *c__, doublereal *d__, doublereal *ctwd, doublereal *wk);


/** 
 * Performs one iteration of the reduced system conjugate gradient method
 * \param n Order of linear system 
 * \param nnb Number of black points
 * \param ia array of row offsets
 * \param ja array of column indices     
 * \param a array of matrix values
 * \param ub estimate for the solution of black points after in iterations
 * \param ub1 estimate for the solution of black points after in-1 iterations, filled with in+1 values
 * \param db pseudo-residual of black points after in iterations
 * \param db1 pseudo-residual of black points after in-1 iterations, filled with in+1 values
 * \param wb used in computation involving black vector
 * \param tri stores the tridiagonal matrix associated with the eigenvalues of the conjugate gradient ploynomial 
 */
extern int itrscg_(integer *n, integer *nnb, integer *ia, integer *ja, doublereal *a, doublereal *ub, doublereal *ub1, doublereal *db, doublereal *db1, doublereal *wb, doublereal *tri);


/** 
 * Performs one iteration of the reduced system semi-iteration method
 * \param n Order of linear system 
 * \param nnb Number of black points
 * \param ia array of row offsets
 * \param ja array of column indices     
 * \param a array of matrix values
 * \param rhs array of right hand side of linear system
 * \param ub estimate for the solution of black points after in iterations
 * \param ub1 pseudo-residual of black points after in-1 iterations, filled with in+1 values
 * \param db current residual
 */
extern int itrssi_(integer *n, integer *nnb, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *ub, doublereal *ub1, doublereal *db);


/**
 * Function which uses a bisection search to find the entry j in the
 * array k such that the value l is greater than or equal to k[j]
 * and strictly less than k[j+1]
 * \param n since of array
 * \param k integer array to search
 * \param l searching criterion
 */
extern integer bisrch_(integer *n, integer *k, integer *l);

/**
 * Computes the solution to the chebyshev equation
 * \param qa ratio of pseudo-residuals
 * \param qt virtual spectral radius
 * \param rrr adaptive parameter
 * \param ip number of iterations since last change of parameters
 * \param cme estimate for largest eigen-value of iteration matrix
 * \param sme estimate for smallest eigen-value of iteration matrix
 */
extern doublereal cheby_(doublereal *qa, doublereal *qt, doublereal *rrr, integer *ip, doublereal *cme, doublereal *sme);

/**
 * Computes estimate for largest eigenvalue for conjugate gradient acceleration
 * \param tri tridiagonal matrix associated with the eigenvalues of the conjugate gradient polynomial
 * \param gamold previous value of acceleration parameters
 * \param rhoold previous value of acceleration parameters
 * \param ibmth flag indicating method being accelerated by conjugate gradient
 *        1 - jacobian
 *        2 - reduced system
 *        3 - ssor
 */
extern int chgcon_(doublereal *tri, doublereal *gamold, doublereal *rhoold, integer *ibmth);

/*
 *
 *
 */
extern int chgsi_(doublereal *dtnrm, integer *ibmth);

/*
 *
 *
 */
extern logical chgsme_(doublereal *oldnrm, integer *icnt);

/*
 *
 *
 */
extern int daxpy_(integer *n, doublereal *da, doublereal *dx, integer *incx, doublereal *dy, integer *incy);

/*
 *
 *
 */
extern int dcopy_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy);

/*
 *
 *
 */
extern doublereal ddot_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy);

/*
 *
 *
 */
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

/**
 * Fill integer array with a value
 * \param n number of elements in array
 * \param iv pointer to integer array
 * \param ival value to fill array with
 */
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
extern int sbend_(integer *nn, integer *nz, integer *ia, integer *ja, doublereal *a, integer *iwork);


/**
 * Initialize sparse matrix storage 
 * \param nn order of matrix
 * \param nz maximum number of non-zero values
 * \param ia array of row offsets
 * \param ja array of column indices
 * \param a array of matrix values
 * \param iwork workspace array used by itpack
 */
extern int sbini_(integer *nn, integer *nz, integer *ia, integer *ja, doublereal *a, integer *iwork);


/**
 * Insert entry into sparse matrix 
 * \param nn order of matrix
 * \param nz maximum number of non-zero values
 * \param ia array of row offsets
 * \param ja array of column indices
 * \param a array of matrix values
 * \param iwork workspace array used by itpack
 * \param ii row index of value to add
 * \param jj column index of value to add
 * \param vall value to add
 * \param mode flag for type of adding to be done
 * \param levell specifier for level of output
 * \param noutt specifier for output
 * \param ierr holds error flag on return
 */
extern int sbsij_(integer *nn, integer *nz, integer *ia, integer *ja, doublereal *a, integer *iwork, integer *ii, integer *jj, doublereal *vall, integer *mode, integer *levell, integer *noutt, integer *ierr);


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
 * \param n size of array
 * \param v array
 * \param val value to fill array with
 */ 
extern int vfill_(integer *n, doublereal *v, doublereal *val);

extern int vout_(integer *n, doublereal *v, integer *iswt, integer *noutt);
extern int wevmw_(integer *n, doublereal *v, doublereal *w);
extern int zbrent_(integer *n, doublereal *tri, doublereal *eps, integer *nsig, doublereal *aa, doublereal *bb, integer *maxfnn, integer *ier);




}}} // end namespace itk::fem::itpack

#endif // #ifndef __itpack_h

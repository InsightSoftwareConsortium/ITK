#ifndef vnl_real_eigensystem_h_
#define vnl_real_eigensystem_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_real_eigensystem - Unsymmetric real eigensystem
// .LIBRARY	vnl-algo
// .HEADER	vxl Package
// .INCLUDE	vnl/algo/vnl_real_eigensystem.h
// .FILE	vnl_real_eigensystem.cxx
// .EXAMPLE	../examples/vnl_planefit.cxx
//
// .SECTION Description
//    vnl_eigensystem is a full-bore real eigensystem.  If your matrix is symmetric,
//    it is *much* better to use vnl_symmetric_eigensystem.
//
// .SECTION Author:
//    Andrew W. Fitzgibbon, Oxford RRG, 23 Jan 97

#include <vnl/vnl_complex.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>

class vnl_real_eigensystem {
public:
  vnl_real_eigensystem(const vnl_matrix<double>& M);
  
public:
  vnl_matrix<double> Vreal;
  
  // -- Output matrix of eigenvectors, which will in general be complex.
  vnl_matrix<vnl_double_complex> V;
  
  // -- Output diagonal matrix of eigenvalues.
  vnl_diag_matrix<vnl_double_complex> D;
};

#endif // vnl_real_eigensystem_h_

#ifndef vnl_real_eigensystem_h_
#define vnl_real_eigensystem_h_
#ifdef __GNUC__
#pragma interface
#endif

//:
//  \file
//  \brief Extract eigensystem of unsymmetric matrix M, using EISPACK
//  \author Andrew W. Fitzgibbon, Oxford RRG, 23 Jan 97
//
//  Modifications
//  dac (Manchester) 28/03/2001: tidied up documentation
//

#include <vcl_complex.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>

//: Extract eigensystem of unsymmetric matrix M, using the EISPACK routine
//  vnl_eigensystem is a full-bore real eigensystem.  If your matrix 
//  is symmetric, it is *much* better to use vnl_symmetric_eigensystem.

class vnl_real_eigensystem {
public:
  vnl_real_eigensystem(const vnl_matrix<double>& M);
  
public:
  vnl_matrix<double> Vreal;
  
  //: Output matrix of eigenvectors, which will in general be complex.
  vnl_matrix<vcl_complex<double> > V;
  
  //: Output diagonal matrix of eigenvalues.
  vnl_diag_matrix<vcl_complex<double> > D;
};

#endif // vnl_real_eigensystem_h_

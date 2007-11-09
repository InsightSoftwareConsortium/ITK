// This is core/vnl/algo/vnl_real_eigensystem.h
#ifndef vnl_real_eigensystem_h_
#define vnl_real_eigensystem_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Extract eigensystem of unsymmetric matrix M, using EISPACK
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   23 Jan 97
//
// \verbatim
// Modifications
//  dac (Manchester) 28/03/2001: tidied up documentation
// \endverbatim
//

#include <vcl_complex.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>

//: Extract eigensystem of unsymmetric matrix M, using the EISPACK routine
//  vnl_eigensystem is a full-bore real eigensystem.  If your matrix
//  is symmetric, it is *much* better to use vnl_symmetric_eigensystem.

class vnl_real_eigensystem
{
 public:
  vnl_real_eigensystem(vnl_matrix<double> const& M);

 public:
  vnl_matrix<double> Vreal;

  //: Output matrix of eigenvectors, which will in general be complex.
  vnl_matrix<vcl_complex<double> > V;

  //: Output diagonal matrix of eigenvalues.
  vnl_diag_matrix<vcl_complex<double> > D;
};

#endif // vnl_real_eigensystem_h_

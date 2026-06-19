// This is core/vnl/algo/vnl_real_eigensystem.h
#ifndef vnl_real_eigensystem_h_
#define vnl_real_eigensystem_h_

// ITK deprecation shim: the netlib EISPACK engine (rg) behind vnl_real_eigensystem
// is being retired; itk::RealEigenDecomposition (Eigen-backed) is the supported
// replacement. The guard is active only when itkConfigure.h is reachable (an ITK
// consumer), so ITK's own VXL build is unaffected.
#if __has_include(<itkConfigure.h>)
#  include <itkConfigure.h>
#  if defined(ITK_FUTURE_LEGACY_REMOVE)
#    error "vnl/algo/vnl_real_eigensystem.h is deprecated; migrate to itk::RealEigenDecomposition (itkRealEigenDecomposition.h, Eigen-backed)."
#  elif defined(ITK_LEGACY_REMOVE) && !defined(ITK_LEGACY_SILENT) && !defined(ITK_LEGACY_TEST)
#    if defined(_MSC_VER)
#      pragma message("vnl/algo/vnl_real_eigensystem.h is deprecated; migrate to itk::RealEigenDecomposition (itkRealEigenDecomposition.h, Eigen-backed).")
#    else
#      warning "vnl/algo/vnl_real_eigensystem.h is deprecated; migrate to itk::RealEigenDecomposition (itkRealEigenDecomposition.h, Eigen-backed)."
#    endif
#  endif
#endif
//:
// \file
// \brief Extract eigensystem of non-symmetric matrix M, using EISPACK
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   23 Jan 97
//
// \verbatim
//  Modifications
//   dac (Manchester) 28/03/2001: tidied up documentation
// \endverbatim
//

#include <complex>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>

#include <vnl/algo/vnl_algo_export.h>

//: Extract eigensystem of asymmetric matrix M, using the EISPACK routine
//  vnl_eigensystem is a full-bore real eigensystem.  If your matrix
//  is symmetric, it is \e much better to use \sa vnl_symmetric_eigensystem.

class VNL_ALGO_EXPORT vnl_real_eigensystem
{
public:
  vnl_real_eigensystem(const vnl_matrix<double> & M);

public:
  vnl_matrix<double> Vreal;

  //: Output matrix of eigenvectors, which will in general be complex.
  vnl_matrix<std::complex<double>> V;

  //: Output diagonal matrix of eigenvalues.
  vnl_diag_matrix<std::complex<double>> D;
};

#endif // vnl_real_eigensystem_h_

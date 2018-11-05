// This is core/vnl/algo/vnl_gaussian_kernel_1d.h
#ifndef vnl_gaussian_kernel_1d_h_
#define vnl_gaussian_kernel_1d_h_
//:
// \file
// \brief Holds one half of a discretely sampled 1D gaussian distribution
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   07 Aug 97
//
// \verbatim
// Modifications
//  970807 AWF Initial version.
//  dac (Manchester) 28/03/2001: tidied up documentation
// \endverbatim

#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_algo_export.h>

//: Holds one half of a discretely sampled 1D gaussian distribution
class VNL_ALGO_EXPORT vnl_gaussian_kernel_1d
{
 public:
  // Constructors/Destructors--------------------------------------------------
  vnl_gaussian_kernel_1d(double sigma, double cutoff = 0.5/256.0);

  double G(double x) const;

  int width() const { return vec_.size(); }
  double operator [] (int i) const { return vec_[i]; }

 protected:
  // Data Members--------------------------------------------------------------
  vnl_vector<double> vec_;
  double inscale_;
};

#endif // vnl_gaussian_kernel_1d_h_

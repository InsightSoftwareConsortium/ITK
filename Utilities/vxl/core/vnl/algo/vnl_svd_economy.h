// This is core/vnl/algo/vnl_svd_economy.h
#ifndef vnl_svd_economy_h_
#define vnl_svd_economy_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief SVD wrapper that doesn't compute the left singular vectors, U.
// \author David Capel (d.capel@2d3.com)
// \date 04 Mar 03
//
// The cost of SVD of an m*n matrix increases with O(m^2) if computation
// of U is required, but only O(m) if not.

#include <vnl/vnl_numeric_traits.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

template <class real_t>
class vnl_svd_economy
{
 public:
  //: The singular values of a matrix of complex<T> are of type T, not complex<T>
  typedef typename vnl_numeric_traits<real_t>::abs_t singval_t;

  vnl_svd_economy(vnl_matrix<real_t> const& M);

  //: Return right singular vectors.
  vnl_matrix<real_t> const& V() const { return V_; }
  vnl_matrix<real_t>      & V()       { return V_; }

  //: Return singular values in decreasing order.
  vnl_vector<singval_t> const& lambdas() const { return sv_; }
  vnl_vector<singval_t>      & lambdas()       { return sv_; }

  //: Return the rightmost column of V.
  vnl_vector<real_t> nullvector();

 protected:
  int m_, n_;
  vnl_matrix<real_t>    V_;
  vnl_vector<singval_t> sv_;

 private:
  vnl_svd_economy( vnl_svd_economy<real_t> const&) { }
  vnl_svd_economy<real_t>& operator=(vnl_svd_economy<real_t> const&) { return *this; }
};

#endif

#include <vnl/algo/vnl_svd.txx>

#include <vnl/vnl_copy.h>

VCL_DEFINE_SPECIALIZATION
vnl_svd<long double>::vnl_svd(vnl_matrix<long double> const& M, double):
  m_(M.rows()),
  n_(M.columns()),
  U_(m_, n_),
  W_(n_),
  Winverse_(n_),
  V_(n_, n_)
{
  vnl_matrix<double> M_(m_, n_);
  for (int i=0; i<m_; ++i)
    for (int j=0; j<n_; ++j)
      M_[i][j] = M[i][j];
  
  vnl_svd<double> svd(M_);

  vnl_copy(svd.U(), U_);
  vnl_copy(svd.W(), W_);
  vnl_copy(svd.V(), V_);
}

VNL_SVD_INSTANTIATE(long double);

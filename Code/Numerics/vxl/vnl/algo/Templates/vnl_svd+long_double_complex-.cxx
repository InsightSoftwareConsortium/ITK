#include <vcl_complex.h>
#include <vnl/algo/vnl_svd.txx>

#include <vnl/vnl_copy.h>

VCL_DEFINE_SPECIALIZATION
vnl_svd<vcl_complex<long double> >::vnl_svd(vnl_matrix<vcl_complex<long double> > const& M, double):
  m_(M.rows()),
  n_(M.columns()),
  U_(m_, n_),
  W_(n_),
  Winverse_(n_),
  V_(n_, n_)
{
  vnl_matrix<vcl_complex<double> > M_(m_, n_);
  for (int i=0; i<m_; ++i)
    for (int j=0; j<n_; ++j)
      M_[i][j] = vcl_complex<double>(M[i][j].real(), M[i][j].imag());
  
  vnl_svd<vcl_complex<double> > svd(M_);

  vnl_copy(svd.U(), U_);
  vnl_copy(svd.W(), W_);
  vnl_copy(svd.V(), V_);
}

VNL_SVD_INSTANTIATE(vcl_complex<long double>);

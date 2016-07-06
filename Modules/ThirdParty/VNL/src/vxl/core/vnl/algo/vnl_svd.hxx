// This is core/vnl/algo/vnl_svd.hxx
#ifndef vnl_svd_hxx_
#define vnl_svd_hxx_
//:
// \file

#include <cstdlib>
#include <complex>
#include <iostream>
#include <algorithm>
#include "vnl_svd.h"

#include <vcl_cassert.h>
#include <vcl_compiler.h>

#include <vnl/vnl_math.h>
#include <vnl/vnl_fortran_copy.h>
#include <vnl/algo/vnl_netlib.h> // dsvdc_()

// use C++ overloading to call the right linpack routine from the template code :
#define macro(p, T) \
inline void vnl_linpack_svdc(vnl_netlib_svd_proto(T)) \
{ v3p_netlib_##p##svdc_(vnl_netlib_svd_params); }
macro(s, float);
macro(d, double);
macro(c, std::complex<float>);
macro(z, std::complex<double>);
#undef macro

//--------------------------------------------------------------------------------

static bool vnl_svd_test_heavily = false;
#include <vnl/vnl_matlab_print.h>

template <class T>
vnl_svd<T>::vnl_svd(vnl_matrix<T> const& M, double zero_out_tol):
  m_(M.rows()),
  n_(M.columns()),
  U_(m_, n_),
  W_(n_),
  Winverse_(n_),
  V_(n_, n_)
{
  assert(m_ > 0);
  assert(n_ > 0);

  {
    long n = M.rows();
    long p = M.columns();
    long mm = std::min(n+1L,p);

    // Copy source matrix into fortran storage
    // SVD is slow, don't worry about the cost of this transpose.
    vnl_fortran_copy<T> X(M);

    // Make workspace vectors.
    vnl_vector<T> work(n, T(0));
    vnl_vector<T> uspace(n*p, T(0));
    vnl_vector<T> vspace(p*p, T(0));
    vnl_vector<T> wspace(mm, T(0)); // complex fortran routine actually _wants_ complex W!
    vnl_vector<T> espace(p, T(0));

    // Call Linpack SVD
    long info = 0;
    const long job = 21; // min(n,p) svs in U, n svs in V (i.e. economy size)
    vnl_linpack_svdc((T*)X, &n, &n, &p,
                     wspace.data_block(),
                     espace.data_block(),
                     uspace.data_block(), &n,
                     vspace.data_block(), &p,
                     work.data_block(),
                     &job, &info);

    // Error return?
    if (info != 0)
    {
      // If info is non-zero, it contains the number of singular values
      // for this the SVD algorithm failed to converge. The condition is
      // not bogus. Even if the returned singular values are sensible,
      // the singular vectors can be utterly wrong.

      // It is possible the failure was due to NaNs or infinities in the
      // matrix. Check for that now.
      M.assert_finite();

      // If we get here it might be because
      // 1. The scalar type has such
      // extreme precision that too few iterations were performed to
      // converge to within machine precision (that is the svdc criterion).
      // One solution to that is to increase the maximum number of
      // iterations in the netlib code.
      //
      // 2. The LINPACK dsvdc_ code expects correct IEEE rounding behaviour,
      // which some platforms (notably x86 processors)
      // have trouble doing. For example, gcc can output
      // code in -O2 and static-linked code that causes this problem.
      // One solution to this is to persuade gcc to output slightly different code
      // by adding and -fPIC option to the command line for v3p/netlib/dsvdc.c. If
      // that doesn't work try adding -ffloat-store, which should fix the problem
      // at the expense of being significantly slower for big problems. Note that
      // if this is the cause, core/vnl/tests/test_svd should have failed.
      //
      // You may be able to diagnose the problem here by printing a warning message.
      std::cerr << __FILE__ ": suspicious return value (" << info << ") from SVDC\n"
               << __FILE__ ": M is " << M.rows() << 'x' << M.cols() << std::endl;

      vnl_matlab_print(std::cerr, M, "M", vnl_matlab_print_format_long);
      valid_ = false;
    }
    else
      valid_ = true;

    // Copy fortran outputs into our storage
    {
      const T *d = uspace.data_block();
      for (int j = 0; j < p; ++j)
        for (int i = 0; i < n; ++i)
          U_(i,j) = *d++;
    }

    for (int j = 0; j < mm; ++j)
      W_(j, j) = std::abs(wspace(j)); // we get rid of complexness here.

    for (int j = mm; j < n_; ++j)
      W_(j, j) = 0;

    {
      const T *d = vspace.data_block();
      for (int j = 0; j < p; ++j)
        for (int i = 0; i < p; ++i)
          V_(i,j) = *d++;
    }
  }

  if (vnl_svd_test_heavily)
  {
    // Test that recomposed matrix == M
    typedef typename vnl_numeric_traits<T>::abs_t abs_t;
    abs_t recomposition_residual = std::abs((recompose() - M).fro_norm());
    abs_t n = std::abs(M.fro_norm());
    abs_t thresh = abs_t(m_) * abs_t(vnl_math::eps) * n;
    if (recomposition_residual > thresh)
    {
      std::cerr << "vnl_svd<T>::vnl_svd<T>() -- Warning, recomposition_residual = "
               << recomposition_residual << std::endl
               << "fro_norm(M) = " << n << std::endl
               << "eps*fro_norm(M) = " << thresh << std::endl
               << "Press return to continue\n";
      char x;
      std::cin.get(&x, 1, '\n');
    }
  }

  if (zero_out_tol >= 0)
    // Zero out small sv's and update rank count.
    zero_out_absolute(double(+zero_out_tol));
  else
    // negative tolerance implies relative to max elt.
    zero_out_relative(double(-zero_out_tol));
}

template <class T>
std::ostream& operator<<(std::ostream& s, const vnl_svd<T>& svd)
{
  s << "vnl_svd<T>:\n"
//  << "M = [\n" << M << "]\n"
    << "U = [\n" << svd.U() << "]\n"
    << "W = " << svd.W() << '\n'
    << "V = [\n" << svd.V() << "]\n"
    << "rank = " << svd.rank() << std::endl;
  return s;
}

//-----------------------------------------------------------------------------
// Chunky bits.

//: find weights below threshold tol, zero them out, and update W_ and Winverse_
template <class T>
void
vnl_svd<T>::zero_out_absolute(double tol)
{
  last_tol_ = tol;
  rank_ = W_.rows();
  for (unsigned k = 0; k < W_.rows(); k++)
  {
    singval_t& weight = W_(k, k);
    if (std::abs(weight) <= tol)
    {
      Winverse_(k,k) = 0;
      weight = 0;
      --rank_;
    }
    else
    {
      Winverse_(k,k) = singval_t(1.0)/weight;
    }
  }
}

//: find weights below tol*max(w) and zero them out
template <class T> void vnl_svd<T>::zero_out_relative(double tol) // sqrt(machine epsilon)
{
  zero_out_absolute(tol * std::abs(sigma_max()));
}

static bool w=false;
inline bool vnl_svn_warned() { if (w) return true; else { w=true; return false; } }

//: Calculate determinant as product of diagonals in W.
template <class T>
typename vnl_svd<T>::singval_t vnl_svd<T>::determinant_magnitude() const
{
  if (!vnl_svn_warned() && m_ != n_)
    std::cerr << __FILE__ ": called determinant_magnitude() on SVD of non-square matrix\n"
             << "(This warning is displayed only once)\n";
  singval_t product = W_(0, 0);
  for (unsigned long k = 1; k < W_.columns(); k++)
    product *= W_(k, k);

  return product;
}

template <class T>
typename vnl_svd<T>::singval_t vnl_svd<T>::norm() const
{
  return std::abs(sigma_max());
}

//: Recompose SVD to U*W*V'
template <class T>
vnl_matrix<T> vnl_svd<T>::recompose(unsigned int rnk) const
{
  if (rnk > rank_) rnk=rank_;
  vnl_matrix<T> Wmatr(W_.rows(),W_.columns());
  Wmatr.fill(T(0));
  for (unsigned int i=0;i<rnk;++i)
    Wmatr(i,i)=W_(i,i);

  return U_*Wmatr*V_.conjugate_transpose();
}


//: Calculate pseudo-inverse.
template <class T>
vnl_matrix<T> vnl_svd<T>::pinverse(unsigned int rnk) const
{
  if (rnk > rank_) rnk=rank_;
  vnl_matrix<T> W_inverse(Winverse_.rows(),Winverse_.columns());
  W_inverse.fill(T(0));
  for (unsigned int i=0;i<rnk;++i)
    W_inverse(i,i)=Winverse_(i,i);

  return V_ * W_inverse * U_.conjugate_transpose();
}


//: Calculate (pseudo-)inverse of transpose.
template <class T>
vnl_matrix<T> vnl_svd<T>::tinverse(unsigned int rnk) const
{
  if (rnk > rank_) rnk=rank_;
  vnl_matrix<T> W_inverse(Winverse_.rows(),Winverse_.columns());
  W_inverse.fill(T(0));
  for (unsigned int i=0;i<rnk;++i)
    W_inverse(i,i)=Winverse_(i,i);

  return U_ * W_inverse * V_.conjugate_transpose();
}


//: Solve the matrix equation M X = B, returning X
template <class T>
vnl_matrix<T> vnl_svd<T>::solve(vnl_matrix<T> const& B)  const
{
  vnl_matrix<T> x;                                      // solution matrix
  if (U_.rows() < U_.columns()) {                       // augment y with extra rows of
    vnl_matrix<T> yy(U_.rows(), B.columns(), T(0));     // zeros, so that it matches
    yy.update(B);                                       // cols of u.transpose. ???
    x = U_.conjugate_transpose() * yy;
  }
  else
    x = U_.conjugate_transpose() * B;
  for (unsigned long i = 0; i < x.rows(); ++i) {        // multiply with diagonal 1/W
    T weight = W_(i, i);
    if (weight != T(0)) // vnl_numeric_traits<T>::zero
      weight = T(1) / weight;
    for (unsigned long j = 0; j < x.columns(); ++j)
      x(i, j) *= weight;
  }
  x = V_ * x;                                           // premultiply with v.
  return x;
}

//: Solve the matrix-vector system M x = y, returning x.
template <class T>
vnl_vector<T> vnl_svd<T>::solve(vnl_vector<T> const& y)  const
{
  // fsm sanity check :
  if (y.size() != U_.rows())
  {
    std::cerr << __FILE__ << ": size of rhs is incompatible with no. of rows in U_\n"
             << "y =" << y << '\n'
             << "m_=" << m_ << '\n'
             << "n_=" << n_ << '\n'
             << "U_=\n" << U_
             << "V_=\n" << V_
             << "W_=\n" << W_;
  }

  vnl_vector<T> x(V_.rows());                   // Solution matrix.
  if (U_.rows() < U_.columns()) {               // Augment y with extra rows of
    vnl_vector<T> yy(U_.rows(), T(0));          // zeros, so that it matches
    if (yy.size()<y.size()) { // fsm
      std::cerr << "yy=" << yy << std::endl
               << "y =" << y  << std::endl;
      // the update() call on the next line will abort...
    }
    yy.update(y);                               // cols of u.transpose.
    x = U_.conjugate_transpose() * yy;
  }
  else
    x = U_.conjugate_transpose() * y;

  for (unsigned i = 0; i < x.size(); i++) {        // multiply with diagonal 1/W
    T weight = W_(i, i), zero_(0);
    if (weight != zero_)
      x[i] /= weight;
    else
      x[i] = zero_;
  }
  return V_ * x;                                // premultiply with v.
}

template <class T> // FIXME. this should implement the above, not the other way round.
void vnl_svd<T>::solve(T const *y, T *x) const
{
  solve(vnl_vector<T>(y, m_)).copy_out(x);
}

//: Solve the matrix-vector system M x = y.
// Assume that the singular values W have been preinverted by the caller.
template <class T>
void vnl_svd<T>::solve_preinverted(vnl_vector<T> const& y, vnl_vector<T>* x_out)  const
{
  vnl_vector<T> x;              // solution matrix
  if (U_.rows() < U_.columns()) {               // augment y with extra rows of
    std::cout << "vnl_svd<T>::solve_preinverted() -- Augmenting y\n";
    vnl_vector<T> yy(U_.rows(), T(0));     // zeros, so that it match
    yy.update(y);                               // cols of u.transpose. ??
    x = U_.conjugate_transpose() * yy;
  }
  else
    x = U_.conjugate_transpose() * y;
  for (unsigned i = 0; i < x.size(); i++)  // multiply with diagonal W, assumed inverted
    x[i] *= W_(i, i);

  *x_out = V_ * x;                                      // premultiply with v.
}

//-----------------------------------------------------------------------------
//: Return N s.t. M * N = 0
template <class T>
vnl_matrix <T> vnl_svd<T>::nullspace()  const
{
  int k = rank();
  if (k == n_)
    std::cerr << "vnl_svd<T>::nullspace() -- Matrix is full rank." << last_tol_ << std::endl;
  return nullspace(n_-k);
}

//-----------------------------------------------------------------------------
//: Return N s.t. M * N = 0
template <class T>
vnl_matrix <T> vnl_svd<T>::nullspace(int required_nullspace_dimension)  const
{
  return V_.extract(V_.rows(), required_nullspace_dimension, 0, n_ - required_nullspace_dimension);
}

//-----------------------------------------------------------------------------
//: Return N s.t. M' * N = 0
template <class T>
vnl_matrix <T> vnl_svd<T>::left_nullspace()  const
{
  int k = rank();
  if (k == n_)
    std::cerr << "vnl_svd<T>::left_nullspace() -- Matrix is full rank." << last_tol_ << std::endl;
  return U_.extract(U_.rows(), n_-k, 0, k);
}

//:
// \todo Implementation to be done yet; currently returns left_nullspace(). - PVr.
template <class T>
vnl_matrix<T> vnl_svd<T>::left_nullspace(int /*required_nullspace_dimension*/) const
{
  return left_nullspace();
}


//-----------------------------------------------------------------------------
//: Return the rightmost column of V.
//  Does not check to see whether or not the matrix actually was rank-deficient -
//  the caller is assumed to have examined W and decided that to his or her satisfaction.
template <class T>
vnl_vector <T> vnl_svd<T>::nullvector()  const
{
  vnl_vector<T> ret(n_);
  for (int i = 0; i < n_; ++i)
    ret(i) = V_(i, n_-1);
  return ret;
}

//-----------------------------------------------------------------------------
//: Return the rightmost column of U.
//  Does not check to see whether or not the matrix actually was rank-deficient.
template <class T>
vnl_vector <T> vnl_svd<T>::left_nullvector()  const
{
  vnl_vector<T> ret(m_);
  int col = std::min(m_, n_) - 1;
  for (int i = 0; i < m_; ++i)
    ret(i) = U_(i, col);
  return ret;
}

//--------------------------------------------------------------------------------

#undef VNL_SVD_INSTANTIATE
#define VNL_SVD_INSTANTIATE(T) \
template class VNL_ALGO_EXPORT vnl_svd<T >; \
template VNL_ALGO_EXPORT std::ostream& operator<<(std::ostream &, vnl_svd<T > const &)

#endif // vnl_svd_hxx_

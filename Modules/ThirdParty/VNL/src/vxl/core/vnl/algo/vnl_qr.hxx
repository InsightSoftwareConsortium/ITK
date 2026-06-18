// This is core/vnl/algo/vnl_qr.hxx
#ifndef vnl_qr_hxx_
#define vnl_qr_hxx_
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   08 Dec 1996

#include <iostream>
#include <complex>
#include "vnl_qr.h"
#include <cassert>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include <vnl/vnl_math.h>
#include <vnl/vnl_complex.h> // vnl_math::detail::squared_magnitude()
#include <vnl/vnl_matlab_print.h>
#include <vnl/vnl_complex_traits.h>
#include <vnl/vnl_numeric_traits.h>
#include <algorithm>
#include "vnl/algo/vnl_netlib.h" // vnl_netlib_qrdc_proto / vnl_netlib_qrsl_proto signatures

// Native, LINPACK-faithful QR engine (unpivoted) replacing the netlib
// {s,d,c,z}qrdc/qrsl routines. The packed output (column-major R in the upper
// triangle, Householder vectors below, scalars in qraux) is byte-identical to
// netlib, so vnl_qr::Q()/R()/solve()/determinant() are unchanged. A single
// template covers real and complex: conjugate-dot reduces to the real dot, and
// the signed/phased norm reduces to dsign for real T.
namespace vnl_qr_detail
{
// Per-type wrappers over the retained netlib BLAS kernels (inc=1), so the
// arithmetic is the SAME compiled code the netlib LINPACK qrdc/qrsl used --
// byte-identical results. std::complex<{float,double}> is layout-compatible
// with v3p_netlib_{complex,doublecomplex} (two reals), so pointers reinterpret.
inline float
blas_nrm2(long n, const float * x)
{
  long i = 1;
  return static_cast<float>(v3p_netlib_snrm2_(&n, const_cast<float *>(x), &i));
}
inline double
blas_nrm2(long n, const double * x)
{
  long i = 1;
  return v3p_netlib_dnrm2_(&n, const_cast<double *>(x), &i);
}
inline float
blas_nrm2(long n, const std::complex<float> * x)
{
  long i = 1;
  return static_cast<float>(v3p_netlib_scnrm2_(&n, (v3p_netlib_complex *)const_cast<std::complex<float> *>(x), &i));
}
inline double
blas_nrm2(long n, const std::complex<double> * x)
{
  long i = 1;
  return v3p_netlib_dznrm2_(&n, (v3p_netlib_doublecomplex *)const_cast<std::complex<double> *>(x), &i);
}

// conj(x).y (ddot for real, zdotc for complex).
inline float
blas_dotc(long n, const float * x, const float * y)
{
  long i = 1;
  return static_cast<float>(v3p_netlib_sdot_(&n, const_cast<float *>(x), &i, const_cast<float *>(y), &i));
}
inline double
blas_dotc(long n, const double * x, const double * y)
{
  long i = 1;
  return v3p_netlib_ddot_(&n, const_cast<double *>(x), &i, const_cast<double *>(y), &i);
}
inline std::complex<float>
blas_dotc(long n, const std::complex<float> * x, const std::complex<float> * y)
{
  long               i = 1;
  v3p_netlib_complex r;
  v3p_netlib_cdotc_(&r, &n, (v3p_netlib_complex *)const_cast<std::complex<float> *>(x), &i,
                    (v3p_netlib_complex *)const_cast<std::complex<float> *>(y), &i);
  return r;
}
inline std::complex<double>
blas_dotc(long n, const std::complex<double> * x, const std::complex<double> * y)
{
  long                     i = 1;
  v3p_netlib_doublecomplex r;
  v3p_netlib_zdotc_(&r, &n, (v3p_netlib_doublecomplex *)const_cast<std::complex<double> *>(x), &i,
                    (v3p_netlib_doublecomplex *)const_cast<std::complex<double> *>(y), &i);
  return r;
}

// y += a*x.
inline void
blas_axpy(long n, float a, const float * x, float * y)
{
  long i = 1;
  v3p_netlib_saxpy_(&n, &a, const_cast<float *>(x), &i, y, &i);
}
inline void
blas_axpy(long n, double a, const double * x, double * y)
{
  long i = 1;
  v3p_netlib_daxpy_(&n, &a, const_cast<double *>(x), &i, y, &i);
}
inline void
blas_axpy(long n, std::complex<float> a, const std::complex<float> * x, std::complex<float> * y)
{
  long i = 1;
  v3p_netlib_caxpy_(&n, (v3p_netlib_complex *)&a, (v3p_netlib_complex *)const_cast<std::complex<float> *>(x), &i,
                    (v3p_netlib_complex *)y, &i);
}
inline void
blas_axpy(long n, std::complex<double> a, const std::complex<double> * x, std::complex<double> * y)
{
  long i = 1;
  v3p_netlib_zaxpy_(&n, (v3p_netlib_doublecomplex *)&a,
                    (v3p_netlib_doublecomplex *)const_cast<std::complex<double> *>(x), &i,
                    (v3p_netlib_doublecomplex *)y, &i);
}

// x *= a.
inline void
blas_scal(long n, float a, float * x)
{
  long i = 1;
  v3p_netlib_sscal_(&n, &a, x, &i);
}
inline void
blas_scal(long n, double a, double * x)
{
  long i = 1;
  v3p_netlib_dscal_(&n, &a, x, &i);
}
inline void
blas_scal(long n, std::complex<float> a, std::complex<float> * x)
{
  long i = 1;
  v3p_netlib_cscal_(&n, (v3p_netlib_complex *)&a, (v3p_netlib_complex *)x, &i);
}
inline void
blas_scal(long n, std::complex<double> a, std::complex<double> * x)
{
  long i = 1;
  v3p_netlib_zscal_(&n, (v3p_netlib_doublecomplex *)&a, (v3p_netlib_doublecomplex *)x, &i);
}

// Magnitude / division reproducing the libf2c helpers (f__cabs, z_div/c_div)
// the netlib complex routines use -- same double-intermediate arithmetic and
// truncation -- so complex scalar ops round identically. (Ported rather than
// linked: the f2c symbols take the f2c struct type, not std::complex.)
inline double
f2c_cabs(double re, double im)
{
  re = std::fabs(re);
  im = std::fabs(im);
  if (im > re)
    std::swap(re, im);
  if (re + im == re)
    return re;
  const double t = im / re;
  return re * std::sqrt(1.0 + t * t);
}
inline float
f2c_abs(float z)
{
  return std::abs(z);
}
inline double
f2c_abs(double z)
{
  return std::abs(z);
}
inline float
f2c_abs(const std::complex<float> & z)
{
  return static_cast<float>(f2c_cabs(z.real(), z.imag()));
}
inline double
f2c_abs(const std::complex<double> & z)
{
  return f2c_cabs(z.real(), z.imag());
}
inline float
f2c_div(float a, float b)
{
  return a / b;
}
inline double
f2c_div(double a, double b)
{
  return a / b;
}
template <class R>
std::complex<R>
f2c_cdiv(const std::complex<R> & a, const std::complex<R> & b)
{
  const double abr = std::fabs(static_cast<double>(b.real()));
  const double abi = std::fabs(static_cast<double>(b.imag()));
  double       cr;
  double       ci;
  if (abr <= abi)
  {
    const double ratio = static_cast<double>(b.real()) / b.imag();
    const double den = static_cast<double>(b.imag()) * (1.0 + ratio * ratio);
    cr = (static_cast<double>(a.real()) * ratio + a.imag()) / den;
    ci = (static_cast<double>(a.imag()) * ratio - a.real()) / den;
  }
  else
  {
    const double ratio = static_cast<double>(b.imag()) / b.real();
    const double den = static_cast<double>(b.real()) * (1.0 + ratio * ratio);
    cr = (static_cast<double>(a.real()) + static_cast<double>(a.imag()) * ratio) / den;
    ci = (static_cast<double>(a.imag()) - static_cast<double>(a.real()) * ratio) / den;
  }
  return { static_cast<R>(cr), static_cast<R>(ci) };
}
inline std::complex<float>
f2c_div(std::complex<float> a, std::complex<float> b)
{
  return f2c_cdiv(a, b);
}
inline std::complex<double>
f2c_div(std::complex<double> a, std::complex<double> b)
{
  return f2c_cdiv(a, b);
}

// |nrm| carrying the sign (real) / phase (complex) of xll: dsign / csign.
template <class T>
T
signed_norm(typename vnl_numeric_traits<T>::abs_t nrm, const T & xll)
{
  using abs_t = typename vnl_numeric_traits<T>::abs_t;
  const abs_t a = f2c_abs(xll);
  if (a == abs_t(0))
    return T(nrm);
  return T(nrm) * (xll / a);
}
} // namespace vnl_qr_detail

template <class T>
void
vnl_linpack_qrdc(vnl_netlib_qrdc_proto(T))
{
  using abs_t = typename vnl_numeric_traits<T>::abs_t;
  const long ld = *ldx;
  const long nn = *n;
  const long pp = *p;
  const long lup = std::min(nn, pp);
  for (long l = 0; l < lup; ++l)
  {
    T * col_l = x + l * ld;
    qraux[l] = T(0);
    if (l == nn - 1) // last row: trivial 1-element Householder, skip
      continue;
    const abs_t nrm = vnl_qr_detail::blas_nrm2(nn - l, col_l + l);
    if (nrm == abs_t(0))
      continue;
    const T nrmxl = vnl_qr_detail::signed_norm(nrm, col_l[l]);
    vnl_qr_detail::blas_scal(nn - l, vnl_qr_detail::f2c_div(T(1), nrmxl), col_l + l);
    col_l[l] += T(1);
    for (long j = l + 1; j < pp; ++j)
    {
      T *     col_j = x + j * ld;
      const T t = vnl_qr_detail::f2c_div(-vnl_qr_detail::blas_dotc(nn - l, col_l + l, col_j + l), col_l[l]);
      vnl_qr_detail::blas_axpy(nn - l, t, col_l + l, col_j + l);
    }
    qraux[l] = col_l[l];
    col_l[l] = -nrmxl;
  }
}

template <class T>
void
vnl_linpack_qrsl(vnl_netlib_qrsl_proto(T))
{
  const long ld = *ldx;
  const long nn = *n;
  const long kk = *k;
  const long jb = *job;
  *info = 0;
  const bool cqy = (jb / 10000) != 0;
  const bool cqty = (jb % 10000) != 0;
  const bool cb = ((jb % 1000) / 100) != 0;
  const bool cr = ((jb % 100) / 10) != 0;
  const bool cxb = (jb % 10) != 0;
  const long ju = std::min(kk, nn - 1);

  // dqrsl applies reflection j over the full subcolumn by temporarily swapping
  // the R-diagonal x(j,j) with the Householder scalar qraux[j], so the BLAS
  // dot/axpy span rows j..n-1 with the reflection diagonal in place, then
  // restores x(j,j). Replicated here verbatim (x is restored => effectively
  // const) so the arithmetic is byte-identical to netlib.
  T * xm = const_cast<T *>(x);
  auto applyHouse = [&](T * vec, long j) {
    if (qraux[j] == T(0))
      return;
    T *     col = xm + j * ld;
    const T temp = col[j];
    col[j] = qraux[j];
    const T t = vnl_qr_detail::f2c_div(-vnl_qr_detail::blas_dotc(nn - j, col + j, vec + j), col[j]);
    vnl_qr_detail::blas_axpy(nn - j, t, col + j, vec + j);
    col[j] = temp;
  };

  if (ju == 0)
  {
    if (cqy)
      qy[0] = y[0];
    if (cqty)
      qty[0] = y[0];
    if (cxb)
      xb[0] = y[0];
    if (cb)
    {
      if (x[0] != T(0))
        b[0] = vnl_qr_detail::f2c_div(y[0], x[0]);
      else
        *info = 1;
    }
    if (cr)
      rsd[0] = T(0);
    return;
  }

  if (cqy)
  {
    for (long i = 0; i < nn; ++i)
      qy[i] = y[i];
    for (long j = ju - 1; j >= 0; --j)
      applyHouse(qy, j);
  }
  if (cqty)
  {
    for (long i = 0; i < nn; ++i)
      qty[i] = y[i];
    for (long j = 0; j < ju; ++j)
      applyHouse(qty, j);
  }
  if (cb)
    for (long i = 0; i < kk; ++i)
      b[i] = qty[i];
  if (cxb)
    for (long i = 0; i < kk; ++i)
      xb[i] = qty[i];
  if (cr && kk < nn)
    for (long i = kk; i < nn; ++i)
      rsd[i] = qty[i];
  if (cxb)
    for (long i = kk; i < nn; ++i)
      xb[i] = T(0);
  if (cr)
    for (long i = 0; i < kk; ++i)
      rsd[i] = T(0);
  if (cb)
  {
    for (long j = kk - 1; j >= 0; --j)
    {
      const T * col = x + j * ld;
      if (col[j] == T(0))
      {
        *info = j + 1;
        break;
      }
      b[j] = vnl_qr_detail::f2c_div(b[j], col[j]);
      if (j != 0)
        vnl_qr_detail::blas_axpy(j, -b[j], col, b);
    }
  }
  if (cr || cxb)
  {
    for (long j = ju - 1; j >= 0; --j)
    {
      if (cr)
        applyHouse(rsd, j);
      if (cxb)
        applyHouse(xb, j);
    }
  }
}

template <class T>
vnl_qr<T>::vnl_qr(vnl_matrix<T> const & M)
  : qrdc_out_(M.columns(), M.rows())
  , qraux_(M.columns())
  , jpvt_(M.rows())
  , Q_(nullptr)
  , R_(nullptr)
{
  assert(!M.empty());

  // Fill transposed O/P matrix
  const long c = M.columns();
  const long r = M.rows();
  for (int i = 0; i < r; ++i)
    for (int j = 0; j < c; ++j)
      qrdc_out_(j, i) = M(i, j);

  const long do_pivot = 0; // Enable[!=0]/disable[==0] pivoting.
  jpvt_.fill(0);           // Allow all columns to be pivoted if pivoting is enabled.

  vnl_vector<T> work(M.rows());
  vnl_linpack_qrdc(qrdc_out_.data_block(), // On output, UT is R, below diag is mangled Q
                   &r,
                   &r,
                   &c,
                   qraux_.data_block(), // Further information required to demangle Q
                   jpvt_.data_block(),
                   work.data_block(),
                   &do_pivot);
}

template <class T>
vnl_qr<T>::~vnl_qr()
{
  delete Q_;
  delete R_;
}

//: Return the determinant of M.  This is computed from M = Q R as follows:
// |M| = |Q| |R|
// |R| is the product of the diagonal elements.
// |Q| is (-1)^n as it is a product of Householder reflections.
// So det = -prod(-r_ii).
template <class T>
T
vnl_qr<T>::determinant() const
{
  const int m = std::min((int)qrdc_out_.columns(), (int)qrdc_out_.rows());
  T det = qrdc_out_(0, 0);

  for (int i = 1; i < m; ++i)
    det *= -qrdc_out_(i, i);

  return det;
}

//: Unpack and return unitary part Q.
template <class T>
const vnl_matrix<T> &
vnl_qr<T>::Q() const
{
  const int m = qrdc_out_.columns(); // column-major storage
  const int n = qrdc_out_.rows();

  const bool verbose = false;

  if (!Q_)
  {
    Q_ = new vnl_matrix<T>(m, m);
    // extract Q.
    if (verbose)
    {
      std::cerr << __FILE__ ": vnl_qr<T>::Q()\n"
                << " m,n = " << m << ", " << n << '\n'
                << " qr0 = [" << qrdc_out_ << "];\n"
                << " aux = [" << qraux_ << "];\n";
    }

    Q_->set_identity();
    vnl_matrix<T> & matrQ = *Q_;

    vnl_vector<T> v(m, T(0));
    vnl_vector<T> w(m, T(0));

    // Golub and vanLoan, p199.  backward accumulation of householder matrices
    // Householder vector k is [zeros(1,k-1) qraux_[k] qrdc_out_[k,:]]
    using abs_t = typename vnl_numeric_traits<T>::abs_t;
    for (int k = n - 1; k >= 0; --k)
    {
      if (k >= m)
        continue;
      // Make housevec v, and accumulate norm at the same time.
      v[k] = qraux_[k];
      abs_t sq = vnl_math::detail::squared_magnitude(v[k]);
      for (int j = k + 1; j < m; ++j)
      {
        v[j] = qrdc_out_(k, j);
        sq += vnl_math::detail::squared_magnitude(v[j]);
      }
      if (verbose)
        vnl_matlab_print(std::cerr, v, "v");
#ifndef DOXYGEN_SHOULD_SKIP_THIS
#  define c vnl_complex_traits<T>::conjugate
#endif
      // Premultiply emerging Q by house(v), noting that v[0..k-1] == 0.
      // Q_new = (1 - (2/v'*v) v v')Q
      // or Q -= (2/v'*v) v (v'Q)
      if (sq > abs_t(0))
      {
        const abs_t scale = abs_t(2) / sq;
        // w = (2/v'*v) v' Q
        for (int i = k; i < m; ++i)
        {
          w[i] = T(0);
          for (int j = k; j < m; ++j)
            w[i] += scale * c(v[j]) * matrQ(j, i);
        }
        if (verbose)
          vnl_matlab_print(std::cerr, w, "w");

        // Q -= v w
        for (int i = k; i < m; ++i)
          for (int j = k; j < m; ++j)
            matrQ(i, j) -= (v[i]) * (w[j]);
      }
#undef c
    }
  }
  return *Q_;
}

//: Unpack and return R.
template <class T>
const vnl_matrix<T> &
vnl_qr<T>::R() const
{
  if (!R_)
  {
    const int m = qrdc_out_.columns(); // column-major storage
    const int n = qrdc_out_.rows();
    R_ = new vnl_matrix<T>(m, n);
    vnl_matrix<T> & Rmatr = *R_;

    for (int i = 0; i < m; ++i)
      for (int j = 0; j < n; ++j)
        if (i > j)
          Rmatr(i, j) = T(0);
        else
          Rmatr(i, j) = qrdc_out_(j, i);
  }

  return *R_;
}

template <class T>
vnl_matrix<T>
vnl_qr<T>::recompose() const
{
  return Q() * R();
}

// JOB: ABCDE decimal
// A     B     C     D              E
// ---   ---   ---   ---            ---
// Qb    Q'b   x     norm(A*x - b)  A*x


//: Solve equation M x = b for x using the computed decomposition.
template <class T>
vnl_vector<T>
vnl_qr<T>::solve(const vnl_vector<T> & b) const
{
  long n = qrdc_out_.columns();
  long p = qrdc_out_.rows();
  const T * b_data = b.data_block();
  vnl_vector<T> Qt_B(n);
  vnl_vector<T> x(p);

  // see comment above
  long JOB = 100;

  long info = 0;
  vnl_linpack_qrsl(qrdc_out_.data_block(),
                   &n,
                   &n,
                   &p,
                   qraux_.data_block(),
                   b_data,
                   (T *)nullptr,
                   Qt_B.data_block(),
                   x.data_block(),
                   (T *)nullptr /*residual*/,
                   (T *)nullptr /*Ax*/,
                   &JOB,
                   &info);

  if (info > 0)
    std::cerr << __FILE__ ": vnl_qr<T>::solve() : matrix is rank-deficient by " << info << '\n';

  return x;
}

//: Return residual vector d of M x = b -> d = Q'b
template <class T>
vnl_vector<T>
vnl_qr<T>::QtB(const vnl_vector<T> & b) const
{
  long n = qrdc_out_.columns();
  long p = qrdc_out_.rows();
  const T * b_data = b.data_block();
  vnl_vector<T> Qt_B(n);

  // see comment above
  long JOB = 1000;

  long info = 0;
  vnl_linpack_qrsl(qrdc_out_.data_block(),
                   &n,
                   &n,
                   &p,
                   qraux_.data_block(),
                   b_data,
                   (T *)nullptr,      // A: Qb
                   Qt_B.data_block(), // B: Q'b
                   (T *)nullptr,      // C: x
                   (T *)nullptr,      // D: residual
                   (T *)nullptr,      // E: Ax
                   &JOB,
                   &info);

  if (info > 0)
    std::cerr << __FILE__ ": vnl_qr<T>::QtB() -- matrix is rank-deficient by " << info << '\n';

  return Qt_B;
}

template <class T>
vnl_matrix<T>
vnl_qr<T>::inverse() const
{
  const unsigned int r = qrdc_out_.columns();
  assert(r > 0 && r == qrdc_out_.rows());
  vnl_matrix<T> inv(r, r);

  // Use solve() to compute the inverse matrix, using (00..010..00) as rhs
  vnl_vector<T> rhs(r, T(0));
  for (unsigned int i = 0; i < r; ++i)
  {
    rhs(i) = T(1);
    const vnl_vector<T> col = this->solve(rhs); // returns i-th column of inverse
    inv.set_column(i, col);
    rhs(i) = T(0);
  }
  return inv;
}

template <class T>
vnl_matrix<T>
vnl_qr<T>::tinverse() const
{
  const unsigned int r = qrdc_out_.columns();
  assert(r > 0 && r == qrdc_out_.rows());
  vnl_matrix<T> tinv(r, r);

  // Use solve() to compute the inverse matrix, using (00..010..00) as rhs
  vnl_vector<T> rhs(r, T(0));
  for (unsigned int i = 0; i < r; ++i)
  {
    rhs(i) = T(1);
    const vnl_vector<T> col = this->solve(rhs); // returns i-th column of inverse
    tinv.set_row(i, col);
    rhs(i) = T(0);
  }
  return tinv;
}

template <class T>
vnl_matrix<T>
vnl_qr<T>::solve(const vnl_matrix<T> & rhs) const
{
  assert(rhs.rows() == qrdc_out_.columns()); // column-major storage
  const int c = qrdc_out_.rows();
  const int n = rhs.columns();
  vnl_matrix<T> result(c, n);

  for (int i = 0; i < n; ++i)
  {
    const vnl_vector<T> b = rhs.get_column(i);
    const vnl_vector<T> col = this->solve(b); // returns i-th column of result
    result.set_column(i, col);
  }
  return result;
}

//------------------------------------------------------------------------------

#define VNL_QR_INSTANTIATE(T)               \
  template class VNL_ALGO_EXPORT vnl_qr<T>; \
  /*template VNL_EXPORT T vnl_qr_determinant(vnl_matrix<T > const&) */

#endif

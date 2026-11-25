// This is core/vnl/vnl_matops.cxx
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   05 Aug 96
//
//-----------------------------------------------------------------------------

#include "vnl_matops.h"
#include <cassert>

vnl_matrix<double>
vnl_matops::cat(const vnl_matrix<double> & A, const vnl_matrix<double> & B)
{
  assert(A.rows() == B.rows());

  vnl_matrix<double> M(A.rows(), A.columns() + B.columns());
  M.update(A, 0, 0);
  M.update(B, 0, A.columns());

  return M;
}

vnl_matrix<double>
vnl_matops::cat(const vnl_matrix<double> & A, const vnl_vector<double> & B)
{
  assert(A.rows() == B.size());

  vnl_matrix<double> M(A.rows(), A.columns() + 1);
  M.update(A, 0, 0);
  M.set_column(A.columns(), B);

  return M;
}

vnl_matrix<double>
vnl_matops::cat(const vnl_vector<double> & A, const vnl_matrix<double> & B)
{
  assert(A.size() == B.rows());

  vnl_matrix<double> M(B.rows(), B.columns() + 1);
  M.set_column(0, A);
  M.update(B, 0, 1);

  return M;
}

vnl_matrix<double>
vnl_matops::vcat(const vnl_matrix<double> & A, const vnl_matrix<double> & B)
{
  assert(A.columns() == B.columns());

  vnl_matrix<double> M(A.rows() + B.rows(), A.columns());
  M.update(A, 0, 0);
  M.update(B, A.rows(), 0);

  return M;
}

//: Return fro_norm( (A ./ B) - mean(A ./ B) )
double
vnl_matops::homg_diff(const vnl_matrix<double> & A, const vnl_matrix<double> & B)
{
  const vnl_matrix<double> ratio = element_quotient(A, B);

  return (ratio - ratio.mean()).fro_norm();
}

#define implement_converters(U, V)                       \
  vnl_matrix<U> make_matrix_##U(vnl_matrix<V> const & M) \
  {                                                      \
    unsigned m = M.rows();                               \
    unsigned n = M.columns();                            \
    vnl_matrix<U> ret(m, n);                             \
    for (unsigned i = 0; i < m; ++i)                     \
      for (unsigned j = 0; j < n; ++j)                   \
        ret(i, j) = static_cast<U>(M(i, j));             \
    return ret;                                          \
  }                                                      \
                                                         \
  vnl_vector<U> make_vector_##U(vnl_vector<V> const & v) \
  {                                                      \
    unsigned n = v.size();                               \
    vnl_vector<U> ret(n);                                \
    for (unsigned i = 0; i < n; ++i)                     \
      ret[i] = static_cast<U>(v[i]);                     \
    return ret;                                          \
  }

implement_converters(double, float)

  implement_converters(float, double)

    vnl_matrix<double> vnl_matops::f2d(vnl_matrix<float> const & M)
{
  return make_matrix_double(M);
}

vnl_matrix<float>
vnl_matops::d2f(const vnl_matrix<double> & M)
{
  return make_matrix_float(M);
}

vnl_vector<double>
vnl_matops::f2d(const vnl_vector<float> & M)
{
  return make_vector_double(M);
}

vnl_vector<float>
vnl_matops::d2f(const vnl_vector<double> & M)
{
  return make_vector_float(M);
}

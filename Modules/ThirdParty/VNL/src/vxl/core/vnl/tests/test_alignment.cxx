// This is core/vnl/tests/test_alignment.cxx
#include <limits>
#include <typeinfo>
#include <iostream>
#include <algorithm>
#include <vcl_compiler.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix_ref.h>
#include <vnl/vnl_vector_ref.h>
#include <vnl/vnl_c_vector.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_random.h>
#include <vnl/vnl_sse.h>
#include <testlib/testlib_test.h>

inline bool test_element_product(const vnl_vector<float> &vec, const vnl_vector<float> &vec2,
                                 vnl_vector<float> &result)
{
  unsigned n = result.size();
  vnl_c_vector<float>::multiply(vec.data_block(), vec2.data_block(), result.data_block(), n);
  vnl_vector<float> correct(n);
  for (unsigned i = 0; i < n; ++i)
    correct(i) = vec(i) * vec2(i);

  float err = vnl_vector_ssd(correct, result);
  float neps = float(n)*std::numeric_limits<float>::epsilon();
  if (err < neps)
    return true;
  else
  {
    std::cout << "err: " << err << " n*eps: " << neps << std::endl;
    return false;
  }
}


inline bool test_dot_product(const vnl_vector<float> &vec, const vnl_vector<float> &vec2)
{
  float val = dot_product(vec, vec2);
  unsigned n = vec.size();
  float correct = 0.f;
  for (unsigned i = 0; i < n; ++i)
    correct += vec(i) * vec2(i);

  float err = std::abs(correct-val);
  float neps = float(n)*std::numeric_limits<float>::epsilon();
  if (err < neps)
    return true;
  else
  {
    std::cout << "dot_product: err: " << err << " n*eps: " << neps << std::endl;
    return false;
  }
}

inline bool test_euclid_dist_sq(const vnl_vector<float> &vec, const vnl_vector<float> &vec2)
{
  float val = vnl_vector_ssd(vec, vec2);
  unsigned n = vec.size();
  float correct(0);
  for (unsigned i = 0; i < n; ++i)
    correct += vnl_math::sqr(vec(i) - vec2(i));

  float err = std::abs(correct-val);
  float neps = float(n)*std::sqrt(std::numeric_limits<float>::epsilon());
  if (err < neps)
    return true;
  else
  {
    std::cout << "euclid_dist_sq: err: " << err << " n*sqrt_eps: " << neps << std::endl;
    return false;
  }
}

inline bool test_matrix_x_vector(const vnl_matrix<float> &mat, const vnl_vector<float> &vec, vnl_vector<float> &result)
{
  unsigned n = vec.size();
  vnl_sse<float>::matrix_x_vector(mat.data_block(), vec.data_block(), result.data_block(), n, n);

  vnl_vector<float> correct(n);
  for (unsigned i=0; i<n; ++i)
  {
    float som(0);
    for (unsigned j=0; j<n; ++j)
      som += mat(i,j) * vec(j);
    correct(i) = som;
  }

  float err = vnl_vector_ssd(correct, result);
  float neps = float(n)*std::numeric_limits<float>::epsilon();
  if (err < neps)
    return true;
  else
  {
    std::cout << "matrix_x_vector: err: " << err << " n*eps: " << neps << std::endl;
    return false;
  }
}

inline bool test_vector_x_matrix(const vnl_vector<float> &vec, const vnl_matrix<float> &mat, vnl_vector<float> &result)
{
  unsigned n = vec.size();
  vnl_sse<float>::vector_x_matrix(vec.data_block(), mat.data_block(), result.data_block(), n, n);

  vnl_vector<float> correct(n);
  for (unsigned j=0; j<n; ++j)
  {
    float som(0);
    for (unsigned i=0; i<n; ++i)
      som += mat(i,j) * vec(i);
    correct(j) = som;
  }

  float err = vnl_vector_ssd(correct, result);
  float neps = float(n)*std::numeric_limits<float>::epsilon();
  if (err < neps)
    return true;
  else
  {
    std::cout << "vector_x_matrix: err: " << err << " n*eps: " << neps << std::endl;
    return false;
  }
}

inline bool test_sum(const vnl_vector<float> &vec)
{
  float val = vec.sum();
  unsigned n = vec.size();
  float correct(0);
  for (unsigned i = 0; i < n; ++i)
    correct += vec(i);

  float err = std::abs(correct-val);
  float neps = float(n)*std::numeric_limits<float>::epsilon();
  if (err < neps)
    return true;
  else
  {
    std::cout << "sum: err: " << err << " n*eps: " << neps << std::endl;
    return false;
  }
}

inline bool test_max(const vnl_vector<float> &vec)
{
  float val = vec.max_value();
  unsigned n = vec.size();
  float correct(-std::numeric_limits<float>::max());
  for (unsigned i = 0; i < n; ++i)
    correct = std::max(vec(i), correct);

  float err = std::abs(correct-val);
  float neps = float(n)*std::numeric_limits<float>::epsilon();
  if (err < neps)
    return true;
  else
  {
    std::cout << "max: err: " << err << " n*eps: " << neps << std::endl;
    return false;
  }
}

inline bool test_min(const vnl_vector<float> &vec)
{
  float val = vec.min_value();
  unsigned n = vec.size();
  float correct(std::numeric_limits<float>::max());
  for (unsigned i = 0; i < n; ++i)
    correct = std::min(vec(i), correct);

  float err = std::abs(correct-val);
  float neps = float(n)*std::numeric_limits<float>::epsilon();
  if (err < neps)
    return true;
  else
  {
    std::cout << "min: err: " << err << " n*eps: " << neps << std::endl;
    return false;
  }
}

inline bool test_arg_max(const vnl_vector<float> &vec)
{
  unsigned idx = vec.arg_max();
  unsigned n = vec.size();
  float correct_val(-std::numeric_limits<float>::max());
  unsigned correct_idx = 0;
  for (unsigned i = 0; i < n; ++i)
    if (vec(i) > correct_val)
      correct_val = vec(i), correct_idx = i;

  if (idx == correct_idx)
    return true;
  else
  {
    std::cout << "max " << correct_val << " found at position " << idx << " instead of at " << correct_idx << std::endl;
    return false;
  }
}

inline bool test_arg_min(const vnl_vector<float> &vec)
{
  unsigned idx = vec.arg_min();
  unsigned n = vec.size();
  float correct_val(std::numeric_limits<float>::max());
  unsigned correct_idx = 0;
  for (unsigned i = 0; i < n; ++i)
    if (vec(i) < correct_val)
      correct_val = vec(i), correct_idx = i;

  if (idx == correct_idx)
    return true;
  else
  {
    std::cout << "min " << correct_val << " found at position " << idx << " instead of at " << correct_idx << std::endl;
    return false;
  }
}


static void test_alignment_type()
{
  std::cout << "*****************************************************\n"
           << "Testing vnl_sse alignment issues in with type " << typeid(float).name() << '\n'
           << "*****************************************************\n"
           << '\n'
           << "VNL_CONFIG_ENABLE_SSE2 is " << VNL_CONFIG_ENABLE_SSE2 << std::endl;

  // Set up random data arrays.
  const unsigned ndata = 13; // not a multiple of 2
  float matrix_data[ndata*ndata];
  float vector_data[ndata];
  float result_data[ndata];

  vnl_random rng;
  for (unsigned int i=0;i<ndata;++i)
  {
    vector_data[i] = static_cast<float>(1.0 + 2.0*rng.normal64());
    for (unsigned int j=0;j<ndata;++j)
      matrix_data[i*ndata + j] = static_cast<float>(1.0 + 2.0*rng.normal64());
  }

  for (unsigned nv = 3; nv < 8; ++nv) // different matrix/vector sizes
  {
    for (unsigned m=0; m+nv<ndata; ++m) // different matrix offsets
      for (unsigned v=0; v+nv<ndata; ++v) // different vector offsets
        for (unsigned r=0; r+nv<ndata; ++r) // different result offsets
        {
          std::cout << "vector size: " << nv << " matrix offset: " << m << " vector offset: " << v
                   << " result offset: " << r << std::endl;
          const vnl_matrix_ref<float> mat(nv, nv, matrix_data+m);
          const vnl_vector_ref<float> vec2(nv, matrix_data+m);
          const vnl_vector_ref<float> vec(nv, vector_data+v);
          vnl_vector_ref<float> result(nv, result_data+r);
          bool rvtest =
            test_element_product(vec, vec2, result) &&
            test_dot_product(vec, vec2) &&
            test_euclid_dist_sq(vec, vec2) &&
            test_matrix_x_vector(mat, vec, result) &&
            test_vector_x_matrix(vec, mat, result) &&
            test_sum(vec) &&
            test_max(vec) &&
            test_min(vec) &&
            test_arg_max(vec) &&
            test_arg_min(vec);
          TEST("SSE", rvtest, true);
        }
  }
}

static
void test_alignment()
{
  test_alignment_type();
}

TESTMAIN(test_alignment);

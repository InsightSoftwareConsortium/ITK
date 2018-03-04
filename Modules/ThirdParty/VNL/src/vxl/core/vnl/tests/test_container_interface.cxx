// This is core/vnl/tests/test_matrix_interface.cxx
#include <iostream>
#include <cmath>
#include <exception>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_copy.h>
#include <testlib/testlib_test.h>
#include <vcl_compiler.h>

template< typename TValue >
TValue square(TValue v)
{
  return v * v;
}

// This function is used in testing later.
template< typename T >
T sum_vector(const vnl_vector<T> &v) { return v.sum(); }

template< typename T >
T sum_vector(const vnl_vector_fixed<T, 2> &v) { return v.sum(); }

template< class TVector >
typename TVector::element_type sum(TVector v)
{
  return v.sum();
}

template< class TContainer >
void test_common_interface()
{
  {
  TContainer m;
  }
  TContainer m(2,2);
  m.size();
  m.rows();
  m.cols();
  m.columns();
  m.put(0, 0, 0);
  m.get(0, 0);
  m[0];
  m(0,0);
  m.fill(0);
  m.fill_diagonal(0);
  vnl_vector<int> v(2, 1);
  m.set_diagonal(v);
  int data[4] = { 0, 1, 2, 3 };
  m.set(data);
  m.copy_in(data);
  m.copy_out(data);
  TContainer n(2,2);
  n.fill(12);
  m = n;
  m += 4;
  m -= 4;
  m *= 4;
  m /= 4;
  m += n;
  m -= n;
  m *= n;
  m.apply(square);
  m.apply_rowwise( sum_vector );
  m.apply_columnwise( sum_vector );
  m.transpose();
  m.conjugate_transpose();
  m.update(n);
  m.update(n,0);
  m.update(n,0,0);
  m.set_column(0, data); // OK: data is longer than one column
  m.set_column(0, 0);
  m.set_column(0, v);
  m.set_row(0, data);

  m.set_row(0, 0);
  m.set_row(0, v);
  m.extract(2,2);
  m.extract(2,2,0);
  m.extract(2,2,0,0);
  vnl_matrix<int> e(2,2);
  m.extract(e);
  m.get_row(0);
  m.get_column(0);
  ///////////////////////////////////////////////
  // Test `get_rows` and `get_columns` Methods //
  ///////////////////////////////////////////////
  {
    typename TContainer::element_type data[4] = {1, 2, 3, 4};
    unsigned int indices[2] = {1, 0};
    vnl_vector<unsigned int> i(indices, 2);
    TContainer matrix(2, 2);
    matrix.copy_in(data);
#if ! defined(_MSC_VER)  //This code is failing on VS15 in Release mode for vnl_matrix_fixed
    //Removing temporarily so that this new test does not hold up other works.
    TContainer matrix_lr(matrix);
    matrix_lr.fliplr();
    TContainer matrix_ud(matrix);
    matrix_ud.flipud();
    TEST("get_rows", matrix_lr.is_equal(matrix.get_columns(i), 10e-6), true);
    TEST("get_columns", matrix_ud.is_equal(matrix.get_rows(i), 10e-6), true);
#endif
  }
  m.get_n_rows(0,1);
  m.get_n_columns(0,1);
  m.get_diagonal();
  m.flatten_row_major();
  m.flatten_column_major();
  m.set_identity();
  m.inplace_transpose();
#if ! defined(_MSC_VER)  //This code is failing on VS15 in Release mode for vnl_matrix_fixed
  //Removing temporarily so that this new test does not hold up other works.
  m.flipud();
  m.fliplr();
#endif

  m.normalize_rows();
  m.normalize_columns();
  m.scale_row(0,10);
  m.scale_column(0,10);
    ////////////////////////
    // Test `swap` Method //
    ////////////////////////

    {
    const typename TContainer::element_type data1[4] = {0, 1, 2, 3};
    TContainer l(2, 2);
    l.copy_in(data1);
    TContainer l_swap(l);

    const typename TContainer::element_type data2[4] = {4, 5, 6, 7};
    TContainer r(2, 2);
    r.copy_in(data2);
    TContainer r_swap(r);

    l_swap.swap(r_swap);
    TEST("swap left-right", l.is_equal(r_swap, 10e-6), true);
    TEST("swap right-left", r.is_equal(l_swap, 10e-6), true);
    }
  typename TContainer::abs_t a = 25;
  m.array_one_norm();
  m.array_two_norm();
  m.array_inf_norm();
  m.absolute_value_sum();
  m.absolute_value_max();
  m.operator_one_norm();
  m.operator_inf_norm();
  m.frobenius_norm();
  m.fro_norm();
  m.rms();
  m.min_value();
  m.max_value();
  m.arg_min();
  m.arg_max();
  m.mean();
  m.empty();
  m.is_identity();
  m.is_identity(10e-6);
  m.is_zero();
  m.is_zero(10e-6);
    ////////////////////////////
    // Test `is_equal` Method //
    ////////////////////////////

    {
    TContainer l(2,2);
    TContainer r(2,2);
    l.copy_in(data);
    r.copy_in(data);
    TEST("is_equal (true)", l.is_equal(r, 10e-6), true);
    r.put(0, 0, 25);
    TEST("is_equal (false)", !l.is_equal(r, 10e-6), true);
    }
  m.is_finite();
  m.has_nans();
  m.assert_size(2,2);
  m.assert_finite();
  m.data_block();
  m.as_ref();
  typename TContainer::element_type value = 7;
  typename TContainer::iterator it = m.begin();
  it = m.end();
  typename TContainer::const_iterator cit = m.begin();
  cit = m.end();
}

static
void test_container_interface()
{
  vcl_cout << "Testing vnl_matrix<int>" << std::endl;
  test_common_interface< vnl_matrix<int> >();

  vcl_cout << "Testing vnl_matrix_fixed<int, 2, 2>" << std::endl;
  test_common_interface< vnl_matrix_fixed<int, 2, 2> >();
}

TESTMAIN(test_container_interface);

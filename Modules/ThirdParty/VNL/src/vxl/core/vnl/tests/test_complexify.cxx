#include <cstdlib>
#include <testlib/testlib_test.h>

#include <vcl_iostream.h>
#include <vcl_complex.h>
#include <vnl/vnl_complexify.h>
#include <vnl/vnl_real.h>
#include <vnl/vnl_imag.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_vector_fixed.h>

#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matrix_fixed.h>

#include <vnl/vnl_diag_matrix.h>
#include <vnl/vnl_diag_matrix_fixed.h>

#include <vnl/vnl_sym_matrix.h>

template <typename ValueType>
static
void
test_complexify_instance(const ValueType &re, const ValueType &im)
{

  const unsigned int length = 2;

  vcl_cout << "##################" << vcl_endl;
  vcl_cout << "Testing vnl_vector" << vcl_endl;
  vcl_cout << "##################" << vcl_endl;

  vnl_vector<ValueType> r_vector(length,re);
  vnl_vector<ValueType> i_vector(length,im);
  vnl_vector<vcl_complex<ValueType> > c_vector
    = vnl_complexify(r_vector);
  for (unsigned int i = 0; i < length; ++i)
    {
    TEST("vnl_vector real",re,c_vector.get(i).real());
    TEST("vnl_vector imag",0.0,c_vector.get(i).imag());
    }
  c_vector
    = vnl_complexify(r_vector,i_vector);
  for (unsigned int i = 0; i < length; ++i)
    {
    TEST("vnl_vector real",re,c_vector.get(i).real());
    TEST("vnl_vector imag",im,c_vector.get(i).imag());
    }
  r_vector = vnl_imag(c_vector);
  i_vector = vnl_real(c_vector);
  for (unsigned int i = 0; i < length; ++i)
    {
    TEST("vnl_vector vnl_real",re,i_vector.get(i));
    TEST("vnl_vector vnl_imag",im,r_vector.get(i));
    }

  vcl_cout << "########################" << vcl_endl;
  vcl_cout << "Testing vnl_vector_fixed" << vcl_endl;
  vcl_cout << "########################" << vcl_endl;

  vnl_vector_fixed<ValueType,length> r_vector_fixed(re);
  vnl_vector_fixed<ValueType,length> i_vector_fixed(im);
  vnl_vector_fixed<vcl_complex<ValueType>,length > c_vector_fixed
    = vnl_complexify(r_vector_fixed);
  for (unsigned int i = 0; i < length; ++i)
    {
    TEST("vnl_vector_fixed real",re,c_vector_fixed.get(i).real());
    TEST("vnl_vector_fixed imag",0.0,c_vector_fixed.get(i).imag());
    }
  c_vector_fixed
    = vnl_complexify(r_vector_fixed,i_vector_fixed);
  for (unsigned int i = 0; i < length; ++i)
    {
    TEST("vnl_vector_fixed real",re,c_vector_fixed.get(i).real());
    TEST("vnl_vector_fixed imag",im,c_vector_fixed.get(i).imag());
    }
  r_vector_fixed = vnl_imag(c_vector_fixed);
  i_vector_fixed = vnl_real(c_vector_fixed);
  for (unsigned int i = 0; i < length; ++i)
    {
    TEST("vnl_vector_fixed vnl_real",re,i_vector_fixed.get(i));
    TEST("vnl_vector_fixed vnl_imag",im,r_vector_fixed.get(i));
    }

  vcl_cout << "##################" << vcl_endl;
  vcl_cout << "Testing vnl_matrix" << vcl_endl;
  vcl_cout << "##################" << vcl_endl;

  vnl_matrix<ValueType> r_matrix(length,length,re);
  vnl_matrix<ValueType> i_matrix(length,length,im);
  vnl_matrix<vcl_complex<ValueType> > c_matrix
    = vnl_complexify(r_matrix);
  for (unsigned int c = 0; c < length; ++c)
    {
    for (unsigned int r = 0; r < length; ++r)
      {
      TEST("vnl_matrix real",re,c_matrix.get(r,c).real());
      TEST("vnl_matrix imag",0.0,c_matrix.get(r,c).imag());
      }
    }
  c_matrix
    = vnl_complexify(r_matrix,i_matrix);
  for (unsigned int c = 0; c < length; ++c)
    {
    for (unsigned int r = 0; r < length; ++r)
      {
      TEST("vnl_matrix real",re,c_matrix.get(r,c).real());
      TEST("vnl_matrix imag",im,c_matrix.get(r,c).imag());
      }
    }
  r_matrix = vnl_imag(c_matrix);
  i_matrix = vnl_real(c_matrix);
  for (unsigned int c = 0; c < length; ++c)
    {
    for (unsigned int r = 0; r < length; ++r)
      {
      TEST("vnl_matrix vnl_real",re,i_matrix.get(r,c));
      TEST("vnl_matrix vnl_imag",im,r_matrix.get(r,c));
      }
    }

  vcl_cout << "########################" << vcl_endl;
  vcl_cout << "Testing vnl_matrix_fixed" << vcl_endl;
  vcl_cout << "########################" << vcl_endl;

  vnl_matrix_fixed<ValueType,length,length> r_matrix_fixed(re);
  vnl_matrix_fixed<ValueType,length,length> i_matrix_fixed(im);
  vnl_matrix_fixed<vcl_complex<ValueType>,length,length > c_matrix_fixed
    = vnl_complexify(r_matrix_fixed);
  for (unsigned int c = 0; c < length; ++c)
    {
    for (unsigned int r = 0; r < length; ++r)
      {
      TEST("vnl_matrix_fixed real",re,c_matrix_fixed.get(r,c).real());
      TEST("vnl_matrix_fixed imag",0.0,c_matrix_fixed.get(r,c).imag());
      }
    }
  c_matrix_fixed
    = vnl_complexify(r_matrix_fixed,i_matrix_fixed);
  for (unsigned int c = 0; c < length; ++c)
    {
    for (unsigned int r = 0; r < length; ++r)
      {
      TEST("vnl_matrix_fixed real",re,c_matrix_fixed.get(r,c).real());
      TEST("vnl_matrix_fixed imag",im,c_matrix_fixed.get(r,c).imag());
      }
    }
  r_matrix_fixed = vnl_imag(c_matrix_fixed);
  i_matrix_fixed = vnl_real(c_matrix_fixed);
  for (unsigned int c = 0; c < length; ++c)
    {
    for (unsigned int r = 0; r < length; ++r)
      {
      TEST("vnl_matrix_fixed vnl_real",re,i_matrix_fixed.get(r,c));
      TEST("vnl_matrix_fixed vnl_imag",im,r_matrix_fixed.get(r,c));
      }
    }

  vcl_cout << "#######################" << vcl_endl;
  vcl_cout << "Testing vnl_diag_matrix" << vcl_endl;
  vcl_cout << "#######################" << vcl_endl;

  vnl_diag_matrix<ValueType> r_diag_matrix(length,re);
  vnl_diag_matrix<ValueType> i_diag_matrix(length,im);
  vnl_diag_matrix<vcl_complex<ValueType> > c_diag_matrix
    = vnl_complexify(r_diag_matrix);
  for (unsigned int i = 0; i < length; ++i)
    {
    TEST("vnl_diag_matrix real",re,c_diag_matrix.get(i,i).real());
    TEST("vnl_diag_matrix imag",0.0,c_diag_matrix.get(i,i).imag());
    }
  c_diag_matrix
    = vnl_complexify(r_diag_matrix,i_diag_matrix);
  for (unsigned int i = 0; i < length; ++i)
    {
    TEST("vnl_diag_matrix real",re,c_diag_matrix.get(i,i).real());
    TEST("vnl_diag_matrix imag",im,c_diag_matrix.get(i,i).imag());
    }
  r_diag_matrix = vnl_imag(c_diag_matrix);
  i_diag_matrix = vnl_real(c_diag_matrix);
  for (unsigned int i = 0; i < length; ++i)
    {
    TEST("vnl_diag_matrix vnl_real",re,i_diag_matrix.get(i,i));
    TEST("vnl_diag_matrix vnl_imag",im,r_diag_matrix.get(i,i));
    }

  vcl_cout << "#############################" << vcl_endl;
  vcl_cout << "Testing vnl_diag_matrix_fixed" << vcl_endl;
  vcl_cout << "#############################" << vcl_endl;

  vnl_diag_matrix_fixed<ValueType,length> r_diag_matrix_fixed(re);
  vnl_diag_matrix_fixed<ValueType,length> i_diag_matrix_fixed(im);
  vnl_diag_matrix_fixed<vcl_complex<ValueType>,length > c_diag_matrix_fixed
    = vnl_complexify(r_diag_matrix_fixed);
  for (unsigned int i = 0; i < length; ++i)
    {
    TEST("vnl_diag_matrix_fixed real",re,c_diag_matrix_fixed.get(i,i).real());
    TEST("vnl_diag_matrix_fixed imag",0.0,c_diag_matrix_fixed.get(i,i).imag());
    }
  c_diag_matrix_fixed
    = vnl_complexify(r_diag_matrix_fixed,i_diag_matrix_fixed);
  for (unsigned int i = 0; i < length; ++i)
    {
    TEST("vnl_diag_matrix_fixed real",re,c_diag_matrix_fixed.get(i,i).real());
    TEST("vnl_diag_matrix_fixed imag",im,c_diag_matrix_fixed.get(i,i).imag());
    }
  r_diag_matrix_fixed = vnl_imag(c_diag_matrix_fixed);
  i_diag_matrix_fixed = vnl_real(c_diag_matrix_fixed);
  for (unsigned int i = 0; i < length; ++i)
    {
    TEST("vnl_diag_matrix_fixed vnl_real",re,i_diag_matrix_fixed.get(i,i));
    TEST("vnl_diag_matrix_fixed vnl_imag",im,r_diag_matrix_fixed.get(i,i));
    }

  vcl_cout << "######################" << vcl_endl;
  vcl_cout << "Testing vnl_sym_matrix" << vcl_endl;
  vcl_cout << "######################" << vcl_endl;

  vnl_sym_matrix<ValueType> r_sym_matrix(length,re);
  vnl_sym_matrix<ValueType> i_sym_matrix(length,im);
  vnl_sym_matrix<vcl_complex<ValueType> > c_sym_matrix
    = vnl_complexify(r_sym_matrix);
  for (unsigned int c = 0; c < length; ++c)
    {
    for (unsigned int r = 0; r < length; ++r)
      {
      TEST("vnl_sym_matrix real",re,c_sym_matrix(r,c).real()); // no get()
      TEST("vnl_sym_matrix imag",0.0,c_sym_matrix(r,c).imag());
      }
    }
  c_sym_matrix
    = vnl_complexify(r_sym_matrix,i_sym_matrix);
  for (unsigned int c = 0; c < length; ++c)
    {
    for (unsigned int r = 0; r < length; ++r)
      {
      TEST("vnl_sym_matrix real",re,c_sym_matrix(r,c).real()); // no get()
      TEST("vnl_sym_matrix imag",im,c_sym_matrix(r,c).imag());
      }
    }
  r_sym_matrix = vnl_imag(c_sym_matrix);
  i_sym_matrix = vnl_real(c_sym_matrix);
  for (unsigned int c = 0; c < length; ++c)
    {
    for (unsigned int r = 0; r < length; ++r)
      {
      TEST("vnl_sym_matrix vnl_real",re,i_sym_matrix(r,c));
      TEST("vnl_sym_matrix vnl_imag",im,r_sym_matrix(r,c));
      }
    }

}

static
void
test_complexify()
{
  test_complexify_instance<float>(7.5,8.5);
  test_complexify_instance<double>(7.5,8.5);
}

TESTMAIN( test_complexify );

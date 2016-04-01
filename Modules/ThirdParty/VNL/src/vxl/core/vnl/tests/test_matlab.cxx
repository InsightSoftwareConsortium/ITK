// This is core/vnl/tests/test_matlab.cxx
#include <iostream>
#include <cstring>
#include <fstream>
#include <testlib/testlib_test.h>
//:
// \file
// \author fsm

#include <vcl_compiler.h>

#include <vpl/vpl.h>

#include <vul/vul_temp_filename.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_matlab_print.h>
#include <vnl/vnl_matlab_write.h>
#include <vnl/vnl_matlab_read.h>
#include <vnl/vnl_matlab_header.h>

#if VXL_LITTLE_ENDIAN
# define NONnative_BYTE_ORDER vnl_matlab_header::vnl_BIG_ENDIAN
#else
# define NONnative_BYTE_ORDER vnl_matlab_header::vnl_LITTLE_ENDIAN
#endif

// this duplicates code from vnl_matlab_write, but it's the only way to
// get a byte-swapped file, short of reading in a native file and swapping it
// and writing it back out, and that isn't any easier.
void matlab_write_swapped(std::ostream &f,
                          float *array,
                          unsigned size,
                          char const *name)
{
  vnl_matlab_header hdr;
  hdr.type = NONnative_BYTE_ORDER
             + vnl_matlab_header::vnl_COLUMN_WISE
             + vnl_matlab_header::vnl_SINGLE_PRECISION;
  hdr.rows = (long)size;
  hdr.cols = 1;
  hdr.imag = 0;                 // not complex
  hdr.namlen = (unsigned long)std::strlen(name)+1L;
  byteswap::swap32(&hdr.type);
  byteswap::swap32(&hdr.rows);
  byteswap::swap32(&hdr.cols);
  byteswap::swap32(&hdr.imag);
  byteswap::swap32(&hdr.namlen);
  f.write((char const *)&hdr, sizeof(hdr));
  f.write((char const *)name, std::strlen(name)+1);
  for (unsigned i = 0; i < size; ++i)
  {
    float dummy = array[i];
    byteswap::swap32(&dummy);
    f.write((char const *)&dummy,sizeof(dummy));
  }
}

static void fsm_assert_(int lineno, bool pass, char const *expr)
{
  std::cout << __FILE__ " : " << lineno << std::endl;
  testlib_test_assert(expr, pass);
}
#define fsm_assert(c) fsm_assert_(__LINE__, c, #c);

static void test_matlab()
{
  vnl_vector<float> v(4);
  vnl_vector_fixed<float,4> vf;
  for (unsigned i=0; i<v.size(); ++i)
    vf[i] = v[i] = 0.1f*float(i);

  vnl_matrix<double> M(3,4);
  vnl_matrix_fixed<double,3,4> Mf;
  for (unsigned i=0; i<M.rows(); ++i)
    for (unsigned j=0; j<M.cols(); ++j)
      Mf(i,j) = M(i,j) = 0.1*i*j;

  { // vnl_matlab_print
    std::cout << v << std::endl;
    vnl_matlab_print(std::cout, v, "v");

    std::cout << vf << std::endl;
    vnl_matlab_print(std::cout, vf, "vf");

    std::cout << M << std::endl;
    vnl_matlab_print(std::cout, M, "M") << std::endl;

    std::cout << Mf << std::endl;
    vnl_matlab_print(std::cout, Mf, "Mf") << std::endl;
  }

  // vnl_matlab_write, vnl_matlab_read
  {
    std::string tmp_nam = vul_temp_filename(),
               tmp_nam2 = vul_temp_filename();
    char const *file = tmp_nam!="" ? tmp_nam.c_str() : "smoo.mat";
    char const *file2 = tmp_nam2!="" ? tmp_nam2.c_str() : "smoo2.mat";
    {
      std::ofstream f(file);
#ifdef LEAVE_IMAGES_BEHIND
      vpl_chmod(file, 0666); // -rw-rw-rw-
#endif
      vnl_matlab_write(f, v.begin(), v.size(), "v");
      vnl_matlab_write(f, (double const * const *)M.data_array(), M.rows(), M.cols(), (char const *)"M");

      // write swapped matlab file
      std::ofstream f2(file2);
      matlab_write_swapped(f2, v.begin(), v.size(), "v");
    }
    {
      std::ifstream f(file);

      vnl_matlab_readhdr vh(f);
      fsm_assert( vh?true:false );
      fsm_assert( vh.is_single());
      fsm_assert( vh.rows() == (int)v.size());
      fsm_assert( vh.cols() == 1);
      fsm_assert(!vh.is_complex());
      fsm_assert(std::strcmp(vh.name(), "v")==0);
      vnl_vector<float> v_(v.size());
      fsm_assert( vh.read_data(v_.begin()));
      fsm_assert(v_ == v);

      std::ifstream f2(file2);
      vnl_matlab_readhdr vh2(f2);
      fsm_assert( vh2?true:false );
      fsm_assert( vh2.is_single());
      fsm_assert( vh2.rows() == (int)v.size());
      fsm_assert( vh2.cols() == 1);
      fsm_assert(!vh2.is_complex());
      fsm_assert(std::strcmp(vh2.name(), "v")==0);
      vnl_vector<float> v_2(v.size());
      fsm_assert( vh2.read_data(v_2.begin()));
      fsm_assert( v_2 == v);

      vnl_matlab_readhdr Mh(f);
      fsm_assert( Mh?true:false );
      fsm_assert(!Mh.is_single());
      fsm_assert( Mh.rows() == (int)M.rows());
      fsm_assert( Mh.cols() == (int)M.cols());
      fsm_assert( Mh.is_rowwise());
      fsm_assert(!Mh.is_complex());
      fsm_assert(std::strcmp(Mh.name(), "M")==0);
      vnl_matrix<double> M_( M.rows(), M.cols());
      fsm_assert( Mh.read_data(M_.data_array()));
      fsm_assert(M_ == M);
      //vnl_matlab_print(cout, M, "M");
      //vnl_matlab_print(cout, M_, "M_");
    }
#ifndef LEAVE_IMAGES_BEHIND
    vpl_unlink(file);
#endif
  }
}

TESTMAIN(test_matlab);

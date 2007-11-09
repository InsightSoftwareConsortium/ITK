// This is core/vnl/tests/test_matlab.cxx
/*
  fsm
*/
#include <vcl_cstring.h>
#include <vcl_fstream.h>

#include <vpl/vpl.h>

#include <vul/vul_temp_filename.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_matlab_print.h>
#include <vnl/vnl_matlab_write.h>
#include <vnl/vnl_matlab_read.h>

#include <testlib/testlib_test.h>

static void fsm_assert_(int lineno, bool pass, char const *expr)
{
  vcl_cout << __FILE__ " : " << lineno << vcl_endl;
  testlib_test_assert(expr, pass);
}
#define fsm_assert(c) fsm_assert_(__LINE__, c, #c);

static void test_matlab()
{
  vnl_vector<float> v(4);
  vnl_vector_fixed<float,4> vf;
  for (unsigned i=0; i<v.size(); ++i)
    vf[i] = v[i] = 0.1f*i;

  vnl_matrix<double> M(3,4);
  vnl_matrix_fixed<double,3,4> Mf;
  for (unsigned i=0; i<M.rows(); ++i)
    for (unsigned j=0; j<M.cols(); ++j)
      Mf(i,j) = M(i,j) = 0.1*i*j;

  { // vnl_matlab_print
    vcl_cout << v << vcl_endl;
    vnl_matlab_print(vcl_cout, v, "v");

    vcl_cout << vf << vcl_endl;
    vnl_matlab_print(vcl_cout, vf, "vf");

    vcl_cout << M << vcl_endl;
    vnl_matlab_print(vcl_cout, M, "M") << vcl_endl;

    vcl_cout << Mf << vcl_endl;
    vnl_matlab_print(vcl_cout, Mf, "Mf") << vcl_endl;
  }

  // vnl_matlab_write, vnl_matlab_read
  {
    vcl_string tmp_nam = vul_temp_filename();
    char const *file = tmp_nam!="" ? tmp_nam.c_str() : "smoo.mat";
    {
      vcl_ofstream f(file);
#ifdef LEAVE_IMAGES_BEHIND
      vpl_chmod(file, 0666); // -rw-rw-rw-
#endif
      vnl_matlab_write(f, v.begin(), v.size(), "v");
      vnl_matlab_write(f, (double const * const *)M.data_array(), M.rows(), M.cols(), (char const *)"M");
    }
    {
      vcl_ifstream f(file);

      vnl_matlab_readhdr vh(f);
      fsm_assert( vh?true:false );
      fsm_assert( vh.is_single());
      fsm_assert( vh.rows() == (int)v.size());
      fsm_assert( vh.cols() == 1);
      fsm_assert(!vh.is_complex());
      fsm_assert(vcl_strcmp(vh.name(), "v")==0);
      vnl_vector<float> v_(v.size());
      fsm_assert( vh.read_data(v_.begin()));
      fsm_assert(v_ == v);

      vnl_matlab_readhdr Mh(f);
      fsm_assert( Mh?true:false );
      fsm_assert(!Mh.is_single());
      fsm_assert( Mh.rows() == (int)M.rows());
      fsm_assert( Mh.cols() == (int)M.cols());
      fsm_assert( Mh.is_rowwise());
      fsm_assert(!Mh.is_complex());
      fsm_assert(vcl_strcmp(Mh.name(), "M")==0);
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

/*
  fsm@robots.ox.ac.uk
*/
#include <vcl_fstream.h>
#include <vcl_cmath.h>

#include <vpl/vpl_unistd.h>

#include <vnl/vnl_test.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matlab_print.h>
#include <vnl/vnl_matlab_write.h>
#include <vnl/vnl_matlab_read.h>

static void fsm_assert_(int lineno, bool pass, char const *expr) {
  vcl_cout << __FILE__ ":" << lineno << vcl_endl;
  Assert(expr, pass);
}
#define fsm_assert(c) fsm_assert_(__LINE__, c, #c);

void test_matlab() {
  vnl_vector<float> v(7);
  for (unsigned i=0; i<v.size(); ++i)
    v[i] = i;

  vnl_matrix<double> M(6,8);
  for (unsigned i=0; i<M.rows(); ++i)
    for (unsigned j=0; j<M.cols(); ++j)
      M(i,j) = i*j;
  
  { // vnl_matlab_print
    vcl_cout << v << vcl_endl;
    vnl_matlab_print(vcl_cout, v, "v");
    
    vcl_cout << M << vcl_endl;
    vnl_matlab_print(vcl_cout, M, "M") << vcl_endl;
  }
  
  // vnl_matlab_write, vnl_matlab_read
  {
    char const *file = "/tmp/smoo.mat";
    { 
      vcl_ofstream f(file);
      vnl_matlab_write(f, v.begin(), v.size(), "v");
      vnl_matlab_write(f, (double const * const *)M.data_array(), M.rows(), M.cols(), (char const *)"M");
    }
    {
      vcl_ifstream f(file);

      vnl_matlab_readhdr vh(f);
      fsm_assert( vh);
      fsm_assert( vh.is_single());
      fsm_assert( vh.rows() == v.size());
      fsm_assert( vh.cols() == 1);
      fsm_assert(!vh.is_complex());
      fsm_assert(strcmp(vh.name(), "v")==0);
      vnl_vector<float> v_(v.size());
      fsm_assert( vh.read_data(v_.begin()));
      fsm_assert(v_ == v);
      
      vnl_matlab_readhdr Mh(f);
      fsm_assert( Mh);
      fsm_assert(!Mh.is_single());
      fsm_assert( Mh.rows() == M.rows());
      fsm_assert( Mh.cols() == M.cols());
      fsm_assert( Mh.is_rowwise());
      fsm_assert(!Mh.is_complex());
      fsm_assert(strcmp(Mh.name(), "M")==0);
      vnl_matrix<double> M_( M.rows(), M.cols());
      fsm_assert( Mh.read_data(M_.data_array()));
      fsm_assert(M_ == M);
      //vnl_matlab_print(cout, M, "M");
      //vnl_matlab_print(cout, M_, "M_");
    }
    vpl_unlink(file);
  }
}

TESTMAIN(test_matlab);

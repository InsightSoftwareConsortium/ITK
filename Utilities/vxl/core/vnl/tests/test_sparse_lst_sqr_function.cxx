#include <testlib/testlib_test.h>
#include <vnl/vnl_sparse_lst_sqr_function.h>
#include <vcl_iostream.h>


// all ai.size() == 2, all bj.size() == 3, all eij.size() == 2
class test_func1 : public vnl_sparse_lst_sqr_function
{
 public:
  test_func1(unsigned int num_a, unsigned int num_b,
             const vcl_vector<vcl_vector<bool> >& xmask,
             UseGradient g = use_gradient)
   : vnl_sparse_lst_sqr_function(num_a,2,num_b,3,xmask,2,g) {}

  void fij(int /*i*/, int /*j*/, vnl_vector<double> const& ai,
           vnl_vector<double> const& bj, vnl_vector<double>& eij)
  {
    eij[0] = (ai[0]*ai[0]-bj[0]*ai[1])*bj[2]*bj[2]*bj[2];
    eij[1] = (ai[1]*ai[1]-bj[1]*ai[0])*bj[2]*bj[2]*bj[2];
  }

  void jac_Aij(int /*i*/, int /*j*/, vnl_vector<double> const& ai,
               vnl_vector<double> const& bj, vnl_matrix<double>& Aij)
  {
    Aij[0][0] = 2.0*ai[0]*bj[2]*bj[2]*bj[2];
    Aij[0][1] = -bj[0]*bj[2]*bj[2]*bj[2];
    Aij[1][0] = -bj[1]*bj[2]*bj[2]*bj[2];
    Aij[1][1] = 2.0*ai[1]*bj[2]*bj[2]*bj[2];
  }

  void jac_Bij(int /*i*/, int /*j*/, vnl_vector<double> const& ai,
               vnl_vector<double> const& bj, vnl_matrix<double>& Bij)
  {
    Bij[0][0] = -ai[1]*bj[2]*bj[2]*bj[2];
    Bij[0][1] = 0.0;
    Bij[0][2] = (ai[0]*ai[0]-bj[0]*ai[1])*3.0*bj[2]*bj[2];
    Bij[1][0] = 0.0;
    Bij[1][1] = -ai[0]*bj[2]*bj[2]*bj[2];
    Bij[1][2] = (ai[1]*ai[1]-bj[1]*ai[0])*3.0*bj[2]*bj[2];
  }
};


static void test_sparse_lst_sqr_function()
{
   vcl_vector<bool> null_row(4,false);
   vcl_vector<vcl_vector<bool> > mask(3,null_row);

   //        |1 1 0 0|
   // mask = |1 1 1 1|
   //        |0 1 1 1|
   mask[0][0] = true;
   mask[0][1] = true;
   mask[1][0] = true;
   mask[1][1] = true;
   mask[1][2] = true;
   mask[1][3] = true;
   mask[2][1] = true;
   mask[2][2] = true;
   mask[2][3] = true;

   test_func1 my_func(3,4,mask,vnl_sparse_lst_sqr_function::use_gradient);

   TEST("number_of_a",my_func.number_of_a(),3);
   TEST("number_of_b",my_func.number_of_b(),4);
   TEST("number_of_e",my_func.number_of_e(),9);

   bool num_valid = true;
   bool index_valid = true;
   for (unsigned int i=0; i<my_func.number_of_a(); ++i) {
     num_valid = (my_func.number_of_params_a(i) == 2) && num_valid;
     index_valid = (my_func.index_a(i) == i*2) && index_valid;
   }
   TEST("number_of_params_a",num_valid,true);
   TEST("index_a",index_valid,true);

   num_valid = true;
   index_valid = true;
   for (unsigned int j=0; j<my_func.number_of_b(); ++j) {
     num_valid = (my_func.number_of_params_b(j) == 3) && num_valid;
     index_valid = (my_func.index_b(j) == j*3) && index_valid;
   }
   TEST("number_of_params_b",num_valid,true);
   TEST("index_b",index_valid,true);

   num_valid = true;
   index_valid = true;
   for (unsigned int k=0; k<my_func.number_of_e(); ++k) {
     num_valid = (my_func.number_of_residuals(k) == 2) && num_valid;
     index_valid = (my_func.index_e(k) == k*2) && index_valid;
   }
   TEST("number_of_residuals",num_valid,true);
   TEST("index_e",index_valid,true);

   vnl_vector<double> ai(2),bj(3),e(2);
   ai[0] = 5.0;  ai[1] = -1.0;
   bj[0] = 1.2;  bj[1] = 1.5;  bj[2] = 2.2;
   const double step = 0.001;

   my_func.fij(0,0,ai,bj,e);
   vcl_cout << "e  = " << e << vcl_endl;
   vnl_matrix<double> Aij(2,2), Bij(2,3), fd_Aij(2,2), fd_Bij(2,3);
   my_func.jac_Aij(0,0,ai,bj,Aij);
   my_func.fd_jac_Aij(0,0,ai,bj,fd_Aij,step);
   my_func.jac_Bij(0,0,ai,bj,Bij);
   my_func.fd_jac_Bij(0,0,ai,bj,fd_Bij,step);

   vcl_cout << "Aij =\n" << Aij << vcl_endl
            << "fd Aij =\n" << fd_Aij << vcl_endl;
   TEST("finite difference Aij", (Aij-fd_Aij).absolute_value_max()<0.001,true);
   vcl_cout << "Bij =\n" << Bij << vcl_endl
            << "fd Bij =\n" << fd_Bij << vcl_endl;
   TEST("finite difference Bij", (Bij-fd_Bij).absolute_value_max()<0.001,true);
}

TESTMAIN(test_sparse_lst_sqr_function);

#include <iostream>
#include <limits>
#include <testlib/testlib_test.h>
#include <vnl/vnl_sparse_lst_sqr_function.h>
#include <vcl_compiler.h>


// all ai.size() == 2, all bj.size() == 3, c.size() == 2, all eij.size() == 2
class test_func1 : public vnl_sparse_lst_sqr_function
{
 public:
  test_func1(unsigned int num_a, unsigned int num_b,
             const std::vector<std::vector<bool> >& xmask,
             UseGradient g = use_gradient,
             UseWeights w = use_weights)
   : vnl_sparse_lst_sqr_function(num_a,2,num_b,3,2,xmask,2,g,w) {}

  void fij(int /*i*/, int /*j*/,
           vnl_vector<double> const& ai,
           vnl_vector<double> const& bj,
           vnl_vector<double> const& c,
           vnl_vector<double>& eij)
  {
    eij[0] = (ai[0]*ai[0]-bj[0]*ai[1])*bj[2]*bj[2]*bj[2] + c[0]*ai[0];
    eij[1] = (ai[1]*ai[1]-bj[1]*ai[0])*bj[2]*bj[2]*bj[2] + c[1]*ai[1];
  }

  void jac_Aij(int /*i*/, int /*j*/,
               vnl_vector<double> const& ai,
               vnl_vector<double> const& bj,
               vnl_vector<double> const& c,
               vnl_matrix<double>& Aij)
  {
    Aij[0][0] = 2.0*ai[0]*bj[2]*bj[2]*bj[2] + c[0];
    Aij[0][1] = -bj[0]*bj[2]*bj[2]*bj[2];
    Aij[1][0] = -bj[1]*bj[2]*bj[2]*bj[2];
    Aij[1][1] = 2.0*ai[1]*bj[2]*bj[2]*bj[2] + c[1];
  }

  void jac_Bij(int /*i*/, int /*j*/,
               vnl_vector<double> const& ai,
               vnl_vector<double> const& bj,
               vnl_vector<double> const& c,
               vnl_matrix<double>& Bij)
  {
    Bij[0][0] = -ai[1]*bj[2]*bj[2]*bj[2];
    Bij[0][1] = 0.0;
    Bij[0][2] = (ai[0]*ai[0]-bj[0]*ai[1])*3.0*bj[2]*bj[2];
    Bij[1][0] = 0.0;
    Bij[1][1] = -ai[0]*bj[2]*bj[2]*bj[2];
    Bij[1][2] = (ai[1]*ai[1]-bj[1]*ai[0])*3.0*bj[2]*bj[2];
  }

  void jac_Cij(int /*i*/, int /*j*/,
               vnl_vector<double> const& ai,
               vnl_vector<double> const& bj,
               vnl_vector<double> const& c,
               vnl_matrix<double>& Cij)
  {
    Cij[0][0] = ai[0];
    Cij[0][1] = 0.0;
    Cij[1][0] = 0.0;
    Cij[1][1] = ai[1];
  }

  void compute_weight_ij(int i, int j,
                         vnl_vector<double> const& /*ai*/,
                         vnl_vector<double> const& /*bj*/,
                         vnl_vector<double> const& /*c*/,
                         vnl_vector<double> const& /*fij*/,
                         double& weight)
  {
    weight = double((i+1)*(j+1))/(this->number_of_a()*this->number_of_b());
  }
};


static void test_sparse_lst_sqr_function()
{
   std::vector<bool> null_row(4,false);
   std::vector<std::vector<bool> > mask(3,null_row);

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

   TEST("number_of_params_c",2,my_func.number_of_params_c());

   num_valid = true;
   index_valid = true;
   for (unsigned int k=0; k<my_func.number_of_e(); ++k) {
     num_valid = (my_func.number_of_residuals(k) == 2) && num_valid;
     index_valid = (my_func.index_e(k) == k*2) && index_valid;
   }
   TEST("number_of_residuals",num_valid,true);
   TEST("index_e",index_valid,true);

   vnl_vector<double> ai(2),bj(3),c(2),e(2);
   ai[0] = 5.0;  ai[1] = -1.0;
   bj[0] = 1.2;  bj[1] = 1.5;  bj[2] = 2.2;
   c[0] = 1.0;   c[1] = 2.0;
   const double step = 0.001;

   my_func.fij(0,0,ai,bj,c,e);
   std::cout << "e  = " << e << std::endl;
   vnl_matrix<double> Aij(2,2), Bij(2,3), Cij(2,2);
   vnl_matrix<double> fd_Aij(2,2), fd_Bij(2,3), fd_Cij(2,2);
   my_func.jac_Aij(0,0,ai,bj,c,Aij);
   my_func.fd_jac_Aij(0,0,ai,bj,c,fd_Aij,step);
   my_func.jac_Bij(0,0,ai,bj,c,Bij);
   my_func.fd_jac_Bij(0,0,ai,bj,c,fd_Bij,step);
   my_func.jac_Cij(0,0,ai,bj,c,Cij);
   my_func.fd_jac_Cij(0,0,ai,bj,c,fd_Cij,step);

   std::cout << "Aij =\n" << Aij << std::endl
            << "fd Aij =\n" << fd_Aij << std::endl;
   TEST("finite difference Aij", (Aij-fd_Aij).absolute_value_max()<0.001,true);
   std::cout << "Bij =\n" << Bij << std::endl
            << "fd Bij =\n" << fd_Bij << std::endl;
   TEST("finite difference Bij", (Bij-fd_Bij).absolute_value_max()<0.001,true);
   std::cout << "Cij =\n" << Cij << std::endl
            << "fd Cij =\n" << fd_Cij << std::endl;
   TEST("finite difference Cij", (Cij-fd_Cij).absolute_value_max()<0.001,true);


   vnl_vector<double> a(my_func.index_a(my_func.number_of_a()),1.0);
   a[0] = 5.0;
   a[2] = -1.0;
   a[5] = 0.0;
   vnl_vector<double> b(my_func.index_b(my_func.number_of_b()),2.0);
   b[1] = 1.0;
   b[5] = 0.0;
   b[6] = -2.0;

   vnl_vector<double> f(my_func.index_e(my_func.number_of_e()));
   std::vector<vnl_matrix<double> > A(my_func.number_of_e(),vnl_matrix<double>(2,2,0.0));
   std::vector<vnl_matrix<double> > B(my_func.number_of_e(),vnl_matrix<double>(2,3,0.0));
   std::vector<vnl_matrix<double> > C(my_func.number_of_e(),vnl_matrix<double>(2,2,0.0));
   my_func.f(a,b,c,f);
   my_func.jac_blocks(a,b,c,A,B,C);


   // compute weights
   vnl_vector<double> weights(my_func.number_of_e(),1.0);
   my_func.compute_weights(a,b,c,f,weights);

   // apply weights
   vnl_vector<double> wf(f);
   std::vector<vnl_matrix<double> > wA(A), wB(B), wC(C);
   my_func.apply_weights(weights,wf);
   my_func.apply_weights(weights,wA,wB,wC);

   double w_norm = my_func.number_of_a()*my_func.number_of_b();
   for (unsigned int i=0; i<my_func.number_of_a(); ++i) {
     for (unsigned int j=0; j<my_func.number_of_b(); ++j) {
       int k = my_func.residual_indices()(i,j);
       if (k<0)
         continue;
       TEST_NEAR("valid weights", weights[k], double((i+1)*(j+1))/w_norm, 1e-10);
       TEST("computed weight wA", wA[k], A[k]*weights[k]);
       TEST("computed weight wB", wB[k], B[k]*weights[k]);
       TEST("computed weight wC", wC[k], C[k]*weights[k]);
       TEST_NEAR("computed weight wf", wf[2*k], f[2*k]*weights[k], 1e-10);
       TEST_NEAR("computed weight wf", wf[2*k+1], f[2*k+1]*weights[k], 1e-10);
     }
   }
   TEST("has weights", my_func.has_weights(), true);
}

TESTMAIN(test_sparse_lst_sqr_function);

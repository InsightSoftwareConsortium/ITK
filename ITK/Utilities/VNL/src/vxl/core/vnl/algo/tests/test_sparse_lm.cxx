#include <testlib/testlib_test.h>
#include <vnl/vnl_sparse_lst_sqr_function.h>
#include <vnl/algo/vnl_sparse_lm.h>
#include <vcl_iostream.h>
#include <vnl/vnl_random.h>


static void normalize(vnl_vector<double>& a, vnl_vector<double>& b)
{
  double x_mean=0.0, y_mean=0.0;
  unsigned int num_pts = b.size()/2;
  for (unsigned int i=0; i<num_pts; ++i) {
    x_mean += b[2*i];
    y_mean += b[2*i+1];
  }
  x_mean /= num_pts;
  y_mean /= num_pts;

  // translate points to the origin
  for (unsigned int i=0; i<num_pts; ++i) {
    b[2*i] -= x_mean;
    b[2*i+1] -= y_mean;
  }
  // translate cameras
  for (unsigned int i=0; i<a.size()/2; ++i) {
    a[2*i] -= x_mean;
    a[2*i+1] -= y_mean;
  }

  double mean_dist = 0.0;
  for (unsigned int i=0; i<num_pts; ++i) {
    mean_dist += vcl_sqrt(b[2*i]*b[2*i] + b[2*i+1]*b[2*i+1]);
  }
  mean_dist /= num_pts;

  // scale points
  for (unsigned int i=0; i<b.size(); ++i) {
    b[i] /= mean_dist;
  }
  // scale cameras
  for (unsigned int i=0; i<a.size(); ++i) {
    a[i] /= mean_dist;
  }
}


// all ai.size() == 2, all bj.size() == 2, all fxij.size() == 1
class bundle_2d : public vnl_sparse_lst_sqr_function
{
 public:
  bundle_2d(unsigned int num_cam, unsigned int num_pts,
            const vnl_vector<double>& data,
            const vcl_vector<vcl_vector<bool> >& xmask,
            UseGradient g = use_gradient)
   : vnl_sparse_lst_sqr_function(num_cam,2,num_pts,2,xmask,1,g), data_(data) {}

  void fij(int i, int j, vnl_vector<double> const& ai,
           vnl_vector<double> const& bj, vnl_vector<double>& fxij)
  {
    fxij[0] = (bj[0]-ai[0])/(bj[1]-ai[1]) - data_[residual_indices_(i,j)];
  }

  void jac_Aij(int /*i*/, int /*j*/, vnl_vector<double> const& ai,
               vnl_vector<double> const& bj, vnl_matrix<double>& Aij)
  {
    Aij[0][0] = 1.0/(ai[1]-bj[1]);
    double tmp = ai[1]-bj[1];
    Aij[0][1] = -(ai[0]-bj[0])/(tmp*tmp);
  }

  void jac_Bij(int /*i*/, int /*j*/, vnl_vector<double> const& ai,
               vnl_vector<double> const& bj, vnl_matrix<double>& Bij)
  {
    Bij[0][0] = 1.0/(bj[1]-ai[1]);
    double tmp = bj[1] - ai[1];
    Bij[0][1] = (ai[0]-bj[0])/(tmp*tmp);
  }

  vnl_vector<double> data_;
};


static void test_sparse_lm()
{
   vcl_vector<bool> null_row(20,true);
   vcl_vector<vcl_vector<bool> > mask(3,null_row);

   const double a_data[] = {-4.0,0.0, 0.0,-4.0, 4.0,2.0};
   const double b_data[] = {-4.0,10.0, -2.0,10.0, 0.0,10.0, 2.0,10.0, 4.0,10.0,
                            -4.0,12.0, -2.0,12.0, 0.0,12.0, 2.0,12.0, 4.0,12.0,
                            -4.0,14.0, -2.0,14.0, 0.0,14.0, 2.0,14.0, 4.0,14.0,
                            -4.0,16.0, -2.0,16.0, 0.0,16.0, 2.0,16.0, 4.0,16.0};


   vnl_vector<double> a(a_data,6), b(b_data,40), proj(60,0.0);

   // create a generator function with ideal data and zeros for all projections
   // the residuals of this functions are the ideal project points
   bundle_2d gen_func(3,20,proj,mask,vnl_sparse_lst_sqr_function::use_gradient);
   gen_func.f(a,b,proj);

   // scale and translate such that the mean is zero and
   // the average distance to the origin is 1
   normalize(a,b);

   // test 2D bundle adjustment with all data and no noise
   {
     // Start all cameras at (0,0) and all points at (1,1)
     vnl_vector<double> pa(6,0.0), pb(40,1.0);

     bundle_2d my_func(3,20,proj,mask,vnl_sparse_lst_sqr_function::use_gradient);

     vnl_sparse_lm slm(my_func);
     slm.minimize(pa,pb);
     slm.diagnose_outcome();

     // scale and translate such that the mean is zero and
     // the average distance to the origin is 1
     normalize(pa,pb);

     double rms_error_a = (a-pa).rms();
     double rms_error_b = (b-pb).rms();
     vcl_cout << "RMS camera error: "<<rms_error_a
              << "\nRMS points error: "<<rms_error_a << vcl_endl;
     TEST("convergence with all projections",rms_error_a + rms_error_b < 1e-10, true);
   }

   // remove several correspondences
   // we must see each point in at least 2 views
   // we must leave >= 46 residuals (since there are 46 unknowns)
   mask[0][1] = false;
   mask[0][17] = false;
   mask[0][18] = false;
   mask[0][19] = false;
   mask[1][2] = false;
   mask[1][6] = false;
   mask[1][7] = false;
   mask[1][8] = false;
   mask[1][15] = false;
   mask[1][16] = false;
   mask[2][0] = false;
   mask[2][10] = false;
   mask[2][11] = false;
   mask[2][12] = false;

   // create a subset of projections based on the mask
   vnl_crs_index crs(mask);
   vnl_vector<double> proj2(crs.num_non_zero());
   for (int i=0; i<crs.num_rows(); ++i) {
     for (int j=0; j<crs.num_cols(); ++j) {
       int k = crs(i,j);
       if (k >= 0)
         proj2[k]   = proj[i*crs.num_cols() + j];
     }
   }

   // test 2D bundle adjustment with missing data and no noise
   {
     // Start all cameras at (0,0) and all points at (1,1)
     vnl_vector<double> pa(6,0.0), pb(40,1.0);
     bundle_2d my_func(3,20,proj2,mask,vnl_sparse_lst_sqr_function::use_gradient);

     vnl_sparse_lm slm(my_func);
     slm.minimize(pa,pb);
     slm.diagnose_outcome();

     // scale and translate such that the mean is zero and
     // the average distance to the origin is 1
     normalize(pa,pb);

     double rms_error_a = (a-pa).rms();
     double rms_error_b = (b-pb).rms();
     vcl_cout << "RMS camera error: "<<rms_error_a
              << "\nRMS points error: "<<rms_error_a << vcl_endl;
     TEST("convergence with missing projections",rms_error_a + rms_error_b < 1e-10, true);
   }

   vnl_random rnd;

   // add uniform random noise to each measurement
   for (unsigned int i=0; i<proj2.size(); ++i) {
     proj2[i] += (rnd.drand32()-0.5)*1e-6;
   }

   // test 2D bundle adjustment with missing data and uniform noise
   {
     // Start all cameras at (0,0) and all points at (1,1)
     vnl_vector<double> pa(6,0.0), pb(40,1.0);
     bundle_2d my_func(3,20,proj2,mask,vnl_sparse_lst_sqr_function::use_gradient);

     vnl_sparse_lm slm(my_func);
     slm.set_verbose(true);
     slm.minimize(pa,pb);
     slm.diagnose_outcome();

     // scale and translate such that the mean is zero and
     // the average distance to the origin is 1
     normalize(pa,pb);

     double rms_error_a = (a-pa).rms();
     double rms_error_b = (b-pb).rms();
     vcl_cout << "RMS camera error: "<<rms_error_a
              << "\nRMS points error: "<<rms_error_a << vcl_endl;
     TEST("convergence with missing projections and noise",
          rms_error_a <1e-4 && rms_error_b < 1e-4, true);
   }
}

TESTMAIN(test_sparse_lm);

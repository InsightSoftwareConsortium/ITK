//-*- c++ -*-------------------------------------------------------------------
// Module: Minimization of Rosenbrock banana function
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 31 Aug 96
// Converted to vxl by Peter Vanroose, February 2000
//-----------------------------------------------------------------------------

#include <vcl_iostream.h>
#include <vnl/vnl_double_2.h>
#include <vnl/vnl_least_squares_function.h>
#include <vnl/vnl_least_squares_cost_function.h>

#include <vnl/algo/vnl_levenberg_marquardt.h>
#include <vnl/algo/vnl_amoeba.h>
#include <vnl/algo/vnl_powell.h>
#include <vnl/algo/vnl_conjugate_gradient.h>
#include <vnl/algo/vnl_lbfgs.h>

// Make a compute object for the "banana" function
// $        f(x, y) = \{ 10(y-x^2), 1-x \}        $
//
// It is called the banana function because of the way the
// curvature bends around the origin. It is notorious in
// optimization examples because of the slow convergence
// of most methods.

class vnl_rosenbrock : public vnl_least_squares_function {
public:
  vnl_rosenbrock(): vnl_least_squares_function(2, 2, no_gradient) {}

  void f(const vnl_vector<double>& x, vnl_vector<double>& fx)
  {
    fx[0] = 10*(x[1] - x[0]*x[0]);
    fx[1] = 1 - x[0];
  }
};

class vnl_rosenbrock_grad_cost_fun : public vnl_cost_function {
public:
  vnl_rosenbrock_grad_cost_fun(): vnl_cost_function(2) {}

  double f(const vnl_vector<double>& x) {
    double a = 10*(x[1] - x[0]*x[0]);
    double b = 1 - x[0];
    return a*a + b*b;
  }

  void gradf(const vnl_vector<double>& x, vnl_vector<double>& g) {
    double a = 10*(x[1] - x[0]*x[0]);
    double b = 1 - x[0];
    g[0] = 2 * a * (-20*x[0]) - 2 * b;
    g[1] = 20 * a;
  }
};

int main()
{
  // Set up a Rosenbrock compute object
  vnl_rosenbrock f;

  // Set up the initial guess
  vnl_double_2 x0(-1.9, 2);

  // Temp variable.
  vnl_vector<double> x(2);

  // Make a Levenberg Marquardt minimizer, attach f to it, and
  // run the minimization
  x = x0;
  vnl_levenberg_marquardt levmarq(f);
  levmarq.minimize(x);

  // Summarize the results, by querying the levmarq object.
  vcl_cout << "** LevenbergMarquardt default **" << vcl_endl;
  vcl_cout << "Rosenbrock min of " << levmarq.get_end_error() << " at " << x << vcl_endl;
  vcl_cout << "Iterations: " << levmarq.get_num_iterations() << "    ";
  vcl_cout << "Evaluations: " << levmarq.get_num_evaluations() << vcl_endl;

  levmarq.diagnose_outcome();

  // Now rerun the optimizer with a new, looser, X tolerance.
  //
  levmarq.set_x_tolerance(0.1);
  x = x0;
  levmarq.minimize(x);

  // Summarize the results. It has taken fewer iterations to reach the same
  // answer.
  vcl_cout << "** LevenbergMarquardt xtol=0.1 **" << vcl_endl
           << "Rosenbrock min of " << levmarq.get_end_error() << " at " << x << vcl_endl
           << "Iterations: " << levmarq.get_num_iterations() << "    "
           << "Evaluations: " << levmarq.get_num_evaluations() << vcl_endl;
  levmarq.diagnose_outcome();

  {
    // Make a vnl_cost_function, and use vnl_amoeba
    vcl_cout << "** Amoeba (Nelder Meade downhill simplex)  ** \n";
    vnl_least_squares_cost_function cf(&f);
    vnl_amoeba amoeba(cf);
    x = x0;
    amoeba.minimize(x);
    vcl_cout << "Rosenbrock min of " << cf.f(x) << " at " << x << vcl_endl
             << "Evaluations: " << amoeba.get_num_evaluations() << vcl_endl;
  }
  {
    vcl_cout << "** Conjugate Gradient ** \n";
    vnl_rosenbrock_grad_cost_fun rcf;
    vnl_conjugate_gradient cg(rcf);
    x = x0;
    cg.minimize(x);
    vcl_cout << "CG min of " << rcf.f(x) << " at " << x << vcl_endl;
    cg.diagnose_outcome();
  }

  {
    vcl_cout << "** LBFGS (Limited memory Broyden Fletcher Goldfarb Shanno) ** \n";
    vnl_rosenbrock_grad_cost_fun rcf;
    vnl_lbfgs lbfgs(rcf);
    x = x0;
    lbfgs.minimize(x);
    //    assert(lbfgs.get_end_error() == rcf.f(x));
    vcl_cout << "L-BFGS min of " << lbfgs.get_end_error() << " at " << x << vcl_endl
             << "Evaluations: " << lbfgs.get_num_evaluations() << vcl_endl;
  }

  {
    vcl_cout << "** Powell (Powell's direction set method) ** \n";
    vnl_rosenbrock_grad_cost_fun rcf;
    vnl_powell powell(&rcf);
    x = x0;
    powell.minimize(x);
    //    assert(lbfgs.get_end_error() == rcf.f(x));
    vcl_cout << "Powell min of " << powell.get_end_error() << " at " << x << vcl_endl
             << "Evaluations: " << powell.get_num_evaluations() << vcl_endl;
  }
  return 0;
}

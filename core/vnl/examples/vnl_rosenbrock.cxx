//-*- c++ -*-------------------------------------------------------------------
// Module: Minimization of Rosenbrock banana function
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 31 Aug 96
// Converted to vxl by Peter Vanroose, February 2000
//-----------------------------------------------------------------------------

#include <iostream>
#include <vcl_compiler.h>
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

class vnl_rosenbrock : public vnl_least_squares_function
{
 public:
  vnl_rosenbrock(): vnl_least_squares_function(2, 2, no_gradient) {}

  void f(const vnl_vector<double>& x, vnl_vector<double>& fx)
  {
    fx[0] = 10*(x[1] - x[0]*x[0]);
    fx[1] = 1 - x[0];
  }
};

class vnl_rosenbrock_grad_cost_fun : public vnl_cost_function
{
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
  vnl_vector<double> x = x0.as_ref();

  // Make a Levenberg Marquardt minimizer, attach f to it, and
  // run the minimization
  vnl_levenberg_marquardt levmarq(f);
  levmarq.minimize(x);

  // Summarize the results, by querying the levmarq object.
  std::cout << "** LevenbergMarquardt default **\n"
           << "Rosenbrock min of " << levmarq.get_end_error() << " at " << x << '\n'
           << "Iterations: " << levmarq.get_num_iterations()
           << "    Evaluations: " << levmarq.get_num_evaluations() << std::endl;

  levmarq.diagnose_outcome();

  // Now rerun the optimizer with a new, looser, X tolerance.
  //
  levmarq.set_x_tolerance(0.1);
  x = x0.as_ref();
  levmarq.minimize(x);

  // Summarize the results. It has taken fewer iterations to reach the same
  // answer.
  std::cout << "** LevenbergMarquardt xtol=0.1 **\n"
           << "Rosenbrock min of " << levmarq.get_end_error() << " at " << x << '\n'
           << "Iterations: " << levmarq.get_num_iterations()
           << "    Evaluations: " << levmarq.get_num_evaluations() << std::endl;
  levmarq.diagnose_outcome();

  {
    // Make a vnl_cost_function, and use vnl_amoeba
    std::cout << "** Amoeba (Nelder Meade downhill simplex)  **\n";
    vnl_least_squares_cost_function cf(&f);
    vnl_amoeba amoeba(cf);
    x = x0.as_ref();
    amoeba.minimize(x);
    std::cout << "Rosenbrock min of " << cf.f(x) << " at " << x << '\n'
             << "Evaluations: " << amoeba.get_num_evaluations() << std::endl;
  }
  {
    std::cout << "** Conjugate Gradient **\n";
    vnl_rosenbrock_grad_cost_fun rcf;
    vnl_conjugate_gradient cg(rcf);
    x = x0.as_ref();
    cg.minimize(x);
    std::cout << "CG min of " << rcf.f(x) << " at " << x << std::endl;
    cg.diagnose_outcome();
  }

  {
    std::cout << "** LBFGS (Limited memory Broyden Fletcher Goldfarb Shanno) **\n";
    vnl_rosenbrock_grad_cost_fun rcf;
    vnl_lbfgs lbfgs(rcf);
    x = x0.as_ref();
    lbfgs.minimize(x);
    //    assert(lbfgs.get_end_error() == rcf.f(x));
    std::cout << "L-BFGS min of " << lbfgs.get_end_error() << " at " << x << '\n'
             << "Evaluations: " << lbfgs.get_num_evaluations() << std::endl;
  }

  {
    std::cout << "** Powell (Powell's direction set method) **\n";
    vnl_rosenbrock_grad_cost_fun rcf;
    vnl_powell powell(&rcf);
    x = x0.as_ref();
    powell.minimize(x);
    //    assert(lbfgs.get_end_error() == rcf.f(x));
    std::cout << "Powell min of " << powell.get_end_error() << " at " << x << '\n'
             << "Evaluations: " << powell.get_num_evaluations() << std::endl;
  }
  return 0;
}

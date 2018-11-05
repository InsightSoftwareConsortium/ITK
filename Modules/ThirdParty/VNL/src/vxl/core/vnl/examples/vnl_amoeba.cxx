//-----------------------------------------------------------------------------
// Module: Minimization of Rosenbrock banana function, downhill simplex
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 31 Aug 96
// Converted to vxl by Peter Vanroose, February 2000
//-----------------------------------------------------------------------------
#include <iostream>
#include <vnl/vnl_double_2.h>
#include <vnl/vnl_cost_function.h>
#include <vnl/algo/vnl_amoeba.h>

// See rosenbrock.cxx for a description of this function.
class vnl_rosenbrock : public vnl_cost_function
{
 public:
  vnl_rosenbrock(): vnl_cost_function(2) {}

  double f(const vnl_vector<double>& x) override
  {
    double u = 10*(x[1] - x[0]*x[0]);
    double v = 1 - x[0];
    return u*u + v*v;
  }
};

int main()
{
  // Set up a Rosenbrock compute object
  vnl_rosenbrock f;

  // Set up the initial guess
  vnl_vector<double> x = vnl_double_2(-1.9,2.0).as_vector();

  // Make a Levenberg Marquardt minimizer, attach f to it, and
  // run the minimization
  vnl_amoeba::minimize(f, x);

  // Summarize the results
  std::cout << "Rosenbrock min at " << x << '\n';

  return 0;
}

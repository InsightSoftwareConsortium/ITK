//-*- c++ -*-------------------------------------------------------------------
//
// vnl_lbfgs
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 22 Aug 99
//
//-----------------------------------------------------------------------------

#include "vnl_lbfgs.h"
#include <vcl_cmath.h>
#include <vcl_cstdio.h>   // sprintf()
#include <vcl_iostream.h>

//: Default constructor.
// memory is set to 5, line_search_accuracy to 0.9.
// Calls init_parameters
vnl_lbfgs::vnl_lbfgs():
  f_(0)
{
  init_parameters();
}

//: Constructor. f is the cost function to be minimized.
// Calls init_parameters
vnl_lbfgs::vnl_lbfgs(vnl_cost_function& f):
  f_(&f)
{
  init_parameters();
}

//: Called by constructors.
// Memory is set to 5, line_search_accuracy to 0.9, default_step_length to 1.
void vnl_lbfgs::init_parameters()
{
  memory = 5;
  line_search_accuracy = 0.9;
  default_step_length = 1.0;
}

#if 0
# include <netlib/f2c.h>
#else
typedef int integer;
typedef double doublereal;
typedef int logical; // not bool
#endif

/* Common Block Declarations */

// Declare this as "static" (=local) to avoid multiple definition, because
// this struct is also defined in v3p/netlib/lbfgs.c
static struct {
// C    GTOL is a DOUBLE PRECISION variable with default value 0.9, which
// C        controls the accuracy of the line search routine MCSRCH. If the
// C        function and gradient evaluations are inexpensive with respect
// C        to the cost of the iteration (which is sometimes the case when
// C        solving very large problems) it may be advantageous to set GTOL
// C        to a small value. A typical small value is 0.1.  Restriction:
// C        GTOL should be greater than 1.D-04.


// C    STPMIN and STPMAX are non-negative DOUBLE PRECISION variables which
// C        specify lower and uper bounds for the step in the line search.
// C        Their default values are 1.D-20 and 1.D+20, respectively. These
// C        values need not be modified unless the exponents are too large
// C        for the machine being used, or unless the problem is extremely
// C        badly scaled (in which case the exponents should be increased).
  integer mp, lp; // Fortran i/o stuff.  Unused here.
  doublereal gtol, stpmin, stpmax;
  doublereal stpawf; // line search default step length, added by awf
} lb3_; // SGI CC warns here about unused variable. Just ignore it.

#define lb3_1 lb3_

extern "C" int
lbfgs_(integer *n, integer *m, doublereal *x,
       doublereal *f, doublereal *g,
       logical *diagco, doublereal *diag, integer *iprint, doublereal *eps, doublereal *xtol,
       doublereal *w, integer *iflag);


bool vnl_lbfgs::minimize(vnl_vector<double>& x)
{
  /* Local variables */
  /*     The driver for vnl_lbfgs must always declare LB2 as EXTERNAL */

  int n = f_->get_number_of_unknowns();
  int m = memory; // The number of basis vectors to remember.

  int iprint[2] = {1, 0};
  vnl_vector<double> g(n);

  // Workspace
  vnl_vector<double> diag(n);

  vnl_vector<double> w(n * (2*m+1)+2*m);
  vcl_cout << "vnl_lbfgs: n = "<< n <<", memory = "<< m <<", Workspace = "<< w.size()
       << "[ "<< ( w.size() / 128.0 / 1024.0) <<" MB], ErrorScale = "
       << f_->reported_error(1) <<", xnorm = "<< x.magnitude() << vcl_endl;

  bool we_trace = (verbose_ && !trace);

  if (we_trace)
    vcl_cout << "vnl_lbfgs: ";

  double best_f = 0;
  vnl_vector<double> best_x;

  bool ok = true;
  this->num_evaluations_ = 0;
  int iflag = 0;
  for(;;) {
    // We do not wish to provide the diagonal matrices Hk0, and therefore set DIAGCO to FALSE.
    int diagco = 0;

    // Set these every iter in case user changes them to bail out
    double eps = gtol; // Gradient tolerance
    double local_xtol = 1e-16;
    lb3_.gtol = line_search_accuracy; // set to 0.1 for huge problems or cheap functions
    lb3_.stpawf = default_step_length;

    // use the variable or else the sgi warns.
    lb3_.gtol = lb3_.gtol;

    // Call function
    double f;
    f_->compute(x, &f, &g);
    if (this->num_evaluations_ == 0) {
      this->start_error_ = f;
      best_f = f;
    } else if (f < best_f) {
      best_x = x;
      best_f = f;
    }

    if (verbose_ && check_derivatives_) {
      vcl_cout << "vnl_lbfgs: f = " << f_->reported_error(f) << ", computing FD gradient\n";
      vnl_vector<double> fdg = f_->fdgradf(x);
#if defined(__GNUG__) && !defined(GNU_LIBSTDCXX_V3)
      int l = n;
      int limit = 100;
      int limit_tail = 10;
      if (l > limit + limit_tail) {
        vcl_cerr << " [ Showing only first " <<limit<< " components ]\n";
        l = limit;
      }
      vcl_cerr.form("%6s %20s %20s %20s %20s\n", "i", "x", "g", "fdg", "dg");
      vcl_cerr.form("%6s %20s %20s %20s %20s\n", "-", "-", "-", "---", "--");
      for(int i = 0; i < l; ++i)
        vcl_cerr.form("%6d %20g %20g %20g %20g\n", i, x[i], g[i], fdg[i], g[i] - fdg[i]);
      if (n > limit) {
        vcl_cerr << "   ...\n";
        for(int i = n - limit_tail; i < n; ++i)
          vcl_cerr.form("%6d %20g %20g %20g %20g\n", i, x[i], g[i], fdg[i], g[i] - fdg[i]);
      }
#endif
      vcl_cerr << "   ERROR = " << (fdg - g).squared_magnitude() / vcl_sqrt(double(n)) << "\n";
    }

    iprint[0] = trace ? 1 : -1; // -1 no o/p, 0 start and end, 1 every iter.
    iprint[1] = 0; // 1 prints X and G
    lbfgs_(&n, &m, x.data_block(), &f, g.data_block(), &diagco, diag.data_block(),
           iprint, &eps, &local_xtol, w.data_block(), &iflag);

    if (we_trace)
      vcl_cerr << iflag << ":" << f_->reported_error(f) << " ";

    if (iflag == 0) {
      // Successful return
      this->end_error_ = f;
      ok = true;
      x = best_x;
      break;
    }

    if (iflag < 0) {
      // Eeek.
      vcl_cerr << "vnl_lbfgs: ** EEEK **\n";
      ok = false;
      x = best_x;
      break;
    }

    if (++this->num_evaluations_ > get_max_function_evals()) {
      failure_code_ = FAILED_TOO_MANY_ITERATIONS;
      ok = false;
      x = best_x;
      break;
    }
  }
  if (we_trace) vcl_cerr << "done\n";

  return ok;
}

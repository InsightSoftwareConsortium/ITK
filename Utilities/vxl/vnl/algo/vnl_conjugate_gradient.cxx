// This is vxl/vnl/algo/vnl_conjugate_gradient.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Geoffrey Cross, Oxford RRG
// \date   15 Feb 99
//
//-----------------------------------------------------------------------------
#include "vnl_conjugate_gradient.h"

#include <vcl_cstdlib.h>
#include <vcl_iostream.h>

#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_vector_ref.h>

// external netlib function
extern "C"
int cg_( double *x,                     // IO start guess
         double *e,                     // O max-norm of gradient
         int    *it,                    // O number of iterations performed
         double *step,                  // I step=0 make guess at first direction
                                        // O step size along search direction for final iteration
         double *t,                     // I tolerance (iterations stop when max-norm of gradient < t)
         int *limit,                    // I maximum number of iterations
         int *n,                        // I number of unknowns
         int *m,                        // I number of iterations before renormalizing (normally m=n)
         double value( double *x),      // I value(x) is cost at x
         int grad( double *g,
                   double *x),          // I grad(g,x) puts gradient into g at x
         int both( double *v,
                   double *g,
                   double *x),          // I both(v,g,x) puts value in v and gradient in g at x
         int pre( double *y,
                  double *z),           // I preconditions (not necessarily needed) pre(y,z)
         double *h );                   // I space to work size h = 3*n

/////////////////////////////////////

class vnl_conjugate_gradient_Activate {
public:
  static vnl_conjugate_gradient* current;

  vnl_conjugate_gradient_Activate(vnl_conjugate_gradient* minimizer) {
    if (current) {
      vcl_cerr << "vnl_conjugate_gradient: ERROR: Nested minimizations not supported.\n";
      vcl_abort();
      // This is a copy of what goes on in LevenbergMarquardt, so if awf decides to
      // fix that one, then maybe he could do the same here...
    }
    current = minimizer;
  }
  ~vnl_conjugate_gradient_Activate() {
    current = 0;
  }
};

vnl_conjugate_gradient *vnl_conjugate_gradient_Activate::current= 0;


/////////////////////////////////////

vnl_conjugate_gradient::~vnl_conjugate_gradient()
{
}

void vnl_conjugate_gradient::init(vnl_cost_function &f)
{
  f_= &f;
  num_iterations_ = 0;
  num_evaluations_ = 0;
  start_error_ = 0;
  end_error_ = 0;
}

///////////////////////////////////////

double vnl_conjugate_gradient::valuecomputer_(double *x)
{
  vnl_conjugate_gradient* active = vnl_conjugate_gradient_Activate::current;
  vnl_cost_function* f = active->f_;
  vnl_vector_ref<double> ref_x(f->get_number_of_unknowns(), x);

  active->num_evaluations_++;

  return f->f(ref_x);
}

int vnl_conjugate_gradient::gradientcomputer_(double *g, double *x)
{
  vnl_conjugate_gradient* active = vnl_conjugate_gradient_Activate::current;
  vnl_cost_function* f = active->f_;
  vnl_vector_ref<double> ref_x(f->get_number_of_unknowns(), x);
  vnl_vector_ref<double> ref_g(f->get_number_of_unknowns(), g);

  f->gradf(ref_x, ref_g);

  return 0;
}

int vnl_conjugate_gradient::valueandgradientcomputer_(double *v, double *g, double *x)
{
  vnl_conjugate_gradient* active = vnl_conjugate_gradient_Activate::current;
  vnl_cost_function* f = active->f_;
  vnl_vector_ref<double> ref_x(f->get_number_of_unknowns(), x);
  vnl_vector_ref<double> ref_g(f->get_number_of_unknowns(), g);

  f->compute(ref_x, v, &ref_g);

  return 0;
}

int vnl_conjugate_gradient::preconditioner_( double *out, double *in)
{
  // FIXME - there should be some way to set a preconditioner if you have one
  // e.g. P = inv(diag(A'A)) for linear least squares systems.

  vnl_conjugate_gradient* active = vnl_conjugate_gradient_Activate::current;
  vnl_cost_function* f = active->f_;

  int n = f->get_number_of_unknowns();
  for (int i=0; i < n; ++i)
    out[i] = in[i];

  return 0;
}

///////////////////////////////////////

// avoid anachronism warning from fussy compilers
#ifdef VCL_SUNPRO_CC
extern "C" double vnl_conjugate_gradient__valuecomputer_( double *x)
{
  return vnl_conjugate_gradient::valuecomputer_(x);
}
extern "C" int vnl_conjugate_gradient__gradientcomputer_( double *g, double *x)
{
  return vnl_conjugate_gradient::gradientcomputer_(g,x);
}
extern "C" int vnl_conjugate_gradient__valueandgradientcomputer_( double *v, double *g, double *x)
{
  return vnl_conjugate_gradient::valueandgradientcomputer_(v,g,x);
}
extern "C" int vnl_conjugate_gradient__preconditioner_( double *out, double *in)
{
  return vnl_conjugate_gradient::preconditioner_(out,in);
}
#endif

bool vnl_conjugate_gradient::minimize( vnl_vector<double> &x)
{
  double *xp = x.data_block();
  double max_norm_of_gradient;
  int number_of_iterations;
  final_step_size_ = 0;
  double gradient_tolerance = gtol;
  vnl_vector<double> workspace(f_->get_number_of_unknowns()*3);
  int number_of_unknowns = f_->get_number_of_unknowns();

  vnl_conjugate_gradient_Activate activator(this);

  start_error_ = valuecomputer_(xp);
  num_evaluations_ = 0;

  cg_( xp,
       &max_norm_of_gradient,
       &number_of_iterations,
       &final_step_size_,
       &gradient_tolerance,
       &maxfev,
       &number_of_unknowns,
       &number_of_unknowns,
#ifdef VCL_SUNPRO_CC
       vnl_conjugate_gradient__valuecomputer_,
       vnl_conjugate_gradient__gradientcomputer_,
       vnl_conjugate_gradient__valueandgradientcomputer_,
       vnl_conjugate_gradient__preconditioner_,
#else
       valuecomputer_,
       gradientcomputer_,
       valueandgradientcomputer_,
       preconditioner_,
#endif
       workspace.data_block());

  end_error_= valuecomputer_(xp);
  num_iterations_ = number_of_iterations;

  return true;
}


void vnl_conjugate_gradient::diagnose_outcome(vcl_ostream& os) const
{
  os << "vnl_conjugate_gradient: "
     << num_iterations_
     << " iterations, "
     << num_evaluations_
     << " evaluations. Cost function reported error"
     << f_->reported_error(start_error_)
     << "/"
     << f_->reported_error(end_error_)
     << " . Final step size = " << final_step_size_
     << vcl_endl;
}

void vnl_conjugate_gradient::diagnose_outcome() const
{
  diagnose_outcome(vcl_cout);
}

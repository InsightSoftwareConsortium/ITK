// This is core/vnl/algo/vnl_lbfgsb.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
//
// \author Brad King, Kitware Inc.
// \date   28 Aug 07
//
//-----------------------------------------------------------------------------

#include <cstring>
#include <iostream>
#include "vnl_lbfgsb.h"
#include <vcl_compiler.h>

#include <vnl/algo/vnl_netlib.h> // setulb_()

//----------------------------------------------------------------------------
vnl_lbfgsb::vnl_lbfgsb(vnl_cost_function& f): f_(&f)
{
  init_parameters();
}

//----------------------------------------------------------------------------
void vnl_lbfgsb::init_parameters()
{
    long n = this->f_->get_number_of_unknowns();
    this->bound_selection_.set_size(n);
    this->bound_selection_.fill(0);
    this->max_corrections_ = 5;
    this->convergence_factor_ = 1e+7;
    this->projected_gradient_tolerance_ = 1e-5;
}

//----------------------------------------------------------------------------
bool vnl_lbfgsb::minimize(vnl_vector<double>& x)
{
  // Basic setup.
  long n = this->f_->get_number_of_unknowns();
  long m = this->max_corrections_;

  // Function and gradient.
  double f = 0;
  vnl_vector<double> gradient(n);

  // Working space.
  // The total work space **wa** required by the new version is
  //
  //                    2*m*n + 11*m*m + 5*n + 8*m
  //
  vnl_vector<double> wa(2*m*n + 11*m*m + 5*n + 8*m);
  //
  // the previous version required:
  //
  //                   2*m*n + 12*m*m + 4*n + 12*m
  //
  //
  vnl_vector<long> iwa(3*n);
  char csave[60];
  long lsave[4];
  long isave[44];
  double dsave[29];

  // Task communication.
  char task[61]="START                                                       ";

  // Verbosity level inside lbfgs implementation.
  // (-1 no o/p, 0 start and end, 1 every iter)
  long const iprint = trace ? 1 : -1;

  // Initialize iteration.
  this->num_evaluations_ = 0;
  this->num_iterations_ = 0;

  // TODO: Deal with verbose_, check_derivatives_, trace, xtol,
  // maxfev, ftol, gtol, epsfcn members of vnl_nonlinear_minimizer.

  // Track the best position found.
  vnl_vector<double> x_best(x);

  bool ok = true;
  for (;;)
  {
    // Call the L-BFGS-B code.
    v3p_netlib_setulb_(
      &n,
      &m,
      x.data_block(),
      this->lower_bound_.data_block(),
      this->upper_bound_.data_block(),
      this->bound_selection_.data_block(),
      &f, gradient.data_block(),
      &this->convergence_factor_,
      &this->projected_gradient_tolerance_,
      wa.data_block(),
      iwa.data_block(),
      task,
      &iprint,
      csave, lsave, isave, dsave
      );

    // Check the current task.
    if (std::strncmp("FG", task, 2) == 0)
    {
      // Evaluate the function and gradient.
      this->f_->compute(x, &f, &gradient);

      if (this->num_evaluations_ == 0)
      {
        x_best = x;
        this->start_error_ = f;
        this->end_error_ = f;
      }
      else if (f < this->end_error_)
      {
        x_best = x;
        this->end_error_ = f;
      }
      this->report_eval(f);
    }
    else if (std::strncmp("NEW_X", task, 5) == 0)
    {
      // dsave[12] = the infinity norm of the projected gradient
      this->inf_norm_projected_gradient_ = dsave[12];

      // Iteration.a
      if (this->report_iter())
      {
        this->failure_code_ = FAILED_USER_REQUEST;
        ok = false;
        break;
      }
    }
    else if (std::strncmp("ERROR", task, 5) == 0)
    {
      // some error
      this->failure_code_ = ERROR_FAILURE;
      ok = false;
      break;
    }
    else if (std::strncmp("CONVERGENCE", task, 11) == 0)
    {
      // convergence has been reached
      if (f < this->end_error_)
      {
        x_best = x;
        this->end_error_ = f;
      }

      if (std::strncmp("CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH",
                      task, 47) == 0)
      {
        // function tolerance reached
        this->failure_code_ = CONVERGED_FTOL;
      }
      else if (std::strncmp("CONVERGENCE: NORM_OF_PROJECTED_GRADIENT_<=_PGTOL",
                           task, 48) == 0)
      {
        // gradient tolerance reached
        this->failure_code_ = CONVERGED_GTOL;
      }
      else
      {
        this->failure_code_ = ERROR_FAILURE;
        if (trace)
        {
          std::cerr << "Unknown convergence type: " << task << std::endl;
        }
      }
      break;
    }
    else
    {
      // unknown task
      this->failure_code_ = ERROR_FAILURE;
      if (trace)
      {
        std::cerr << "Unknown failure with task: " << task << std::endl;
      }
      ok = false;
      break;
    }

    if (this->num_evaluations_ > this->get_max_function_evals())
    {
      this->failure_code_ = TOO_MANY_ITERATIONS;
      ok = false;
      break;
    }
  }

  // Store the best known position no matter the outcome.
  x = x_best;

  return ok;
}

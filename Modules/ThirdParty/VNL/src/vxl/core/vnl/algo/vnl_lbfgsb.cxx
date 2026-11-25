// This is core/vnl/algo/vnl_lbfgsb.cxx
//:
// \file
//
// \author Brad King, Kitware Inc.
// \date   28 Aug 07
//
//-----------------------------------------------------------------------------

#include <array>
#include <cstring>
#include <iostream>
#include <mutex>
#include "vnl_lbfgsb.h"

#include <vnl/algo/vnl_netlib.h> // setulb_()

namespace
{
// L-BFGS-B (v3p_netlib_setulb_) is the f2c translation of FORTRAN
// code that uses SAVE'd locals; concurrent reverse-communication
// drivers interleaving setulb_ calls corrupt that global state.
// The lock is acquired at minimize() entry and held for the full
// driver loop so that one minimize() invocation completes its
// reverse-communication sequence before another may begin.
//
// This is a band-aid: regenerating netlib via thread-safe LAPACK
// (issue #23) would let us drop this. See also core/vnl/algo/
// README_threading.md for the full inventory of serialised paths.
std::mutex &
lbfgsb_call_mutex()
{
  static std::mutex m;
  return m;
}
} // namespace

//----------------------------------------------------------------------------
vnl_lbfgsb::vnl_lbfgsb(vnl_cost_function & f)
  : f_(&f)
{
  init_parameters();
}

//----------------------------------------------------------------------------
void
vnl_lbfgsb::init_parameters()
{
  const long n = this->f_->get_number_of_unknowns();
  this->bound_selection_.set_size(n);
  this->bound_selection_.fill(0);
  this->max_corrections_ = 5;
  this->convergence_factor_ = 1e+7;
  this->projected_gradient_tolerance_ = 1e-5;
}

//----------------------------------------------------------------------------
bool
vnl_lbfgsb::minimize(vnl_vector<double> & x)
{
  // Serialize the entire reverse-communication driver loop. See the
  // anonymous-namespace comment for lbfgsb_call_mutex() above.
  std::lock_guard<std::mutex> lbfgsb_minimize_guard(lbfgsb_call_mutex());

  // Basic setup.
  const long n = this->f_->get_number_of_unknowns();
  const long m = this->max_corrections_;

  // Function and gradient.
  double f = 0;
  vnl_vector<double> gradient(n);

  // Working space.
  // The total work space **wa** required by the new version is
  //
  //                    2*m*n + 11*m*m + 5*n + 8*m
  //
  vnl_vector<double> wa(2 * m * n + 11 * m * m + 5 * n + 8 * m);
  //
  // the previous version required:
  //
  //                   2*m*n + 12*m*m + 4*n + 12*m
  //
  //
  vnl_vector<long> iwa(3 * n);
  std::array<char, 60> csave{};
  std::array<long, 4> lsave{};
  std::array<long, 44> isave{};
  std::array<double, 29> dsave{};

  // Task communication.
  std::array<char, 61> task = { "START                                                       " };

  // Verbosity level inside lbfgs implementation.
  // (-1 no o/p, 0 start and end, 1 every iter)
  const long iprint = trace ? 1 : -1;

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
    // Call the L-BFGS-B code. The full driver loop is serialised by
    // the lock_guard at minimize() entry.
    v3p_netlib_setulb_(&n,
                       &m,
                       x.data_block(),
                       this->lower_bound_.data_block(),
                       this->upper_bound_.data_block(),
                       this->bound_selection_.data_block(),
                       &f,
                       gradient.data_block(),
                       &this->convergence_factor_,
                       &this->projected_gradient_tolerance_,
                       wa.data_block(),
                       iwa.data_block(),
                       task.data(),
                       &iprint,
                       csave.data(),
                       lsave.data(),
                       isave.data(),
                       dsave.data());

    // Check the current task.
    if (std::strncmp("FG", task.data(), 2) == 0)
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
    else if (std::strncmp("NEW_X", task.data(), 5) == 0)
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
    else if (std::strncmp("ERROR", task.data(), 5) == 0)
    {
      // some error
      this->failure_code_ = ERROR_FAILURE;
      ok = false;
      break;
    }
    else if (std::strncmp("ABNORMAL_TERMINATION_IN_LNSRCH", task.data(), 30) == 0)
    {
      // some error
      this->failure_code_ = ABNORMAL_TERMINATION_IN_LNSRCH;
      ok = false;
      break;
    }
    else if (std::strncmp("CONVERGENCE", task.data(), 11) == 0)
    {
      // convergence has been reached
      if (f < this->end_error_)
      {
        x_best = x;
        this->end_error_ = f;
      }

      if (std::strncmp("CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH", task.data(), 47) == 0)
      {
        // function tolerance reached
        this->failure_code_ = CONVERGED_FTOL;
      }
      else if (std::strncmp("CONVERGENCE: NORM_OF_PROJECTED_GRADIENT_<=_PGTOL", task.data(), 48) == 0)
      {
        // gradient tolerance reached
        this->failure_code_ = CONVERGED_GTOL;
      }
      else
      {
        this->failure_code_ = ERROR_FAILURE;
        if (trace)
        {
          std::cerr << "Unknown convergence type: " << task.data() << std::endl;
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
        std::cerr << "Unknown failure with task: " << task.data() << std::endl;
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

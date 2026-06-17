// This is core/vnl/algo/vnl_levenberg_marquardt.cxx
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date 31 Aug 96
//
// ITK: minimization uses Eigen's NonLinearOptimization Levenberg-Marquardt.

#include <iostream>
#include <cassert>
#include <cmath>
#include "vnl_levenberg_marquardt.h"
#include "vnl/vnl_fastops.h"
#include "vnl/vnl_matrix_ref.h"
#include "vnl/vnl_vector_ref.h"
#include "vnl/vnl_least_squares_function.h"

#include "itkeigen/unsupported/Eigen/NonLinearOptimization"
#include "itkeigen/unsupported/Eigen/NumericalDiff"

namespace
{
//: Eigen functor adapting a vnl_least_squares_function.
struct vnl_lsf_functor
{
  using Scalar = double;
  using InputType = Eigen::VectorXd;
  using ValueType = Eigen::VectorXd;
  using JacobianType = Eigen::MatrixXd;
  enum
  {
    InputsAtCompileTime = Eigen::Dynamic,
    ValuesAtCompileTime = Eigen::Dynamic
  };

  vnl_least_squares_function * f_;
  int                          m_; // residuals
  int                          n_; // unknowns

  int
  inputs() const
  {
    return n_;
  }
  int
  values() const
  {
    return m_;
  }

  int
  operator()(const Eigen::VectorXd & x, Eigen::VectorXd & fx) const
  {
    const vnl_vector_ref<double> rx(n_, const_cast<double *>(x.data()));
    vnl_vector_ref<double>       rfx(m_, fx.data());
    f_->f(rx, rfx);
    if (f_->failure)
    {
      f_->clear_failure();
      return -1;
    }
    return 0;
  }

  // Eigen wants fjac(i,j) = d r_i / d x_j (m-by-n). vnl gradf fills the same
  // residual-by-unknown layout.
  int
  df(const Eigen::VectorXd & x, Eigen::MatrixXd & fjac) const
  {
    const vnl_vector_ref<double> rx(n_, const_cast<double *>(x.data()));
    vnl_matrix<double>           jac(m_, n_);
    f_->gradf(rx, jac);
    for (int i = 0; i < m_; ++i)
    {
      for (int j = 0; j < n_; ++j)
      {
        fjac(i, j) = jac(i, j);
      }
    }
    if (f_->failure)
    {
      f_->clear_failure();
      return -1;
    }
    return 0;
  }
};

double
rms_of(const vnl_vector<double> & v)
{
  return v.size() ? v.magnitude() / std::sqrt(static_cast<double>(v.size())) : 0.0;
}
} // namespace

// see header
vnl_vector<double>
vnl_levenberg_marquardt_minimize(vnl_least_squares_function & f, const vnl_vector<double> & initial_estimate)
{
  vnl_vector<double>      x = initial_estimate;
  vnl_levenberg_marquardt lm(f);
  lm.minimize(x);
  return x;
}

// ctor
void
vnl_levenberg_marquardt::init(vnl_least_squares_function * f)
{
  f_ = f;

  // If changing these defaults, check the help comments in vnl_levenberg_marquardt.h,
  // and MAKE SURE they're consistent.
  xtol = 1e-8;                                // Termination tolerance on X (solution vector)
  maxfev = 400 * f->get_number_of_unknowns(); // Termination maximum number of iterations.
  ftol = xtol * 0.01;                         // Termination tolerance on F (sum of squared residuals)
  gtol = 1e-5;                                // Termination tolerance on Grad(F)' * F = 0
  epsfcn = xtol * 0.001;                      // Step length for FD Jacobian

  const unsigned int m = f_->get_number_of_residuals(); // I  Number of residuals, must be > #unknowns
  const unsigned int n = f_->get_number_of_unknowns();  // I  Number of unknowns

  set_covariance_ = false;
  fdjac_.set_size(n, m);
  fdjac_.fill(0.0);
  ipvt_.set_size(n);
  ipvt_.fill(0);
  inv_covar_.set_size(n, n);
  inv_covar_.fill(0.0);
}

vnl_levenberg_marquardt::~vnl_levenberg_marquardt() = default;

//--------------------------------------------------------------------------------

namespace
{
//: Store the R factor and permutation from an Eigen LM solve into the vnl
// members consumed by get_JtJ(). Eigen's fjac holds R in its top n-by-n; vnl
// get_JtJ() expects fdjac_.extract(n,n).transpose() to be that R, so store the
// transpose. ipvt_ is 1-based.
template <typename TEigenMatrix, typename TPermutation>
void
store_r_and_permutation(const TEigenMatrix &  fjac,
                        const TPermutation &  permutation,
                        unsigned int          n,
                        vnl_matrix<double> &  fdjac,
                        vnl_vector<long> &    ipvt)
{
  for (unsigned int a = 0; a < n; ++a)
  {
    for (unsigned int b = 0; b < n; ++b)
    {
      fdjac(a, b) = fjac(b, a);
    }
  }
  for (unsigned int i = 0; i < n; ++i)
  {
    ipvt[i] = permutation.indices()[i] + 1;
  }
}
} // namespace

bool
vnl_levenberg_marquardt::minimize(vnl_vector<double> & x)
{
  if (f_->has_gradient())
    return minimize_using_gradient(x);
  else
    return minimize_without_gradient(x);
}

bool
vnl_levenberg_marquardt::minimize_without_gradient(vnl_vector<double> & x)
{
  if (f_->has_gradient())
  {
    std::cerr << __FILE__ " : WARNING. calling minimize_without_gradient(), but f_ has gradient.\n";
  }

  const long m = f_->get_number_of_residuals();
  const long n = f_->get_number_of_unknowns();

  if (m < n)
  {
    std::cerr << "vnl_levenberg_marquardt: Number of unknowns(" << n << ") greater than number of data (" << m << ")\n";
    failure_code_ = ERROR_DODGY_INPUT;
    return false;
  }
  if (long(x.size()) != n)
  {
    std::cerr << "vnl_levenberg_marquardt: Input vector length (" << x.size() << ") not equal to num unknowns (" << n
              << ")\n";
    failure_code_ = ERROR_DODGY_INPUT;
    return false;
  }

  vnl_lsf_functor                       functor{ f_, static_cast<int>(m), static_cast<int>(n) };
  Eigen::NumericalDiff<vnl_lsf_functor> numdiff(functor, epsfcn);
  Eigen::LevenbergMarquardt<Eigen::NumericalDiff<vnl_lsf_functor>> lm(numdiff);
  lm.parameters.ftol = ftol;
  lm.parameters.xtol = xtol;
  lm.parameters.gtol = gtol;
  lm.parameters.maxfev = maxfev;
  lm.parameters.factor = 100;
  lm.parameters.epsfcn = epsfcn;

  Eigen::VectorXd ex(n);
  for (long i = 0; i < n; ++i)
    ex[i] = x[i];

  num_iterations_ = 0;
  set_covariance_ = false;
  start_error_ = 0;

  Eigen::LevenbergMarquardtSpace::Status status = lm.minimizeInit(ex);
  if (lm.fvec.size() > 0)
    start_error_ = lm.fvec.norm() / std::sqrt(static_cast<double>(lm.fvec.size()));
  if (status != Eigen::LevenbergMarquardtSpace::ImproperInputParameters)
  {
    do
    {
      status = lm.minimizeOneStep(ex);
      ++num_iterations_;
    } while (status == Eigen::LevenbergMarquardtSpace::Running);
  }

  for (long i = 0; i < n; ++i)
    x[i] = ex[i];
  num_evaluations_ = static_cast<long>(lm.nfev);
  failure_code_ = (static_cast<int>(status) < 0) ? ERROR_FAILURE : static_cast<ReturnCodes>(status);
  store_r_and_permutation(lm.fjac, lm.permutation, static_cast<unsigned int>(n), fdjac_, ipvt_);

  vnl_vector<double> fx(m, 0.0);
  f_->f(x, fx);
  end_error_ = rms_of(fx);

  switch (static_cast<int>(failure_code_))
  {
    case 1:
    case 2:
    case 3:
    case 4:
      return true;
    default:
      return false;
  }
}

bool
vnl_levenberg_marquardt::minimize_using_gradient(vnl_vector<double> & x)
{
  if (!f_->has_gradient())
  {
    std::cerr << __FILE__ ": called method minimize_using_gradient(), but f_ has no gradient.\n";
    return false;
  }

  const long m = f_->get_number_of_residuals();
  const long n = f_->get_number_of_unknowns();

  if (m < n)
  {
    std::cerr << __FILE__ ": Number of unknowns(" << n << ") greater than number of data (" << m << ")\n";
    failure_code_ = ERROR_DODGY_INPUT;
    return false;
  }

  vnl_lsf_functor                            functor{ f_, static_cast<int>(m), static_cast<int>(n) };
  Eigen::LevenbergMarquardt<vnl_lsf_functor> lm(functor);
  lm.parameters.ftol = ftol;
  lm.parameters.xtol = xtol;
  lm.parameters.gtol = gtol;
  lm.parameters.maxfev = maxfev;
  lm.parameters.factor = 100;

  Eigen::VectorXd ex(n);
  for (long i = 0; i < n; ++i)
    ex[i] = x[i];

  num_iterations_ = 0;
  set_covariance_ = false;
  start_error_ = 0;

  Eigen::LevenbergMarquardtSpace::Status status = lm.minimizeInit(ex);
  if (lm.fvec.size() > 0)
    start_error_ = lm.fvec.norm() / std::sqrt(static_cast<double>(lm.fvec.size()));
  if (status != Eigen::LevenbergMarquardtSpace::ImproperInputParameters)
  {
    do
    {
      status = lm.minimizeOneStep(ex);
      ++num_iterations_;
    } while (status == Eigen::LevenbergMarquardtSpace::Running);
  }

  for (long i = 0; i < n; ++i)
    x[i] = ex[i];
  num_evaluations_ = num_iterations_; // for lmder, these are the same.
  failure_code_ = (static_cast<int>(status) < 0) ? ERROR_FAILURE : static_cast<ReturnCodes>(status);
  store_r_and_permutation(lm.fjac, lm.permutation, static_cast<unsigned int>(n), fdjac_, ipvt_);

  vnl_vector<double> fx(m, 0.0);
  f_->f(x, fx);
  end_error_ = rms_of(fx);

  switch (static_cast<int>(failure_code_))
  {
    case 1:
    case 2:
    case 3:
    case 4:
      return true;
    default:
      return false;
  }
}

//--------------------------------------------------------------------------------

void
vnl_levenberg_marquardt::diagnose_outcome() const
{
  diagnose_outcome(std::cerr);
}

// fsm: should this function be a method on vnl_nonlinear_minimizer?
// if not, the return codes should be moved into LM.
void
vnl_levenberg_marquardt::diagnose_outcome(std::ostream & s) const
{
#define whoami "vnl_levenberg_marquardt"
  switch (failure_code_)
  {
    case ERROR_FAILURE:
      s << (whoami ": OIOIOI -- failure in leastsquares function\n");
      break;
    case ERROR_DODGY_INPUT:
      s << (whoami ": OIOIOI -- lmdif dodgy input\n");
      break;
    case CONVERGED_FTOL: // ftol
      s << (whoami ": converged to ftol\n");
      break;
    case CONVERGED_XTOL: // xtol
      s << (whoami ": converged to xtol\n");
      break;
    case CONVERGED_XFTOL: // both
      s << (whoami ": converged nicely\n");
      break;
    case CONVERGED_GTOL:
      s << (whoami ": converged via gtol\n");
      break;
    case TOO_MANY_ITERATIONS:
      s << (whoami ": too many iterations\n");
      break;
    case FAILED_FTOL_TOO_SMALL:
      s << (whoami ": ftol is too small. no further reduction in the sum of squares is possible.\n");
      break;
    case FAILED_XTOL_TOO_SMALL:
      s << (whoami ": xtol is too small. no further improvement in the approximate solution x is possible.\n");
      break;
    case FAILED_GTOL_TOO_SMALL:
      s << (whoami ": gtol is too small. Fx is orthogonal to the columns of the jacobian to machine precision.\n");
      break;
    default:
      s << (whoami ": OIOIOI: unkown info code from lmder.\n");
      break;
  }
  const unsigned int m = f_->get_number_of_residuals();
  s << whoami ": " << num_iterations_ << " iterations, " << num_evaluations_ << " evaluations, " << m
    << " residuals.  RMS error start/end " << get_start_error() << '/' << get_end_error() << std::endl;
#undef whoami
}

// fjac is an output m by n array. the upper n by n submatrix
//         of fjac contains an upper triangular matrix r with
//         diagonal elements of nonincreasing magnitude such that
//
//                t     t           t
//               p *(jac *jac)*p = r *r,
//
//         where p is a permutation matrix and jac is the final
//         calculated jacobian. column j of p is column ipvt(j)
//         (see below) of the identity matrix. the lower trapezoidal
//         part of fjac contains information generated during
//         the computation of r.

// fdjac is target m*n

//: Get INVERSE of covariance at last minimum.
// Code thanks to Joss Knight (joss@robots.ox.ac.uk)
const vnl_matrix<double> &
vnl_levenberg_marquardt::get_JtJ()
{
  if (!set_covariance_)
  {
    std::cerr << __FILE__ ": get_covariance() not confirmed tested  yet\n";
    const unsigned int n = fdjac_.rows();

    // matrix in FORTRAN is column-wise.
    // transpose it to get C style order
    vnl_matrix<double> r = fdjac_.extract(n, n).transpose();
    // r is upper triangular matrix according to documentation.
    // But the lower part has non-zeros somehow.
    // clear the lower part
    for (unsigned int i = 0; i < n; ++i)
      for (unsigned int j = 0; j < i; ++j)
        r(i, j) = 0.0;

    // compute r^T * r
    vnl_matrix<double> rtr;
    vnl_fastops::AtA(rtr, r);
    vnl_matrix<double> rtrpt(n, n);

    // Permute. First order columns.
    // Note, *ipvt_ contains 1 to n, not 0 to n-1
    vnl_vector<int> jpvt(n);
    for (unsigned int j = 0; j < n; ++j)
    {
      int i = 0;
      for (; i < static_cast<int>(n); i++)
      {
        if (ipvt_[i] == (int)j + 1)
        {
          jpvt(j) = i;
          break;
        }
      }
      rtrpt.set_column(j, rtr.get_column(i));
    }

    // Now order rows
    for (unsigned int j = 0; j < n; ++j)
    {
      inv_covar_.set_row(j, rtrpt.get_row(jpvt(j)));
    }

    set_covariance_ = true;
  }
  return inv_covar_;
}

// This is core/vnl/algo/vnl_levenberg_marquardt.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date 31 Aug 96
//
//-----------------------------------------------------------------------------

#include "vnl_levenberg_marquardt.h"

#include <vcl_cassert.h>
#include <vcl_cstdlib.h>
#include <vcl_iostream.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_fastops.h>
#include <vnl/vnl_vector_ref.h>
#include <vnl/vnl_matrix_ref.h>
#include <vnl/vnl_least_squares_function.h>
#include <vnl/algo/vnl_netlib.h> // lmdif_()

// see header
vnl_vector<double> vnl_levenberg_marquardt_minimize(vnl_least_squares_function& f,
                                                    vnl_vector<double> const& initial_estimate)
{
  vnl_vector<double> x = initial_estimate;
  vnl_levenberg_marquardt lm(f);
  lm.minimize(x);
  return x;
}

// ctor
void vnl_levenberg_marquardt::init(vnl_least_squares_function* f)
{
  f_ = f;

  // If changing these defaults, check the help comments in vnl_levenberg_marquardt.h,
  // and MAKE SURE they're consistent.
  xtol = 1e-8;           // Termination tolerance on X (solution vector)
  maxfev = 400 * f->get_number_of_unknowns(); // Termination maximum number of iterations.
  ftol = xtol * 0.01;    // Termination tolerance on F (sum of squared residuals)
  gtol = 1e-5;           // Termination tolerance on Grad(F)' * F = 0
  epsfcn = xtol * 0.001; // Step length for FD Jacobian

  unsigned int m = f_->get_number_of_residuals(); // I  Number of residuals, must be > #unknowns
  unsigned int n = f_->get_number_of_unknowns();  // I  Number of unknowns

  set_covariance_ = false;
  fdjac_.set_size(n,m);
  fdjac_.fill(0.0);
  ipvt_.set_size(n);
  ipvt_.fill(0);
  inv_covar_.set_size(n,n);
  inv_covar_.fill(0.0);
  //fdjac_ = new vnl_matrix<double>(n,m);
  //ipvt_ = new vnl_vector<int>(n);
  //covariance_ = new vnl_matrix<double>(n,n);
}

vnl_levenberg_marquardt::~vnl_levenberg_marquardt()
{
  //delete covariance_;
  //delete fdjac_;
  //delete ipvt_;
}


class vnl_levenberg_marquardt_Activate
{
 public:
  static vnl_levenberg_marquardt* current;

  vnl_levenberg_marquardt_Activate(vnl_levenberg_marquardt* minimizer) {
    if (current) {
      vcl_cerr << "vnl_levenberg_marquardt: ERROR: Nested minimizations not supported.\n";
      vcl_abort();
      // Ask awf to fix this if you need to run one minimization inside another.
      // Or just make "current" a stack.
      // Actually....  I think it might be a lot harder than that, the statics in the fortran.
    }
    current = minimizer;
  }
  ~vnl_levenberg_marquardt_Activate() {
    current = 0;
  }
};

vnl_levenberg_marquardt* vnl_levenberg_marquardt_Activate::current = 0;

//--------------------------------------------------------------------------------

#ifdef VCL_SUNPRO_CC
extern "C"
#endif
void vnl_levenberg_marquardt::lmdif_lsqfun(int* n,     // I   Number of residuals
                                           int* p,     // I   Number of unknowns
                                           double* x,  // I   Solution vector, size n
                                           double* fx, // O   Residual vector f(x)
                                           int* iflag) // IO  0 ==> print, -1 ==> terminate
{
  vnl_levenberg_marquardt* active = vnl_levenberg_marquardt_Activate::current;
  vnl_least_squares_function* f = active->f_;
  assert(*p == (int)f->get_number_of_unknowns());
  assert(*n == (int)f->get_number_of_residuals());
  vnl_vector_ref<double> ref_x(*p, const_cast<double*>(x));
  vnl_vector_ref<double> ref_fx(*n, fx);

  if (*iflag == 0) {
    if (active->trace)
      vcl_cerr << "lmdif: iter " << active->num_iterations_ << " err ["
               << x[0] << ", " << x[1] << ", " << x[2] << ", " << x[3] << ", "
               << x[4] << ", ... ] = " << ref_fx.magnitude() << '\n';

    f->trace(active->num_iterations_, ref_x, ref_fx);
    ++(active->num_iterations_);
  } else {
    f->f(ref_x, ref_fx);
  }

  if (active->start_error_ == 0)
    active->start_error_ = ref_fx.rms();

  if (f->failure) {
    f->clear_failure();
    *iflag = -1; // fsm
  }
}


// This function shouldn't be inlined, because (1) modification of the
// body will not cause the timestamp on the header to change, and so
// others will not be forced to recompile, and (2) the cost of making
// one more function call is far, far less than the cost of actually
// performing the minimisation, so the inline doesn't gain you
// anything.
//
bool vnl_levenberg_marquardt::minimize(vnl_vector<double>& x)
{
  if ( f_->has_gradient() )
    return minimize_using_gradient(x);
  else
    return minimize_without_gradient(x);
}


//
bool vnl_levenberg_marquardt::minimize_without_gradient(vnl_vector<double>& x)
{
  //fsm
  if (f_->has_gradient()) {
    vcl_cerr << __FILE__ " : WARNING. calling minimize_without_gradient(), but f_ has gradient.\n";
  }

  // e04fcf
  int m = f_->get_number_of_residuals(); // I  Number of residuals, must be > #unknowns
  int n = f_->get_number_of_unknowns();  // I  Number of unknowns

  if (m < n) {
    vcl_cerr << "vnl_levenberg_marquardt: Number of unknowns("<<n<<") greater than number of data ("<<m<<")\n";
    failure_code_ = ERROR_DODGY_INPUT;
    return false;
  }

  if (int(x.size()) != n) {
    vcl_cerr << "vnl_levenberg_marquardt: Input vector length ("<<x.size()<<") not equal to num unknowns ("<<n<<")\n";
    failure_code_ = ERROR_DODGY_INPUT;
    return false;
  }

  vnl_vector<double> fx(m);    // W m   Storage for residual vector
  vnl_vector<double> diag(n);  // I     Multiplicative scale factors for variables
  int user_provided_scale_factors = 1;  // 1 is no, 2 is yes
  double factor = 100;
  int nprint = 1;

  vnl_vector<double> qtf(n);
  vnl_vector<double> wa1(n);
  vnl_vector<double> wa2(n);
  vnl_vector<double> wa3(n);
  vnl_vector<double> wa4(m);

  //vcl_cerr << "STATUS: " << failure_code_ << vcl_endl;
  vnl_levenberg_marquardt_Activate activator(this);

  double errors[2] = {0,0};
  num_iterations_ = 0;
  set_covariance_ = false;
  int info;
  start_error_ = 0; // Set to 0 so first call to lmdif_lsqfun will know to set it.
  lmdif_(lmdif_lsqfun, &m, &n,
         x.data_block(),
         fx.data_block(),
         &ftol, &xtol, &gtol, &maxfev, &epsfcn,
         &diag[0],
         &user_provided_scale_factors, &factor, &nprint,
         &info, &num_evaluations_,
         fdjac_.data_block(), &m, ipvt_.data_block(),
         &qtf[0],
         &wa1[0], &wa2[0], &wa3[0], &wa4[0],
         errors);
  failure_code_ = (ReturnCodes) info;

  // One more call to compute final error.
  lmdif_lsqfun(&m,              // I    Number of residuals
               &n,              // I    Number of unknowns
               x.data_block(),  // I    Solution vector, size n
               fx.data_block(), // O    Residual vector f(x)
               &info);
  end_error_ = fx.rms();

#ifdef _SGI_CC_6_
  // Something fundamentally odd about the switch below on SGI native... FIXME
  vcl_cerr << "vnl_levenberg_marquardt: termination code = " << failure_code_ << vcl_endl;
  // diagnose_outcome(vcl_cerr);
  return 1;
#endif

  // Translate status code
  switch ((int)failure_code_) {
  case 1: // ftol
  case 2: // xtol
  case 3: // both
  case 4: // gtol
    return true;
  default:
    diagnose_outcome();
    return false;
  }
}

//--------------------------------------------------------------------------------

#ifdef VCL_SUNPRO_CC
extern "C"
#endif
void vnl_levenberg_marquardt::lmder_lsqfun(int* n,     // I   Number of residuals
                                           int* p,     // I   Number of unknowns
                                           double* x,  // I   Solution vector, size n
                                           double* fx, // O   Residual vector f(x)
                                           double* fJ, // O   m * n Jacobian f(x)
                                           int*,
                                           int* iflag) // I   1 -> calc fx, 2 -> calc fjac
{
  vnl_levenberg_marquardt* active = vnl_levenberg_marquardt_Activate::current;
  vnl_least_squares_function* f = active->f_;
  assert(*p == (int)f->get_number_of_unknowns());
  assert(*n == (int)f->get_number_of_residuals());
  vnl_vector_ref<double> ref_x(*p, (double*)x); // const violation!
  vnl_vector_ref<double> ref_fx(*n, fx);
  vnl_matrix_ref<double> ref_fJ(*n, *p, fJ);

  if (*iflag == 0) {
    if (active->trace)
      vcl_cerr << "lmder: iter " << active->num_iterations_ << " err ["
               << x[0] << ", " << x[1] << ", " << x[2] << ", " << x[3] << ", "
               << x[4] << ", ... ] = " << ref_fx.magnitude() << '\n';
    f->trace(active->num_iterations_, ref_x, ref_fx);
  }
  else if (*iflag == 1) {
    f->f(ref_x, ref_fx);
    if (active->start_error_ == 0)
      active->start_error_ = ref_fx.rms();
    ++(active->num_iterations_);
  }
  else if (*iflag == 2) {
    f->gradf(ref_x, ref_fJ);
    ref_fJ.inplace_transpose();

    // check derivative?
    if ( active->check_derivatives_ > 0 )
    {
      active->check_derivatives_--;

      // use finite difference to compute Jacobian 
      vnl_vector<double> feval( *n );
      vnl_matrix<double> finite_jac( *p, *n, 0.0 );
      vnl_vector<double> wa1( *n );
      int info=1;
      double diff;
      f->f( ref_x, feval );
      fdjac2_(lmdif_lsqfun, n, p, x, 
              feval.data_block(), 
              finite_jac.data_block(),
              n, 
              &info,
              &(active->epsfcn),
              wa1.data_block());
      // compute difference
      for( unsigned i=0; i<ref_fJ.cols(); ++i )
        for( unsigned j=0; j<ref_fJ.rows(); ++j ) {
          diff = ref_fJ(j,i) - finite_jac(j,i);
          diff = diff*diff;
          if( diff > active->epsfcn ) {
            vcl_cerr << "Jac(" << i << ", " << j << ") diff: " << ref_fJ(j,i) << ' ' << finite_jac(j,i) << vcl_endl;
          }
        }
    }
  }

  if (f->failure) {
    f->clear_failure();
    *iflag = -1; // fsm
  }
}


//
bool vnl_levenberg_marquardt::minimize_using_gradient(vnl_vector<double>& x)
{
  //fsm
  if (! f_->has_gradient()) {
    vcl_cerr << __FILE__ ": called method minimize_using_gradient(), but f_ has no gradient.\n";
    return false;
  }

  int m = f_->get_number_of_residuals(); // I  Number of residuals, must be > #unknowns
  int n = f_->get_number_of_unknowns();  // I  Number of unknowns

  if (m < n) {
    vcl_cerr << __FILE__ ": Number of unknowns("<<n<<") greater than number of data ("<<m<<")\n";
    failure_code_ = ERROR_DODGY_INPUT;
    return false;
  }

  vnl_vector<double> fx(m);    // W m   Storage for residual vector
  vnl_vector<double> wa1(5*n + m);

  vnl_levenberg_marquardt_Activate activator(this);

  num_iterations_ = 0;
  set_covariance_ = false;
  int info;
  int size = wa1.size();
  start_error_ = 0; // Set to 0 so first call to lmder_lsqfun will know to set it.
  lmder1_(lmder_lsqfun, &m, &n,
          x.data_block(),
          fx.data_block(),
          fdjac_.data_block(), &m,
          &ftol,
          &info,
          ipvt_.data_block(),
          wa1.data_block(),
          &size);
  num_evaluations_ = num_iterations_; // for lmder, these are the same.
  if (info<0)
    info = ERROR_FAILURE;
  failure_code_ = (ReturnCodes) info;
  end_error_ = fx.rms();

  // Translate status code
  switch (failure_code_) {
  case 1: // ftol
  case 2: // xtol
  case 3: // both
  case 4: // gtol
    return true;
  default:
    diagnose_outcome();
    return false;
  }
}

//--------------------------------------------------------------------------------

void vnl_levenberg_marquardt::diagnose_outcome() const
{
  diagnose_outcome(vcl_cerr);
}

// fsm: should this function be a method on vnl_nonlinear_minimizer?
// if not, the return codes should be moved into LM.
void vnl_levenberg_marquardt::diagnose_outcome(vcl_ostream& s) const
{
#define whoami "vnl_levenberg_marquardt"
  //if (!verbose_) return;
  switch (failure_code_) {
    //  case -1:
    // have already warned.
    //    return;
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
  case FAILED_TOO_MANY_ITERATIONS:
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
  unsigned int m = f_->get_number_of_residuals();
  s << whoami ": " << num_iterations_ << " iterations, "
    << num_evaluations_ << " evaluations, "<< m <<" residuals.  RMS error start/end "
    << get_start_error() << '/' << get_end_error() << vcl_endl;
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
vnl_matrix<double> const& vnl_levenberg_marquardt::get_JtJ()
{
  if (!set_covariance_)
  {
    vcl_cerr << __FILE__ ": get_covariance() not confirmed tested  yet\n";
    unsigned int n = fdjac_.rows();

    // matrix in FORTRAN is column-wise.
    // transpose it to get C style order
    vnl_matrix<double> r = fdjac_.extract(n,n).transpose();
    // r is upper triangular matrix according to documentation.
    // But the lower part has non-zeros somehow.
    // clear the lower part
    for (unsigned int i=0; i<n; ++i)
      for (unsigned int j=0; j<i; ++j)
        r(i,j) = 0.0;

    // compute r^T * r
    vnl_matrix<double> rtr;
    vnl_fastops::AtA(rtr, r);
    vnl_matrix<double> rtrpt (n, n);

    // Permute. First order columns.
    // Note, *ipvt_ contains 1 to n, not 0 to n-1
    vnl_vector<int> jpvt(n);
    for (unsigned int j = 0; j < n; ++j) {
      unsigned int i = 0;
      for (; i < n; i++) {
        if (ipvt_[i] == (int)j+1) {
          jpvt (j) = i;
          break;
        }
      }
      rtrpt.set_column(j, rtr.get_column(i));
    }

    // Now order rows
    for (unsigned int j = 0; j < n; ++j) {
      inv_covar_.set_row (j, rtrpt.get_row (jpvt(j)));
    }

    set_covariance_ = true;
  }
  return inv_covar_;
}

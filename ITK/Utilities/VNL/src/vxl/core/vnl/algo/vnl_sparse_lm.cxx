// This is core/vnl/algo/vnl_sparse_lm.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Matt Leotta (Brown)
// \date   April 14, 2005
//
//-----------------------------------------------------------------------------

#include "vnl_sparse_lm.h"

#include <vcl_iostream.h>
#include <vcl_iomanip.h>
#include <vcl_algorithm.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_fastops.h>
#include <vnl/vnl_vector_ref.h>
#include <vnl/vnl_crs_index.h>
#include <vnl/vnl_sparse_lst_sqr_function.h>

#include <vnl/algo/vnl_cholesky.h>


// ctor
void vnl_sparse_lm::init(vnl_sparse_lst_sqr_function* f)
{
  f_ = f;

  // If changing these defaults, check the help comments in vnl_sparse_lm.h,
  // and MAKE SURE they're consistent.
  xtol = 1e-15;          // Termination tolerance on X (solution vector)
  maxfev = 1000;         // Termination maximum number of iterations.
  ftol = 1e-15;          // Termination tolerance on F (sum of squared residuals)
  gtol = 1e-15;          // Termination tolerance on Grad(F)' * F = 0
  epsfcn = 0.001;        // Step length for FD Jacobian

  tau_ = 0.001;
}

vnl_sparse_lm::~vnl_sparse_lm()
{
}


//: Minimize the function supplied in the constructor until convergence or failure.
//  On return, a and b are such that f(a,b) is the lowest value achieved.
//  Returns true for convergence, false for failure.
//  if use_gradient is set to false, a finite difference approximation will be used,
//  even if the Jacobian functions have been provided
bool vnl_sparse_lm::minimize(vnl_vector<double>& a, vnl_vector<double>& b, bool use_gradient)
{
  int num_a = f_->number_of_a();
  int num_b = f_->number_of_b();
  int num_e = f_->number_of_e();

  int size_a = f_->index_a(num_a);
  int size_b = f_->index_b(num_b);
  int size_e = f_->index_e(num_e);

  if (size_a+size_b > size_e) {
    vcl_cerr << "vnl_sparse_lm: Number of unknowns("<<size_a+size_b<<") greater than number of data ("<<size_e<<")\n";
    failure_code_ = ERROR_DODGY_INPUT;
    return false;
  }

  if (int(a.size()) != size_a) {
    vcl_cerr << "vnl_sparse_lm: Input vector \"a\" length ("<<a.size()<<") not equal to num parameters in \"a\" ("<<size_a<<")\n";
    failure_code_ = ERROR_DODGY_INPUT;
    return false;
  }

  if (int(b.size()) != size_b) {
    vcl_cerr << "vnl_sparse_lm: Input vector \"b\" length ("<<b.size()<<") not equal to num parameters in \"b\" ("<<size_b<<")\n";
    failure_code_ = ERROR_DODGY_INPUT;
    return false;
  }

  // CRS matrix of indices into e, A, B, W, Y
  const vnl_crs_index& crs = f_->residual_indices();
  // Storage for each of the Jacobians A_ij and B_ij
  vcl_vector<vnl_matrix<double> > A(crs.num_non_zero()), B(crs.num_non_zero());
  vcl_vector<vnl_matrix<double> > U(num_a),  V(num_b), inv_V(num_b);
  vnl_vector<double> ea(size_a), eb(size_b);
  vcl_vector<vnl_matrix<double> > W(crs.num_non_zero()), Y(crs.num_non_zero());
  // Storage for residual vector
  vnl_vector<double> e(size_e);
  //: System to solve will be S*da=e
  vnl_matrix<double> S(a.size(), a.size());
  // update vectors
  vnl_vector<double> da(a.size()), db(b.size());

  // sparse vector iterator
  typedef vnl_crs_index::sparse_vector::iterator sv_itr;

  // Iterate through all i and j to set the size of the matrices and vectors defined above
  for (unsigned int i=0; i<f_->number_of_a(); ++i)
  {
    unsigned int ai_size = f_->number_of_params_a(i);
    U[i].set_size(ai_size,ai_size);

    vnl_crs_index::sparse_vector row = crs.sparse_row(i);
    for (sv_itr r_itr=row.begin(); r_itr!=row.end(); ++r_itr)
    {
      unsigned int j = r_itr->second;
      unsigned int k = r_itr->first;
      unsigned int bj_size = f_->number_of_params_b(j);
      unsigned int eij_size = f_->number_of_residuals(k);
      A[k].set_size(eij_size, ai_size);
      B[k].set_size(eij_size, bj_size);
      W[k].set_size(ai_size, bj_size);
      Y[k].set_size(ai_size, bj_size);
    }
  }
  for (unsigned int j=0; j<f_->number_of_b(); ++j)
  {
    unsigned int bj_size = f_->number_of_params_b(j);
    V[j].set_size(bj_size,bj_size);
    inv_V[j].set_size(bj_size,bj_size);
  }

// mu is initialized now because the compiler produces warnings -MM
  double mu=0; // damping term (initialized later)
  double nu=2.0;
  bool stop = false;
  // compute the initial residual
  f_->f(a,b,e);
  num_evaluations_ = 1;

  double sqr_error = e.squared_magnitude();
  start_error_ = vcl_sqrt(sqr_error/e.size()); // RMS error

  for (num_iterations_=0; num_iterations_<(unsigned int)maxfev && !stop; ++num_iterations_)
  {
    if (verbose_)
      vcl_cout << "iteration "<<vcl_setw(4)<<num_iterations_
               << " RMS error = "<< vcl_setprecision(6)<< vcl_setw(12)<<vcl_sqrt(sqr_error/e.size())
               << " mu = "<<vcl_setprecision(6)<< vcl_setw(12) <<mu<< " nu = "<< nu << vcl_endl;

    // Compute the Jacobian in block form J = [A|B]
    // where A and B are sparse and contain subblocks Aij and Bij
    if (use_gradient && f_->has_gradient())
      f_->jac_blocks(a,b,A,B);
    else
      f_->fd_jac_blocks(a,b,A,B,epsfcn); // use finite differences

    // clear the ea and eb for summation
    ea.fill(0.0);
    eb.fill(0.0);
    // clear the V for summation
    for (unsigned int j=0; j<f_->number_of_b(); ++j)
    {
      V[j].fill(0.0);
    }
    // compute blocks U, V, W, ea, and eb from A
    // JtJ = |U  W|  with U and V block diagonal
    //       |Wt V|  and W with same sparsity as residuals
    for (unsigned int i=0; i<f_->number_of_a(); ++i)
    {
      vnl_matrix<double>& Ui = U[i];    Ui.fill(0.0);
      unsigned int ai_size = f_->number_of_params_a(i);
      vnl_vector_ref<double> eai(ai_size, ea.data_block()+f_->index_a(i));
      vnl_vector_ref<double> ai(ai_size, a.data_block()+f_->index_a(i));

      vnl_crs_index::sparse_vector row = crs.sparse_row(i);
      for (sv_itr r_itr=row.begin(); r_itr!=row.end(); ++r_itr)
      {
        unsigned int j = r_itr->second;
        unsigned int k = r_itr->first;;
        vnl_matrix<double>& Aij = A[k];
        vnl_matrix<double>& Bij = B[k];
        vnl_matrix<double>& Vj = V[j];
        vnl_vector_ref<double> bj(Bij.cols(), b.data_block()+f_->index_b(j));
        vnl_vector_ref<double> ebj(Bij.cols(), eb.data_block()+f_->index_b(j));

        vnl_fastops::inc_X_by_AtA(Ui,Aij);       // Ui += A_ij^T * A_ij
        vnl_fastops::inc_X_by_AtA(Vj,Bij);       // Vj += B_ij^T * B_ij
        vnl_fastops::AtB(W[k],Aij,Bij);          // Wij = A_ij^T * B_ij

        vnl_vector_ref<double> eij(f_->number_of_residuals(k), e.data_block()+f_->index_e(k));
        vnl_fastops::inc_X_by_AtB(eai,Aij,eij);  // e_a_i += A_ij^T * e_ij
        vnl_fastops::inc_X_by_AtB(ebj,Bij,eij);  // e_b_j += B_ij^T * e_ij
      }
    }

    // check for convergence in gradient
    if (vcl_max(ea.inf_norm(),eb.inf_norm()) <= gtol) {
      failure_code_ = CONVERGED_GTOL;
      stop = true;
      break;
    }


    double sqr_params = a.squared_magnitude() + b.squared_magnitude();

    // Extract the diagonal of J^T*J as a vector
    vnl_vector<double> diag_UV(a.size()+b.size());
    int z = 0;
    for (unsigned int i=0; i<f_->number_of_a(); ++i) {
      vnl_matrix<double>& Ui = U[i];
      for (unsigned int ii=0; ii<Ui.rows(); ++ii)
        diag_UV[z++] = Ui(ii,ii);
    }
    for (unsigned int j=0; j<f_->number_of_b(); ++j) {
      vnl_matrix<double>& Vj = V[j];
      for (unsigned int ii=0; ii<Vj.rows(); ++ii)
        diag_UV[z++] = Vj(ii,ii);
    }


    // initialize mu if this is the first iteration
    // proportional to the diagonal entry with the largest magnitude
    if (num_iterations_==0)
      mu = tau_*diag_UV.inf_norm();

    // Resolve the system while adapting mu until we decrease error or converge
    while (true)
    {
      // augment the diagonals with damping term mu
      z=0;
      for (unsigned int i=0; i<f_->number_of_a(); ++i) {
        vnl_matrix<double>& Ui = U[i];
        for (unsigned int ii=0; ii<Ui.rows(); ++ii)
          Ui(ii,ii) = diag_UV[z++] + mu;
      }
      for (unsigned int j=0; j<f_->number_of_b(); ++j) {
        vnl_matrix<double>& Vj = V[j];
        for (unsigned int ii=0; ii<Vj.rows(); ++ii)
          Vj(ii,ii) = diag_UV[z++] + mu;
      }

      // compute inv(Vj) and Yij
      for (unsigned int j=0; j<f_->number_of_b(); ++j) {
        vnl_matrix<double>& inv_Vj = inv_V[j];
        inv_Vj = vnl_cholesky(V[j]).inverse();

        vnl_crs_index::sparse_vector col = crs.sparse_col(j);
        for (sv_itr c_itr=col.begin(); c_itr!=col.end(); ++c_itr)
        {
          unsigned int k = c_itr->first;
          Y[k] = W[k]*inv_Vj;  // Y_ij = W_ij * inv(V_j)
        }
      }

      // |I -W*inv(V)| * |U  W| * |da| = |I -W*inv(V)| * |ea|
      // |0     I    |   |Wt V|   |db|   |0     I    |   |eb|
      //
      // premultiplying as shown above gives:
      // |S  0| * |da| = |se|
      // |Wt V|   |db|   |eb|
      //
      // so we can first solve  S*da = se  and then substitute to find db

      // compute S and se
      vnl_vector<double> se(ea); // make a working copy of ea
      for (unsigned int i=0; i<f_->number_of_a(); ++i)
      {
        vnl_vector_ref<double> sei(f_->number_of_params_a(i),se.data_block()+f_->index_a(i));
        vnl_crs_index::sparse_vector row_i = crs.sparse_row(i);

        // handle the diagonal blocks and computation of se separately
        vnl_matrix<double> Sii(U[i]); // copy Ui to initialize Sii
        for (sv_itr ri = row_i.begin(); ri != row_i.end();  ++ri)
        {
          unsigned int k = ri->first;
          vnl_matrix<double>& Yij = Y[k];
          vnl_fastops::dec_X_by_ABt(Sii,Yij,W[k]); // S_ii -= Y_ij * W_ij^T
          vnl_vector_ref<double> ebj(Yij.cols(), eb.data_block()+f_->index_b(ri->second));
          sei -= Yij*ebj;  // se_i -= Y_ij * e_b_j
        }
        S.update(Sii,f_->index_a(i),f_->index_a(i));

        // handle the (symmetric) off diagonal blocks
        for (unsigned int h=i+1; h<f_->number_of_a(); ++h)
        {
          vnl_crs_index::sparse_vector row_h = crs.sparse_row(h);
          vnl_matrix<double> Sih(f_->number_of_params_a(i),f_->number_of_params_a(h),0.0);

          // iterate through both sparse rows finding matching columns
          bool row_done = false;
          for (sv_itr ri = row_i.begin(), rh = row_h.begin();
               ri != row_i.end() && rh != row_h.end();  ++ri, ++rh)
          {
            while (!row_done && ri->second != rh->second)
            {
              while (!row_done && ri->second < rh->second)
                row_done = (++ri == row_i.end());
              while (!row_done && rh->second < ri->second)
                row_done = (++rh == row_h.end());
            }
            if (row_done)
              break;
            vnl_matrix<double>& Yij = Y[ri->first];
            vnl_fastops::dec_X_by_ABt(Sih,Yij,W[rh->first]); // S_ih -= Y_ij * W_hj^T
          }
          // this should also be a symmetric matrix
          S.update(Sih,f_->index_a(i),f_->index_a(h));
          S.update(Sih.transpose(),f_->index_a(h),f_->index_a(i));
        }
      }

      // We could use a faster solver here, maybe conjugate gradients?
      // Solve the system  S*da = se  for da
      da = vnl_cholesky(S).solve(se);

      // substitute da to compute db
      for (unsigned int j=0; j<f_->number_of_b(); ++j) {
        vnl_vector<double> temp(eb.data_block()+f_->index_b(j),f_->number_of_params_b(j));
        vnl_crs_index::sparse_vector col = crs.sparse_col(j);
        for (sv_itr c_itr=col.begin(); c_itr!=col.end(); ++c_itr)
        {
          unsigned int k = c_itr->first;
          unsigned int i = c_itr->second;
          vnl_vector_ref<double> dai(f_->number_of_params_a(i),da.data_block()+f_->index_a(i));
          vnl_fastops::dec_X_by_AtB(temp,W[k],dai);
        }
        vnl_vector_ref<double> dbi(f_->number_of_params_b(j),db.data_block()+f_->index_b(j));
        vnl_fastops::Ab(dbi,inv_V[j],temp);
      }

      // check for convergence in parameters (change in parameters is below a tolerance)
      if (da.squared_magnitude()+db.squared_magnitude() < xtol*xtol*sqr_params) {
        failure_code_ = CONVERGED_XTOL;
        stop = true;
        break;
      }

      // compute updated parameters and residuals of the new parameters
      vnl_vector<double> new_a(a-da), new_b(b-db), new_e(e.size());
      f_->f(new_a,new_b,new_e); // compute the new residual vector
      ++num_evaluations_;
      double new_sqr_error = new_e.squared_magnitude();

      double dF = sqr_error - new_sqr_error;
      double dL = dot_product(da,(mu*da+ea)) + dot_product(db,(mu*db+eb));
      if (dF>0.0 && dL>0.0) {
        double tmp = 2.0*dF/dL-1.0;
        mu *= vcl_max(1.0/3.0, 1.0 - tmp*tmp*tmp);
        nu = 2.0;
        a = new_a;
        b = new_b;
        e = new_e;
        sqr_error = new_sqr_error;
        break;
      }

      mu *= nu;
      nu *= 2.0;

      if (verbose_)
        vcl_cout <<"               RMS error = "<< vcl_setprecision(6)
                 << vcl_setw(12) << vcl_sqrt(sqr_error/e.size())
                 << " mu = " << vcl_setprecision(6) << vcl_setw(12) << mu
                 << " nu = " << nu << vcl_endl;
    }
  }


  end_error_ = vcl_sqrt(sqr_error/e.size()); // RMS error

  if ((int)num_iterations_ >= maxfev) {
    failure_code_ = TOO_MANY_ITERATIONS;
  }

  // Translate status code
  switch (failure_code_) {
   case CONVERGED_FTOL:
   case CONVERGED_XTOL:
   case CONVERGED_XFTOL:
   case CONVERGED_GTOL:
    return true;
   default:
    diagnose_outcome();
    return false;
  }
}


//------------------------------------------------------------------------------

void vnl_sparse_lm::diagnose_outcome() const
{
  diagnose_outcome(vcl_cerr);
}

// fsm: should this function be a method on vnl_nonlinear_minimizer?
// if not, the return codes should be moved into LM.
void vnl_sparse_lm::diagnose_outcome(vcl_ostream& s) const
{
#define whoami "vnl_sparse_lm"
  //if (!verbose_) return;
  switch (failure_code_)
  {
   case ERROR_FAILURE:
    s << (whoami ": OIOIOI -- failure in leastsquares function\n");
    break;
   case ERROR_DODGY_INPUT:
    s << (whoami ": OIOIOI -- lmdif dodgy input\n");
    break;
   case CONVERGED_FTOL:
    s << (whoami ": converged to ftol\n");
    break;
   case CONVERGED_XTOL:
    s << (whoami ": converged to xtol\n");
    break;
   case CONVERGED_XFTOL:
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
    s << (whoami ": gtol is too small. f(a,b) is orthogonal to the columns of the jacobian to machine precision.\n");
    break;
   default:
    s << (whoami ": OIOIOI: unkown info code from lmder.\n");
    break;
  }
  unsigned int num_e = f_->number_of_e();
  s << whoami ": " << num_iterations_ << " iterations, "
    << num_evaluations_ << " evaluations, "<< num_e <<" residuals.  RMS error start/end "
    << get_start_error() << '/' << get_end_error() << vcl_endl;
#undef whoami
}


vnl_matrix<double> const& vnl_sparse_lm::get_JtJ()
{
  return inv_covar_;
}

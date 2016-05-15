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

#include <iostream>
#include <iomanip>
#include <algorithm>
#include "vnl_sparse_lm.h"

#include <vcl_compiler.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_fastops.h>
#include <vnl/vnl_vector_ref.h>
#include <vnl/vnl_crs_index.h>
#include <vnl/vnl_sparse_lst_sqr_function.h>

#include <vnl/algo/vnl_cholesky.h>
#include <vnl/algo/vnl_svd.h>


//: Initialize with the function object that is to be minimized.
vnl_sparse_lm::vnl_sparse_lm(vnl_sparse_lst_sqr_function& f)
 : num_a_(f.number_of_a()),
   num_b_(f.number_of_b()),
   num_e_(f.number_of_e()),
   num_nz_(f.residual_indices().num_non_zero()),
   size_a_(f.index_a(num_a_)),
   size_b_(f.index_b(num_b_)),
   size_c_(f.number_of_params_c()),
   size_e_(f.index_e(num_e_)),
   A_(num_nz_),
   B_(num_nz_),
   C_(num_nz_),
   U_(num_a_),
   V_(num_b_),
   T_(size_c_, size_c_),
   W_(num_nz_),
   R_(num_b_),
   Q_(num_a_),
   ea_(size_a_),
   eb_(size_b_),
   ec_(size_c_),
   e_(size_e_),
   weights_(f.has_weights() ? num_e_ : 0, 1.0),
   inv_V_(num_b_),
   Y_(num_nz_),
   Z_(num_a_),
   Ma_(num_a_),
   Mb_(num_b_)
{
  init(&f);
}


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

  allocate_matrices();
}

vnl_sparse_lm::~vnl_sparse_lm()
{
}


//: Minimize the function supplied in the constructor until convergence or failure.
//  On return, a, b, and c are such that f(a,b,c) is the lowest value achieved.
//  Returns true for convergence, false for failure.
//  If use_gradient is set to false, a finite difference approximation will be used,
//  even if the Jacobian functions have been provided.
//  If use_weights is set to false, weights will not be computed even if a
//  weighting function has been provided.
bool vnl_sparse_lm::minimize(vnl_vector<double>& a,
                             vnl_vector<double>& b,
                             vnl_vector<double>& c,
                             bool use_gradient,
                             bool use_weights)
{
  // verify that the vectors are of the correct size
  if (!check_vector_sizes(a,b,c))
    return false;

  //: Systems to solve will be Sc*dc=sec and Sa*da=sea
  vnl_matrix<double> Sc(size_c_,size_c_), Sa(size_a_, size_a_);
  vnl_vector<double> sec(size_c_), sea(size_a_);
  // update vectors
  vnl_vector<double> da(size_a_), db(size_b_), dc(size_c_);


  // mu is initialized now because the compiler produces warnings -MM
  double mu=0; // damping term (initialized later)
  double nu=2.0;
  // compute the initial residual
  f_->f(a,b,c,e_);
  num_evaluations_ = 1;

  // Compute and apply the weights if applicable
  if (use_weights && f_->has_weights())
  {
    f_->compute_weights(a,b,c,e_,weights_);
    f_->apply_weights(weights_, e_);
  }

  double sqr_error = e_.squared_magnitude();
  start_error_ = std::sqrt(sqr_error/e_.size()); // RMS error

  for (num_iterations_=0; num_iterations_<(unsigned int)maxfev; ++num_iterations_)
  {
    if (verbose_)
      std::cout << "iteration "<<std::setw(4)<<num_iterations_
               << " RMS error = "<< std::setprecision(6)<< std::setw(12)<<std::sqrt(sqr_error/e_.size())
               << " mu = "<<std::setprecision(6)<< std::setw(12) <<mu<< " nu = "<< nu << std::endl;
    if (trace)
      f_->trace(num_iterations_,a,b,c,e_);

    // Compute the Jacobian in block form J = [A|B|C]
    // where A, B, and C are sparse and contain subblocks Aij, Bij, and Cij
    if (use_gradient && f_->has_gradient())
      f_->jac_blocks(a,b,c,A_,B_,C_);
    else
      f_->fd_jac_blocks(a,b,c,A_,B_,C_,epsfcn); // use finite differences

    // Apply the weights if applicable
    if (use_weights && f_->has_weights())
    {
      f_->apply_weights(weights_, A_,B_,C_);
    }

    compute_normal_equations();

    // check for convergence in gradient
    if (std::max(std::max(ea_.inf_norm(),eb_.inf_norm()),ec_.inf_norm()) <= gtol)
    {
      failure_code_ = CONVERGED_GTOL;
      break;
    }


    double sqr_params = a.squared_magnitude();
    sqr_params += b.squared_magnitude();
    sqr_params += c.squared_magnitude();

    // Extract the diagonal of J^T*J as a vector
    vnl_vector<double> diag_UVT = extract_diagonal();

    // initialize mu if this is the first iteration
    // proportional to the diagonal entry with the largest magnitude
    if (num_iterations_==0)
      mu = tau_*diag_UVT.inf_norm();

    // Re-solve the system while adapting mu until we decrease error or converge
    while (true)
    {
      // augment the diagonals with damping term mu
      set_diagonal(diag_UVT + mu);

      // compute inv(Vj) and Yij
      compute_invV_Y();

      if ( size_c_ > 0 )
      {
        // compute Z = RYt-Q and Sa
        compute_Z_Sa(Sa);

        // this large inverse is the bottle neck of this algorithm
        vnl_matrix<double> H;
        vnl_cholesky Sa_cholesky(Sa,vnl_cholesky::quiet);
        vnl_svd<double> *Sa_svd = VXL_NULLPTR;
        // use SVD as a backup if Cholesky is deficient
        if ( Sa_cholesky.rank_deficiency() > 0 )
        {
          Sa_svd = new vnl_svd<double>(Sa);
          H = Sa_svd->inverse();
        }
        else
          H = Sa_cholesky.inverse();

        // construct the Ma = ZH
        compute_Ma(H);
        // construct Mb = (R+MaW)inv(V)
        compute_Mb();

        // use Ma and Mb to solve for dc
        solve_dc(dc);

        // compute sea from ea, Z, dc, Y, and eb
        compute_sea(dc,sea);


        if ( Sa_svd )
          da = Sa_svd->solve(sea);
        else
          da = Sa_cholesky.solve(sea);
        delete Sa_svd;
      }
      else // size_c_ == 0
      {
        // |I -W*inv(V)| * |U  W| * |da| = |I -W*inv(V)| * |ea|
        // |0     I    |   |Wt V|   |db|   |0     I    |   |eb|
        //
        // premultiplying as shown above gives:
        // |Sa 0| * |da| = |sea|
        // |Wt V|   |db|   |eb |
        //
        // so we can first solve  Sa*da = sea  and then substitute to find db

        // compute Sa and sea
        compute_Sa_sea(Sa,sea);

#ifdef DEBUG
        std::cout << "singular values = "<< vnl_svd<double>(Sa).W() <<std::endl;
#endif
        // We could use a faster solver here, maybe conjugate gradients?
        // Solve the system  Sa*da = sea  for da

        vnl_cholesky Sa_cholesky(Sa,vnl_cholesky::quiet);
        // use SVD as a backup if Cholesky is deficient
        if ( Sa_cholesky.rank_deficiency() > 0 )
        {
          vnl_svd<double> Sa_svd(Sa);
          da = Sa_svd.solve(sea);
        }
        else
          da = Sa_cholesky.solve(sea);
      }

      // substitute da and dc to compute db
      backsolve_db(da, dc, db);

      // check for convergence in parameters
      // (change in parameters is below a tolerance)
      double sqr_delta = da.squared_magnitude();
      sqr_delta += db.squared_magnitude();
      sqr_delta += dc.squared_magnitude();
      if (sqr_delta < xtol*xtol*sqr_params) {
        failure_code_ = CONVERGED_XTOL;
        break;
      }

      // compute updated parameters and residuals of the new parameters
      vnl_vector<double> new_a(a-da), new_b(b-db), new_c(c-dc);
      vnl_vector<double> new_e(e_.size()), new_weights(weights_.size());
      f_->f(new_a,new_b,new_c,new_e); // compute the new residual vector
      ++num_evaluations_;

      // Compute and apply the weights if applicable
      if (use_weights && f_->has_weights())
      {
        f_->compute_weights(new_a,new_b,new_c,new_e,new_weights);
        f_->apply_weights(new_weights, new_e);
      }

      double new_sqr_error = new_e.squared_magnitude();

      double dF = sqr_error - new_sqr_error;
      double dL = dot_product(da,(mu*da+ea_))
                 +dot_product(db,(mu*db+eb_))
                 +dot_product(dc,(mu*dc+ec_));
      if (dF>0.0 && dL>0.0) {
        double tmp = 2.0*dF/dL-1.0;
        mu *= std::max(1.0/3.0, 1.0 - tmp*tmp*tmp);
        nu = 2.0;
        a.swap(new_a);
        b.swap(new_b);
        c.swap(new_c);
        e_.swap(new_e);
        weights_.swap(new_weights);
        sqr_error = new_sqr_error;
        break;
      }

      mu *= nu;
      nu *= 2.0;

      if (verbose_)
        std::cout <<"               RMS error = "<< std::setprecision(6)
                 << std::setw(12) << std::sqrt(sqr_error/e_.size())
                 << " mu = " << std::setprecision(6) << std::setw(12) << mu
                 << " nu = " << nu << std::endl;
    }
  }


  end_error_ = std::sqrt(sqr_error/e_.size()); // RMS error

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


//: check vector sizes and verify that they match the problem size
bool vnl_sparse_lm::check_vector_sizes(vnl_vector<double> const& a,
                                       vnl_vector<double> const& b,
                                       vnl_vector<double> const& c)
{
  if (size_a_+size_b_ > size_e_) {
    std::cerr << "vnl_sparse_lm: Number of unknowns("<<size_a_+size_b_<<')'
             << " greater than number of data ("<<size_e_<<")\n";
    failure_code_ = ERROR_DODGY_INPUT;
    return false;
  }

  if (int(a.size()) != size_a_) {
    std::cerr << "vnl_sparse_lm: Input vector \"a\" length ("<<a.size()<<')'
             << " not equal to num parameters in \"a\" ("<<size_a_<<")\n";
    failure_code_ = ERROR_DODGY_INPUT;
    return false;
  }

  if (int(b.size()) != size_b_) {
    std::cerr << "vnl_sparse_lm: Input vector \"b\" length ("<<b.size()<<')'
             << " not equal to num parameters in \"b\" ("<<size_b_<<")\n";
    failure_code_ = ERROR_DODGY_INPUT;
    return false;
  }

  if (int(c.size()) != size_c_) {
    std::cerr << "vnl_sparse_lm: Input vector \"c\" length ("<<c.size()<<')'
             << " not equal to num parameters in \"c\" ("<<size_c_<<")\n";
    failure_code_ = ERROR_DODGY_INPUT;
    return false;
  }

  return true;
}


//: allocate matrix memory by setting all the matrix sizes
void vnl_sparse_lm::allocate_matrices()
{
  // CRS matrix of indices into e, A, B, C, W, Y
  const vnl_crs_index& crs = f_->residual_indices();
  // sparse vector iterator
  typedef vnl_crs_index::sparse_vector::iterator sv_itr;

  // Iterate through all i and j to set the size of the matrices and vectors defined above
  for (int i=0; i<num_a_; ++i)
  {
    const unsigned int ai_size = f_->number_of_params_a(i);
    U_[i].set_size(ai_size,ai_size);
    Q_[i].set_size(size_c_, ai_size);
    Z_[i].set_size(size_c_, ai_size);
    Ma_[i].set_size(size_c_, ai_size);

    vnl_crs_index::sparse_vector row = crs.sparse_row(i);
    for (sv_itr r_itr=row.begin(); r_itr!=row.end(); ++r_itr)
    {
      const unsigned int j = r_itr->second;
      const unsigned int k = r_itr->first;
      const unsigned int bj_size = f_->number_of_params_b(j);
      const unsigned int eij_size = f_->number_of_residuals(k);
      A_[k].set_size(eij_size, ai_size);
      B_[k].set_size(eij_size, bj_size);
      C_[k].set_size(eij_size, size_c_);
      W_[k].set_size(ai_size, bj_size);
      Y_[k].set_size(ai_size, bj_size);
    }
  }
  for (int j=0; j<num_b_; ++j)
  {
    const unsigned int bj_size = f_->number_of_params_b(j);
    V_[j].set_size(bj_size,bj_size);
    R_[j].set_size(size_c_, bj_size);
    Mb_[j].set_size(size_c_, bj_size);
    inv_V_[j].set_size(bj_size,bj_size);
  }
}


//: compute the blocks making up the the normal equations: Jt J d = Jt e
void vnl_sparse_lm::compute_normal_equations()
{
  // CRS matrix of indices into e, A, B, C, W, Y
  const vnl_crs_index& crs = f_->residual_indices();
  // sparse vector iterator
  typedef vnl_crs_index::sparse_vector::iterator sv_itr;

  // clear the ea and eb for summation
  ea_.fill(0.0);
  eb_.fill(0.0);
  ec_.fill(0.0);
  // clear the V for summation
  for (unsigned int j=0; j<f_->number_of_b(); ++j)
  {
    V_[j].fill(0.0);
    R_[j].fill(0.0);
  }
  T_.fill(0.0);
  // compute blocks T, Q, R, U, V, W, ea, eb, and ec
  // JtJ = |T  Q  R|
  //       |Qt U  W|  with U and V block diagonal
  //       |Rt Wt V|  and W with same sparsity as residuals
  for (unsigned int i=0; i<f_->number_of_a(); ++i)
  {
    vnl_matrix<double>& Ui = U_[i];
    Ui.fill(0.0);
    vnl_matrix<double>& Qi = Q_[i];
    Qi.fill(0.0);
    unsigned int ai_size = f_->number_of_params_a(i);
    vnl_vector_ref<double> eai(ai_size, ea_.data_block()+f_->index_a(i));

    vnl_crs_index::sparse_vector row = crs.sparse_row(i);
    for (sv_itr r_itr=row.begin(); r_itr!=row.end(); ++r_itr)
    {
      unsigned int j = r_itr->second;
      unsigned int k = r_itr->first;;
      vnl_matrix<double>& Aij = A_[k];
      vnl_matrix<double>& Bij = B_[k];
      vnl_matrix<double>& Cij = C_[k];
      vnl_matrix<double>& Vj = V_[j];
      vnl_matrix<double>& Rj = R_[j];
      vnl_vector_ref<double> ebj(Bij.cols(), eb_.data_block()+f_->index_b(j));

      vnl_fastops::inc_X_by_AtA(T_, Cij);       // T = C^T * C
      vnl_fastops::inc_X_by_AtA(Ui,Aij);       // Ui += A_ij^T * A_ij
      vnl_fastops::inc_X_by_AtA(Vj,Bij);       // Vj += B_ij^T * B_ij
      vnl_fastops::AtB(W_[k],Aij,Bij);          // Wij = A_ij^T * B_ij
      vnl_fastops::inc_X_by_AtB(Qi,Cij,Aij);   // Qi += C_ij^T * A_ij
      vnl_fastops::inc_X_by_AtB(Rj,Cij,Bij);   // Rj += C_ij^T * B_ij

      vnl_vector_ref<double> eij(f_->number_of_residuals(k), e_.data_block()+f_->index_e(k));
      vnl_fastops::inc_X_by_AtB(eai,Aij,eij);  // e_a_i += A_ij^T * e_ij
      vnl_fastops::inc_X_by_AtB(ebj,Bij,eij);  // e_b_j += B_ij^T * e_ij
      vnl_fastops::inc_X_by_AtB(ec_,Cij,eij);   // e_c   += C_ij^T * e_ij
    }
  }
}


//: extract the vector on the diagonal of Jt J
vnl_vector<double> vnl_sparse_lm::extract_diagonal() const
{
  // Extract the diagonal of J^T*J as a vector
  vnl_vector<double> diag_UVT(size_a_+size_b_+size_c_);
  int z = 0;
  for (int i=0; i<num_a_; ++i) {
    const vnl_matrix<double>& Ui = U_[i];
    for (unsigned int ii=0; ii<Ui.rows(); ++ii)
      diag_UVT[z++] = Ui(ii,ii);
  }
  for (int j=0; j<num_b_; ++j) {
    const vnl_matrix<double>& Vj = V_[j];
    for (unsigned int ii=0; ii<Vj.rows(); ++ii)
      diag_UVT[z++] = Vj(ii,ii);
  }
  for (int ii=0; ii<size_c_; ++ii)
    diag_UVT[z++] = T_(ii,ii);

  return diag_UVT;
}


//: set the vector on the diagonal of Jt J
void vnl_sparse_lm::set_diagonal(const vnl_vector<double>& diag)
{
  int z=0;
  for (int i=0; i<num_a_; ++i) {
    vnl_matrix<double>& Ui = U_[i];
    for (unsigned int ii=0; ii<Ui.rows(); ++ii)
      Ui(ii,ii) = diag[z++];
  }
  for (int j=0; j<num_b_; ++j) {
    vnl_matrix<double>& Vj = V_[j];
    for (unsigned int ii=0; ii<Vj.rows(); ++ii)
      Vj(ii,ii) = diag[z++];
  }
  for (int ii=0; ii<size_c_; ++ii)
    T_(ii,ii) = diag[z++];
}


//: compute all inv(Vi) and Yij
void vnl_sparse_lm::compute_invV_Y()
{
  // CRS matrix of indices into e, A, B, C, W, Y
  const vnl_crs_index& crs = f_->residual_indices();
  // sparse vector iterator
  typedef vnl_crs_index::sparse_vector::iterator sv_itr;

  for (int j=0; j<num_b_; ++j) {
    vnl_matrix<double>& inv_Vj = inv_V_[j];
    vnl_cholesky Vj_cholesky(V_[j],vnl_cholesky::quiet);
    // use SVD as a backup if Cholesky is deficient
    if ( Vj_cholesky.rank_deficiency() > 0 )
    {
      vnl_svd<double> Vj_svd(V_[j]);
      inv_Vj = Vj_svd.inverse();
    }
    else
      inv_Vj = Vj_cholesky.inverse();

    vnl_crs_index::sparse_vector col = crs.sparse_col(j);
    for (sv_itr c_itr=col.begin(); c_itr!=col.end(); ++c_itr)
    {
      unsigned int k = c_itr->first;
      Y_[k] = W_[k]*inv_Vj;  // Y_ij = W_ij * inv(V_j)
    }
  }
}


// compute Z and Sa
void vnl_sparse_lm::compute_Z_Sa(vnl_matrix<double>& Sa)
{
  // CRS matrix of indices into e, A, B, C, W, Y
  const vnl_crs_index& crs = f_->residual_indices();
  // sparse vector iterator
  typedef vnl_crs_index::sparse_vector::iterator sv_itr;

  // compute Z = RYt-Q and Sa
  for (int i=0; i<num_a_; ++i)
  {
    vnl_crs_index::sparse_vector row_i = crs.sparse_row(i);
    vnl_matrix<double>& Zi = Z_[i];
    Zi.fill(0.0);
    Zi -= Q_[i];

    // handle the diagonal blocks separately
    vnl_matrix<double> Sii(U_[i]); // copy Ui to initialize Sii
    for (sv_itr ri = row_i.begin(); ri != row_i.end();  ++ri)
    {
      unsigned int j = ri->second;
      unsigned int k = ri->first;
      vnl_matrix<double>& Yij = Y_[k];
      vnl_fastops::dec_X_by_ABt(Sii,Yij,W_[k]); // S_ii -= Y_ij * W_ij^T
      vnl_fastops::inc_X_by_ABt(Zi,R_[j],Yij);  // Z_i  += R_j * Y_ij^T
    }
    Sa.update(Sii,f_->index_a(i),f_->index_a(i));

    // handle the (symmetric) off diagonal blocks
    for (int h=i+1; h<num_a_; ++h)
    {
      vnl_crs_index::sparse_vector row_h = crs.sparse_row(h);
      vnl_matrix<double> Sih(f_->number_of_params_a(i),
                             f_->number_of_params_a(h), 0.0);

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
        // S_ih -= Y_ij * W_hj^T
        vnl_fastops::dec_X_by_ABt(Sih,Y_[ri->first],W_[rh->first]);
      }
      // this should also be a symmetric matrix
      Sa.update(Sih,f_->index_a(i),f_->index_a(h));
      Sa.update(Sih.transpose(),f_->index_a(h),f_->index_a(i));
    }
  }
}


//: compute Ma
void vnl_sparse_lm::compute_Ma(const vnl_matrix<double>& H)
{
  // construct Ma = ZH
  vnl_matrix<double> Hik;
  for (int i=0; i<num_a_; ++i)
  {
    vnl_matrix<double>& Mai = Ma_[i];
    Mai.fill(0.0);

    for (int k=0; k<num_a_; ++k)
    {
      Hik.set_size(f_->number_of_params_a(i), f_->number_of_params_a(k));
      H.extract(Hik,f_->index_a(i), f_->index_a(k));
      vnl_fastops::inc_X_by_AB(Mai, Z_[k], Hik);
    }
  }
}


//: compute Mb
void vnl_sparse_lm::compute_Mb()
{
  // CRS matrix of indices into e, A, B, C, W, Y
  const vnl_crs_index& crs = f_->residual_indices();
  // sparse vector iterator
  typedef vnl_crs_index::sparse_vector::iterator sv_itr;

  vnl_matrix<double> temp;
  // construct Mb = (-R-MaW)inv(V)
  for (int j=0; j<num_b_; ++j)
  {
    temp.set_size(size_c_,f_->number_of_params_b(j));
    temp.fill(0.0);
    temp -= R_[j];

    vnl_crs_index::sparse_vector col = crs.sparse_col(j);
    for (sv_itr c_itr=col.begin(); c_itr!=col.end(); ++c_itr)
    {
      unsigned int k = c_itr->first;
      unsigned int i = c_itr->second;
      vnl_fastops::dec_X_by_AB(temp,Ma_[i],W_[k]);
    }
    vnl_fastops::AB(Mb_[j],temp,inv_V_[j]);
  }
}


//: solve for dc
void vnl_sparse_lm::solve_dc(vnl_vector<double>& dc)
{
  vnl_matrix<double> Sc(T_); // start with a copy of T
  vnl_vector<double> sec(ec_); // start with a copy of ec

  for (int i=0; i<num_a_; ++i)
  {
    const vnl_vector_ref<double> eai(f_->number_of_params_a(i),
                                     const_cast<double*>(ea_.data_block()+f_->index_a(i)));
    vnl_fastops::inc_X_by_ABt(Sc,Ma_[i],Q_[i]);
    sec += Ma_[i] * eai;
  }
  for (int j=0; j<num_b_; ++j)
  {
    const vnl_vector_ref<double> ebi(f_->number_of_params_b(j),
                                     const_cast<double*>(eb_.data_block()+f_->index_b(j)));
    vnl_fastops::inc_X_by_ABt(Sc,Mb_[j],R_[j]);
    sec += Mb_[j] * ebi;
  }

  if (size_c_ == 1)
  {
    dc[0] = sec[0] / Sc(0,0);
  }
  else
  {
    // Solve Sc*dc = sec for dc
    vnl_cholesky Sc_cholesky(Sc,vnl_cholesky::quiet);
    // use SVD as a backup if Cholesky is deficient
    if ( Sc_cholesky.rank_deficiency() > 0 )
    {
      vnl_svd<double> Sc_svd(Sc);
      dc = Sc_svd.solve(sec);
    }
    else
      dc = Sc_cholesky.solve(sec);
  }
}


//: compute sea using ea, Z, dc, Y, and eb
void vnl_sparse_lm::compute_sea(vnl_vector<double> const& dc,
                                vnl_vector<double>& sea)
{
  // CRS matrix of indices into e, A, B, C, W, Y
  const vnl_crs_index& crs = f_->residual_indices();
  // sparse vector iterator
  typedef vnl_crs_index::sparse_vector::iterator sv_itr;

  sea = ea_; // initialize se to ea_
  for (int i=0; i<num_a_; ++i)
  {
    vnl_vector_ref<double> sei(f_->number_of_params_a(i),sea.data_block()+f_->index_a(i));
    vnl_crs_index::sparse_vector row_i = crs.sparse_row(i);

    vnl_fastops::inc_X_by_AtB(sei,Z_[i],dc);

    for (sv_itr ri = row_i.begin(); ri != row_i.end();  ++ri)
    {
      unsigned int k = ri->first;
      vnl_matrix<double>& Yij = Y_[k];
      vnl_vector_ref<double> ebj(Yij.cols(), eb_.data_block()+f_->index_b(ri->second));
      sei -= Yij*ebj;  // se_i -= Y_ij * e_b_j
    }
  }
}


//: compute Sa and sea
// only used when size_c_ == 0
void vnl_sparse_lm::compute_Sa_sea(vnl_matrix<double>& Sa,
                                   vnl_vector<double>& sea)
{
  // CRS matrix of indices into e, A, B, C, W, Y
  const vnl_crs_index& crs = f_->residual_indices();
  // sparse vector iterator
  typedef vnl_crs_index::sparse_vector::iterator sv_itr;

  sea = ea_; // initialize se to ea_
  for (int i=0; i<num_a_; ++i)
  {
    vnl_vector_ref<double> sei(f_->number_of_params_a(i),sea.data_block()+f_->index_a(i));
    vnl_crs_index::sparse_vector row_i = crs.sparse_row(i);

    // handle the diagonal blocks and computation of se separately
    vnl_matrix<double> Sii(U_[i]); // copy Ui to initialize Sii
    for (sv_itr ri = row_i.begin(); ri != row_i.end();  ++ri)
    {
      unsigned int k = ri->first;
      vnl_matrix<double>& Yij = Y_[k];
      vnl_fastops::dec_X_by_ABt(Sii,Yij,W_[k]); // S_ii -= Y_ij * W_ij^T
      vnl_vector_ref<double> ebj(Yij.cols(), eb_.data_block()+f_->index_b(ri->second));
      sei -= Yij*ebj;  // se_i -= Y_ij * e_b_j
    }
    Sa.update(Sii,f_->index_a(i),f_->index_a(i));

    // handle the (symmetric) off diagonal blocks
    for (int h=i+1; h<num_a_; ++h)
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
        // S_ih -= Y_ij * W_hj^T
        vnl_fastops::dec_X_by_ABt(Sih,Y_[ri->first],W_[rh->first]);
      }
      // this should also be a symmetric matrix
      Sa.update(Sih,f_->index_a(i),f_->index_a(h));
      Sa.update(Sih.transpose(),f_->index_a(h),f_->index_a(i));
    }
  }
}


//: back solve to find db using da and dc
void vnl_sparse_lm::backsolve_db(vnl_vector<double> const& da,
                                 vnl_vector<double> const& dc,
                                 vnl_vector<double>& db)
{
  // CRS matrix of indices into e, A, B, C, W, Y
  const vnl_crs_index& crs = f_->residual_indices();
  // sparse vector iterator
  typedef vnl_crs_index::sparse_vector::iterator sv_itr;

  for (int j=0; j<num_b_; ++j)
  {
    vnl_vector<double> seb(eb_.data_block()+f_->index_b(j),f_->number_of_params_b(j));
    vnl_crs_index::sparse_vector col = crs.sparse_col(j);
    if ( size_c_ > 0 )
    {
      vnl_fastops::dec_X_by_AtB(seb,R_[j],dc);
    }
    for (sv_itr c_itr=col.begin(); c_itr!=col.end(); ++c_itr)
    {
      unsigned int k = c_itr->first;
      unsigned int i = c_itr->second;
      const vnl_vector_ref<double> dai(f_->number_of_params_a(i),
                                       const_cast<double*>(da.data_block()+f_->index_a(i)));
      vnl_fastops::dec_X_by_AtB(seb,W_[k],dai);
    }
    vnl_vector_ref<double> dbi(f_->number_of_params_b(j),db.data_block()+f_->index_b(j));
    vnl_fastops::Ab(dbi,inv_V_[j],seb);
  }
}

//------------------------------------------------------------------------------

void vnl_sparse_lm::diagnose_outcome() const
{
  diagnose_outcome(std::cerr);
}

// fsm: should this function be a method on vnl_nonlinear_minimizer?
// if not, the return codes should be moved into LM.
void vnl_sparse_lm::diagnose_outcome(std::ostream& s) const
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
    << get_start_error() << '/' << get_end_error() << std::endl;
#undef whoami
}


vnl_matrix<double> const& vnl_sparse_lm::get_JtJ()
{
  return inv_covar_;
}

// This is core/vnl/vnl_sparse_matrix_linear_system.h
#ifndef vnl_sparse_matrix_linear_system_h_
#define vnl_sparse_matrix_linear_system_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief vnl_sparse_matrix -> vnl_linear_system adaptor
//
//  An adaptor that converts a vnl_sparse_matrix<T> to a vnl_linear_system
//
//  \author David Capel, capes@robots
//  \date   July 2000
//
// \verbatim
//  Modifications
//  LSB (Manchester) 19/3/01 Documentation tidied
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_linear_system.h>
#include <vnl/vnl_sparse_matrix.h>

//: vnl_sparse_matrix -> vnl_linear_system adaptor
//  An adaptor that converts a vnl_sparse_matrix<T> to a vnl_linear_system
template <class T>
class vnl_sparse_matrix_linear_system : public vnl_linear_system
{
 public:
  //::Constructor from vnl_sparse_matrix<double> for system Ax = b
  // Keeps a reference to the original sparse matrix A and vector b so DO NOT DELETE THEM!!
  vnl_sparse_matrix_linear_system(vnl_sparse_matrix<T> const& A, vnl_vector<T> const& b) :
    vnl_linear_system(A.columns(), A.rows()), A_(A), b_(b) {}

  //:  Implementations of the vnl_linear_system virtuals.
  void multiply(vnl_vector<double> const& x, vnl_vector<double> & b) const;
  //:  Implementations of the vnl_linear_system virtuals.
  void transpose_multiply(vnl_vector<double> const& b, vnl_vector<double> & x) const;
  //:  Implementations of the vnl_linear_system virtuals.
  void get_rhs(vnl_vector<double>& b) const;
  //:  Implementations of the vnl_linear_system virtuals.
  void apply_preconditioner(vnl_vector<double> const& x, vnl_vector<double> & px) const;

 protected:
  vnl_sparse_matrix<T> const& A_;
  vnl_vector<T> const& b_;
  vnl_vector<double> jacobi_precond_;
};

#if !defined(VCL_VC70)
VCL_DEFINE_SPECIALIZATION
void vnl_sparse_matrix_linear_system<double>::get_rhs(vnl_vector<double>& b) const;
VCL_DEFINE_SPECIALIZATION
void vnl_sparse_matrix_linear_system<double>::transpose_multiply(vnl_vector<double> const& b, vnl_vector<double> & x) const;
VCL_DEFINE_SPECIALIZATION
void vnl_sparse_matrix_linear_system<float>::get_rhs(vnl_vector<double>& b) const;
VCL_DEFINE_SPECIALIZATION
void vnl_sparse_matrix_linear_system<float>::transpose_multiply(vnl_vector<double> const& b, vnl_vector<double> & x) const;
VCL_DEFINE_SPECIALIZATION
void vnl_sparse_matrix_linear_system<double>::multiply(vnl_vector<double> const& x, vnl_vector<double> & b) const;
VCL_DEFINE_SPECIALIZATION
void vnl_sparse_matrix_linear_system<float>::multiply(vnl_vector<double> const& x, vnl_vector<double> & b) const;
#endif

#endif // vnl_sparse_matrix_linear_system_h_

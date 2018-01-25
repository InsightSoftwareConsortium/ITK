// This is core/vnl/algo/vnl_sparse_lu.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
#include <iostream>
#include "vnl_sparse_lu.h"
#include <vcl_cassert.h>
#include <vcl_compiler.h>

#include <sparse/spMatrix.h>

// destructor - undo the spCreate() from the constructor(s)
// (memory leak fix of 7 Feb. 2008 by Toon Huysmans)
vnl_sparse_lu::~vnl_sparse_lu()
{
  spDestroy( pmatrix_ );
}

//: constructor - controls if condition information is computed
vnl_sparse_lu::vnl_sparse_lu(vnl_sparse_matrix<double> const & M, operation mode):
  A_(M), factored_(false),condition_computed_(false), mode_(mode),norm_(0), rcond_(0), largest_(0), pivot_thresh_(0),absolute_thresh_(0),diag_pivoting_(1),pmatrix_(VXL_NULLPTR)
{
  int n = (int)M.columns();
  assert(n == (int)(M.rows()));
  int error = 0;
  pmatrix_ = spCreate(n, 0, &error);
  if (error!=spOKAY)
  {
    std::cout << "In vnl_sparse_lu::vnl_sparse_lu - error in creating matrix\n";
    return;
  }
  // fill the internal sparse matrix from A_
  spElement* pelement = VXL_NULLPTR;
  for (A_.reset(); A_.next();)
  {
    int r = A_.getrow();
    int c = A_.getcolumn();
    double v = A_.value();
    pelement = spGetElement(pmatrix_, r+1, c+1);
    if (pelement == VXL_NULLPTR)
    {
      std::cout<< "In vnl_sparse_lu::vnl_sparse_lu - error in getting element\n";
      return;
    }
    *pelement = v;
  }
  if (mode==estimate_condition || mode_==estimate_condition_verbose)
  {
    largest_ = spLargestElement(pmatrix_);
    if (mode_==estimate_condition_verbose)
      std::cout << " Largest element in matrix = " << largest_ << '\n';
    norm_ = spNorm(pmatrix_);
  }
}

//: estimate the condition number.
bool vnl_sparse_lu::est_condition()
{
  int error = spOKAY;
  rcond_ = spCondition(pmatrix_, norm_, &error);
  if (error!=spOKAY)
  {
    std::cout << "In vnl_sparse_lu::est_condition(..) - error in condition number calculation\n";
    return false;
  }
  condition_computed_ = true;
  return true;
}

//: Solve least squares problem M x = b.
void vnl_sparse_lu::solve(vnl_vector<double> const& b, vnl_vector<double>* x)
{
  if (!pmatrix_)
  {
    std::cout << "In vnl_sparse_lu::solve(..) - matrix not defined\n";
    return;
  }
  unsigned n = b.size();
  assert(n == A_.columns());
  spREAL* rhs = new spREAL[n+1];
  for (unsigned i = 0; i<n; ++i)
    rhs[i+1]=b[i];
  if (mode_==verbose || mode_==estimate_condition_verbose)
  {
    std::cout << "Matrix before ordering\n";
    spPrint(pmatrix_,1,1,1);
  }

  int error = 0;
  //check if the matrix needs ordering
  //if not, run the decomposition
  if (!factored_)
  {
    error = spOrderAndFactor(pmatrix_, rhs, pivot_thresh_,
                             absolute_thresh_, diag_pivoting_);
    if (error != spOKAY)
    {
      std::cout << "In vnl_sparse_lu::solve(..) - error in factoring\n";
      return;
    }
    if (mode_==estimate_condition || mode_==estimate_condition_verbose)
      if (!est_condition())
        return;
    factored_ = true;
  }

  if (mode_==verbose || mode_==estimate_condition_verbose)
  {
    std::cout << "Matrix after ordering\n";
    spPrint(pmatrix_,1,1,1);
  }

  spSolve(pmatrix_, rhs, rhs);

  for (unsigned i = 0; i<n; ++i)
    (*x)[i]=rhs[i+1];

  delete [] rhs;
}

//: Solve least squares problem M x = b.
vnl_vector<double> vnl_sparse_lu::solve(vnl_vector<double> const& b)
{
  vnl_vector<double> ret(b.size());
  this->solve(b, &ret);
  return ret;
}

//: Solve problem M^t x = b
void vnl_sparse_lu::solve_transpose(vnl_vector<double> const& b, vnl_vector<double>* x)
{
  if (!pmatrix_)
  {
    std::cout << "In vnl_sparse_lu::solve(..) - matrix not defined\n";
    return;
  }
  unsigned n = b.size();
  assert(n == A_.columns());
  spREAL* rhs = new spREAL[n+1];
  for (unsigned i = 0; i<n; ++i)
    rhs[i+1]=b[i];
  int error = 0;
  if (mode_== verbose || mode_== estimate_condition_verbose)
  {
    std::cout << "Matrix before ordering\n";
    spPrint(pmatrix_,1,1,1);
  }

  //check if the matrix needs ordering
  //if not, run the decomposition
  if (!factored_)
  {
    error = spOrderAndFactor(pmatrix_, rhs, pivot_thresh_,
                             absolute_thresh_, diag_pivoting_);
    if (error != spOKAY)
    {
      std::cout << "In vnl_sparse_lu::solve(..) - error in factoring\n";
      return;
    }
    if (mode_==estimate_condition || mode_==estimate_condition_verbose)
      if (!est_condition())
        return;
    factored_ = true;
  }

  if (mode_==verbose || mode_== estimate_condition_verbose)
  {
    std::cout << "Matrix after ordering\n";
    spPrint(pmatrix_,1,1,1);
  }

  spSolveTransposed(pmatrix_, rhs, rhs);

  for (unsigned i = 0; i<n; ++i)
    (*x)[i]=rhs[i+1];

  delete [] rhs;
}

//: Solve problem M^t x = b
vnl_vector<double> vnl_sparse_lu::solve_transpose(vnl_vector<double> const& b)
{
  vnl_vector<double> ret(b.size());
  this->solve_transpose(b, &ret);
  return ret;
}

//: Compute determinant.
double vnl_sparse_lu::determinant()
{
  int exponent;
  double determ;
  if (!factored_)
  {
    spFactor(pmatrix_);
    if (mode_==estimate_condition || mode_==estimate_condition_verbose)
      if (!est_condition())
        return 0;
    factored_ = true;
  }
  spDeterminant(pmatrix_, &exponent, &determ);
  if (exponent<0)
  {
    while (exponent<0)
    {
      determ *= 0.1;
      exponent++;
    }
    return determ;
  }
  else if (exponent>0)
    while (exponent>0)
    {
      determ *= 10.0;
      exponent--;
    }

  return determ;
}

//: the reciprocal of the condition number
double vnl_sparse_lu::rcond()
{
  if (!factored_)
  {
    spFactor(pmatrix_);
    if (mode_==estimate_condition || mode_==estimate_condition_verbose)
      if (!est_condition())
        return 0;
    factored_ = true;
  }
  return rcond_;
}

//:Estimated upper bound of error in solution
double vnl_sparse_lu::max_error_bound()
{
  if (mode_!=estimate_condition && mode_ != estimate_condition_verbose)
    return 0;

  if (!factored_)
  {
    spFactor(pmatrix_);
    if (!est_condition())
      return 0;
    factored_ = true;
  }
  double roundoff = spRoundoff(pmatrix_, largest_);
  if (rcond_>0)
    return roundoff/rcond_;
  return 0;
}

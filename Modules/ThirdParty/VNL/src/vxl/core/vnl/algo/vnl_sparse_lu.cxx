// This is core/vnl/algo/vnl_sparse_lu.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
#include "vnl_sparse_lu.h"
#include <vcl_cassert.h>
#include <vcl_iostream.h>

// destructor - undo the spCreate() from the constructor(s)
// (memory leak fix of 7 Feb. 2008 by Toon Huysmans)
vnl_sparse_lu::~vnl_sparse_lu()
{
  spDestroy( pmatrix_ );
}

//: constructor - controls if condition information is computed
vnl_sparse_lu::vnl_sparse_lu(vnl_sparse_matrix<double> const & M, operation mode):
  A_(M), factored_(false),condition_computed_(false), mode_(mode),norm_(0), rcond_(0), largest_(0), pivot_thresh_(0),absolute_thresh_(0),diag_pivoting_(1),pmatrix_(0)
{
  int n = (int)M.columns();
  assert(n == (int)(M.rows()));
  int error = 0;
  pmatrix_ = spCreate(n, 0, &error);
  if (error!=spOKAY)
  {
    vcl_cout << "In vnl_sparse_lu::vnl_sparse_lu - error in creating matrix\n";
    return;
  }
  // fill the internal sparse matrix from A_
  spElement* pelement = 0;
  for (A_.reset(); A_.next();)
  {
    int r = A_.getrow();
    int c = A_.getcolumn();
    double v = A_.value();
    pelement = spGetElement(pmatrix_, r+1, c+1);
    if (pelement == 0)
    {
      vcl_cout<< "In vnl_sparse_lu::vnl_sparse_lu - error in getting element\n";
      return;
    }
    *pelement = v;
  }
  if (mode==estimate_condition || mode_==estimate_condition_verbose)
  {
    largest_ = spLargestElement(pmatrix_);
    if (mode_==estimate_condition_verbose)
      vcl_cout << " Largest element in matrix = " << largest_ << '\n';
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
    vcl_cout << "In vnl_sparse_lu::est_condition(..) - error in condition number calculation\n";
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
    vcl_cout << "In vnl_sparse_lu::solve(..) - matrix not defined\n";
    return;
  }
  unsigned n = b.size();
  assert(n == A_.columns());
  spREAL* rhs = new spREAL[n+1];
  for (unsigned i = 0; i<n; ++i)
    rhs[i+1]=b[i];
  if (mode_==verbose || mode_==estimate_condition_verbose)
  {
    vcl_cout << "Matrix before ordering\n";
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
      vcl_cout << "In vnl_sparse_lu::solve(..) - error in factoring\n";
      return;
    }
    if (mode_==estimate_condition || mode_==estimate_condition_verbose)
      if (!est_condition())
        return;
    factored_ = true;
  }

  if (mode_==verbose || mode_==estimate_condition_verbose)
  {
    vcl_cout << "Matrix after ordering\n";
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
    vcl_cout << "In vnl_sparse_lu::solve(..) - matrix not defined\n";
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
    vcl_cout << "Matrix before ordering\n";
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
      vcl_cout << "In vnl_sparse_lu::solve(..) - error in factoring\n";
      return;
    }
    if (mode_==estimate_condition || mode_==estimate_condition_verbose)
      if (!est_condition())
        return;
    factored_ = true;
  }

  if (mode_==verbose || mode_== estimate_condition_verbose)
  {
    vcl_cout << "Matrix after ordering\n";
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

// This routine might be eventually useful if other operations are to be done
// after the factoring. Note that the routine spRowColOrder was
// added to the sparse library (in spoutput.c).
#if 0
//: copy the matrix into a vnl_sparse_matrix
vnl_sparse_matrix<double> vnl_sparse_lu::lu_matrix()
{
  unsigned n = A_.rows();
  vnl_sparse_matrix<double> temp(n, n);
  int error = 0;
  spElement* pelement = 0;
  if (!factored_)
  {
    error = spFactor(pmatrix_);
    if (error != spOKAY)
    {
      vcl_cout << "In vnl_sparse_lu::lu_matrix() - factoring failed\n";
      return temp;
    }
    factored_=true;
  }
  // get the row and column maps
  int* row_map = new int[n+1];
  int* col_map = new int[n+1];
  spRowColOrder(pmatrix_, row_map, col_map);

  // create inverse maps
  int* inv_row_map = new int[n+1];
  int* inv_col_map = new int[n+1];
  for (unsigned i = 1; i<=n; ++i)
  {
    inv_row_map[row_map[i]]=i;
    inv_col_map[col_map[i]]=i;
  }

  if (mode_==verbose || mode_==estimate_condition_verbose)
    for (unsigned i = 1; i<=n; ++i)
      vcl_cout << "inv_row_map[" << i << "]= " << inv_row_map[i]
               << "    inv_col_map[" << i << "]= " << inv_col_map[i] << '\n';
  for (unsigned r = 0; r<n; r++)
    for (unsigned c = 0; c<n; c++)
    {
      pelement = spGetElement(pmatrix_, inv_row_map[r+1], inv_col_map[c+1]);
      if (pelement)
      {
        double v = *pelement;
        temp(r,c)= v;
      }
    }
  delete [] row_map;
  delete [] col_map;
  return temp;
}
#endif

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

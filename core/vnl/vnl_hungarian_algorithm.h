#ifndef vnl_hungarian_algorithm_h_
#define vnl_hungarian_algorithm_h_
//:
// \file
// \author Amitha Perera
// \date   Sep 2004
// Refactored by Nicolas Rannou (Harvard), Oct 2010, such that the algorithm
// presents itself in a templated and more object-oriented manner.

#include <vector>
#include <vcl_compiler.h>
#include <vnl/vnl_matrix.h>
#include "vnl/vnl_export.h"

//: Find the best column to row assignment given a cost matrix.
//
// This is an implementation of the Hungarian algorithm (also known
// as the Munkres algorithm). It finds the minimum cost assignment of
// the rows of the cost matrix \a cost (workers) to the columns
// (jobs).
//
// \param cost An N x M cost matrix. The costs cannot be -Infinity.
//
// \returns A vector v of size N such that v[i] = j means that row i
// should be assigned to column j. \code v[i] = -1u \endcode (= \code
// unsigned(-1) \endcode ) means that row i was not assigned to any
// column. If N \> M, then every column will be assigned to some
// row. If N \< M then every row will be assigned to some column.
//
//  \relatesalso vnl_matrix
template <class T>
class VNL_TEMPLATE_EXPORT vnl_hungarian_algorithm
{
 public:

  // The steps of the algorithm described below are taken from
  // http://www.cs.duke.edu/brd/Teaching/Bio/asmb/current/Handouts/munkres.html
  enum STEP_TYPE {
    STEP_0 = 0,
    STEP_1,
    STEP_2,
    STEP_3,
    STEP_4,
    STEP_5,
    STEP_6,
    STEP_done
  };

  enum STATE_TYPE {
    NORMAL=0,
    STAR,
    PRIME
  };

  typedef vnl_matrix< int >    AssignmentMatrixType;
  typedef std::vector<unsigned> AssignmentVectorType;

  vnl_hungarian_algorithm() : m_TotalCost(0) {}

  ~vnl_hungarian_algorithm() {}

  //: This constructor (and the following cast operator) is provided for backward compatibility with the original function implementation
  vnl_hungarian_algorithm(vnl_matrix<T> const& cost) { SetCostMatrix(cost); StartAssignment(); }
  operator std::vector<unsigned>() { return GetAssignmentVector(); }

  //: Starts the assignment
  void SetCostMatrix( vnl_matrix< T > const& );

  //: Starts the assignment
  void StartAssignment();

  //: Returns the total cost of the assignment
  T GetTotalCost();

  //: Returns the assignment matrix
  AssignmentMatrixType GetAssignmentMatrix();

  //: Returns the assignment vector
  AssignmentVectorType  GetAssignmentVector();

 protected:
  //: Step 0 - Make sure there are at least as many rows as columns in the cost matrix
  //  The nxm cost matrix is a matrix in which each element represents
  //  the cost of assigning one of n workers to one of m jobs.
  //  \returns the next step to go to (which is Step 1).
  STEP_TYPE   Step_0();

  //: Step 1 - For each row of the matrix, find the smallest element and subtract it from every element in its row.
  //  \returns the next step to go to (which is Step 2).
  STEP_TYPE   Step_1();

  //: Step 2 - Find a zero (Z) in the resulting matrix.
  //  If there is no starred zero in its row or column, star Z.
  //  Repeat for each element in the matrix.
  //  \returns the next step to go to (which is Step 3)
  STEP_TYPE   Step_2();

  //: Step 3 - Cover each column containing a starred zero.
  //  If K columns are covered, the starred zeros describe a complete
  //  set of unique assignments.
  //  In this case, Go to DONE, otherwise, Go to Step 4.
  //  \returns the next step to go to
  STEP_TYPE   Step_3();

  //: Step 4: Find a noncovered zero and prime it.
  //  If there is no starred zero in the row containing this primed zero, Go to Step 5.
  //  Otherwise, cover this row and uncover the column containing the starred
  //  zero. Continue in this manner until there are no uncovered zeros left.
  //  Save the smallest uncovered value and Go to Step 6.
  //  \returns the next step to go to
  STEP_TYPE   Step_4();

  //: Step 5 - Construct a series of alternating primed and starred zeros as follows.
  //  Let Z0 represent the uncovered primed zero found in Step 4.
  //  Let Z1 denote the starred zero in the column of Z0 (if any).
  //  Let Z2 denote the primed zero in the row of Z1 (there will always be
  //  one).  Continue until the series terminates at a primed zero that has
  //  no starred zero in its column.  Unstar each starred zero of the series,
  //  star each primed zero of the series, erase all primes and uncover every
  //  line in the matrix. Return to Step 3.
  //  \returns the next step to go to
  STEP_TYPE   Step_5();

  //: Step 6 -
  //  Add the value found in Step 4 to every element of each covered row,
  //  and subtract it from every element of each uncovered column.
  //  Return to Step 4 without altering any stars, primes, or covered lines.
  //  \returns the next step to go to
  STEP_TYPE   Step_6();

  //: Step done - Returns a vector containing the result of the assignment.
  //  Assignment pairs are indicated by the positions of the
  //  starred zeros in the cost matrix.  If C(i,j) is a starred zero,
  //  then the element associated with row i is assigned to the element
  //  associated with column j.
  void        Step_done();

  //: Sets all the values in the input bool vector to false.
  void clear_vector( std::vector<bool>& );

  vnl_matrix<T> m_Cost;
  vnl_matrix<T> m_Cost_in;

  //: Size of the input matrix
  unsigned m_N;

  //: Row and col of the primed zero in step four to pass to step five.
  unsigned m_Z0_r, m_Z0_c;

  //:
  //   m_M(i,j) = 1   =>  m_Cost(i,j) is starred
  //   m_M(i,j) = 2   =>  m_Cost(i,j) is primed
  AssignmentMatrixType m_M;

  //: m_R_cov[i] = true  => row i is covered
  std::vector<bool> m_R_cov;

  //: m_C_cov[j] = true  => column j is covered
  std::vector<bool> m_C_cov;

  //: Total cost of the assignment
  T m_TotalCost;

  //: m_C_cov[j] = true  => column j is covered
  AssignmentVectorType m_AssignmentVector;
};

#define VNL_HUNGARIAN_ALGORITHM_INSTANTIATE(T) extern "please #include vnl/vnl_hungarian_algorithm.hxx instead"

#endif // vnl_hungarian_algorithm_h_

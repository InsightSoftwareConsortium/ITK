#ifndef vnl_hungarian_algorithm_hxx_
#define vnl_hungarian_algorithm_hxx_

#include <vector>
#include <limits>
#include <algorithm>
#include <iostream>
#include "vnl_hungarian_algorithm.h"

#include <vnl/vnl_matrix.h>
#include <vcl_compiler.h>
#include <vcl_cassert.h>

#ifdef DEBUG
#endif

//-----------------------------------------------------------------------------
// Set Cost Matrix
//-----------------------------------------------------------------------------
template<class T>
void
vnl_hungarian_algorithm<T>::
SetCostMatrix( vnl_matrix<T> const& cost_in )
{
  m_Cost_in = cost_in;
  m_TotalCost = 0;

  // The Hungarian algorithm (seems to) only work for NxN cost
  // matrices. We can solve the NxM case by padding the matrix with a
  // constant cost.

  // Get Max size of the matrix
  m_N = std::max( cost_in.rows(), cost_in.cols() );

  m_Cost.set_size( m_N, m_N);
  m_Cost.fill( static_cast<T>(0) );

  // Initialize cost matrix
  // Update the cost matrix ()
  m_Cost.update( cost_in, 0, 0 );
}

//-----------------------------------------------------------------------------
// Clear vector
//-----------------------------------------------------------------------------
template <class T>
void
vnl_hungarian_algorithm<T>::
clear_vector( std::vector<bool>& v )
{
  typedef std::vector<bool>::iterator iter;
  iter end = v.end();
  for ( iter i = v.begin(); i != end; ++i )
  {
    *i = false;
  }
}

//-----------------------------------------------------------------------------
// Run Algorithm
//-----------------------------------------------------------------------------
template <class T>
void
vnl_hungarian_algorithm<T>::
StartAssignment()
{
  // Step 0
  // Make sure there are at least as many rows as columns
  // [ This seems to be misleading since the algorithm presented here
  // seems to only work for NxN matrices.]

  Step_0();

  // Step 1:
  // For each row of the matrix, find the smallest element and subtract
  // it from every element in its row.  Go to Step 2.

  Step_1();

  // Step 2:
  // Find a zero (Z) in the resulting matrix.  If there is no starred
  // zero in its row or column, star Z. Repeat for each element in the
  // matrix. Go to Step 3.

  Step_2();

  STEP_TYPE step = STEP_3;

  while ( step != STEP_done )
  {
    switch ( step )
    {
      case STEP_3 :
      // Step 3: Cover each column containing a starred zero.  If K
      // columns are covered, the starred zeros describe a complete set of
      // unique assignments.  In this case, Go to DONE, otherwise, Go to
      // Step 4.
        step = Step_3();
        break;

      case STEP_4 :
      // Step 4: Find a noncovered zero and prime it.  If there is no
      // starred zero in the row containing this primed zero, Go to Step
      // 5.  Otherwise, cover this row and uncover the column containing
      // the starred zero. Continue in this manner until there are no
      // uncovered zeros left. Save the smallest uncovered value and Go to
      // Step 6.
        step = Step_4();
        break;

      case STEP_5 :
      // Step 5: Construct a series of alternating primed and starred
      // zeros as follows.  Let Z0 represent the uncovered primed zero
      // found in Step 4.  Let Z1 denote the starred zero in the column of
      // Z0 (if any). Let Z2 denote the primed zero in the row of Z1
      // (there will always be one).  Continue until the series terminates
      // at a primed zero that has no starred zero in its column.  Unstar
      // each starred zero of the series, star each primed zero of the
      // series, erase all primes and uncover every line in the matrix.
      // Return to Step 3.
        step = Step_5();
        break;

      case STEP_6 :
      // Step 6: Add the value found in Step 4 to every element of each
      // covered row, and subtract it from every element of each uncovered
      // column.  Return to Step 4 without altering any stars, primes, or
      // covered
        step = Step_6();
        break;

      default :
        step = STEP_done;
        break;
    }
  }

  // DONE: Assignment pairs are indicated by the positions of the
  // starred zeros in the cost matrix.  If C(i,j) is a starred zero,
  // then the element associated with row i is assigned to the element
  // associated with column j.

  Step_done();
}

//-----------------------------------------------------------------------------
// Step 0
// Make sure there are at least as many rows as columns
// [ This seems to be misleading since the algorithm presented here
// seems to only work for NxN matrices.]
//-----------------------------------------------------------------------------
template <class T>
typename vnl_hungarian_algorithm<T>::STEP_TYPE
vnl_hungarian_algorithm<T>::Step_0()
{
  // M(i,j) = NORMAL   =>  cost(i,j) is normal
  // M(i,j) = STAR     =>  cost(i,j) is starred
  // M(i,j) = PRIME    =>  cost(i,j) is primed

  m_M.set_size( m_N, m_N);
  m_M.fill( NORMAL );

  // R_cov[i] = true  => row i is covered
  // C_cov[j] = true  => column j is covered

  m_R_cov.assign(m_N, false);
  m_C_cov.assign(m_N, false);
  return STEP_1;
}

//-----------------------------------------------------------------------------
// Step 1:
// For each row of the matrix, find the smallest element and subtract
// it from every element in its row.  Go to Step 2.
//-----------------------------------------------------------------------------
template <class T>
typename vnl_hungarian_algorithm<T>::STEP_TYPE
vnl_hungarian_algorithm<T>::Step_1()
{
//creer j a l'exterieur

  for ( unsigned i = 0; i < m_N; ++i )
  {
    T mn = m_Cost(i,0);
    for ( unsigned j = 1; j < m_N; ++j )
    {
      if ( mn > m_Cost(i,j) )
      {
        mn = m_Cost(i,j);
      }
    }
    for ( unsigned j = 0; j < m_N; ++j )
    {
      m_Cost(i,j) -= mn;
    }
  }
  return STEP_2;
}


//-----------------------------------------------------------------------------
// Step 2:
// Find a zero (Z) in the resulting matrix.  If there is no starred
// zero in its row or column, star Z. Repeat for each element in the
// matrix. Go to Step 3.
//-----------------------------------------------------------------------------
template <class T>
typename vnl_hungarian_algorithm<T>::STEP_TYPE
vnl_hungarian_algorithm<T>::Step_2()
{
  // We'll use C_cov and R_cov to indicate if there is a starred
  // zero in that column or row, respectively
  for ( unsigned i = 0; i < m_N; ++i )
  {
    if ( ! m_R_cov[i] )
    {
      for ( unsigned j = 0; j < m_N; ++j )
      {
        if ( m_Cost(i,j) == 0 && ! m_C_cov[j] )
        {
          m_M(i,j) = STAR; // star it
          m_R_cov[i] = true; // and update the row & col status.
          m_C_cov[j] = true;
          break; // the row is now starred. Don't look at the rest.
        }
      }
    }
  }
  clear_vector( m_R_cov );
  clear_vector( m_C_cov );
  return STEP_3;
}

//-----------------------------------------------------------------------------
// Step 3: Cover each column containing a starred zero.  If K
// columns are covered, the starred zeros describe a complete set of
// unique assignments.  In this case, Go to DONE, otherwise, Go to
// Step 4.
//-----------------------------------------------------------------------------
template <class T>
typename vnl_hungarian_algorithm<T>::STEP_TYPE
vnl_hungarian_algorithm<T>::Step_3()
{
  unsigned count = 0;
  for ( unsigned j = 0; j < m_N; ++j )
  {
    for ( unsigned i = 0; i < m_N; ++i )
    {
      if ( m_M(i,j) == STAR )
      {
        m_C_cov[j] = true;
        ++count;
        break; // to the next column
      }
    }
  }

  if ( count == m_N )
  {
    return STEP_done;
  }
  else
  {
    return STEP_4;
  }
}

//-----------------------------------------------------------------------------
// Step 4: Find a noncovered zero and prime it.  If there is no
// starred zero in the row containing this primed zero, Go to Step
// 5.  Otherwise, cover this row and uncover the column containing
// the starred zero. Continue in this manner until there are no
// uncovered zeros left. Save the smallest uncovered value and Go to
// Step 6.
//-----------------------------------------------------------------------------
template <class T>
typename vnl_hungarian_algorithm<T>::STEP_TYPE
vnl_hungarian_algorithm<T>::Step_4()
{
  m_Z0_r = m_Z0_c = (unsigned int)(-1);
  // Find an uncovered zero
  // This loop will exit with a goto step_five or step_six.

    unsigned i, j; // row and column of the uncovered zero, if any.
    for (i = 0 ; i < m_N; ++i )
    {
      if ( ! m_R_cov[i] )
      {
        for ( j = 0; j < m_N; ++j )
        {
#ifdef DEBUG
          std::cout << m_Cost(i,j) << std::endl;
#endif
          if ( m_Cost(i,j) == 0 && ! m_C_cov[j] )
          {
            m_M(i,j) = PRIME; // prime it
            bool star_in_row = false;
            for ( unsigned j2 = 0; j2 < m_N; ++j2 )
            {
              if ( m_M(i,j2) == STAR )
              {
              star_in_row = true;
              // cover the row, uncover the star column
              m_R_cov[i] = true;
              m_C_cov[j2] = false;
              break; // out of searching for stars
            }
          }

          // If there isn't go to step 5
          if ( ! star_in_row )
          {
            m_Z0_r = i;
            m_Z0_c = j;
            return STEP_5;
          }
          return STEP_4;
        }
      }
    }
  }

  // We should find the smallest uncovered value, but it's more
  // convenient to find it when we get to step six. We only need
  // it there anyway.
  return STEP_6;
}

//-----------------------------------------------------------------------------
// Step 5: Construct a series of alternating primed and starred
// zeros as follows.  Let Z0 represent the uncovered primed zero
// found in Step 4.  Let Z1 denote the starred zero in the column of
// Z0 (if any). Let Z2 denote the primed zero in the row of Z1
// (there will always be one).  Continue until the series terminates
// at a primed zero that has no starred zero in its column.  Unstar
// each starred zero of the series, star each primed zero of the
// series, erase all primes and uncover every line in the matrix.
// Return to Step 3.
//-----------------------------------------------------------------------------
template <class T>
typename vnl_hungarian_algorithm<T>::STEP_TYPE
vnl_hungarian_algorithm<T>::Step_5()
{
  unsigned i = m_Z0_r;
  unsigned j = m_Z0_c;
  std::vector<unsigned> rows, cols;

  while ( true )
  {
    // This is the primed zero
    assert( m_M(i,j) == PRIME );
    rows.push_back( i );
    cols.push_back( j );

    // Look for a starred zero in this column
    for ( i = 0; i < m_N; ++i )
    {
      if ( m_M(i,j) == STAR )
      {
        break;
      }
    }

    if ( i == m_N )
    {
      // we didn't find a starred zero. Stop the loop
      break;
    }

    // This is the starred zero
    rows.push_back( i );
    cols.push_back( j );

    // Look for the primed zero in the row of the starred zero
    for ( j = 0; j < m_N; ++j )
    {
      if ( m_M(i,j) == PRIME )
      {
        break;
      }
    }
    assert( j < m_N ); // there should always be one

    // go back to the top to mark the primed zero, and repeat.
  }

  // Series has terminated. Unstar each star and star each prime in
  // the series.
  for ( unsigned idx = 0; idx < rows.size(); ++idx )
  {
    unsigned i = rows[idx];
    unsigned j = cols[idx];
    if ( m_M(i,j) == STAR )
    {
      m_M(i,j) = NORMAL; // unstar each starred zero
      }
    else
    {
      assert( m_M(i,j) == PRIME );
      m_M(i,j) = STAR; // star each primed zero
    }
  }

  // Erase all primes.
  for ( unsigned i = 0; i < m_N; ++i )
  {
    for ( unsigned j = 0; j < m_N; ++j )
    {
      if ( m_M(i,j) == PRIME )
      {
        m_M(i,j) = NORMAL;
      }
    }
  }

  // Uncover everything
  clear_vector( m_R_cov );
  clear_vector( m_C_cov );

  return STEP_3;
}

//-----------------------------------------------------------------------------
// Step 6: Add the value found in Step 4 to every element of each
// covered row, and subtract it from every element of each uncovered
// column.  Return to Step 4 without altering any stars, primes, or
// covered lines.
//-----------------------------------------------------------------------------
template <class T>
typename vnl_hungarian_algorithm<T>::STEP_TYPE
vnl_hungarian_algorithm<T>::Step_6()
{
  // The value found in step 4 is the smallest uncovered value. Find it now.
  T minval = std::numeric_limits<T>::max();
  for ( unsigned i = 0; i < m_N; ++i )
  {
    if ( ! m_R_cov[i] )
    {
      for ( unsigned j = 0; j < m_N; ++j )
      {
        if ( ! m_C_cov[j] && m_Cost(i,j) < minval )
        {
          minval = m_Cost(i,j);
        }
      }
    }
  }

  // Modify the matrix as instructed.
  for ( unsigned i = 0; i < m_N; ++i )
  {
    for ( unsigned j = 0; j < m_N; ++j )
    {
      if ( m_R_cov[i] )
      {
        m_Cost(i,j) += minval;
      }
      if ( ! m_C_cov[j] )
      {
        m_Cost(i,j) -= minval;
      }
    }
  }

  return STEP_4;
}


//-----------------------------------------------------------------------------
// DONE: Assignment pairs are indicated by the positions of the
// starred zeros in the cost matrix.  If C(i,j) is a starred zero,
// then the element associated with row i is assigned to the element
// associated with column j.
//-----------------------------------------------------------------------------
template <class T>
void
vnl_hungarian_algorithm<T>::Step_done()
{
  std::vector<unsigned> assign( m_Cost_in.rows(), (unsigned int)(-1) );
  m_AssignmentVector = assign;

  // Find the stars and generate the resulting assignment. Only
  // check the sub-matrix of cost that corresponds to the input cost
  // matrix. The remaining rows and columns are unassigned.
  for ( unsigned j = 0; j < m_Cost_in.cols(); ++j )
  {
    for ( unsigned i = 0; i < m_Cost_in.rows(); ++i )
    {
      if ( m_M(i,j) == STAR )
      {
        m_AssignmentVector[i] = j;
        m_TotalCost += m_Cost_in[i][j];
      }
    }
  }
}

//-----------------------------------------------------------------------------
// Returns the total cost of the assignment
//-----------------------------------------------------------------------------
template <class T>
T
vnl_hungarian_algorithm<T>::GetTotalCost()
{
  return m_TotalCost;
}

//-----------------------------------------------------------------------------
// Returns the assignment matrix
//-----------------------------------------------------------------------------
template <class T>
typename vnl_hungarian_algorithm<T>::AssignmentMatrixType
vnl_hungarian_algorithm<T>::GetAssignmentMatrix()
{
  return m_M;
}

//-----------------------------------------------------------------------------
// Returns the assignment vector
//-----------------------------------------------------------------------------
template <class T>
typename vnl_hungarian_algorithm<T>::AssignmentVectorType
vnl_hungarian_algorithm<T>::GetAssignmentVector()
{
  return m_AssignmentVector;
}

#undef VNL_HUNGARIAN_ALGORITHM_INSTANTIATE
#define VNL_HUNGARIAN_ALGORITHM_INSTANTIATE(T) \
template class VNL_EXPORT vnl_hungarian_algorithm<T >

#endif // vnl_hungarian_algorithm_hxx_

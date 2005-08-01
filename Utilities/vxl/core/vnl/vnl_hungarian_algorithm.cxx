#include "vnl_hungarian_algorithm.h"

#include <vcl_vector.h>
#include <vcl_limits.h>
#include <vcl_algorithm.h>
#include <vnl/vnl_matrix.h>
#include <vcl_cassert.h>

// set all the elements of v to false.
static void clear_vector( vcl_vector<bool>& v )
{
  typedef vcl_vector<bool>::iterator iter;
  iter end = v.end();
  for ( iter i = v.begin(); i != end; ++i ) {
    *i = false;
  }
}

vcl_vector<unsigned> vnl_hungarian_algorithm( vnl_matrix<double> const& cost_in )
{
  // The Hungarian algorithm (seems to) only work for NxN cost
  // matrices. We can solve the NxM case by padding the matrix with a
  // constant cost.

  unsigned const N = vcl_max( cost_in.rows(), cost_in.cols() );

  vnl_matrix<double> cost( N, N, 0 );

  // Copy in the pieces of the original matrix
  cost.update( cost_in, 0, 0 );

  // The steps of the algorithm described below are taken from
  // http://www.public.iastate.edu/~ddoty/HungarianAlgorithm.html

  // Step 0
  // Make sure there are at least as many rows as columns
  // [ This seems to be misleading since the algorithm presented here
  // seems to only work for NxN matrices.]

  // M(i,j) = 1   =>  cost(i,j) is starred
  // M(i,j) = 2   =>  cost(i,j) is primed
  vnl_matrix<int> M( N, N, 0 );

  // R_cov[i] = true  => row i is covered
  // C_cov[j] = true  => column j is covered
  vcl_vector<bool> R_cov( N, false );
  vcl_vector<bool> C_cov( N, false );

  // row and col of the primed zero in step four to pass to step five.
  unsigned Z0_r, Z0_c;

  // Step 1:
  // For each row of the matrix, find the smallest element and subtract
  // it from every element in its row.  Go to Step 2.
  {
    for ( unsigned i = 0; i < N; ++i ) {
      double mn = cost(i,0);
      for ( unsigned j = 1; j < N; ++j ) {
        if ( mn > cost(i,j) ) mn = cost(i,j);
      }
      for ( unsigned j = 0; j < N; ++j ) {
        cost(i,j) -= mn;
      }
    }

    // and on to step 2.
  }

  // Step 2:
  // Find a zero (Z) in the resulting matrix.  If there is no starred
  // zero in its row or column, star Z. Repeat for each element in the
  // matrix. Go to Step 3.
  {
    // We'll use C_cov and R_cov to indicate if there is a starred
    // zero in that column or row, respectively
    for ( unsigned i = 0; i < N; ++i ) {
      if ( ! R_cov[i] ) {
        for ( unsigned j = 0; j < N; ++j ) {
          if ( cost(i,j) == 0.0 && ! C_cov[j] ) {
            M(i,j) = 1; // star it
            R_cov[i] = true; // and update the row & col status.
            C_cov[j] = true;
            break; // the row is now starred. Don't look at the rest.
          }
        }
      }
    }
    clear_vector( R_cov );
    clear_vector( C_cov );

    // and on to step 3.
  }

  // Step 3: Cover each column containing a starred zero.  If K
  // columns are covered, the starred zeros describe a complete set of
  // unique assignments.  In this case, Go to DONE, otherwise, Go to
  // Step 4.
  step_three:
  {
    unsigned count = 0;
    for ( unsigned j = 0; j < N; ++j ) {
      for ( unsigned i = 0; i < N; ++i ) {
        if ( M(i,j) == 1 ) {
          C_cov[j] = true;
          ++count;
          break; // to the next column
        }
      }
    }
    if ( count == N )
      goto step_done;

    // otherwise, on to step 4.
  }

 step_four:
  // Step 4: Find a noncovered zero and prime it.  If there is no
  // starred zero in the row containing this primed zero, Go to Step
  // 5.  Otherwise, cover this row and uncover the column containing
  // the starred zero. Continue in this manner until there are no
  // uncovered zeros left. Save the smallest uncovered value and Go to
  // Step 6.
  Z0_r = -1u;
  Z0_c = -1u;
  // Find an uncovered zero
  // This loop will exit with a goto step_five or step_six.
  while ( true )
  {
    unsigned i, j; // row and column of the uncovered zero, if any.
    for (i = 0 ; i < N; ++i ) {
      if ( ! R_cov[i] ) {
        for ( j = 0; j < N; ++j ) {
          if ( cost(i,j) == 0.0 && ! C_cov[j] ) {
            M(i,j) = 2; // prime it
            goto exit_loop;
          }
        }
      }
    }
    // We should find the smallest uncovered value, but it's more
    // convenient to find it when we get to step six. We only need
    // it there anyway.
    goto step_six;

 exit_loop:
    // Check if there is a starred zero in the row.
    bool star_in_row = false;
    for ( unsigned j2 = 0; j2 < N; ++j2 ) {
      if ( M(i,j2) == 1 ) {
        star_in_row = true;
        // cover the row, uncover the star column
        R_cov[i] = true;
        C_cov[j2] = false;
        break; // out of searching for stars
      }
    }

    // If there isn't go to step 5
    if ( ! star_in_row ) {
      Z0_r = i;
      Z0_c = j;
      break; // out of while loop and go to step 5
    }
  } // go back to find more uncovered zeros

  // Step 5: Construct a series of alternating primed and starred
  // zeros as follows.  Let Z0 represent the uncovered primed zero
  // found in Step 4.  Let Z1 denote the starred zero in the column of
  // Z0 (if any). Let Z2 denote the primed zero in the row of Z1
  // (there will always be one).  Continue until the series terminates
  // at a primed zero that has no starred zero in its column.  Unstar
  // each starred zero of the series, star each primed zero of the
  // series, erase all primes and uncover every line in the matrix.
  // Return to Step 3.
  {
    unsigned i = Z0_r;
    unsigned j = Z0_c;
    vcl_vector<unsigned> rows, cols;
    while ( true )
    {
      // This is the primed zero
      assert( M(i,j) == 2 );
      rows.push_back( i );
      cols.push_back( j );

      // Look for a starred zero in this column
      for ( i = 0; i < N; ++i ) {
        if ( M(i,j) == 1 )  break;
      }

      if ( i == N ) {
        // we didn't find a starred zero. Stop the loop
        break;
      }

      // This is the starred zero
      rows.push_back( i );
      cols.push_back( j );

      // Look for the primed zero in the row of the starred zero
      for ( j = 0; j < N; ++j ) {
        if ( M(i,j) == 2 )  break;
      }
      assert( j < N ); // there should always be one

      // go back to the top to mark the primed zero, and repeat.
    }

    // Series has terminated. Unstar each star and star each prime in
    // the series.
    for ( unsigned idx = 0; idx < rows.size(); ++idx ) {
      unsigned i = rows[idx];
      unsigned j = cols[idx];
      if ( M(i,j) == 1 ) {
        M(i,j) = 0; // unstar each starred zero
      } else {
        assert( M(i,j) == 2 );
        M(i,j) = 1; // star each primed zero
      }
    }

    // Erase all primes.
    for ( unsigned i = 0; i < N; ++i ) {
      for ( unsigned j = 0; j < N; ++j ) {
        if ( M(i,j) == 2 )  M(i,j) = 0;
      }
    }

    // Uncover everything
    clear_vector( R_cov );
    clear_vector( C_cov );

    goto step_three;
  }

  // Step 6: Add the value found in Step 4 to every element of each
  // covered row, and subtract it from every element of each uncovered
  // column.  Return to Step 4 without altering any stars, primes, or
  // covered lines.
  step_six:
  {
    // The value found in step 4 is the smallest uncovered value. Find it now.
    double minval = vcl_numeric_limits<double>::infinity();
    for ( unsigned i = 0; i < N; ++i ) {
      if ( ! R_cov[i] ) {
        for ( unsigned j = 0; j < N; ++j ) {
          if ( ! C_cov[j] && cost(i,j) < minval ) {
            minval = cost(i,j);
          }
        }
      }
    }

    // Modify the matrix as instructed.
    for ( unsigned i = 0; i < N; ++i ) {
      for ( unsigned j = 0; j < N; ++j ) {
        if ( R_cov[i] )    cost(i,j) += minval;
        if ( ! C_cov[j] )  cost(i,j) -= minval;
      }
    }

    goto step_four;
  }

  // DONE: Assignment pairs are indicated by the positions of the
  // starred zeros in the cost matrix.  If C(i,j) is a starred zero,
  // then the element associated with row i is assigned to the element
  // associated with column j.
  step_done:
  {
    vcl_vector<unsigned> assign( cost_in.rows(), -1u );

    // Find the stars and generate the resulting assignment. Only
    // check the sub-matrix of cost that corresponds to the input cost
    // matrix. The remaining rows and columns are unassigned.
    for ( unsigned j = 0; j < cost_in.cols(); ++j ) {
      for ( unsigned i = 0; i < cost_in.rows(); ++i ) {
        if ( M(i,j) == 1 ) {
          assign[i] = j;
        }
      }
    }

    return assign;
  }
}

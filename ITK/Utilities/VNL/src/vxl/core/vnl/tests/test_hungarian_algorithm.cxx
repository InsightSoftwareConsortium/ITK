#include <vnl/vnl_hungarian_algorithm.h>
#include <testlib/testlib_test.h>

#include <vcl_iostream.h>
#include <vcl_limits.h>
#include <vcl_algorithm.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_random.h>

static vnl_random randgen;

static
void check_solution( vcl_vector<unsigned> const& assign,
                     unsigned const* solution, unsigned const N )
{
  TEST( "  assignment vector size", assign.size(), N );
  bool okay = true;
  vcl_cout << "  assignment:\n";
  for ( unsigned i = 0; i < N; ++i ) {
    if ( assign[i] != unsigned(-1) || solution[i] != unsigned(-1) ) {
      vcl_cout << "    " << i << " -> " << assign[i]
               << " (expected " << solution[i] << ")\n";
      if ( assign[i] != solution[i] ) {
        vcl_cout << "        (mismatch)\n";
        okay = false;
      }
    }
  }
  TEST( "  assignment result", okay, true );
}


static
vcl_vector<unsigned> make_up_solution( unsigned const M, unsigned const N )
{
  // True solution
  vcl_vector<unsigned> true_assn( M );
  for ( unsigned i = 0; i < M; ++i ) {
    bool okay;
    do {
      true_assn[i] = randgen.lrand32( N );
      okay = true;
      for ( unsigned j = 0; j < i; ++j ) {
        if ( true_assn[j] == true_assn[i] ) {
          okay = false;
          break;
        }
      }
    } while ( ! okay );
  }

  return true_assn;
}

static void test_skewed_problem( unsigned const M, unsigned const N )
{
  vcl_cout << "Creating " << M << 'x' << N << " matrix" << vcl_endl;
  vnl_matrix<double> cost( M, N );
  double low = vcl_min(M,N) + 5.0;
  for ( unsigned i = 0; i < M; ++i ) {
    for ( unsigned j = 0; j < N; ++j ) {
      cost(i,j) = randgen.drand32( low, 100000.0 );
    }
  }

  vcl_vector<unsigned> true_assn;
  if ( M < N ) {
    true_assn = make_up_solution( M, N );
    for ( unsigned i = 0; i < M; ++i ) {
      cost(i, true_assn[i]) = i;
    }
  }
  else {
    vcl_vector<unsigned> transposed_assn = make_up_solution( N, M );
    true_assn.resize( M, unsigned(-1) );
    for ( unsigned j = 0; j < N; ++j ) {
      true_assn[ transposed_assn[j] ] = j;
      cost(transposed_assn[j],j) = j;
    }
  }

  vcl_cout << "Costs computed for " << M << 'x' << N << " matrix" << vcl_endl;

  vcl_vector<unsigned> assn = vnl_hungarian_algorithm( cost );

  check_solution( assn, &true_assn[0], M );
}


static
void run_test( vnl_matrix<double> const& cost, unsigned solution[] )
{
  {
    vcl_cout << "Test " << cost.rows() << 'x' << cost.cols()
             << " matrix" << vcl_endl;
    vcl_vector<unsigned> assign = vnl_hungarian_algorithm( cost );
    check_solution( assign, solution, cost.rows() );
  }

  {
    vcl_cout << "Test transposed problem" << vcl_endl;
    vnl_matrix<double> costT = cost.transpose();
    vcl_vector<unsigned> assign = vnl_hungarian_algorithm( costT );

    vcl_vector<unsigned> solutionT( costT.rows(), unsigned(-1) );
    for ( unsigned i = 0; i < cost.rows(); ++i ) {
      if ( solution[i] != unsigned(-1) ) {
        solutionT[ solution[i] ] = i;
      }
    }

    check_solution( assign, &solutionT[0], costT.rows() );
  }
}


static void test_hungarian_algorithm( int, char*[] )
{
  {
    double cost_val[3][3] = { { 1, 2, 3 },
                              { 2, 4, 6 },
                              { 3, 6, 9 } };

    vnl_matrix<double> cost( &cost_val[0][0], 3, 3 );

    vcl_vector<unsigned> assign = vnl_hungarian_algorithm( cost );
    TEST( "Test 3x3 cost matrix" , assign.size()==3 &&
          assign[0]==2 && assign[1]==1 && assign[2]==0, true);
  }

  {
    double cost_val[4][4] = { { 2.0, 1.0, 5.0, 3.0 },
                              { 0.5, 6.0, 3.0, 0.5 },
                              { 5.0, 2.0, 1.0, 6.0 },
                              { 7.0, 1.0, 3.0, 0.1 } };
    vnl_matrix<double> cost( &cost_val[0][0], 4, 4 );

    vcl_vector<unsigned> assign = vnl_hungarian_algorithm( cost );
    TEST( "Test 4x4 cost matrix" , assign.size()==4 &&
          assign[0]==1 && assign[1]==0 && assign[2]==2 && assign[3]==3, true);
  }

  {
    double cost_val[3][4] = { { 2.0, 1.0, 5.0, 3.0 },
                              { 0.5, 6.0, 3.0, 0.5 },
                              { 7.0, 1.0, 3.0, 0.1 } };
    vnl_matrix<double> cost( &cost_val[0][0], 3, 4 );
    unsigned solution[] = { 1, 0, 3 };
    run_test( cost, solution );
  }

  {
    // test where the greedy solution is not the optimal
    vcl_cout << "\n\nTest when greedy != optimal\n";
    double cost_val[3][4] = { { 2.0, 1.0, 5.0, 3.0 },
                              { 0.5, 0.2, 3.0, 0.5 },
                              { 7.0, 1.0, 3.0, 0.1 } };
    vnl_matrix<double> cost( &cost_val[0][0], 3, 4 );
    unsigned solution[] = { 1, 0, 3 };
    run_test( cost, solution );
  }

  {
    // a white box test where the row-by-row minimum is not the
    // solution
    vcl_cout << "\n\nTest when row-by-row min != optimal\n";
    double cost_val[3][4] = { { 2.0, 1.0, 5.0, 3.0 },
                              { 0.5, 6.0, 3.0, 0.5 },
                              { 0.1, 1.0, 3.0, 0.2 } };
    vnl_matrix<double> cost( &cost_val[0][0], 3, 4 );
    unsigned solution[] = { 1, 3, 0 };
    run_test( cost, solution );
  }

  {
    double cost_val[5][3] = { { 2.0, 0.5, 7.0 },
                              { 1.1, 6.0, 1.0 },
                              { 1.0, 2.0, 1.0 },
                              { 5.0, 3.0, 3.0 },
                              { 3.0, 0.5, 0.1 } };

    vnl_matrix<double> cost( &cost_val[0][0], 5, 3 );
    unsigned solution[] = { 1, unsigned(-1), 0, unsigned(-1), 2 };
    run_test( cost, solution );
  }

  double Inf = vcl_numeric_limits<double>::infinity();

  {
    vcl_cout << "\n\nTest with Inf\n";
    double cost_val[5][3] = { { 2.0, 0.5, 7.0 },
                              { 1.1, 6.0, 1.0 },
                              { 1.0, 2.0, 1.0 },
                              { Inf, 3.0, 3.0 },
                              { 3.0, 0.5, 0.1 } };

    vnl_matrix<double> cost( &cost_val[0][0], 5, 3 );
    unsigned solution[] = { 1, unsigned(-1), 0, unsigned(-1), 2 };
    run_test( cost, solution );
  }

  {
    vcl_cout << "\n\nTest with Inf, greedy not optimal\n";
    double cost_val[5][3] = { { 2.0, 0.5, 7.0 },
                              { 1.1, 6.0, 1.0 },
                              { 1.0, 2.0, 1.0 },
                              { Inf, 3.0, 3.0 },
                              { 3.0, 0.5, 0.1 } };

    vnl_matrix<double> cost( &cost_val[0][0], 5, 3 );
    unsigned solution[] = { 1, unsigned(-1), 0, unsigned(-1), 2 };
    run_test( cost, solution );
  }


  // Verify that an O(mn) problem with m<<n does not explode into a
  // O(n^2) problem.
  {
    vcl_cout << "\n\nTest that O(N) is doesn't become O(N^2)\n";
    // MN ~= 800 KB, N^2 ~= 20 GB
    test_skewed_problem( 2, 50000 );
    test_skewed_problem( 50000, 2 );
  }
}

TESTMAIN_ARGS( test_hungarian_algorithm )

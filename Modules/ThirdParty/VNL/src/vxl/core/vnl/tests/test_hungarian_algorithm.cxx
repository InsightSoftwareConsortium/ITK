#include <iostream>
#include <limits>
#include <algorithm>
#include <vnl/vnl_hungarian_algorithm.h>
#include <testlib/testlib_test.h>

#include <vcl_compiler.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_random.h>

static void test_hungarian_algorithm_1()
{
  // Create input image
  int cost_val[6][6] = { {  4, 12, 11, 20, 16, 19 },
                         { 12,  8, 20, 13, 22, 18 },
                         {  6,  9,  4, 15,  9, 12 },
                         { 12,  4, 12,  6, 14,  8 },
                         { 12, 10,  6,  9,  5,  3 },
                         { 13,  7, 12,  2, 10,  4 }
                       };

  // Create the cost matrix
  vnl_matrix< int > cost( &cost_val[0][0], 6, 6 );

  vnl_hungarian_algorithm< int > HungarianClassTest;
  HungarianClassTest.SetCostMatrix( cost );
  HungarianClassTest.StartAssignment();
  std::cout<<"//-----------------------//\n"
          <<"//       Matrix 1        //\n"
          <<"//-----------------------//"<<std::endl;
  // the input matrix
  std::cout<< "input matrix:\n" << cost << std::endl;

  //returns the assignment matrix
  std::cout<< "assignment matrix:\n"
          << HungarianClassTest.GetAssignmentMatrix() << std::endl;

  TEST("assignment matrix", HungarianClassTest.GetAssignmentMatrix().is_identity(), true);

  //returns the assignment vector
  std::vector<unsigned> assignment = HungarianClassTest.GetAssignmentVector();

  std::cout<< "assignment vector:\n" << '(';
  for (unsigned int i=0; i<assignment.size() ;++i) std::cout << ' ' << assignment[i];
  std::cout << " )\n";

  TEST("assignment vector", assignment[0]==0 && assignment[1]==1 && assignment[2]==2 && assignment[3]==3 && assignment[4]==4 && assignment[5]==5, true);

  //returns the cost of the assignment
  TEST_NEAR("total cost", HungarianClassTest.GetTotalCost(), 31, 1e-11);

  double cost_val2[6][6] = { { 2, 3.0, 1, 0.1, 2, 7 },
                             { 1, 0, 1, 2, 3.0, 4 },
                             { 0, 0, 9, 5, 4.4, 2 },
                             { 1, 5, 6, 3, 0, 1 },
                             { 0, 1, 2, 0, 1, 2 },
                             { 2, 3, 1, 0.1, 1, 1 }
                           };

  // Create the cost matrix
  vnl_matrix< double > cost2( &cost_val2[0][0], 6, 6 );

  vnl_hungarian_algorithm< double > HungarianClassTest2;
  HungarianClassTest2.SetCostMatrix( cost2 );
  HungarianClassTest2.StartAssignment();
  std::cout <<"//-----------------------//\n"
           <<"//       Matrix 2        //\n"
           <<"//-----------------------//"<<std::endl;
  // the input matrix
  std::cout<< "input matrix:\n" << cost2 << std::endl;

  //returns the assignment matrix
  vnl_matrix<int> ass = HungarianClassTest2.GetAssignmentMatrix();
  std::cout << "assignment matrix:\n" << ass << std::endl;

  TEST("assignment matrix", ass[0][0]==0 && ass[0][2]==1 && ass[1][1]==1 && ass[2][0]==1 && ass[3][4]==1 && ass[4][3]==1 && ass[5][5]==1, true);

  //returns the assignment vector
  std::vector<unsigned> assignment2 = HungarianClassTest2.GetAssignmentVector();

  std::cout<< "assignment vector:" << std::endl << '(';
  for (unsigned int i=0; i<assignment2.size() ;++i) std::cout << ' ' << assignment2[i];
  std::cout << " )\n";

  TEST("assignment vector", assignment2[0]==2 && assignment2[1]==1 && assignment2[2]==0 && assignment2[3]==4 && assignment2[4]==3 && assignment2[5]==5, true);

  //returns the cost of the assignment
  TEST_NEAR("total cost", HungarianClassTest.GetTotalCost(), 31, 1e-11);
}

static vnl_random randgen;

static
void check_solution( std::vector<unsigned> const& assign,
                     unsigned const* solution, unsigned const N )
{
  TEST( "  assignment vector size", assign.size(), N );
  bool okay = true;
  std::cout << "  assignment:\n";
  for ( unsigned i = 0; i < N; ++i ) {
    if ( assign[i] != unsigned(-1) || solution[i] != unsigned(-1) ) {
      std::cout << "    " << i << " -> " << assign[i]
               << " (expected " << solution[i] << ")\n";
      if ( assign[i] != solution[i] ) {
        std::cout << "        (mismatch)\n";
        okay = false;
      }
    }
  }
  TEST( "  assignment result", okay, true );
}


static
std::vector<unsigned> make_up_solution( unsigned const M, unsigned const N )
{
  // True solution
  std::vector<unsigned> true_assn( M );
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
  std::cout << "Creating " << M << 'x' << N << " matrix" << std::endl;
  vnl_matrix<double> cost( M, N );
  double low = std::min(M,N) + 5.0;
  for ( unsigned i = 0; i < M; ++i ) {
    for ( unsigned j = 0; j < N; ++j ) {
      cost(i,j) = randgen.drand32( low, 100000.0 );
    }
  }

  std::vector<unsigned> true_assn;
  if ( M < N ) {
    true_assn = make_up_solution( M, N );
    for ( unsigned i = 0; i < M; ++i ) {
      cost(i, true_assn[i]) = i;
    }
  }
  else {
    std::vector<unsigned> transposed_assn = make_up_solution( N, M );
    true_assn.resize( M, unsigned(-1) );
    for ( unsigned j = 0; j < N; ++j ) {
      true_assn[ transposed_assn[j] ] = j;
      cost(transposed_assn[j],j) = j;
    }
  }

  std::cout << "Costs computed for " << M << 'x' << N << " matrix" << std::endl;

  std::vector<unsigned> assn = vnl_hungarian_algorithm<double>( cost );

  check_solution( assn, &true_assn[0], M );
}


static
void run_test( vnl_matrix<double> const& cost, unsigned solution[] )
{
  {
    std::cout << "Test " << cost.rows() << 'x' << cost.cols()
             << " matrix" << std::endl;
    std::vector<unsigned> assign = vnl_hungarian_algorithm<double>( cost );
    check_solution( assign, solution, cost.rows() );
  }

  {
    std::cout << "Test transposed problem" << std::endl;
    vnl_matrix<double> costT = cost.transpose();
    std::vector<unsigned> assign = vnl_hungarian_algorithm<double>( costT );

    std::vector<unsigned> solutionT( costT.rows(), unsigned(-1) );
    for ( unsigned i = 0; i < cost.rows(); ++i ) {
      if ( solution[i] != unsigned(-1) ) {
        solutionT[ solution[i] ] = i;
      }
    }

    check_solution( assign, &solutionT[0], costT.rows() );
  }
}


static void test_hungarian_algorithm_2()
{
  {
    double cost_val[3][3] = { { 1, 2, 3 },
                              { 2, 4, 6 },
                              { 3, 6, 9 } };

    // Create the cost matrix
    vnl_matrix<double> cost( &cost_val[0][0], 3, 3 );

    std::cout<<"//-----------------------//\n"
            <<"//       Matrix 3        //\n"
            <<"//-----------------------//"<<std::endl;
    // the input matrix
    std::cout<< "input matrix:\n" << cost << std::endl;

    std::vector<unsigned> assign = vnl_hungarian_algorithm<double>( cost );
    std::cout<< "assignment vector:\n" << '(';
    for (unsigned int i=0; i<assign.size(); ++i) std::cout << ' ' << assign[i];
    std::cout << " )" << std::endl;
    TEST( "Test 3x3 cost matrix" , assign.size()==3 &&
          assign[0]==2 && assign[1]==1 && assign[2]==0, true);
  }

  {
    double cost_val[4][4] = { { 2.0, 1.0, 5.0, 3.0 },
                              { 0.5, 6.0, 3.0, 0.5 },
                              { 5.0, 2.0, 1.0, 6.0 },
                              { 7.0, 1.0, 3.0, 0.1 } };
    vnl_matrix<double> cost( &cost_val[0][0], 4, 4 );

    std::cout<<"//-----------------------//\n"
            <<"//       Matrix 4        //\n"
            <<"//-----------------------//"<<std::endl;
    // the input matrix
    std::cout<< "input matrix:\n" << cost << std::endl;

    std::vector<unsigned> assign = vnl_hungarian_algorithm<double>( cost );
    TEST( "Test 4x4 cost matrix" , assign.size()==4 &&
          assign[0]==1 && assign[1]==0 && assign[2]==2 && assign[3]==3, true);
  }

  {
    double cost_val[3][4] = { { 2.0, 1.0, 5.0, 3.0 },
                              { 0.5, 6.0, 3.0, 0.5 },
                              { 7.0, 1.0, 3.0, 0.1 } };
    vnl_matrix<double> cost( &cost_val[0][0], 3, 4 );

    std::cout<<"//-----------------------//\n"
            <<"//       Matrix 5        //\n"
            <<"//-----------------------//"<<std::endl;
    // the input matrix
    std::cout<< "input matrix:\n" << cost << std::endl;

    unsigned solution[] = { 1, 0, 3 };
    run_test( cost, solution );
  }

  {
    // test where the greedy solution is not the optimal
    std::cout << "\n\nTest when greedy != optimal\n";
    double cost_val[3][4] = { { 2.0, 1.0, 5.0, 3.0 },
                              { 0.5, 0.2, 3.0, 0.5 },
                              { 7.0, 1.0, 3.0, 0.1 } };
    vnl_matrix<double> cost( &cost_val[0][0], 3, 4 );

    std::cout<<"//-----------------------//\n"
            <<"//       Matrix 6        //\n"
            <<"//-----------------------//"<<std::endl;
    // the input matrix
    std::cout<< "input matrix:\n" << cost << std::endl;

    unsigned solution[] = { 1, 0, 3 };
    run_test( cost, solution );
  }

  {
    // a white box test where the row-by-row minimum is not the
    // solution
    std::cout << "\n\nTest when row-by-row min != optimal\n";
    double cost_val[3][4] = { { 2.0, 1.0, 5.0, 3.0 },
                              { 0.5, 6.0, 3.0, 0.5 },
                              { 0.1, 1.0, 3.0, 0.2 } };
    vnl_matrix<double> cost( &cost_val[0][0], 3, 4 );

    std::cout<<"//-----------------------//\n"
            <<"//       Matrix 7        //\n"
            <<"//-----------------------//"<<std::endl;
    // the input matrix
    std::cout<< "input matrix:\n" << cost << std::endl;

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

    std::cout<<"//-----------------------//\n"
            <<"//       Matrix 8        //\n"
            <<"//-----------------------//"<<std::endl;
    // the input matrix
    std::cout<< "input matrix:\n" << cost << std::endl;

    unsigned solution[] = { 1, unsigned(-1), 0, unsigned(-1), 2 };
    run_test( cost, solution );
  }

  double Inf = std::numeric_limits<double>::infinity();

  {
    std::cout << "\n\nTest with Inf\n";
    double cost_val[5][3] = { { 2.0, 0.5, 7.0 },
                              { 1.1, 6.0, 1.0 },
                              { 1.0, 2.0, 1.0 },
                              { Inf, 3.0, 3.0 },
                              { 3.0, 0.5, 0.1 } };

    vnl_matrix<double> cost( &cost_val[0][0], 5, 3 );

    std::cout<<"//-----------------------//\n"
            <<"//       Matrix 9        //\n"
            <<"//-----------------------//"<<std::endl;
    // the input matrix
    std::cout<< "input matrix:\n" << cost << std::endl;

    unsigned solution[] = { 1, unsigned(-1), 0, unsigned(-1), 2 };
    run_test( cost, solution );
  }

  {
    std::cout << "\n\nTest with Inf, greedy not optimal\n";
    double cost_val[5][3] = { { 2.0, 0.5, 7.0 },
                              { 1.1, 6.0, 1.0 },
                              { 1.0, 2.0, 1.0 },
                              { Inf, 3.0, 3.0 },
                              { 3.0, 0.5, 0.1 } };

    vnl_matrix<double> cost( &cost_val[0][0], 5, 3 );

    std::cout<<"//-----------------------//\n"
            <<"//       Matrix 10       //\n"
            <<"//-----------------------//"<<std::endl;
    // the input matrix
    std::cout<< "input matrix:\n" << cost << std::endl;

    unsigned solution[] = { 1, unsigned(-1), 0, unsigned(-1), 2 };
    run_test( cost, solution );
  }


  // Verify that an O(mn) problem with m<<n does not explode into a
  // O(n^2) problem.
  {
    std::cout << "\n\nTest that O(N) doesn't become O(N^2)\n";
    // MN ~= 800 KB, N^2 ~= 20 GB
    test_skewed_problem( 2, 5000 );
    test_skewed_problem( 5000, 2 );
  }
}

static void test_hungarian_algorithm()
{
  // first test: uses the new, templated class interface:
  test_hungarian_algorithm_1();
  // second test: uses the old, function-based interface:
  test_hungarian_algorithm_2();
}

TESTMAIN( test_hungarian_algorithm )

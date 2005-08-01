#include <vnl/vnl_hungarian_algorithm.h>
#include <testlib/testlib_test.h>

#include <vcl_iostream.h>
#include <vcl_limits.h>
#include <vnl/vnl_matrix.h>

static void test_hungarian_algorithm( int, char*[] )
{
  {
    double cost_val[3][3] = { { 1, 2, 3 },
                              { 2, 4, 6 },
                              { 3, 6, 9 } };

    vnl_matrix<double> cost( &cost_val[0][0], 3, 3 );

    testlib_test_begin( "Test 3x3 cost matrix" );
    vcl_vector<unsigned> assign = vnl_hungarian_algorithm( cost );
    testlib_test_perform( assign.size()==3 &&
                          assign[0]==2 &&
                          assign[1]==1 &&
                          assign[2]==0 );
  }


  {
    double cost_val[4][4] = { { 2.0, 1.0, 5.0, 3.0 },
                              { 0.5, 6.0, 3.0, 0.5 },
                              { 5.0, 2.0, 1.0, 6.0 },
                              { 7.0, 1.0, 3.0, 0.1 } };
    vnl_matrix<double> cost( &cost_val[0][0], 4, 4 );

    testlib_test_begin( "Test 4x4 cost matrix" );
    vcl_vector<unsigned> assign = vnl_hungarian_algorithm( cost );
    testlib_test_perform( assign.size()==4 &&
                          assign[0]==1 &&
                          assign[1]==0 &&
                          assign[2]==2 &&
                          assign[3]==3 );
  }

  {
    double cost_val[3][4] = { { 2.0, 1.0, 5.0, 3.0 },
                              { 0.5, 6.0, 3.0, 0.5 },
                              { 7.0, 1.0, 3.0, 0.1 } };
    vnl_matrix<double> cost( &cost_val[0][0], 3, 4 );

    testlib_test_begin( "Test 3x4 cost matrix" );
    vcl_vector<unsigned> assign = vnl_hungarian_algorithm( cost );
    testlib_test_perform( assign.size()==3 &&
                          assign[0]==1 &&
                          assign[1]==0 &&
                          assign[2]==3 );
  }

  {
    double cost_val[4][3] = { { 2.0, 0.5, 7.0 },
                              { 1.0, 6.0, 1.0 },
                              { 5.0, 3.0, 3.0 },
                              { 3.0, 0.5, 0.1 } };

    vnl_matrix<double> cost( &cost_val[0][0], 4, 3 );

    testlib_test_begin( "Test 4x3 cost matrix" );
    vcl_vector<unsigned> assign = vnl_hungarian_algorithm( cost );
    testlib_test_perform( assign.size()==4 &&
                          assign[0]==1 &&
                          assign[1]==0 &&
                          assign[2]==-1u &&
                          assign[3]==2 );
  }

  {
    double cost_val[5][3] = { { 2.0, 0.5, 7.0 },
                              { 1.1, 6.0, 1.0 },
                              { 1.0, 2.0, 1.0 },
                              { 5.0, 3.0, 3.0 },
                              { 3.0, 0.5, 0.1 } };

    vnl_matrix<double> cost( &cost_val[0][0], 5, 3 );

    testlib_test_begin( "Test 5x3 cost matrix" );
    vcl_vector<unsigned> assign = vnl_hungarian_algorithm( cost );
    testlib_test_perform( assign.size()==5 &&
                          assign[0]==1 &&
                          assign[1]==-1u &&
                          assign[2]==0 &&
                          assign[3]==-1u &&
                          assign[4]==2 );

    testlib_test_begin( "Test 3x5 cost matrix" );
    vcl_vector<unsigned> assign2 = vnl_hungarian_algorithm( cost.transpose() );
    testlib_test_perform( assign2.size()==3 &&
                          assign2[0]==2 &&
                          assign2[1]==0 &&
                          assign2[2]==4 );
  }

  double Inf = vcl_numeric_limits<double>::infinity();
  {
    double cost_val[5][3] = { { 2.0, 0.5, 7.0 },
                              { 1.1, 6.0, 1.0 },
                              { 1.0, 2.0, 1.0 },
                              { Inf, 3.0, 3.0 },
                              { 3.0, 0.5, 0.1 } };

    vnl_matrix<double> cost( &cost_val[0][0], 5, 3 );

    testlib_test_begin( "Test 5x3 cost matrix with Inf" );
    vcl_vector<unsigned> assign = vnl_hungarian_algorithm( cost );
    testlib_test_perform( assign.size()==5 &&
                          assign[0]==1 &&
                          assign[1]==-1u &&
                          assign[2]==0 &&
                          assign[3]==-1u &&
                          assign[4]==2 );

    testlib_test_begin( "Test 3x5 cost matrix with Inf" );
    vcl_vector<unsigned> assign2 = vnl_hungarian_algorithm( cost.transpose() );
    testlib_test_perform( assign2.size()==3 &&
                          assign2[0]==2 &&
                          assign2[1]==0 &&
                          assign2[2]==4 );
  }
}

TESTMAIN_ARGS( test_hungarian_algorithm )

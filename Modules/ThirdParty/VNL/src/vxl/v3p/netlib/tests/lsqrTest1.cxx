/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "linalg/lsqrDense.h"

#include <iostream>

#include <cstdlib>
#include <cmath>


int main( int , char * [] )
{

  lsqrDense solver;

  const double tolerance = 1e-9;

  const unsigned int n = 2;
  double x[n];
  double z[n];

  x[0] = 3.0;
  x[1] = 5.0;

  z[0] = 0.0;
  z[1] = 1.0;

  solver.HouseholderTransformation(n,z,x);

  std::cout << x[0] << " " << x[1] << std::endl;

  { // Test 1 Dnrm2()
  const unsigned int n1 = 5;
  double x1[n1];
  x1[0] = 1.0;
  x1[1] = 1.0;
  x1[2] = 1.0;
  x1[3] = 1.0;
  x1[4] = 1.0;

  const double norm =solver.Dnrm2( n1, x1 );
  const double expectedNorm = sqrt(5.0);

  const double ratioOfDifference =
    fabs( norm - expectedNorm ) / expectedNorm;

  if( ratioOfDifference > tolerance )
    {
    std::cerr << "Error in Dnrm2() test 1 " << std::endl;
    std::cerr << "Expected = " << expectedNorm << std::endl;
    std::cerr << "Received = " << norm << std::endl;
    std::cerr << "ratioOfDifference = " << ratioOfDifference << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Dnrm2 test 1 passed " << std::endl;
  }

  { // Test 2 Dnrm2()
  const unsigned int n2 = 5;

  const double dominantValue = 1e+300;

  double x2[n2];
  x2[0] = 1e+30;
  x2[1] = 1e+200;
  x2[2] = dominantValue;
  x2[3] = 1e+2;
  x2[4] = 1e+1;

  const double norm =solver.Dnrm2( n2, x2 );
  const double expectedNorm = dominantValue;

  const double ratioOfDifference =
    fabs( norm - expectedNorm ) / expectedNorm;

  if( ratioOfDifference > tolerance )
    {
    std::cerr << "Error in Dnrm2() test 2 " << std::endl;
    std::cerr << "Expected = " << expectedNorm << std::endl;
    std::cerr << "Received = " << norm << std::endl;
    std::cerr << "ratioOfDifference = " << ratioOfDifference << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Dnrm2 test 2 passed " << std::endl;
  }


  { // Testing D2Norm
  const double dominantValue = 1e+300;
  const double minorValue = 1e-100;
  const double a = dominantValue;
  const double b = minorValue;
  const double d2norm = solver.D2Norm( a, b );

  const double ratioOfDifference =
    fabs( d2norm - dominantValue ) / dominantValue;

  if( ratioOfDifference > tolerance )
    {
    std::cerr << "Error in D2Norm() test 1 " << std::endl;
    std::cerr << "Expected = " << dominantValue << std::endl;
    std::cerr << "Received = " << d2norm << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "D2Norm test 1 passed " << std::endl;
  }

  { // Testing D2Norm
  const double friendlyValue1 = 1e+3;
  const double friendlyValue2 = 1e+2;
  const double a = friendlyValue1;
  const double b = friendlyValue2;
  const double d2norm = solver.D2Norm( a, b );
  const double expectedD2norm = sqrt( a*a + b*b );

  const double ratioOfDifference =
    fabs( d2norm - expectedD2norm ) / expectedD2norm;

  if( ratioOfDifference > tolerance )
    {
    std::cerr << "Error in D2Norm() test 2 " << std::endl;
    std::cerr << "Expected = " << expectedD2norm << std::endl;
    std::cerr << "Received = " << d2norm << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "D2Norm test 2 passed " << std::endl;
  }


  { // Testing D2Norm
  const double zero = 0.0;
  const double a = zero;
  const double b = zero;
  const double d2norm = solver.D2Norm( a, b );

  const double difference = fabs( d2norm - zero );

  if( difference > tolerance )
    {
    std::cerr << "Error in D2Norm() test 3 " << std::endl;
    std::cerr << "Expected = " << zero << std::endl;
    std::cerr << "Received = " << d2norm << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "D2Norm test 3 passed " << std::endl;
  }


  solver.SetOutputStream( std::cout );

  const double eps = 1e-15;

  solver.SetEpsilon( eps );

  const double damp = 0.0;

  solver.SetDamp( damp );

  solver.SetMaximumNumberOfIterations( 100 );

  solver.SetToleranceA( 1e-16 );
  solver.SetToleranceB( 1e-16 );

  solver.SetUpperLimitOnConditional( 1.0 / ( 10 * sqrt( eps ) ) );

  solver.SetStandardErrorEstimatesFlag( true );


  { // basic test for Scale()
  const double factor = 8.0;
  const unsigned int mm = 5;
  double xx[mm];
  for ( unsigned int i = 0; i < mm; i++ )
    {
    xx[i] = i * 5.0;
    }

  solver.Scale( mm, factor, xx );

  for ( unsigned int i = 0; i < mm; i++ )
    {
    const double expectedValue = ( i*5.0*factor );
    const double ratioOfDifference =
      fabs( xx[i] - expectedValue ) / expectedValue;

    if ( ratioOfDifference > tolerance )
      {
      std::cerr << "Error in method Scale() " << std::endl;
      return EXIT_FAILURE;
      }
    }
  }


  { // basic test for Aprod1()
  const unsigned int mm = 2;
  const unsigned int nn = 2;
  double xx[nn];
  xx[0] = 1.0;
  xx[1] = 7.0;
  typedef double * RowType;
  RowType A[mm];
  double AA[4];
  A[0] = &(AA[0]);
  A[1] = &(AA[2]);
  A[0][0] = 1.0;
  A[0][1] = 0.0;
  A[1][0] = 0.0;
  A[1][1] = 1.0;
  solver.SetMatrix( A );
  double yy[mm];
  yy[0] = 0.0;
  yy[1] = 0.0;
  solver.Aprod1( mm, nn, xx, yy );
  std::cout << "yy = " << yy[0] << " " << yy[1] << std::endl;
  }

  { // basic test for Aprod1()
  const unsigned int mm = 2;
  const unsigned int nn = 3;
  double xx[nn];
  xx[0] = 1.0;
  xx[1] = 7.0;
  xx[2] = 9.0;
  typedef double * RowType;
  RowType A[mm];
  double AA[6];
  A[0] = &(AA[0]);
  A[1] = &(AA[3]);
  A[0][0] = 1.0;
  A[0][1] = 2.0;
  A[0][2] = 3.0;
  A[1][0] = 4.0;
  A[1][1] = 5.0;
  A[1][2] = 6.0;
  solver.SetMatrix( A );
  double yy[mm];
  yy[0] = 0.0;
  yy[1] = 0.0;
  solver.Aprod1( mm, nn, xx, yy );
  std::cout << "yy = " << yy[0] << " " << yy[1] << std::endl;
  }

  { // basic test for Aprod1()
  const unsigned int mm = 3;
  const unsigned int nn = 2;
  double xx[nn];
  xx[0] = 1.0;
  xx[1] = 7.0;
  typedef double * RowType;
  RowType A[mm];
  double AA[6];
  A[0] = &(AA[0]);
  A[1] = &(AA[2]);
  A[2] = &(AA[4]);
  A[0][0] = 1.0;
  A[0][1] = 2.0;
  A[1][0] = 3.0;
  A[1][1] = 4.0;
  A[2][0] = 5.0;
  A[2][1] = 6.0;
  solver.SetMatrix( A );
  double yy[mm];
  yy[0] = 0.0;
  yy[1] = 0.0;
  yy[2] = 0.0;
  solver.Aprod1( mm, nn, xx, yy );
  std::cout << "yy = " << yy[0] << " " << yy[1] << " " << yy[2] << std::endl;
  }

  { // basic test for Aprod2()
  const unsigned int mm = 2;
  const unsigned int nn = 2;
  double xx[nn];
  xx[0] = 0.0;
  xx[1] = 0.0;
  typedef double * RowType;
  RowType A[mm];
  double AA[4];
  A[0] = &(AA[0]);
  A[1] = &(AA[2]);
  A[0][0] = 1.0;
  A[0][1] = 0.0;
  A[1][0] = 0.0;
  A[1][1] = 1.0;
  solver.SetMatrix( A );
  double yy[mm];
  yy[0] = 1.0;
  yy[1] = 7.0;
  solver.Aprod2( mm, nn, xx, yy );
  std::cout << "xx = " << xx[0] << " " << xx[1] << std::endl;
  }

  { // basic test for Aprod2()
  const unsigned int mm = 2;
  const unsigned int nn = 3;
  double xx[nn];
  xx[0] = 0.0;
  xx[1] = 0.0;
  xx[2] = 0.0;
  typedef double * RowType;
  RowType A[mm];
  double AA[6];
  A[0] = &(AA[0]);
  A[1] = &(AA[3]);
  A[0][0] = 1.0;
  A[0][1] = 2.0;
  A[0][2] = 3.0;
  A[1][0] = 4.0;
  A[1][1] = 5.0;
  A[1][2] = 6.0;
  solver.SetMatrix( A );
  double yy[mm];
  yy[0] = 1.0;
  yy[1] = 5.0;
  solver.Aprod2( mm, nn, xx, yy );
  std::cout << "xx = " << xx[0] << " " << xx[1] << " " << xx[2] << std::endl;
  }

  { // basic test for Aprod2()
  const unsigned int mm = 3;
  const unsigned int nn = 2;
  double xx[nn];
  xx[0] = 0.0;
  xx[1] = 0.0;
  typedef double * RowType;
  RowType A[mm];
  double AA[6];
  A[0] = &(AA[0]);
  A[1] = &(AA[2]);
  A[2] = &(AA[4]);
  A[0][0] = 1.0;
  A[0][1] = 2.0;
  A[1][0] = 3.0;
  A[1][1] = 4.0;
  A[2][0] = 5.0;
  A[2][1] = 6.0;
  solver.SetMatrix( A );
  double yy[mm];
  yy[0] = 1.0;
  yy[1] = 7.0;
  yy[2] = 9.0;
  solver.Aprod2( mm, nn, xx, yy );
  std::cout << "xx = " << xx[0] << " " << xx[1] << std::endl;
  }

  { // basic test for Solve()
  const unsigned int mm = 2;
  const unsigned int nn = 2;
  double bb[nn];
  bb[0] = 1.0;
  bb[1] = 7.0;
  double xx[mm];
  solver.SetStandardErrorEstimatesFlag( false );
  typedef double * RowType;
  RowType A[mm];
  double AA[4];
  A[0] = &(AA[0]);
  A[1] = &(AA[2]);
  A[0][0] = 1.0;
  A[0][1] = 0.0;
  A[1][0] = 0.0;
  A[1][1] = 1.0;
  solver.SetMatrix( A );
  solver.SetMaximumNumberOfIterations( 10 );
  solver.Solve( mm, nn, bb, xx );
  std::cout << "xx = " << xx[0] << " " << xx[1] << std::endl;
  }

  { // basic test for Solve()
  const unsigned int mm = 2;
  const unsigned int nn = 2;
  double bb[nn];
  bb[0] = 1.0;
  bb[1] = 7.0;
  double xx[mm];
  solver.SetStandardErrorEstimatesFlag( true );
  double se[nn];
  solver.SetStandardErrorEstimates( se );
  typedef double * RowType;
  RowType A[mm];
  double AA[4];
  A[0] = &(AA[0]);
  A[1] = &(AA[2]);
  A[0][0] = 1.0;
  A[0][1] = 0.0;
  A[1][0] = 0.0;
  A[1][1] = 1.0;
  solver.SetMatrix( A );
  solver.SetMaximumNumberOfIterations( 10 );
  solver.Solve( mm, nn, bb, xx );
  std::cout << "se = " << se[0] << " " << se[1] << std::endl;
  std::cout << "xx = " << xx[0] << " " << xx[1] << std::endl;
  }


  { // basic test for Solve()
  const unsigned int mm = 2;
  const unsigned int nn = 2;
  double bb[nn];
  bb[0] = -9.0;
  bb[1] = 15.0;
  double xx[mm];
  solver.SetStandardErrorEstimatesFlag( true );
  double se[nn];
  solver.SetStandardErrorEstimates( se );
  typedef double * RowType;
  RowType A[mm];
  double AA[4];
  A[0] = &(AA[0]);
  A[1] = &(AA[2]);
  A[0][0] = 1.0;
  A[0][1] = 0.0;
  A[1][0] = 0.0;
  A[1][1] = 1.0;
  solver.SetMatrix( A );
  solver.SetMaximumNumberOfIterations( 10 );
  solver.Solve( mm, nn, bb, xx );
  std::cout << "se = " << se[0] << " " << se[1] << std::endl;
  std::cout << "xx = " << xx[0] << " " << xx[1] << std::endl;
  }


  std::cout << "Stopped because " << solver.GetStoppingReason() << std::endl;
  std::cout << "Used " << solver.GetNumberOfIterationsPerformed() << " Iterations" << std::endl;
  std::cout << "Frobenius norm estimation of Abar = " << solver.GetFrobeniusNormEstimateOfAbar() << std::endl;
  std::cout << "Condition number estimation of Abar = " << solver.GetConditionNumberEstimateOfAbar() << std::endl;
  std::cout << "Estimate of final value of norm(rbar) = " << solver.GetFinalEstimateOfNormRbar() << std::endl;
  std::cout << "Estimate of final value of norm of residuals = " << solver.GetFinalEstimateOfNormOfResiduals() << std::endl;
  std::cout << "Estimate of norm of final solution = " << solver.GetFinalEstimateOfNormOfX() << std::endl;

  return EXIT_SUCCESS;
}

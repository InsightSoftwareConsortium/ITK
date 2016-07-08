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


//
// A more interesting testing case, but still a basic one.
//
int main( int , char * [] )
{

  lsqrDense solver;

  const double eps = 1e-15;

  solver.SetOutputStream( std::cout );
  solver.SetEpsilon( eps );
  solver.SetDamp( 0.0 );
  solver.SetMaximumNumberOfIterations( 100 );
  solver.SetToleranceA( 1e-16 );
  solver.SetToleranceB( 1e-16 );
  solver.SetUpperLimitOnConditional( 1.0 / ( 10 * sqrt( eps ) ) );
  solver.SetStandardErrorEstimatesFlag( true );


  const unsigned int mm = 3;
  const unsigned int nn = 2;

  double bb[mm];
  double xx[nn];

  bb[0] = 7.0;
  bb[1] =-7.0;
  bb[2] = 2.0;

  solver.SetStandardErrorEstimatesFlag( true );
  double se[nn];

  solver.SetStandardErrorEstimates( se );

  //  -3  5
  typedef double * RowType;
  RowType A[mm];
  double AA[6];
  A[0] = &(AA[0]);
  A[1] = &(AA[2]);
  A[2] = &(AA[4]);
  A[0][0] = 1.0;
  A[0][1] = 2.0;
  A[1][0] = 4.0;
  A[1][1] = 1.0;
  A[2][0] = 1.0;
  A[2][1] = 1.0;
  solver.SetMatrix( A );

  solver.Solve( mm, nn, bb, xx );

  std::cout << "Stopped because " << solver.GetStoppingReason() << std::endl;
  std::cout << "Used " << solver.GetNumberOfIterationsPerformed() << " Iterations" << std::endl;
  std::cout << "Frobenius norm estimation of Abar = " << solver.GetFrobeniusNormEstimateOfAbar() << std::endl;
  std::cout << "Condition number estimation of Abar = " << solver.GetConditionNumberEstimateOfAbar() << std::endl;
  std::cout << "Estimate of final value of norm(rbar) = " << solver.GetFinalEstimateOfNormRbar() << std::endl;
  std::cout << "Estimate of final value of norm of residuals = " << solver.GetFinalEstimateOfNormOfResiduals() << std::endl;
  std::cout << "Estimate of norm of final solution = " << solver.GetFinalEstimateOfNormOfX() << std::endl;
  std::cout << "Solution = xx = " << xx[0] << " " << xx[1] << std::endl;

  return EXIT_SUCCESS;
}

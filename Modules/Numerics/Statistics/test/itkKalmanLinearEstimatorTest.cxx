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

#include "itkKalmanLinearEstimator.h"

#include <iostream>

/**
 *  This program test one instantiation of the itk::KalmanLinearEstimator class
 *
 *  The test is done by providing a Linear Equation in 6D for which the
 *  coefficients are known. A population of samples is generated and
 *  passed to the KalmanLinearEstimator.
 *
 */

int itkKalmanLinearEstimatorTest(int, char* [] )
{


  typedef itk::KalmanLinearEstimator<double,6> KalmanFilterType;

  typedef KalmanFilterType::VectorType    VectorType;
  typedef KalmanFilterType::ValueType     ValueType;

  KalmanFilterType filter;

  filter.ClearEstimation();
  filter.SetVariance(1.0);

  ValueType     measure;
  VectorType    predictor;

  VectorType    planeEquation;

  planeEquation(0) = 9.0;
  planeEquation(1) = 6.0;
  planeEquation(2) = 7.0;
  planeEquation(3) = 9.0;
  planeEquation(4) = 4.0;
  planeEquation(5) = 6.0;

  const unsigned int N = 10;

  predictor(5)  =  1.0;
  for(unsigned int ax=0; ax < N; ax++)
    {
    predictor(0)  = ax;
    for(unsigned int bx=0; bx < N; bx++)
      {
      predictor(1)  = bx;
      for(unsigned int cx=0; cx < N; cx++)
        {
        predictor(2)  = cx;
        for(unsigned int dx=0; dx < N; dx++)
          {
          predictor(3)  = dx;
          for(unsigned int ex=0; ex < N; ex++)
            {
            predictor(4)  =  ex;

            measure = dot_product( predictor, planeEquation );

            filter.UpdateWithNewMeasure(measure,predictor);

            }
          }
        }
      }
    }

  VectorType estimation = filter.GetEstimator();

  std::cout << std::endl << "The Right answer should be : " << std::endl;
  std::cout << planeEquation;

  std::cout << std::endl << "The Estimation is : " << std::endl;
  std::cout << estimation;

  VectorType error = estimation - planeEquation;
  ValueType errorMagnitude =  dot_product( error, error );

  std::cout << std::endl << "Errors : " << std::endl;
  std::cout << error;

  std::cout << std::endl << "Error Magnitude : " << std::endl;
  std::cout << errorMagnitude;

  std::cout << std::endl << "Variance : " << std::endl;
  std::cout << filter.GetVariance();

  std::cout << std::endl << std::endl;

  bool pass = true;

  const float tolerance = 1e-4;

  if( errorMagnitude > tolerance )
    {
    pass = false;
    }

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}

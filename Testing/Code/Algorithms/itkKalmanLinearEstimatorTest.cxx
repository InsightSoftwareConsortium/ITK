/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkKalmanLinearEstimatorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

#include "itkKalmanLinearEstimator.h"

/** 
 *  This program test one instantiation of the itk::KalmanLinearEstimator class
 * 
 *  The test is done by providing a Linear Equation in 6D for which the 
 *  coefficients are known. A population of samples is generated and 
 *  passed to the KalmanLinearEstimator.
 *
 */ 

int main()
{


  typedef itk::KalmanLinearEstimator<double,6> KalmanFilterType;

  typedef KalmanFilterType::VectorType    VectorType;
  typedef KalmanFilterType::MatrixType    MatrixType;
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

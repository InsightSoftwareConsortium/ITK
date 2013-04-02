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

#include <iostream>
#include "itkMultivariateLegendrePolynomial.h"

/**
 *  This test exercise the functionality of the
 *
 *  itk::MultivariateLegendrePolynomial  class
 *
 */


int itkMultivariateLegendrePolynomialTest(int , char* [] )
{

  typedef itk::MultivariateLegendrePolynomial PolynomialType;

  const unsigned int dimension = 3;
  const unsigned int degree    = 3;

  PolynomialType::DomainSizeType domainSize(dimension);
  domainSize[0] = 2;
  domainSize[1] = 2;
  domainSize[2] = 2;

  PolynomialType polynomial( dimension, degree, domainSize );

  if ( polynomial.GetDimension() != dimension )
    {
    std::cout << "Test fails: GetDimension()" << std::endl;
    return EXIT_FAILURE;
    }

  if ( polynomial.GetDegree() != degree )
    {
    std::cout << "Test fails: GetDegree()" << std::endl;
    return EXIT_FAILURE;
    }

  if ( polynomial.GetDomainSize() != domainSize )
    {
    std::cout << "Test fails: GetDomainSize()" << std::endl;
    return EXIT_FAILURE;
    }

  if ( polynomial.GetNumberOfCoefficients() != 20 )
    {
    std::cout << "Test fails: GetNumberOfCoefficients()" << std::endl;
    return EXIT_FAILURE;
    }

  PolynomialType::CoefficientArrayType  coefficients(20);

  std::fill(coefficients.begin(), coefficients.end(), 0.1);

  try
    {
    polynomial.SetCoefficients( coefficients );
    }
  catch ( ... )
    {
    }

  PolynomialType::SimpleForwardIterator bIter( &polynomial );
  bIter.Begin();
  while (!bIter.IsAtEnd())
    {
    bIter.Get();
    ++bIter;
    }

  std::cout << "Test succeeded." << std::endl;
  return EXIT_SUCCESS;
}

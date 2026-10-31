/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkGTest.h"

/**
 *  This test exercise the functionality of the
 *
 *  itk::MultivariateLegendrePolynomial  class
 *
 */


TEST(MultivariateLegendrePolynomial, ConvertedLegacyTest)
{

  using PolynomialType = itk::MultivariateLegendrePolynomial;

  constexpr unsigned int dimension{ 3 };
  constexpr unsigned int degree{ 3 };

  PolynomialType::DomainSizeType domainSize(dimension);
  domainSize[0] = 2;
  domainSize[1] = 2;
  domainSize[2] = 2;

  PolynomialType polynomial(dimension, degree, domainSize);

  EXPECT_EQ(polynomial.GetDimension(), dimension);

  EXPECT_EQ(polynomial.GetDegree(), degree);

  EXPECT_EQ(polynomial.GetDomainSize(), domainSize);

  EXPECT_EQ(polynomial.GetNumberOfCoefficients(), 20u);

  PolynomialType::CoefficientArrayType coefficients(20);

  std::fill(coefficients.begin(), coefficients.end(), 0.1);

  ASSERT_NO_THROW(polynomial.SetCoefficients(coefficients));

  PolynomialType::SimpleForwardIterator bIter(&polynomial);
  bIter.Begin();
  while (!bIter.IsAtEnd())
  {
    bIter.Get();
    ++bIter;
  }
}

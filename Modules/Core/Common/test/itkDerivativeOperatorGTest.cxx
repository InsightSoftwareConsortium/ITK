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


#include "itkMath.h"
#include "itkDerivativeOperator.h"
#include "itkGTest.h"

namespace itk
{
template <typename TPixel, unsigned int VDimension = 2>
class DerivativeOperatorTestHelper : public DerivativeOperator<TPixel, VDimension>
{
public:
  using Superclass = DerivativeOperator<TPixel, VDimension>;
  using typename Superclass::CoefficientVector;

  bool
  CheckCoefficients(const CoefficientVector & expected)
  {
    CoefficientVector coefficients = this->GenerateCoefficients();

    if (itk::Math::NotAlmostEquals(expected.size(), coefficients.size()))
    {
      std::cerr << "Wrong coefficient vector size" << std::endl;
      std::cerr << "expected " << expected.size() << std::endl;
      std::cerr << "computed " << coefficients.size() << std::endl;
      return false;
    }

    for (unsigned int i = 0; i < expected.size(); ++i)
    {
      if (itk::Math::NotAlmostEquals(expected[i], coefficients[i]))
      {
        std::cerr << "Wrong coefficient value at " << i << std::endl;
        std::cerr << "expected " << expected[i] << std::endl;
        std::cerr << "computed " << coefficients[i] << std::endl;
        return false;
      }
    }
    return true;
  }
};
} // namespace itk

TEST(DerivativeOperator, BasicMethodsAndCoefficients)
{
  constexpr unsigned int Dimension{ 1 };
  using PixelType = float;

  // Exercise object basic methods
  using DerivativeOperatorType = itk::DerivativeOperator<PixelType, Dimension>;

  DerivativeOperatorType   derivativeOperator;
  DerivativeOperatorType * derivativeOperatorPtr = &derivativeOperator;

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(derivativeOperatorPtr, DerivativeOperator, NeighborhoodOperator);


  // Test other methods using the helper
  using OperatorType = itk::DerivativeOperatorTestHelper<PixelType, Dimension>;

  OperatorType op1;

  unsigned int order = 1;
  op1.SetOrder(order);
  EXPECT_EQ(op1.GetOrder(), order);

  order = 2;
  op1.SetOrder(order);
  EXPECT_EQ(op1.GetOrder(), order);

  order = 1;
  op1.SetOrder(order);
  EXPECT_EQ(op1.GetOrder(), order);


  OperatorType::CoefficientVector expected1{ 0.5, 0.0, -0.5 };

  // Check actual coefficient values
  EXPECT_TRUE(op1.CheckCoefficients(expected1));

  // Test copy constructor
  OperatorType op1b(op1);

  // Check actual coefficient values
  EXPECT_TRUE(op1b.CheckCoefficients(expected1));

  // Test operator assignment
  OperatorType op1c(op1);

  // Check actual coefficient values
  EXPECT_TRUE(op1c.CheckCoefficients(expected1));

  // Test second order
  OperatorType::CoefficientVector expected2{ 1, -2, 1 };

  OperatorType op2;

  op2.SetOrder(2);

  // Check actual coefficient values
  EXPECT_TRUE(op2.CheckCoefficients(expected2));


  std::cout << "Test finished." << std::endl;
}

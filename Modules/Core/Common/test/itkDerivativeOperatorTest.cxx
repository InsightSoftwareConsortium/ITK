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


#include "itkMath.h"
#include "itkDerivativeOperator.h"

namespace itk {
template< typename TPixel, unsigned int VDimension = 2>
class DerivativeOperatorTestHelper : public DerivativeOperator< TPixel, VDimension >
{
public:
  typedef DerivativeOperator< TPixel, VDimension >  Superclass;
  typedef typename Superclass::CoefficientVector    CoefficientVector;

  bool CheckCoefficients( const CoefficientVector & expected )
    {
    CoefficientVector coefficients = this->GenerateCoefficients();

    if( itk::Math::NotAlmostEquals( expected.size(), coefficients.size() ) )
      {
      std::cerr << "Wrong coefficient vector size" << std::endl;
      std::cerr << "expected " << expected.size() << std::endl;
      std::cerr << "computed " << coefficients.size() << std::endl;
      return false;
      }

    for( unsigned int i=0; i < expected.size(); i++ )
      {
      if( itk::Math::NotAlmostEquals( expected[i], coefficients[i] ) )
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
}

int itkDerivativeOperatorTest(int, char* [] )
{
  const unsigned int  Dimension = 1;
  typedef float       PixelType;

  typedef itk::DerivativeOperatorTestHelper< PixelType, Dimension > OperatorType;

  OperatorType op1;

  op1.SetOrder(1);
  if( op1.GetOrder() != 1 )
    {
    std::cerr << "Set/GetOrder() failed" << std::endl;
    return EXIT_FAILURE;
    }

  op1.SetOrder(2);
  if( op1.GetOrder() != 2 )
    {
    std::cerr << "Set/GetOrder() failed" << std::endl;
    return EXIT_FAILURE;
    }

  op1.SetOrder(1);
  if( op1.GetOrder() != 1 )
    {
    std::cerr << "Set/GetOrder() failed" << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise PrintSelf
  op1.Print( std::cout );

  OperatorType::CoefficientVector expected1;
  expected1.push_back(  0.5 );
  expected1.push_back(  0.0 );
  expected1.push_back( -0.5 );

  // Check actual coefficient values
  if( ! op1.CheckCoefficients( expected1 ) )
    {
    return EXIT_FAILURE;
    }

  // Test copy constructor
  OperatorType op1b( op1 );

  // Check actual coefficient values
  if( ! op1b.CheckCoefficients( expected1 ) )
    {
    std::cerr << "Copy constructor failed" << std::endl;
    return EXIT_FAILURE;
    }

  // Test operator assignment
  OperatorType op1c( op1 );

  // Check actual coefficient values
  if( ! op1c.CheckCoefficients( expected1 ) )
    {
    std::cerr << "Operator assignment failed" << std::endl;
    return EXIT_FAILURE;
    }

  // Test second order
  OperatorType::CoefficientVector expected2;
  expected2.push_back(  1 );
  expected2.push_back( -2 );
  expected2.push_back(  1 );

  OperatorType op2;

  op2.SetOrder(2);

  // Check actual coefficient values
  if( ! op2.CheckCoefficients( expected2 ) )
    {
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;
}

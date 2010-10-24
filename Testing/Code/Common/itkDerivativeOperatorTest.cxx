/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeOperatorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkDerivativeOperator.h"

namespace itk {
template< class TPixel, unsigned int VDimension = 2>
class DerivativeOperatorTestHelper : public DerivativeOperator< TPixel, VDimension >
{
public:
  typedef DerivativeOperator< TPixel, VDimension >  Superclass;
  typedef typename Superclass::CoefficientVector    CoefficientVector;

  bool CheckCoefficients( const CoefficientVector & expected )
    {
    CoefficientVector coefficients = this->GenerateCoefficients();

    if( expected.size() != coefficients.size() )
      {
      std::cerr << "Wrong coefficient vector size" << std::endl;
      std::cerr << "expected " << expected.size() << std::endl;
      std::cerr << "computed " << coefficients.size() << std::endl;
      return false;
      }

    for( unsigned int i=0; i < expected.size(); i++ )
      {
      if( expected[i] != coefficients[i] )
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

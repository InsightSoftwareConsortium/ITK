/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultivariateLegendrePolynomialTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
  domainSize.fill(2);

  PolynomialType polynomial( dimension, degree, domainSize );

  if ( polynomial.GetDimension() != dimension )
    {
    std::cout << "Test fails: GetDimension()" << std::endl ; 
    return EXIT_FAILURE ;
    }

  if ( polynomial.GetDegree() != degree )
    {
    std::cout << "Test fails: GetDegree()" << std::endl ; 
    return EXIT_FAILURE ;
    }

  if ( polynomial.GetDomainSize() != domainSize )
    {
    std::cout << "Test fails: GetDomainSize()" << std::endl ; 
    return EXIT_FAILURE ;
    }

  if ( polynomial.GetNumberOfCoefficients() != 20 )
    {
    std::cout << "Test fails: GetNumberOfCoefficients()" << std::endl ; 
    return EXIT_FAILURE ;
    }

  PolynomialType::CoefficientArrayType  coefficients(20) ;

  coefficients.Fill( 0.1 ) ;

  try
    {
    polynomial.SetCoefficients( coefficients ) ;
    }
  catch ( ... )
    {
    }

  PolynomialType::SimpleForwardIterator bIter( &polynomial ) ;
  bIter.Begin() ;
  while (!bIter.IsAtEnd())
    {
    bIter.Get() ;
    ++bIter ;
    }

  std::cout << "Test succeeded." << std::endl;
  return EXIT_SUCCESS ;
}




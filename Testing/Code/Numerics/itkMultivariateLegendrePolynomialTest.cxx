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



#include "itkMultivariateLegendrePolynomial.h"



/** 
 *  This test exercise the functionality of the
 *
 *  itk::MultivariateLegendrePolynomial  class
 *
 *
 */ 


int itkMultivariateLegendrePolynomialTest(int , char* [] )
{
  
  typedef itk::MultivariateLegendrePolynomial PolynomialType;

  const unsigned int dimension = 3;
  const unsigned int degree    = 3;

  PolynomialType::DomainSizeType domainSize(dimension);

  PolynomialType polynomial( dimension, degree, domainSize );

  return 0;
}




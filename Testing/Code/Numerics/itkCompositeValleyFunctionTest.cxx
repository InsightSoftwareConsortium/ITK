/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompositeValleyFunctionTest.cxx
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
#include "vnl/vnl_math.h"

#include "itkCompositeValleyFunction.h"
#include "itkArray.h"
#include "itkNumericTraits.h"

int itkCompositeValleyFunctionTest(int , char* [] )
{
  itk::Array< double > means(2) ;
  itk::Array< double > sigmas(2) ;
  
  means[0] = 0.0 ;
  means[1] = 100.0 ;
  sigmas[0] = 20.0 ;
  sigmas[1] = 20.0 ;

  itk::CompositeValleyFunction function(means, sigmas) ;

  if ( function.GetUpperBound() != 280.0 )
    {
    std::cout << "Test fails: GetUpperBound()" << std::endl ;
    return EXIT_FAILURE ;
    }

  if ( function.GetLowerBound() != -180.0 )
    {
    std::cout << "Test fails: GetLowerBound()" << std::endl ;
    return EXIT_FAILURE ;
    }

  std::cout.setf(std::ios::scientific) ;
  std::cout.precision(12) ;
  double interval1 = function.GetInterval() ;
  double interval2 = 
    ( function.GetUpperBound() - function.GetLowerBound() )
    / double(1000000 - 1) ;
  if (  interval1 != interval2 )
    {
    std::cout << "Test fails: GetInterval()" << std::endl ;
    return EXIT_FAILURE ;
    }

  long numberOfSamples = function.GetNumberOfSamples() ;
  double measure = function.GetLowerBound() + interval1* numberOfSamples * 0.5 ;
  double value1 = function( measure ) ;
  double value2 = function.Evaluate( measure ) ;

  if ( vnl_math_abs(value1 - value2) > 
       itk::NumericTraits< double >::epsilon())
    {
    std::cout << "diff = " << vnl_math_abs(value1 - value2) << std::endl ;
    std::cout << "Test fails: operator()" << std::endl ;
    return EXIT_FAILURE ;
    }

  std::cout << "Test succeed" << std::endl ;
  return EXIT_SUCCESS ;
}

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalVariateGeneratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNormalVariateGenerator.h"
#include "vnl/vnl_math.h"

int itkNormalVariateGeneratorTest(int, char* [] ) 
{
  std::cout << "NormalVariateGenerator Test \n \n"; 
  
  typedef  float      MeasureType;

  typedef  itk::Statistics::NormalVariateGenerator GeneratorType;

  GeneratorType::Pointer generator = GeneratorType::New();

    
  const int randomSeed = 14543 ; // any number to initialize the seed.

  generator->Initialize( randomSeed );

  const unsigned long numberOfSamples = 1000000UL;
  
  double sum  = 0.0f;
  double sum2 = 0.0f;

  std::cout << "Generating " << numberOfSamples << " samples... " << std::endl;

  for(unsigned int i=0; i < numberOfSamples; i++)
    {
    const double variate = generator->GetVariate();
    sum  += variate;
    sum2 += variate * variate;
    }

  const double mean     = sum  / numberOfSamples;
  const double variance = sum2 / numberOfSamples - mean * mean;

  std::cout << "Mean     = " << mean     << std::endl;
  std::cout << "Variance = " << variance << std::endl;


  // Test values for tolerance...
  const double tolerance = 1e-2;

  if( vnl_math_abs( mean - 0.0f ) > tolerance )
    {
    std::cout << "Mean is out of tolerance = " << tolerance << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  if( vnl_math_abs( variance - 1.0f ) > tolerance )
    {
    std::cout << "Variance is out of tolerance = " << tolerance << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;

}


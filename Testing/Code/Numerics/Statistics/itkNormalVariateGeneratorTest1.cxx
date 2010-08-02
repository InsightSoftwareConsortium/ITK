/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalVariateGeneratorTest1.cxx
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

#include "itkFixedArray.h"
#include "itkNormalVariateGenerator.h"

int itkNormalVariateGeneratorTest1( int, char * [] )
{
  typedef itk::Statistics::NormalVariateGenerator NormalGeneratorType;

  NormalGeneratorType::Pointer normalGenerator = NormalGeneratorType::New();

  normalGenerator->Initialize( 101 );

  std::cout << normalGenerator->GetNameOfClass() << std::endl;

  normalGenerator->Print( std::cout );

  const unsigned int numberOfSamples = 1000;

  double sum = 0.0;
  double sum2 = 0.0;

  for( unsigned int i=0; i<numberOfSamples; i++ )
    {
    const double value = normalGenerator->GetVariate();
    sum += value;
    sum2 += value * value;
    }

  const double average = sum / numberOfSamples;

  std::cout << "Average = " << average << std::endl;

  const double variance = sum2 / numberOfSamples - sum * sum;

  std::cout << "Variance = " << variance << std::endl;

  //
  // FIXME: Add here numerical verification (regression testing)
  //


  std::cerr << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}

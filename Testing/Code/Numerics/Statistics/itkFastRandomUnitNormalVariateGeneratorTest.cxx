/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastRandomUnitNormalVariateGeneratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <itkFastRandomUnitNormalVariateGenerator.h>



int itkFastRandomUnitNormalVariateGeneratorTest(int, char**) 
{
  std::cout << "FastRandomUnitNormalVariateGenerator Test \n \n"; 
  
  typedef  float      MeasureType;

  typedef  itk::FastRandomUnitNormalVariateGenerator GeneratorType;

  GeneratorType::Pointer generator = GeneratorType::New();

    
  const unsigned long randomSeed = 14543UL; // any number to initialize the seed.

  generator->Initialize( randomSeed );

  const unsigned int numberOfSamples = 100;
  for(unsigned int i=0; i < numberOfSamples; i++)
    {
    const double variate = generator->GetNormalVariate();
    std::cout << variate << std::endl;
    }

  bool pass = true;

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDenseFrequencyContainerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkDenseFrequencyContainer.h"
#include "vnl/vnl_math.h"



int itkDenseFrequencyContainerTest(int, char* [] ) 
{
  std::cout << "DenseFrequencyContainer Test \n \n"; 
  
  typedef  float      FrequencyType;

  const FrequencyType tolerance = 1e-6;

  typedef  itk::Statistics::DenseFrequencyContainer< FrequencyType
                                                     >   DenseFrequencyContainerType;


  DenseFrequencyContainerType::Pointer container =
                                            DenseFrequencyContainerType::New();

  const unsigned int numberOfBins = 1250;

  container->Initialize( numberOfBins );




  // Test the SetFrequency() / GetFrequency() methods
  {
  std::cout << "Testing Set/Get Frequency methods...";
  for( unsigned int bin=0; bin < numberOfBins; bin++ )
    {
    // Compute any value as frequency just to test the SetFrequency() method
    const FrequencyType frequency = static_cast<FrequencyType>( bin * bin ); 
    container->SetFrequency( bin, frequency );
    }

  for( unsigned int bin=0; bin < numberOfBins; bin++ )
    {
    // Test if the values can be read back
    const FrequencyType frequency = static_cast<FrequencyType>( bin * bin ); 
    const FrequencyType stored    = container->GetFrequency( bin );
    if( vnl_math_abs( stored - frequency ) > tolerance )
      {
      std::cout << "Failed !" << std::endl;
      std::cout << "Stored Frequency in bin " << bin << " doesn't match value" << std::endl;
      std::cout << "Value is = " << stored << " value should be " << frequency << std::endl;
      return EXIT_FAILURE;
      }
    }
  std::cout << " PASSED !" << std::endl;
  }   // end of SetFrequency() / GetFrequency() test






  // Test the IncreaseFrequency() method
  {
  std::cout << "Testing IncreaseFrequency method...";
  // Try not to depend on previous tests....So, we initialize the histogram again.
  for( unsigned int bin=0; bin < numberOfBins; bin++ )
    {
    // Compute any value as frequency just to test the SetFrequency() method
    const FrequencyType frequency = static_cast<FrequencyType>( bin * bin ); 
    container->SetFrequency( bin, frequency );
    }

  // Now increment by a number (we use "bin", but any other will do it...)
  for( unsigned int bin=0; bin < numberOfBins; bin++ )
    {
    // Compute any value as frequency just to test the IncreaseFrequency() method
    const FrequencyType frequency = static_cast<FrequencyType>( bin ); 
    container->IncreaseFrequency( bin, frequency );
    }


  // Test if the values can be read back
  for( unsigned int bin=0; bin < numberOfBins; bin++ )
    {
    const FrequencyType frequency = static_cast<FrequencyType>( bin * bin + bin ); 
    const FrequencyType stored    = container->GetFrequency( bin );
    if( vnl_math_abs( stored - frequency ) > tolerance )
      {
      std::cout << "Failed !" << std::endl;
      std::cout << "Stored Frequency in bin " << bin << " doesn't match value" << std::endl;
      std::cout << "Value is = " << stored << " value should be " << frequency << std::endl;
      return EXIT_FAILURE;
      }
    }
  std::cout << " PASSED !" << std::endl;
  }   // end of SetFrequency() / GetFrequency() test










  bool pass = true;

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}




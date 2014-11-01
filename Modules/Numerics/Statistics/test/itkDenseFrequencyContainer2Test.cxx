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

#include "itkDenseFrequencyContainer2.h"


int itkDenseFrequencyContainer2Test(int, char* [] )
{
  std::cout << "DenseFrequencyContainer2 Test \n \n";


  typedef  itk::Statistics::DenseFrequencyContainer2
                                            DenseFrequencyContainer2Type;


  DenseFrequencyContainer2Type::Pointer container =
                                            DenseFrequencyContainer2Type::New();

  typedef  DenseFrequencyContainer2Type::AbsoluteFrequencyType     AbsoluteFrequencyType;

  const unsigned int numberOfBins = 1250;

  container->Initialize( numberOfBins );


  // Test the SetFrequency() / GetFrequency() methods
    {
    std::cout << "Testing Set/Get Frequency methods...";
    for( unsigned int bin=0; bin < numberOfBins; bin++ )
      {
      // Compute any value as frequency just to test the SetFrequency() method
      const AbsoluteFrequencyType frequency = static_cast<AbsoluteFrequencyType>( bin * bin );
      container->SetFrequency( bin, frequency );
      }


    for( unsigned int bin=0; bin < numberOfBins; bin++ )
      {
      // Test if the values can be read back
      const AbsoluteFrequencyType frequency = static_cast<AbsoluteFrequencyType>( bin * bin );
      const AbsoluteFrequencyType stored    = container->GetFrequency( bin );
      if( stored != frequency )
        {
        std::cout << "Failed !" << std::endl;
        std::cout << "Stored Frequency in bin " << bin << " doesn't match value" << std::endl;
        std::cout << "Value is = " << stored << " value should be " << frequency << std::endl;
        return EXIT_FAILURE;
        }
      }

    //Test Set/Get frequency of a out of bound bin
    unsigned int binOutOfBound = numberOfBins;
    const AbsoluteFrequencyType frequency = static_cast<AbsoluteFrequencyType>( binOutOfBound * binOutOfBound );

    if ( container->SetFrequency( binOutOfBound, frequency ) )
      {
      std::cerr << "SetFrequency() method should have returned false boolean\
                    since the bin index is out of bound \n" << std::endl;
      return EXIT_FAILURE;
      }

    if ( container->GetFrequency( binOutOfBound ) != itk::NumericTraits< AbsoluteFrequencyType >::ZeroValue() )
      {
      std::cerr << "GetFrequency() method should have returned zero frequency\
                    since the bin index is out of bound \n" << std::endl;
      return EXIT_FAILURE;
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
      const AbsoluteFrequencyType frequency = static_cast<AbsoluteFrequencyType>( bin * bin );
      container->SetFrequency( bin, frequency );
      }

    // Now increment by a number (we use "bin", but any other will do it...)
    for( unsigned int bin=0; bin < numberOfBins; bin++ )
      {
      // Compute any value as frequency just to test the IncreaseFrequency() method
      const AbsoluteFrequencyType frequency = static_cast<AbsoluteFrequencyType>( bin );
      container->IncreaseFrequency( bin, frequency );
      }


    // Test if the values can be read back
    for( unsigned int bin=0; bin < numberOfBins; bin++ )
      {
      const AbsoluteFrequencyType frequency = static_cast<AbsoluteFrequencyType>( bin * bin + bin );
      const AbsoluteFrequencyType stored    = container->GetFrequency( bin );
      if( stored != frequency )
        {
        std::cout << "Failed !" << std::endl;
        std::cout << "Stored Frequency in bin " << bin << " doesn't match value" << std::endl;
        std::cout << "Value is = " << stored << " value should be " << frequency << std::endl;
        return EXIT_FAILURE;
        }
      }
    unsigned int binOutOfBound = numberOfBins;
    const AbsoluteFrequencyType frequency = static_cast<AbsoluteFrequencyType>( binOutOfBound );

    if ( container->IncreaseFrequency( binOutOfBound, frequency ) )
      {
      std::cerr << "IncreaseFrequency() method should have returned a false boolean\
                    since the bin index is out of bound \n" << std::endl;
      return EXIT_FAILURE;

      }

    std::cout << " PASSED !" << std::endl;
    }   // end of SetFrequency() / GetFrequency() test

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}

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


#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkTestingMacros.h"

#include <iostream>


int itkMersenneTwisterRandomVariateGeneratorTest( int, char* [] )
{

  typedef itk::Statistics::MersenneTwisterRandomVariateGenerator Twister;

  Twister::IntegerType seed = 1234;

  // Test Get/SetSeed
  Twister::GetInstance()->SetSeed( seed );
  TEST_SET_GET_VALUE( seed, Twister::GetInstance()->GetSeed() );

  Twister::Pointer twister = Twister::New();

  EXERCISE_BASIC_OBJECT_METHODS( twister, MersenneTwisterRandomVariateGenerator,
    RandomVariateGeneratorBase );

  // Does the new instance have the same seed?
  if ( Twister::GetInstance()->GetSeed()+1 != twister->GetSeed() )
    {
    std::cerr << "New instance does not have the next seed!" << std::endl;
    return EXIT_FAILURE;
    }

  twister->SetSeed( Twister::GetInstance()->GetSeed());

  // Check that we get the same series of numbers from the two.  Use integers.
  for ( int i = 0; i < 200; i++ )
    {
    if ( Twister::GetInstance()->GetIntegerVariate() != twister->GetIntegerVariate() )
      {
      std::cerr << "Singleton and new instance deviated at " << i << "th iteration" << std::endl;
      return EXIT_FAILURE;
      }
    }

  // Ensure we get the same series of numbers
  const Twister::IntegerType expected[5] = { Twister::IntegerType(3294740812u),
                                             Twister::IntegerType(4175194053u),
                                             Twister::IntegerType(3041332341u),
                                             Twister::IntegerType(199851601u),
                                             Twister::IntegerType(3422518480u) };

  bool sameSequence = true;
  for ( int i = 0; i < 5; i++ )
    {
    Twister::IntegerType actual = twister->GetIntegerVariate();
    if ( actual != expected[i] )
      {
      std::cout << "GetIntegerVariate: expected " << expected[i] << " got " << actual << std::endl;
      sameSequence = false;
      }
    }
  if ( !sameSequence )
    {
    return EXIT_FAILURE;
    }

  // Do we get roughly zero mean and unit variance?
  // NB: requires a large number of iterations to have variance converge...
  double sum = 0.0;
  double sum2 = 0.0;
  int count = 500000;
  for ( int i = 0; i < count; i++ )
    {
    double v = twister->GetNormalVariate();
    sum += v;
    sum2 += v * v;
    }
  double mean = sum / (double) count;
  double variance = sum2 / (double) count - mean * mean;
  if ( fabs ( mean ) > 0.01 )
    {
      std::cerr << "Mean was " << mean << " expected 0.0 " << std::endl;
      return EXIT_FAILURE;
    }
  if ( fabs ( variance - 1.0 ) > 0.01 )
    {
      std::cerr << "Variance was " << variance << " expected 1.0 " << std::endl;
      return EXIT_FAILURE;
    }

  twister->Initialize();
  twister->SetSeed( 1234 );
  const Twister::IntegerType withSetSeed = twister->GetIntegerVariate();
  twister->Initialize( 1234 );
  const Twister::IntegerType withInitialize = twister->GetIntegerVariate();
  if( withSetSeed != withInitialize )
    {
    std::cerr << "Result initializing with SetSeed() did not equal result with Initialize()." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

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

#include "itkMath.h"
#include "itkIntTypes.h"
#include "itkStdStreamStateSave.h"
#include "itkTestingMacros.h"

#include <iostream>

int main( int, char *[] )
{
// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
// scope.
  itk::StdStreamStateSave coutState(std::cout);

  std::cout << "e: " << itk::Math::e << std::endl;
  std::cout << "log2e: " << itk::Math::log2e << std::endl;
  std::cout << "log10e: " << itk::Math::log10e << std::endl;
  std::cout << "ln2: " << itk::Math::ln2 << std::endl;
  std::cout << "pi: " << itk::Math::pi << std::endl;
  std::cout << "pi_over_2: " << itk::Math::pi_over_2 << std::endl;
  std::cout << "two_over_pi: " << itk::Math::two_over_pi << std::endl;
  std::cout << "two_over_sqrtpi: " << itk::Math::two_over_sqrtpi << std::endl;
  std::cout << "one_over_sqrt2pi: " << itk::Math::one_over_sqrt2pi << std::endl;
  std::cout << "sqrt2: " << itk::Math::sqrt2 << std::endl;
  std::cout << "sqrt1_2: " << itk::Math::sqrt1_2 << std::endl;


  std::cout << itk::Math::e*itk::Math::log2e*
    itk::Math::log10e*itk::Math::ln2*
    itk::Math::pi*itk::Math::pi_over_2*
    itk::Math::pi_over_4*itk::Math::one_over_pi*
    itk::Math::two_over_pi*itk::Math::two_over_sqrtpi*
    itk::Math::one_over_sqrt2pi*itk::Math::sqrt2*
    itk::Math::sqrt1_2 << std::endl;


  std::cout << "\n\n\n===Testing FloatAlmostEqual===\n" << std::endl;
  union FloatRepresentationF
    {
    float asFloat;
    itk::int32_t asInt;
    };

  FloatRepresentationF floatRepresentationfx1;
  floatRepresentationfx1.asFloat = -1.0f;
  std::cout << "floatRepresentationfx1.asFloat: " << floatRepresentationfx1.asFloat << std::endl;
  std::cout << "floatRepresentationfx1.asInt:   " << floatRepresentationfx1.asInt << std::endl;

  FloatRepresentationF floatRepresentationfx2;
  floatRepresentationfx2.asFloat = floatRepresentationfx1.asFloat;
  floatRepresentationfx2.asInt -= 1; // makes it 1 *higher* because it is a negative sign-magnitude integer!
  std::cout << "floatRepresentationfx2.asFloat: " << floatRepresentationfx2.asFloat << std::endl;
  std::cout << "floatRepresentationfx2.asInt:   " << floatRepresentationfx2.asInt << std::endl;
  std::cout << "Distance: " << itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) != -1 )
    {
    std::cout << "Unexpected float distance." << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::FloatAlmostEqual( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) )
    {
    std::cout << "floatRepresentationfx1 is almost equal to floatRepresentationfx2\n" << std::endl;
    }
  else
    {
    std::cout << "floatRepresentationfx1 is NOT almost equal to floatRepresentationfx2\n" << std::endl;
    return EXIT_FAILURE;
    }

  floatRepresentationfx2.asFloat = floatRepresentationfx1.asFloat;
  floatRepresentationfx2.asInt += 1; // makes it 1 *lower* because it is a negative sign-magnitude integer!
  std::cout << "floatRepresentationfx2.asFloat: " << floatRepresentationfx2.asFloat << std::endl;
  std::cout << "floatRepresentationfx2.asInt:   " << floatRepresentationfx2.asInt << std::endl;
  std::cout << "Distance: " << itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) != 1 )
    {
    std::cout << "Unexpected float distance." << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::FloatAlmostEqual( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) )
    {
    std::cout << "floatRepresentationfx1 is almost equal to floatRepresentationfx2\n" << std::endl;
    }
  else
    {
    std::cout << "floatRepresentationfx1 is NOT almost equal to floatRepresentationfx2\n" << std::endl;
    return EXIT_FAILURE;
    }

  floatRepresentationfx1.asFloat = 1.0f;
  std::cout << "floatRepresentationfx1.asFloat: " << floatRepresentationfx1.asFloat << std::endl;
  std::cout << "floatRepresentationfx1.asInt:   " << floatRepresentationfx1.asInt << std::endl;

  floatRepresentationfx2.asFloat = floatRepresentationfx1.asFloat;
  floatRepresentationfx2.asInt += 1;
  std::cout << "floatRepresentationfx2.asFloat: " << floatRepresentationfx2.asFloat << std::endl;
  std::cout << "floatRepresentationfx2.asInt:   " << floatRepresentationfx2.asInt << std::endl;
  std::cout << "Distance: " << itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) != -1 )
    {
    std::cout << " result is: " << itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) << std::endl;
    std::cout << "Unexpected float distance." << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::FloatAlmostEqual( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) )
    {
    std::cout << "floatRepresentationfx1 is almost equal to floatRepresentationfx2\n" << std::endl;
    }
  else
    {
    std::cout << "floatRepresentationfx1 is NOT almost equal to floatRepresentationfx2\n" << std::endl;
    return EXIT_FAILURE;
    }

  floatRepresentationfx2.asFloat = floatRepresentationfx1.asFloat;
  floatRepresentationfx2.asInt -= 1;
  std::cout << "floatRepresentationfx2.asFloat: " << floatRepresentationfx2.asFloat << std::endl;
  std::cout << "floatRepresentationfx2.asInt:   " << floatRepresentationfx2.asInt << std::endl;
  std::cout << "Distance: " << itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) != 1 )
    {
    std::cout << "Unexpected float distance." << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::FloatAlmostEqual( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) )
    {
    std::cout << "floatRepresentationfx1 is almost equal to floatRepresentationfx2\n" << std::endl;
    }
  else
    {
    std::cout << "floatRepresentationfx1 is NOT almost equal to floatRepresentationfx2\n" << std::endl;
    return EXIT_FAILURE;
    }

  // The default maxUlps is 4, so this should be considered almost equals.
  floatRepresentationfx2.asFloat = floatRepresentationfx1.asFloat;
  floatRepresentationfx2.asInt += 6;
  std::cout << "floatRepresentationfx2.asFloat: " << floatRepresentationfx2.asFloat << std::endl;
  std::cout << "floatRepresentationfx2.asInt:   " << floatRepresentationfx2.asInt << std::endl;
  std::cout << "Distance: " << itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) != -6 )
    {
    std::cout << "Unexpected float distance." << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::FloatAlmostEqual( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) )
    {
    std::cout << "floatRepresentationfx1 is almost equal to floatRepresentationfx2\n" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "floatRepresentationfx1 is NOT almost equal to floatRepresentationfx2\n" << std::endl;
    }

  floatRepresentationfx2.asFloat = floatRepresentationfx1.asFloat;
  floatRepresentationfx2.asInt -= 6;
  std::cout << "floatRepresentationfx2.asFloat: " << floatRepresentationfx2.asFloat << std::endl;
  std::cout << "floatRepresentationfx2.asInt:   " << floatRepresentationfx2.asInt << std::endl;
  std::cout << "Distance: " << itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) != 6 )
    {
    std::cout << "Unexpected float distance." << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::FloatAlmostEqual( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) )
    {
    std::cout << "floatRepresentationfx1 is almost equal to floatRepresentationfx2\n" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "floatRepresentationfx1 is NOT almost equal to floatRepresentationfx2\n" << std::endl;
    }

  floatRepresentationfx1.asFloat = -0.0f;
  std::cout << "floatRepresentationfx1.asFloat: " << floatRepresentationfx1.asFloat << std::endl;
  std::cout << "floatRepresentationfx1.asInt:   " << floatRepresentationfx1.asInt << std::endl;
  floatRepresentationfx2.asFloat = 0.0f;
  std::cout << "floatRepresentationfx2.asFloat: " << floatRepresentationfx2.asFloat << std::endl;
  std::cout << "floatRepresentationfx2.asInt:   " << floatRepresentationfx2.asInt << std::endl;
  std::cout << "Distance: " << itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) != 0 )
    {
    std::cout << "Unexpected float distance." << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::FloatAlmostEqual( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) )
    {
    std::cout << "floatRepresentationfx1 is almost equal to floatRepresentationfx2\n" << std::endl;
    }
  else
    {
    std::cout << "floatRepresentationfx1 is NOT almost equal to floatRepresentationfx2\n" << std::endl;
    return EXIT_FAILURE;
    }

  floatRepresentationfx1.asFloat = 0.0f;
  std::cout << "floatRepresentationfx1.asFloat: " << floatRepresentationfx1.asFloat << std::endl;
  std::cout << "floatRepresentationfx1.asInt:   " << floatRepresentationfx1.asInt << std::endl;
  // Bad -- should not do this -- we should call FloatAlmostEqual on the numbers
  // directly.  As a result of our naughtiness, the maxAbsoluteDifference
  // tolerance has to be increased for the comparison to work.  Now our
  // comparison is dependent on the magnitude of the values.
  floatRepresentationfx2.asFloat = 67329.234f - 67329.242f;
  std::cout << "floatRepresentationfx2.asFloat: " << floatRepresentationfx2.asFloat << std::endl;
  std::cout << "floatRepresentationfx2.asInt:   " << floatRepresentationfx2.asInt << std::endl;
  std::cout << "Distance: " << itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) << std::endl;

  if( itk::Math::FloatAlmostEqual( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat, 4, 0.1f) )
    {
    std::cout << "floatRepresentationfx1 is almost equal to floatRepresentationfx2\n" << std::endl;
    }
  else
    {
    std::cout << "floatRepresentationfx1 is NOT almost equal to floatRepresentationfx2\n" << std::endl;
    return EXIT_FAILURE;
    }

  floatRepresentationfx1.asFloat = 1e-8f;
  std::cout << "floatRepresentationfx1.asFloat: " << floatRepresentationfx1.asFloat << std::endl;
  std::cout << "floatRepresentationfx1.asInt:   " << floatRepresentationfx1.asInt << std::endl;
  floatRepresentationfx2.asFloat = -1e-8f;
  std::cout << "floatRepresentationfx2.asFloat: " << floatRepresentationfx2.asFloat << std::endl;
  std::cout << "floatRepresentationfx2.asInt:   " << floatRepresentationfx2.asInt << std::endl;
  std::cout << "Distance: " << itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) < 0 )
    {
    std::cout << "Did not get the expected FloatDifferenceULP sign." << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "Got the expected FloatDifferenceULP sign.\n" << std::endl;
    }

  floatRepresentationfx1.asFloat = -1e-8f;
  std::cout << "floatRepresentationfx1.asFloat: " << floatRepresentationfx1.asFloat << std::endl;
  std::cout << "floatRepresentationfx1.asInt:   " << floatRepresentationfx1.asInt << std::endl;
  floatRepresentationfx2.asFloat = 1e-8f;
  std::cout << "floatRepresentationfx2.asFloat: " << floatRepresentationfx2.asFloat << std::endl;
  std::cout << "floatRepresentationfx2.asInt:   " << floatRepresentationfx2.asInt << std::endl;
  std::cout << "Distance: " << itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat ) > 0 )
    {
    std::cout << "Did not get the expected FloatDifferenceULP sign." << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "Got the expected FloatDifferenceULP sign.\n" << std::endl;
    }

  union FloatRepresentationD
    {
    double  asFloat;
    itk::int64_t asInt;
    };

  FloatRepresentationF floatRepresentationdx1;
  floatRepresentationdx1.asFloat = -1.0;
  std::cout << "floatRepresentationdx1.asFloat: " << floatRepresentationdx1.asFloat << std::endl;
  std::cout << "floatRepresentationdx1.asInt:   " << floatRepresentationdx1.asInt << std::endl;

  FloatRepresentationF floatRepresentationdx2;
  floatRepresentationdx2.asFloat = floatRepresentationdx1.asFloat;
  floatRepresentationdx2.asInt -= 1; // makes it 1 *higher* because it is a negative sign-magnitude integer!

  std::cout << "floatRepresentationdx2.asFloat: " << floatRepresentationdx2.asFloat << std::endl;
  std::cout << "floatRepresentationdx2.asInt:   " << floatRepresentationdx2.asInt << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) != -1 )
    {
    std::cout << "Unexpected float distance." << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::FloatAlmostEqual( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) )
    {
    std::cout << "floatRepresentationdx1 is almost equal to floatRepresentationdx2\n" << std::endl;
    }
  else
    {
    std::cout << "floatRepresentationdx1 is NOT almost equal to floatRepresentationdx2\n" << std::endl;
    return EXIT_FAILURE;
    }

  floatRepresentationdx2.asFloat = floatRepresentationdx1.asFloat;
  floatRepresentationdx2.asInt += 1; // makes it 1 *lower* because it is a negative sign-magnitude integer!
  std::cout << "floatRepresentationdx2.asFloat: " << floatRepresentationdx2.asFloat << std::endl;
  std::cout << "floatRepresentationdx2.asInt:   " << floatRepresentationdx2.asInt << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) != 1 )
    {
    std::cout << "Unexpected float distance." << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::FloatAlmostEqual( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) )
    {
    std::cout << "floatRepresentationdx1 is almost equal to floatRepresentationdx2\n" << std::endl;
    }
  else
    {
    std::cout << "floatRepresentationdx1 is NOT almost equal to floatRepresentationdx2\n" << std::endl;
    return EXIT_FAILURE;
    }

  // The default maxUlps is 4, so this should be considered almost equals.
  floatRepresentationdx2.asFloat = floatRepresentationdx1.asFloat;
  floatRepresentationdx2.asInt -= 6; // makes it 6 *higher* because it is a negative sign-magnitude integer!
  std::cout << "floatRepresentationdx2.asFloat: " << floatRepresentationdx2.asFloat << std::endl;
  std::cout << "floatRepresentationdx2.asInt:   " << floatRepresentationdx2.asInt << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) != -6 )
    {
    std::cout << "Unexpected float distance." << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::FloatAlmostEqual( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) )
    {
    std::cout << "floatRepresentationdx1 is almost equal to floatRepresentationdx2\n" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "floatRepresentationdx1 is NOT almost equal to floatRepresentationdx2\n" << std::endl;
    }

  floatRepresentationdx2.asFloat = floatRepresentationdx1.asFloat;
  floatRepresentationdx2.asInt += 6; // makes it 6 *lower* because it is a negative sign-magnitude integer!
  std::cout << "floatRepresentationdx2.asFloat: " << floatRepresentationdx2.asFloat << std::endl;
  std::cout << "floatRepresentationdx2.asInt:   " << floatRepresentationdx2.asInt << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) != 6 )
    {
    std::cout << "Unexpected float distance." << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::FloatAlmostEqual( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) )
    {
    std::cout << "floatRepresentationdx1 is almost equal to floatRepresentationdx2\n" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "floatRepresentationdx1 is NOT almost equal to floatRepresentationdx2\n" << std::endl;
    }

  floatRepresentationdx1.asFloat = -0.0;
  std::cout << "floatRepresentationdx1.asFloat: " << floatRepresentationdx1.asFloat << std::endl;
  std::cout << "floatRepresentationdx1.asInt:   " << floatRepresentationdx1.asInt << std::endl;
  floatRepresentationdx2.asFloat = 0.0;
  std::cout << "floatRepresentationdx2.asFloat: " << floatRepresentationdx2.asFloat << std::endl;
  std::cout << "floatRepresentationdx2.asInt:   " << floatRepresentationdx2.asInt << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) != 0 )
    {
    std::cout << "Unexpected float distance." << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::FloatAlmostEqual( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) )
    {
    std::cout << "floatRepresentationdx1 is almost equal to floatRepresentationdx2\n" << std::endl;
    }
  else
    {
    std::cout << "floatRepresentationdx1 is NOT almost equal to floatRepresentationdx2\n" << std::endl;
    return EXIT_FAILURE;
    }

  floatRepresentationdx1.asFloat = 0.0;
  std::cout << "floatRepresentationdx1.asFloat: " << floatRepresentationdx1.asFloat << std::endl;
  std::cout << "floatRepresentationdx1.asInt:   " << floatRepresentationdx1.asInt << std::endl;
  // Bad -- should not do this -- we should call FloatAlmostEqual on the numbers
  // directly.  As a result of our naughtiness, the maxAbsoluteDifference
  // tolerance has to be increased for the comparison to work.  Now our
  // comparison is dependent on the magnitude of the values.
  floatRepresentationdx2.asFloat = 67329.234f - 67329.242f;
  std::cout << "floatRepresentationdx2.asFloat: " << floatRepresentationdx2.asFloat << std::endl;
  std::cout << "floatRepresentationdx2.asInt:   " << floatRepresentationdx2.asInt << std::endl;
  if( itk::Math::FloatAlmostEqual( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat, 4, 0.1f ) )
    {
    std::cout << "floatRepresentationdx1 is almost equal to floatRepresentationdx2\n" << std::endl;
    }
  else
    {
    std::cout << "floatRepresentationdx1 is NOT almost equal to floatRepresentationdx2\n" << std::endl;
    return EXIT_FAILURE;
    }

  floatRepresentationdx1.asFloat = 1e-8f;
  std::cout << "floatRepresentationdx1.asFloat: " << floatRepresentationdx1.asFloat << std::endl;
  std::cout << "floatRepresentationdx1.asInt:   " << floatRepresentationdx1.asInt << std::endl;
  floatRepresentationdx2.asFloat = -1e-8f;
  std::cout << "floatRepresentationdx2.asFloat: " << floatRepresentationdx2.asFloat << std::endl;
  std::cout << "floatRepresentationdx2.asInt:   " << floatRepresentationdx2.asInt << std::endl;
  std::cout << "Distance: " << itk::Math::FloatDifferenceULP( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) < 0 )
    {
    std::cout << "Did not get the expected FloatDifferenceULP sign." << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "Got the expected FloatDifferenceULP sign.\n" << std::endl;
    }

  floatRepresentationdx1.asFloat = -1e-8f;
  std::cout << "floatRepresentationdx1.asFloat: " << floatRepresentationdx1.asFloat << std::endl;
  std::cout << "floatRepresentationdx1.asInt:   " << floatRepresentationdx1.asInt << std::endl;
  floatRepresentationdx2.asFloat = 1e-8f;
  std::cout << "floatRepresentationdx2.asFloat: " << floatRepresentationdx2.asFloat << std::endl;
  std::cout << "floatRepresentationdx2.asInt:   " << floatRepresentationdx2.asInt << std::endl;
  std::cout << "Distance: " << itk::Math::FloatDifferenceULP( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) << std::endl;

  if( itk::Math::FloatDifferenceULP( floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat ) > 0 )
    {
    std::cout << "Did not get the expected FloatDifferenceULP sign." << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "Got the expected FloatDifferenceULP sign.\n" << std::endl;
    }

  TEST_EXPECT_TRUE(itk::Math::IsPrime(0u) == false);
  TEST_EXPECT_TRUE(itk::Math::IsPrime(1u) == false);
  TEST_EXPECT_TRUE(itk::Math::IsPrime(2u));
  TEST_EXPECT_TRUE(itk::Math::IsPrime(3u));
  TEST_EXPECT_TRUE(itk::Math::IsPrime(4u) == false);
  TEST_EXPECT_TRUE(itk::Math::IsPrime(5u));
  TEST_EXPECT_TRUE(itk::Math::IsPrime(6u) == false);
  TEST_EXPECT_TRUE(itk::Math::IsPrime(7u));
  TEST_EXPECT_TRUE(itk::Math::IsPrime(8u) == false);
  TEST_EXPECT_TRUE(itk::Math::IsPrime(9u) == false);
  TEST_EXPECT_TRUE(itk::Math::IsPrime(10u) == false);
  TEST_EXPECT_TRUE(itk::Math::IsPrime(11u));
  TEST_EXPECT_TRUE(itk::Math::IsPrime(12u) == false);
  TEST_EXPECT_TRUE(itk::Math::IsPrime(13u));

  TEST_EXPECT_EQUAL(itk::Math::GreatestPrimeFactor(12u), 3);
  TEST_EXPECT_EQUAL(itk::Math::GreatestPrimeFactor(75u), 5);
  TEST_EXPECT_EQUAL(itk::Math::GreatestPrimeFactor(1024u), 2);

  return EXIT_SUCCESS;
}

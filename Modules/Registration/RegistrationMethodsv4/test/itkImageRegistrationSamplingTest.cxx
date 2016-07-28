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

#include "itkImageRegistrationMethodv4.h"

/*
 * Test the SetMetricSamplingPercentage and SetMetricSamplingPercentagePerLevel.
 * We only need to explicitly run the SetMetricSamplingPercentage method because it
 * invokes the SetMetricSamplingPercentagePerLevel method.
 */
int itkImageRegistrationSamplingTest( int, char *[] )
{
  typedef double                   PixelType;
  typedef itk::Image<PixelType, 2> FixedImageType;
  typedef itk::Image<PixelType, 2> MovingImageType;

  typedef itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType> RegistrationType;
  RegistrationType::Pointer registrationMethod = RegistrationType::New();

  try
    {
    registrationMethod->SetMetricSamplingPercentage( 0.1 );
    }
  catch ( itk::ExceptionObject &e )
    {
    std::cerr << "Unexpected exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned NUM_ERRORS = 3;
  RegistrationType::RealType errorValues[NUM_ERRORS] = { -0.1, 0.0, 1.1 };
  for( unsigned int i = 0; i<NUM_ERRORS; i++ )
    {
    try
      {
      registrationMethod->SetMetricSamplingPercentage( errorValues[i] );
      return EXIT_FAILURE;
      }
    catch ( itk::ExceptionObject & )
      {
      std::cerr << "Caught expected exception." << std::endl;
      }
    }
  return EXIT_SUCCESS;
}

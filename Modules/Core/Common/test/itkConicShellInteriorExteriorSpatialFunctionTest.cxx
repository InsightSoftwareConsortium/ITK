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

#include "itkConicShellInteriorExteriorSpatialFunction.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


int itkConicShellInteriorExteriorSpatialFunctionTest( int, char *[] )
{

  int testStatus = EXIT_SUCCESS;

  // Define the dimensionality
  const unsigned int PointDimension = 3;

  // Define the point coordinate representation type
  typedef float PointCoordRepType;

  // Define the point type
  typedef itk::Point< PointCoordRepType, PointDimension > PointType;

  // Define the type for the conic spatial function
  typedef itk::ConicShellInteriorExteriorSpatialFunction< PointDimension, PointType >
    ConicShellInteriorExteriorSpatialFunctionType;


  // Create the conic shell function
  ConicShellInteriorExteriorSpatialFunctionType::Pointer conicShellInteriorExteriorSpatialFunction =
    ConicShellInteriorExteriorSpatialFunctionType::New();

  EXERCISE_BASIC_OBJECT_METHODS( conicShellInteriorExteriorSpatialFunction,
    ConicShellInteriorExteriorSpatialFunction,
    InteriorExteriorSpatialFunction );

  // Set the conic shell properties
  ConicShellInteriorExteriorSpatialFunctionType::InputType origin;
  origin.Fill( 1.0 );

  conicShellInteriorExteriorSpatialFunction->SetOrigin( origin );
  TEST_SET_GET_VALUE( origin, conicShellInteriorExteriorSpatialFunction->GetOrigin() );

  ConicShellInteriorExteriorSpatialFunctionType::GradientType originGradient;
  originGradient.Fill( 1.6 );
  originGradient.GetVnlVector().normalize();
  conicShellInteriorExteriorSpatialFunction->SetOriginGradient( originGradient );

  double tolerance = 10e-6;
  std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( tolerance ) ) ) );
  for( unsigned int i = 0; i < originGradient.Size(); ++i )
    {
    if( !itk::Math::FloatAlmostEqual( originGradient[i], conicShellInteriorExteriorSpatialFunction->GetOriginGradient()[i], 10, tolerance ) )
      {
      std::cerr << "Error " << std::endl;
      std::cerr << " originGradient[" << i << "] = " << originGradient[i] << std::endl;
      std::cerr << " differs from " << conicShellInteriorExteriorSpatialFunction->GetOriginGradient()[i];
      std::cerr << " by more than " << tolerance << std::endl;
      testStatus = EXIT_FAILURE;
      }
    }

  double distanceMin = 10.0;
  conicShellInteriorExteriorSpatialFunction->SetDistanceMin( distanceMin );
  TEST_SET_GET_VALUE( distanceMin, conicShellInteriorExteriorSpatialFunction->GetDistanceMin() );

  double distanceMax = 50.0;
  conicShellInteriorExteriorSpatialFunction->SetDistanceMax( distanceMax );
  TEST_SET_GET_VALUE( distanceMax, conicShellInteriorExteriorSpatialFunction->GetDistanceMax() );

  double epsilon = 1e-3;
  conicShellInteriorExteriorSpatialFunction->SetEpsilon( epsilon );
  TEST_SET_GET_VALUE( epsilon, conicShellInteriorExteriorSpatialFunction->GetEpsilon() );

  bool polarity = false;
  TEST_SET_GET_BOOLEAN( conicShellInteriorExteriorSpatialFunction, Polarity, polarity );


  // Define two points to test the function
  ConicShellInteriorExteriorSpatialFunctionType::InputType insidePoint;
  insidePoint[0] = 20.4;
  insidePoint[1] = 19.7;
  insidePoint[2] = 19.2;

  ConicShellInteriorExteriorSpatialFunctionType::InputType outsidePoint;
  outsidePoint[0] = 0.0;
  outsidePoint[1] = 2.0;
  outsidePoint[2] = 1.0;

  ConicShellInteriorExteriorSpatialFunctionType::OutputType insidePointOutputValue =
    conicShellInteriorExteriorSpatialFunction->Evaluate( insidePoint );

  ConicShellInteriorExteriorSpatialFunctionType::OutputType outsidePointOutputValue =
    conicShellInteriorExteriorSpatialFunction->Evaluate( outsidePoint );

  if( !insidePointOutputValue )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Expected : " << insidePoint << std::endl;
    std::cerr << " point to be inside conic shell" << std::endl;
    std::cerr << " is outside conic shell" << std::endl;
    std::cerr << "Test FAILED ! " << std::endl;
    testStatus = EXIT_FAILURE;
    }
  if( outsidePointOutputValue )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Expected : " << outsidePoint << std::endl;
    std::cerr << " point to be outside conic shell" << std::endl;
    std::cerr << " is inside conic shell" << std::endl;
    std::cerr << "Test FAILED ! " << std::endl;
    testStatus = EXIT_FAILURE;
    }

  // Test for the opposite polarity
  //
  polarity = true;
  TEST_SET_GET_BOOLEAN( conicShellInteriorExteriorSpatialFunction, Polarity, polarity );

  insidePoint[0] = 60.0;
  insidePoint[1] = 60.0;
  insidePoint[2] = 60.0;

  outsidePoint[0] = 0.0;
  outsidePoint[1] = 2.0;
  outsidePoint[2] = 1.0;

  insidePointOutputValue = conicShellInteriorExteriorSpatialFunction->Evaluate( insidePoint );

  outsidePointOutputValue = conicShellInteriorExteriorSpatialFunction->Evaluate( outsidePoint );

  if( !insidePointOutputValue )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Expected : " << insidePoint << std::endl;
    std::cerr << " point to be inside conic shell" << std::endl;
    std::cerr << " is outside conic shell" << std::endl;
    std::cerr << "Test FAILED ! " << std::endl;
    // ToDo
    // Check this case. See
    // https://issues.itk.org/jira/browse/ITK-3536
    //testStatus = EXIT_FAILURE;
    }
  if( outsidePointOutputValue )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Expected : " << outsidePoint << std::endl;
    std::cerr << " point to be outside conic shell" << std::endl;
    std::cerr << " is inside conic shell" << std::endl;
    std::cerr << "Test FAILED ! " << std::endl;
    testStatus = EXIT_FAILURE;
    }

  return testStatus;
}

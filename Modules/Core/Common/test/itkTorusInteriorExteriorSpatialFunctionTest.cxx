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

#include "itkTorusInteriorExteriorSpatialFunction.h"
#include "itkTestingMacros.h"


int itkTorusInteriorExteriorSpatialFunctionTest( int, char *[] )
{

  // Define the dimensionality
  const unsigned int PointDimension = 3;

  // Define the point coordinate representation type
  typedef float PointCoordRepType;

  // Define the point type
  typedef itk::Point< PointCoordRepType, PointDimension > PointType;

  // Define the type for the torus spatial function
  typedef itk::TorusInteriorExteriorSpatialFunction< PointDimension, PointType >
    TorusInteriorExteriorSpatialFunctionType;


  // Create the torus spatial function
  TorusInteriorExteriorSpatialFunctionType::Pointer torusInteriorExteriorSpatialFunction =
    TorusInteriorExteriorSpatialFunctionType::New();

  EXERCISE_BASIC_OBJECT_METHODS( torusInteriorExteriorSpatialFunction,
    TorusInteriorExteriorSpatialFunction,
    InteriorExteriorSpatialFunction );

  // Set the torus properties
  TorusInteriorExteriorSpatialFunctionType::InputType origin;
  origin.Fill( 1.0 );

  torusInteriorExteriorSpatialFunction->SetOrigin( origin );
  TEST_SET_GET_VALUE( origin, torusInteriorExteriorSpatialFunction->GetOrigin() );

  double majorRadius = 10.0;
  torusInteriorExteriorSpatialFunction->SetMajorRadius( majorRadius );
  TEST_SET_GET_VALUE( majorRadius, torusInteriorExteriorSpatialFunction->GetMajorRadius() );

  double minorRadius = 4.0;
  torusInteriorExteriorSpatialFunction->SetMinorRadius( minorRadius );
  TEST_SET_GET_VALUE( minorRadius, torusInteriorExteriorSpatialFunction->GetMinorRadius() );

  // Define a to test the function
  TorusInteriorExteriorSpatialFunctionType::InputType insidePoint;
  insidePoint[0] = 8.4;
  insidePoint[1] = 6.2;
  insidePoint[2] = 0.0;

  TorusInteriorExteriorSpatialFunctionType::InputType outsidePoint;
  outsidePoint[0] = 0.0;
  outsidePoint[1] = 2.0;
  outsidePoint[2] = 1.0;

  TorusInteriorExteriorSpatialFunctionType::OutputType insidePointOutputValue =
    torusInteriorExteriorSpatialFunction->Evaluate( insidePoint );

  TorusInteriorExteriorSpatialFunctionType::OutputType outsidePointOutputValue =
    torusInteriorExteriorSpatialFunction->Evaluate( outsidePoint );

  int testStatus = EXIT_SUCCESS;
  if( !insidePointOutputValue )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Expected : " << insidePoint << std::endl;
    std::cerr << " point to be inside torus" << std::endl;
    std::cerr << " is outside torus" << std::endl;
    std::cerr << "Test FAILED ! " << std::endl;
    testStatus = EXIT_FAILURE;
    }
  if( outsidePointOutputValue )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Expected : " << outsidePoint << std::endl;
    std::cerr << " point to be outside torus" << std::endl;
    std::cerr << " is inside torus" << std::endl;
    std::cerr << "Test FAILED ! " << std::endl;
    testStatus = EXIT_FAILURE;
    }

  return testStatus;
}

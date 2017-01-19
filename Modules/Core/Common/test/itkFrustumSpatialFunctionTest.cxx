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

#include "itkFrustumSpatialFunction.h"
#include "itkTestingMacros.h"


int itkFrustumSpatialFunctionTest( int, char *[] )
{

  // Define the dimensionality
  const unsigned int PointDimension = 3;

  // Define the point coordinate representation type
  typedef float PointCoordRepType;

  // Define the point type
  typedef itk::Point< PointCoordRepType, PointDimension > PointType;

  // Define the type for the frustum spatial function
  typedef itk::FrustumSpatialFunction< PointDimension, PointType >
    FrustumSpatialFunctionType;

  // Create the frustum spatial function
  FrustumSpatialFunctionType::Pointer frustrumSpatialFunction =
    FrustumSpatialFunctionType::New();

  EXERCISE_BASIC_OBJECT_METHODS( frustrumSpatialFunction, FrustumSpatialFunction,
    InteriorExteriorSpatialFunction );

  // Set the frustum properties
  FrustumSpatialFunctionType::InputType apex;
  apex.Fill( 1.1 );

  frustrumSpatialFunction->SetApex( apex );
  TEST_SET_GET_VALUE( apex, frustrumSpatialFunction->GetApex() );

  double topPlane = 50.0;
  frustrumSpatialFunction->SetTopPlane( topPlane );
  TEST_SET_GET_VALUE( topPlane, frustrumSpatialFunction->GetTopPlane() );

  double bottomPlane = 10.0;
  frustrumSpatialFunction->SetBottomPlane( bottomPlane );
  TEST_SET_GET_VALUE( bottomPlane, frustrumSpatialFunction->GetBottomPlane() );

  double angleZ = 36;
  frustrumSpatialFunction->SetAngleZ( angleZ );
  TEST_SET_GET_VALUE( angleZ, frustrumSpatialFunction->GetAngleZ() );

  double apertureAngleX = 54;
  frustrumSpatialFunction->SetApertureAngleX( apertureAngleX );
  TEST_SET_GET_VALUE( apertureAngleX, frustrumSpatialFunction->GetApertureAngleX() );

  double apertureAngleY = 120;
  frustrumSpatialFunction->SetApertureAngleY( apertureAngleY );
  TEST_SET_GET_VALUE( apertureAngleY, frustrumSpatialFunction->GetApertureAngleY() );


  // Test for a rotation in the XZ plane
  //
  FrustumSpatialFunctionType::FrustumRotationPlaneType rotationPlane =
    static_cast< FrustumSpatialFunctionType::FrustumRotationPlaneType > ( 1 );
  frustrumSpatialFunction->SetRotationPlane( rotationPlane );
  TEST_SET_GET_VALUE( rotationPlane, frustrumSpatialFunction->GetRotationPlane() );

  // Define inside/outside points to test the function
  FrustumSpatialFunctionType::InputType insidePoint;
  insidePoint[0] = 20.0;
  insidePoint[1] = 15.0;
  insidePoint[2] = 1.0;

  FrustumSpatialFunctionType::InputType outsidePoint1;
  outsidePoint1[0] = 0.0;
  outsidePoint1[1] = 2.0;
  outsidePoint1[2] = 1.0;

  FrustumSpatialFunctionType::InputType outsidePoint2;
  outsidePoint2[0] = 20.0;
  outsidePoint2[1] = 2.0;
  outsidePoint2[2] = 20.0;

  FrustumSpatialFunctionType::InputType outsidePoint3;
  outsidePoint3[0] = 1.0;
  outsidePoint3[1] = 40.0;
  outsidePoint3[2] = 200.0;

  FrustumSpatialFunctionType::InputType outsidePoint4;
  outsidePoint4[0] = 20.0;
  outsidePoint4[1] = 40.0;
  outsidePoint4[2] = 1.0;

  FrustumSpatialFunctionType::OutputType insidePointOutputValue =
    frustrumSpatialFunction->Evaluate( insidePoint );

  FrustumSpatialFunctionType::OutputType outsidePointOutputValue1 =
    frustrumSpatialFunction->Evaluate( outsidePoint1 );
  FrustumSpatialFunctionType::OutputType outsidePointOutputValue2 =
    frustrumSpatialFunction->Evaluate( outsidePoint2 );

  int testStatus = EXIT_SUCCESS;
  if( !insidePointOutputValue )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Inside point: " << insidePoint << std::endl;
    std::cerr << " is outside frustum" << std::endl;
    std::cerr << "Test FAILED ! " << std::endl;
    testStatus = EXIT_FAILURE;
    }
  if( outsidePointOutputValue1 )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Expected : " << outsidePoint1 << std::endl;
    std::cerr << " point to be outside bottom/top planes" << std::endl;
    std::cerr << " is inside frustum" << std::endl;
    std::cerr << "Test FAILED ! " << std::endl;
    testStatus = EXIT_FAILURE;
    }
  if( outsidePointOutputValue2 )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Expected : " << outsidePoint2 << std::endl;
    std::cerr << " point to be outside due to aperture in X" << std::endl;
    std::cerr << " is inside frustum" << std::endl;
    std::cerr << "Test FAILED ! " << std::endl;
    testStatus = EXIT_FAILURE;
    }

  // Test for a rotation in the YZ plane
  //
  rotationPlane =
    static_cast< FrustumSpatialFunctionType::FrustumRotationPlaneType > ( 2 );
  frustrumSpatialFunction->SetRotationPlane( rotationPlane );
  TEST_SET_GET_VALUE( rotationPlane, frustrumSpatialFunction->GetRotationPlane() );

  insidePoint[0] = 20.0;
  insidePoint[1] = 15.0;
  insidePoint[2] = 1.0;

  outsidePoint1[0] = 0.0;
  outsidePoint1[1] = 2.0;
  outsidePoint1[2] = 1.0;

  outsidePoint2[0] = 20.0;
  outsidePoint2[1] = 2.0;
  outsidePoint2[2] = 20.0;

  insidePointOutputValue = frustrumSpatialFunction->Evaluate( insidePoint );

  outsidePointOutputValue1 = frustrumSpatialFunction->Evaluate( outsidePoint1 );
  outsidePointOutputValue2 = frustrumSpatialFunction->Evaluate( outsidePoint2 );

  if( !insidePointOutputValue )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Inside point: " << insidePoint << std::endl;
    std::cerr << " is outside frustum" << std::endl;
    std::cerr << "Test FAILED ! " << std::endl;
    testStatus = EXIT_FAILURE;
    }
  if( outsidePointOutputValue1 )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Expected : " << outsidePoint1 << std::endl;
    std::cerr << " point to be outside bottom/top planes" << std::endl;
    std::cerr << " is inside frustum" << std::endl;
    std::cerr << "Test FAILED ! " << std::endl;
    testStatus = EXIT_FAILURE;
    }
  if( outsidePointOutputValue2 )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Expected : " << outsidePoint2 << std::endl;
    std::cerr << " point to be outside due to aperture in Y" << std::endl;
    std::cerr << " is inside frustum" << std::endl;
    std::cerr << "Test FAILED ! " << std::endl;
    testStatus = EXIT_FAILURE;
    }

  // Test for the condition where the top plane is less than the bottom plane
  topPlane = 10.0;
  frustrumSpatialFunction->SetTopPlane( topPlane );

  bottomPlane = 50.0;
  frustrumSpatialFunction->SetBottomPlane( bottomPlane );

  outsidePointOutputValue1 = frustrumSpatialFunction->Evaluate( outsidePoint1 );
  if( outsidePointOutputValue1 )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Expected : " << outsidePoint1 << std::endl;
    std::cerr << " point to be outside bottom/top planes" << std::endl;
    std::cerr << " is inside frustum" << std::endl;
    std::cerr << "Test FAILED ! " << std::endl;
    testStatus = EXIT_FAILURE;
    }


  // Check that an exception is thrown for an unsupported rotation plane
  rotationPlane =
    static_cast< FrustumSpatialFunctionType::FrustumRotationPlaneType > ( 4 );
  frustrumSpatialFunction->SetRotationPlane( rotationPlane );
  TRY_EXPECT_EXCEPTION( frustrumSpatialFunction->Evaluate( insidePoint ) );

  return testStatus;
}

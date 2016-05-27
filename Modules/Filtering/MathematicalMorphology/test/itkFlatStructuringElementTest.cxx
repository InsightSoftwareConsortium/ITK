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

#include "itkFlatStructuringElement.h"
#include "itkTestingMacros.h"

// Helper function
template< class SEType>
bool ComputeAreaError(const SEType &k, unsigned int thickness = 0);

int itkFlatStructuringElementTest(int, char *[])
{
  int scalarRadius = 5;
  int scalarThickness = 2;
  bool radiusIsParametric = true;

  typedef itk::FlatStructuringElement< 2 > SE2Type;
  SE2Type::RadiusType r2;
  r2.Fill( scalarRadius );

  SE2Type::Self result2 = SE2Type::Self();
  result2.RadiusIsParametricOn();
  TEST_SET_GET_VALUE( true, result2.GetRadiusIsParametric() );

  SE2Type k2;

  k2 = SE2Type::Box( r2 );
  k2.Print(std::cout);

  std::cout << "2D ball of radius " << scalarRadius
  << " with radiusIsParametric mode off:" << std::endl;
  k2 = SE2Type::Ball( r2 );
  ComputeAreaError( k2 );

  // Test the radiusIsParametric mode.
  std::cout << "2D ball of radius " << scalarRadius
  << " with radiusIsParametric mode on:" << std::endl;
  k2 = SE2Type::Ball( r2, radiusIsParametric );
  ComputeAreaError( k2 );

  std::cout << "2D annulus of radius " << scalarRadius
  << " and thickness " <<  scalarThickness
  << " with radiusIsParametric mode off:" << std::endl;
  k2 = SE2Type::Annulus( r2, scalarThickness, false );
  ComputeAreaError( k2, scalarThickness );

  // Test the radiusIsParametric mode.
  std::cout << "2D annulus of radius " << scalarRadius
  << " and thickness " <<  scalarThickness
  << " with radiusIsParametric mode on:" << std::endl;
  k2 = SE2Type::Annulus( r2, scalarThickness, false, radiusIsParametric );
  ComputeAreaError( k2, scalarThickness );

  k2 = SE2Type::Polygon( r2, 2 );
  k2.Print(std::cout);
  k2 = SE2Type::Polygon( r2, 3 );
  k2.Print(std::cout);
  k2 = SE2Type::Polygon( r2, 4 );
  k2.Print(std::cout);
  k2 = SE2Type::Polygon( r2, 5 );

  typedef itk::FlatStructuringElement< 3 > SE3Type;
  SE3Type::RadiusType r3;

  SE3Type::Self result3 = SE3Type::Self();
  result3.RadiusIsParametricOff();
  TEST_SET_GET_VALUE( false, result3.GetRadiusIsParametric() );

  r3.Fill( scalarRadius );
  SE3Type k3;

  k3 = SE3Type::Box( r3 );
  //k3.Print(std::cout);

  std::cout << "3D ball of radius " << scalarRadius
  << " with radiusIsParametric mode off:" << std::endl;
  k3 = SE3Type::Ball( r3 );
  ComputeAreaError( k3 );

  // Test the radiusIsParametric mode.
  std::cout << "3D ball of radius " << scalarRadius
  << " with radiusIsParametric mode on:" << std::endl;
  k3 = SE3Type::Ball( r3, radiusIsParametric );
  ComputeAreaError( k3 );

  std::cout << "3D annulus of radius " << scalarRadius
  << " and thickness " <<  scalarThickness
  << " with radiusIsParametric mode off:" << std::endl;
  k3 = SE3Type::Annulus( r3, scalarThickness, false );
  ComputeAreaError( k3, scalarThickness );

  // Test the radiusIsParametric mode.
  std::cout << "3D annulus of radius " << scalarRadius
  << " and thickness " <<  scalarThickness
  << " with radiusIsParametric mode on:" << std::endl;
  k3 = SE3Type::Annulus( r3, scalarThickness, false, radiusIsParametric );
  ComputeAreaError( k3, scalarThickness );

  k3 = SE3Type::Polygon( r3, 6 );
  k3.Print(std::cout);
  k3 = SE3Type::Polygon( r3, 7 );
  k3.Print(std::cout);
  k3 = SE3Type::Polygon( r3, 10 );
  k3.Print(std::cout);
  k3 = SE3Type::Polygon( r3, 16 );
  k3.Print(std::cout);

  bool catched = false;
  try
    {
    k3 = SE3Type::Polygon( r3, 200 );
    }
  catch(...)
    {
    catched = true;
    std::cout << "expected exception catched." << std::endl;
    }
  if( !catched )
    {
    std::cout << "expected exception NOT catched." << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::FlatStructuringElement< 4 > SE4Type;
  SE4Type::RadiusType r4;

  SE4Type::Self result4 = SE4Type::Self();
  result4.RadiusIsParametricOn();
  TEST_SET_GET_VALUE( true, result4.GetRadiusIsParametric() );

  r4.Fill( scalarRadius );
  SE4Type k4;

  k4 = SE4Type::Box( r4 );
  //k4.Print(std::cout);

  std::cout << "4D ball of radius " << scalarRadius
  << " with radiusIsParametric mode off:" << std::endl;
  k4 = SE4Type::Ball( r4 );
  ComputeAreaError( k4 );

  // Test the radiusIsParametric mode.
  std::cout << "4D ball of radius " << scalarRadius
  << " with radiusIsParametric mode on:" << std::endl;
  k4 = SE4Type::Ball( r4, radiusIsParametric );
  ComputeAreaError( k4 );

  std::cout << "4D annulus of radius " << scalarRadius
  << " and thickness " <<  scalarThickness
  << " with radiusIsParametric mode off:" << std::endl;
  k4 = SE4Type::Annulus( r4, scalarThickness, false );
  ComputeAreaError( k4, scalarThickness );

  // Test the radiusIsParametric mode.
  std::cout << "4D annulus of radius " << scalarRadius
  << " and thickness " <<  scalarThickness
  << " with radiusIsParametric mode on:" << std::endl;
  k4 = SE4Type::Annulus( r4, scalarThickness, false, radiusIsParametric );
  ComputeAreaError( k4, scalarThickness );

  catched = false;
  try
    {
    k4 = SE4Type::Polygon( r4, 2 );
    }
  catch(...)
    {
    catched = true;
    std::cout << "expected exception catched." << std::endl;
    }
  if( !catched )
    {
    std::cout << "expected exception NOT catched." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

template< class SEType >
bool ComputeAreaError(const SEType &k, unsigned int thickness)
{
  float expectedOuterForegroundArea = 1;
  float expectedInnerForegroundArea;
  if( thickness == 0 )
    {
    // Circle/Ellipse has no inner area to subract.
    expectedInnerForegroundArea = 0;
    }
  else
    {
    // Annulus does have inner area to subract.
    expectedInnerForegroundArea = 1;
    }
  if( SEType::NeighborhoodDimension == 2)
    {
    expectedOuterForegroundArea *= itk::Math::pi;
    expectedInnerForegroundArea *= itk::Math::pi;
    }
  else if( SEType::NeighborhoodDimension == 3 )
    {
    expectedOuterForegroundArea *= 4.0/3.0 * itk::Math::pi;
    expectedInnerForegroundArea *= 4.0/3.0 * itk::Math::pi;
    }
  else if ( SEType::NeighborhoodDimension == 4 )
    {
    expectedOuterForegroundArea *= 0.5 * itk::Math::pi * itk::Math::pi;
    expectedInnerForegroundArea *= 0.5 * itk::Math::pi * itk::Math::pi;
    }
  else
    {
    return EXIT_FAILURE;
    }
  for( unsigned int i = 0; i < SEType::NeighborhoodDimension; i++ )
    {
    expectedOuterForegroundArea *= k.GetRadius()[i];
    expectedInnerForegroundArea *= (k.GetRadius()[i] - thickness);
    }

  float expectedForegroundArea = expectedOuterForegroundArea - expectedInnerForegroundArea;

  // Show the neighborhood if it is 2D.
  typename SEType::ConstIterator SEIt;
  if( SEType::NeighborhoodDimension == 2 )
    {
    for( SEIt = k.Begin(); SEIt != k.End(); ++SEIt )
      {
      std::cout << *SEIt << "\t";
      if( (SEIt - k.Begin()+1) % k.GetSize()[0] == 0 )
        {
        std::cout << std::endl;
        }
      }
    }

  // Compute the area/volume.
  float computedForegroundArea = 0;
  for( SEIt = k.Begin(); SEIt != k.End(); ++SEIt )
    {
    if( *SEIt )
      {
      computedForegroundArea++;
      }
    }

  std::cout << "Expected foreground area: " << expectedForegroundArea << std::endl;
  std::cout << "Computed foreground area: " << computedForegroundArea << std::endl;
  std::cout << "Foreground area error: "
  << 100 * itk::Math::abs(expectedForegroundArea-computedForegroundArea)/expectedForegroundArea
  << "%" << "\n\n";

  return EXIT_FAILURE;
}

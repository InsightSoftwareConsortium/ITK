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

#include "itkPointsLocator.h"
#include "itkMapContainer.h"

template< typename TPointsContainer >
int testPointsLocatorTest()
{
  /**
   * Create the point set through its object factory.
   */
  const unsigned int PointDimension = 3;

  typedef itk::Point<float, PointDimension> PointType;

  typedef TPointsContainer PointsContainerType;
  typename PointsContainerType::Pointer points = PointsContainerType::New();
  points->Initialize();

  typedef itk::PointsLocator<PointsContainerType> PointsLocatorType;
  typename PointsLocatorType::Pointer pointsLocator = PointsLocatorType::New();

  /**
   * Create a simple point set structure that will create a non-degenerate
   * bounding box but will allow simple querying tests.
   */
  for( unsigned int i=1; i <= 100; ++i )
    {
    PointType testPoint;
    testPoint[0] = static_cast<float>( i );
    testPoint[1] = static_cast<float>( i );
    testPoint[2] = static_cast<float>( i );
    points->InsertElement( i - 1, testPoint );
    }

  pointsLocator->SetPoints( points );
  pointsLocator->Initialize();

  /**
   * Perform some geometric operations to see if they are working.
   */
  std::cout << "Test:  FindClosestPoint()" << std::endl;

  PointType coords;
  coords[0] = 50;
  coords[1] = 50;
  coords[2] = 50;

  typename PointsLocatorType::PointIdentifier pointId =
    pointsLocator->FindClosestPoint( coords );
  if( pointId != 49 )
    {
    std::cerr << "Error with FindClosestPoint()" << std::endl;
    return EXIT_FAILURE;
    }

  typename PointsLocatorType::NeighborsIdentifierType neighborhood;

  std::cout << "Test:  FindClosestNPoints()" << std::endl;

  pointsLocator->FindClosestNPoints( coords, 10u, neighborhood );
  if( neighborhood.size() != 10 )
    {
    std::cerr << "Error with FindClosestNPoints()" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test:  Search() 1" << std::endl;

  pointsLocator->Search( coords, 10u, neighborhood );
  if( neighborhood.size() != 10 )
    {
    std::cerr << "Error with Search() 1" << std::endl;
    return EXIT_FAILURE;
    }

  double radius = std::sqrt( 3 * itk::Math::sqr( 5.1 ) );

  std::cout << "Test:  FindPointsWithinRadius()" << std::endl;

  pointsLocator->FindPointsWithinRadius( coords, radius, neighborhood );
  if( neighborhood.size() != 11 )
    {
    std::cerr << "Error with FindPointsWithinRadius()" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test:  Search() 2" << std::endl;

  pointsLocator->Search( coords, radius, neighborhood );
  if( neighborhood.size() != 11 )
    {
    std::cerr << "Error with Search() 2" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int itkPointsLocatorTest( int, char* [] )
{
  const unsigned int PointDimension = 3;
  typedef itk::Point<float, PointDimension> PointType;

  typedef itk::VectorContainer<unsigned int, PointType> VectorContainerType;
  typedef itk::MapContainer< unsigned int, PointType >  MapContainerType;

  std::cout << "VectorContainerType" << std::endl;
  if( testPointsLocatorTest< VectorContainerType >() == EXIT_FAILURE )
    {
    std::cerr << "### FAILURE" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "--- SUCCESS" << std::endl;
  std::cout << std::endl;

  std::cout << "MapContainerType" << std::endl;
  if( testPointsLocatorTest< MapContainerType >() == EXIT_FAILURE )
    {
    std::cerr << "### FAILURE" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "--- SUCCESS" << std::endl;

  return EXIT_SUCCESS;
}

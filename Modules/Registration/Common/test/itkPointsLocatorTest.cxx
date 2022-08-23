/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkTestingMacros.h"

template <typename TPointsContainer>
int
testPointsLocatorTest()
{
  /**
   * Create the point set through its object factory.
   */
  constexpr unsigned int PointDimension = 3;

  using PointType = itk::Point<float, PointDimension>;

  using PointsContainerType = TPointsContainer;
  auto points = PointsContainerType::New();
  points->Initialize();

  using PointsLocatorType = itk::PointsLocator<PointsContainerType>;
  auto pointsLocator = PointsLocatorType::New();

  /**
   * Create a simple point set structure that will create a non-degenerate
   * bounding box but will allow simple querying tests.
   */
  for (unsigned int i = 1; i <= 100; ++i)
  {
    PointType testPoint;
    testPoint[0] = static_cast<float>(i);
    testPoint[1] = static_cast<float>(i);
    testPoint[2] = static_cast<float>(i);
    points->InsertElement(i - 1, testPoint);
  }

  pointsLocator->SetPoints(points);
  ITK_TEST_SET_GET_VALUE(points, pointsLocator->GetPoints());

  pointsLocator->Initialize();

  /**
   * Perform some geometric operations to see if they are working.
   */
  std::cout << "Test:  FindClosestPoint()" << std::endl;

  PointType coords;
  coords[0] = 50;
  coords[1] = 50;
  coords[2] = 50;

  typename PointsLocatorType::PointIdentifier pointId = pointsLocator->FindClosestPoint(coords);
  if (pointId != 49)
  {
    std::cerr << "Error with FindClosestPoint(), poindId does not match" << std::endl;
    return EXIT_FAILURE;
  }

  typename PointsLocatorType::NeighborsIdentifierType neighborhood;

  std::cout << "Test:  FindClosestNPoints()" << std::endl;

  pointsLocator->FindClosestNPoints(coords, 10u, neighborhood);
  if (neighborhood.size() != 10)
  {
    std::cerr << "Error with FindClosestNPoints(), size of returned points does not match" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test:  FindClosestNPoints() with distances  (1)" << std::endl;
  std::vector<double> distances;
  pointsLocator->FindClosestNPoints(coords, 10u, neighborhood, distances);
  if (neighborhood.size() != 10 || distances.size() != 10)
  {
    std::cerr << "Error with FindClosestNPoints(), size of returned points or distances does not match" << std::endl;
    return EXIT_FAILURE;
  }

  // Check if returned distances are correct
  for (unsigned int i = 0; i < neighborhood.size(); ++i)
  {
    auto dist = coords.EuclideanDistanceTo(points->GetElement(neighborhood[i]));
    ITK_TEST_EXPECT_EQUAL(dist, distances[i]);
    std::cout << dist << " + " << distances[i] << std::endl;
  }

  std::cout << "Test:  FindClosestNPoints() with distances  (2)" << std::endl;
  pointsLocator->FindClosestNPoints(coords, 200u, neighborhood, distances);
  if (neighborhood.size() != 100 || distances.size() != 100)
  {
    std::cerr << "Error with FindClosestNPoints(), size of returned points or distances is incorrect" << std::endl;
    return EXIT_FAILURE;
  }

  // Check if returned distances are correct
  for (unsigned int i = 0; i < neighborhood.size(); ++i)
  {
    auto dist = coords.EuclideanDistanceTo(points->GetElement(neighborhood[i]));
    ITK_TEST_EXPECT_EQUAL(dist, distances[i]);
    std::cout << dist << " * " << distances[i] << std::endl;
  }

  double radius = std::sqrt(3 * itk::Math::sqr(5.1));

  std::cout << "Test:  FindPointsWithinRadius()" << std::endl;

  pointsLocator->FindPointsWithinRadius(coords, radius, neighborhood);
  if (neighborhood.size() != 11)
  {
    std::cerr << "Error with FindPointsWithinRadius(), size of returned points within radius does not match"
              << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkPointsLocatorTest(int, char *[])
{
  constexpr unsigned int PointDimension = 3;
  using PointType = itk::Point<float, PointDimension>;

  using VectorContainerType = itk::VectorContainer<unsigned int, PointType>;
  using MapContainerType = itk::MapContainer<unsigned int, PointType>;

  std::cout << "VectorContainerType" << std::endl;
  if (testPointsLocatorTest<VectorContainerType>() == EXIT_FAILURE)
  {
    std::cerr << "### FAILURE" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "--- SUCCESS" << std::endl;
  std::cout << std::endl;

  std::cout << "MapContainerType" << std::endl;
  if (testPointsLocatorTest<MapContainerType>() == EXIT_FAILURE)
  {
    std::cerr << "### FAILURE" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "--- SUCCESS" << std::endl;

  return EXIT_SUCCESS;
}

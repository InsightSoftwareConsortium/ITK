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

#include "itkPointSet.h"
#include "vnl/vnl_sample.h"
#include "itkTestingMacros.h"
#include <iostream>

/**
 * Define a PointSet type that stores a PixelType of "int".  Use the defaults
 * for the other template parameters.
 */
using PointSet = itk::PointSet<int>;
using PointType = PointSet::PointType;
using PointsVectorContainer = PointSet::PointsVectorContainer;
using PointsVectorContainerPointer = typename PointsVectorContainer::Pointer;

/**
 * The point set that is created consists of a 100 random points.
 */

int
itkPointSetTest(int, char *[])
{
  int pointDimension = 3;
  int numOfPoints = 100;

  /**
   * Define the 3d geometric positions for 8 points in a cube.
   */
  PointSet::CoordRepType testPointCoords[3];

  /**
   * Create the point set through its object factory.
   */
  PointSet::Pointer pset(PointSet::New());

  ITK_EXERCISE_BASIC_OBJECT_METHODS(pset, PointSet, DataObject);


  // Test point container existence exception
  auto pId = static_cast<PointSet::PointIdentifier>(numOfPoints + 1);
  ITK_TRY_EXPECT_EXCEPTION(pset->GetPoint(pId));

  /**
   * Add our test points to the mesh.
   * pset->SetPoint(pointId, point)
   * Note that the constructor for Point is public, and takes an array
   * of coordinates for the point.
   */
  try
  {
    for (int i = 0; i < numOfPoints; ++i)
    {
      testPointCoords[0] = (PointSet::CoordRepType)vnl_sample_uniform(-1.0, 1.0);
      testPointCoords[1] = (PointSet::CoordRepType)vnl_sample_uniform(-1.0, 1.0);
      testPointCoords[2] = (PointSet::CoordRepType)vnl_sample_uniform(-1.0, 1.0);
      pset->SetPoint(i, PointType(testPointCoords));
    }
  }
  catch (...)
  {
    std::cerr << "Error setting points." << std::endl;
    return EXIT_FAILURE;
  }

  // Test non-existing point id exception
  ITK_TRY_EXPECT_EXCEPTION(pset->GetPoint(pId));

  PointSet::RegionType region = 0;
  pset->SetRequestedRegion(region);

  pset->SetBufferedRegion(region);
  ITK_TEST_SET_GET_VALUE(region, pset->GetBufferedRegion());

  // Clear the point set
  pset->Initialize();

  // Get the points after clearing points.
  auto pointsArrayAfter = pset->GetPoints();
  if (pointsArrayAfter->Size() != 0)
  {
    std::cerr << "Mismatch in count after clearing pointset." << std::endl;
    return EXIT_FAILURE;
  }

  // Insert the points using SetPointArray.
  PointsVectorContainerPointer pointsArray = PointsVectorContainer::New();

  // Test for in-compatible input array dimension
  pointsArray->InsertElement(0, 1.0);
  ITK_TRY_EXPECT_EXCEPTION(pset->SetPoints(pointsArray));

  pointsArray->Reserve(numOfPoints * pointDimension);

  int index = 0;
  for (int i = 0; i < numOfPoints; ++i)
  {
    pointsArray->SetElement(index++, 1.0);
    pointsArray->SetElement(index++, 2.0);
    pointsArray->SetElement(index++, 3.0);
  }

  pset->SetPoints(pointsArray);

  // Check the count of points after insertion using SetPoints.
  pointsArrayAfter = pset->GetPoints();
  if (static_cast<int>(pointsArrayAfter->Size()) != numOfPoints)
  {
    std::cerr << "Mismatch in count after inserting points." << std::endl;
    return EXIT_FAILURE;
  }

  // Insert a single point that increases the total count of points.
  pset->SetPoint(numOfPoints + 4, PointType(testPointCoords));

  // Check the count of points after inserting individual point.
  pointsArrayAfter = pset->GetPoints();
  if (static_cast<int>(pointsArrayAfter->Size()) != (numOfPoints + 5))
  {
    std::cerr << "Mismatch in count after inserting individual point" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

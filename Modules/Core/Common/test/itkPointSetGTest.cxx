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

// First include the header file to be tested:
#include "itkPointSet.h"
#include "../../QuadEdgeMesh/include/itkQuadEdgeMeshTraits.h"
#include <gtest/gtest.h>
#include <algorithm> // For equal.
#include <numeric>   // For iota.

namespace
{
template <typename TPointSet>
void
TestSetPointsByCoordinates(TPointSet & pointSet)
{
  using CoordRepType = typename TPointSet::CoordRepType;

  static constexpr auto PointDimension = TPointSet::PointDimension;

  for (unsigned int numberOfCoordinates{ 1 }; numberOfCoordinates < PointDimension; ++numberOfCoordinates)
  {
    // SetPointsByCoordinates is expected to throw an exception when the specified number of coordinates is not a
    // multiple of PointDimension.
    EXPECT_THROW(pointSet.SetPointsByCoordinates(std::vector<CoordRepType>(numberOfCoordinates)), itk::ExceptionObject);
  }

  for (const unsigned int numberOfPoints : { 2, 1, 0 })
  {
    std::vector<CoordRepType> coordinates(numberOfPoints * PointDimension);

    // Just make sure that all coordinates have different values, for the purpose of the test.
    std::iota(coordinates.begin(), coordinates.end(), CoordRepType());
    {
      const auto modifiedTime = pointSet.GetMTime();
      pointSet.SetPointsByCoordinates(coordinates);
      EXPECT_GT(pointSet.GetMTime(), modifiedTime);
    }

    using PointsContainerType = typename TPointSet::PointsContainer;
    using PointIdentifier = typename TPointSet::PointIdentifier;
    using PointType = typename TPointSet::PointType;

    const typename PointsContainerType::ConstPointer points = pointSet.GetPoints();

    ASSERT_NE(points, nullptr);
    ASSERT_EQ(points->size(), numberOfPoints);

    const typename PointsContainerType::STLContainerType & stlContainer = points->CastToSTLConstContainer();
    auto                                                   coordinateIterator = coordinates.cbegin();

    for (PointIdentifier pointIdentifier{}; pointIdentifier < numberOfPoints; ++pointIdentifier)
    {
      const PointType & point = stlContainer.at(pointIdentifier);
      EXPECT_TRUE(std::equal(point.cbegin(), point.cend(), coordinateIterator));
      coordinateIterator += PointDimension;
    }
  }
}
} // namespace


TEST(PointSet, SetPointsByCoordinates)
{
  TestSetPointsByCoordinates(*itk::PointSet<int>::New());
  TestSetPointsByCoordinates(*itk::PointSet<double, 2, itk::QuadEdgeMeshTraits<double, 2, bool, bool>>::New());
}

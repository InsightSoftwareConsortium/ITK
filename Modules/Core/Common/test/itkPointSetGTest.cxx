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
#include "itkDeref.h"
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
  using CoordinateType = typename TPointSet::CoordinateType;

  static constexpr auto PointDimension = TPointSet::PointDimension;

  for (unsigned int numberOfCoordinates{ 1 }; numberOfCoordinates < PointDimension; ++numberOfCoordinates)
  {
    // SetPointsByCoordinates is expected to throw an exception when the specified number of coordinates is not a
    // multiple of PointDimension.
    EXPECT_THROW(pointSet.SetPointsByCoordinates(std::vector<CoordinateType>(numberOfCoordinates)),
                 itk::ExceptionObject);
  }

  for (const unsigned int numberOfPoints : { 2, 1, 0 })
  {
    std::vector<CoordinateType> coordinates(numberOfPoints * PointDimension);

    // Just make sure that all coordinates have different values, for the purpose of the test.
    std::iota(coordinates.begin(), coordinates.end(), CoordinateType());
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


template <typename TPointSet>
void
TestCloneDoesDeepCopy(const TPointSet & pointSet)
{
  const auto clone = pointSet.Clone();

  // Using a const reference to the clone, because the non-const overloads of GetPointData() and GetPoints() might
  // potentially modify the point set! (Specifically when the container pointers were initially null.)
  const TPointSet & constClone = itk::Deref(clone.get());

  if (const auto * const pointData = pointSet.GetPointData())
  {
    const auto * const clonedPointData = constClone.GetPointData();

    ASSERT_NE(clonedPointData, nullptr);
    EXPECT_NE(clonedPointData, pointData) << "It should not just be a shallow copy!";
    EXPECT_EQ(clonedPointData->CastToSTLConstContainer(), pointData->CastToSTLConstContainer());
  }
  else
  {
    EXPECT_EQ(constClone.GetPointData(), nullptr);
  }

  if (const auto * const points = pointSet.GetPoints())
  {
    const auto * const clonedPoints = constClone.GetPoints();

    ASSERT_NE(clonedPoints, nullptr);
    EXPECT_NE(clonedPoints, points) << "It should not just be a shallow copy!";
    EXPECT_EQ(clonedPoints->CastToSTLConstContainer(), points->CastToSTLConstContainer());
  }
  else
  {
    EXPECT_EQ(constClone.GetPoints(), nullptr);
  }
}
} // namespace


TEST(PointSet, SetPointsByCoordinates)
{
  TestSetPointsByCoordinates(*itk::PointSet<int>::New());
  TestSetPointsByCoordinates(*itk::PointSet<double, 2, itk::QuadEdgeMeshTraits<double, 2, bool, bool>>::New());
}


// Tests that `PointSet::Graft` just copies the *pointers* to the points and the data. It does not do a "deep copy".
TEST(PointSet, GraftDoesShallowCopy)
{
  const auto check = [](const auto & pointSet) {
    const auto clone = pointSet.Clone();

    // Check that Clone() did not return null, by using itk::Deref(ptr).
    const auto & constClone = itk::Deref(clone.get());

    clone->Graft(&pointSet);

    // Expect that the pointers to the points and the data of the clone are equal to those of the original.
    EXPECT_EQ(constClone.GetPoints(), pointSet.GetPoints());
    EXPECT_EQ(constClone.GetPointData(), pointSet.GetPointData());
  };

  // First check an empty point set:
  check(*itk::PointSet<int>::New());

  // Then check a non-empty 2-D point set with `double` data:
  using PixelType = double;
  using PointSetType = itk::PointSet<PixelType, 2>;
  using PointType = PointSetType::PointType;

  const auto pointSet = PointSetType::New();
  pointSet->SetPoints(itk::MakeVectorContainer<PointType>({ PointType(), itk::MakeFilled<PointType>(1.0f) }));
  pointSet->SetPointData(itk::MakeVectorContainer<PixelType>({ 0.0, 1.0, 2.0 }));

  check(*pointSet);
}


// Tests that `PointSet::Clone` copies the points and the data. So it does a "deep copy".
TEST(PointSet, CloneDoesDeepCopy)
{
  // First check an empty point set:
  TestCloneDoesDeepCopy(*itk::PointSet<int>::New());

  // Then check a non-empty 2-D point set with `double` data:
  using PixelType = double;
  using PointSetType = itk::PointSet<PixelType, 2>;
  using PointType = PointSetType::PointType;

  const auto pointSet = PointSetType::New();
  pointSet->SetPoints(itk::MakeVectorContainer<PointType>({ PointType(), itk::MakeFilled<PointType>(1.0f) }));
  pointSet->SetPointData(itk::MakeVectorContainer<PixelType>({ 0.0, 1.0, 2.0 }));

  TestCloneDoesDeepCopy(*pointSet);
}

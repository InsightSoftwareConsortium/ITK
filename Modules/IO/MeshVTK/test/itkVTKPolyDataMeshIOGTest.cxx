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
#include "itkMeshFileReader.h"

#include "itkMesh.h"
#include "itkMeshFileWriter.h"
#include "itkVTKPolyDataMeshIO.h"

#include <cmath> // For isnan
#include <limits>
#include <random>
#include <string>
#include <vector>

#include <gtest/gtest.h>

namespace
{
template <unsigned int VDimension>
auto
MakePointOfIncreasingCoordValues()
{
  itk::Point<float, VDimension> point;
  std::iota(point.begin(), point.end(), 1.0f);
  return point;
}


// Expects that VTKPolyDataMeshIO supports writing points, and then reading them back losslessly. (When a NaN coordinate
// value is written, a NaN coordinate value is expected to be read back.)
template <typename TMesh>
void
Expect_lossless_writing_and_reading_of_points(const std::string &                            fileName,
                                              const std::vector<typename TMesh::PointType> & points,
                                              const bool                                     writeAsBinary)
{
  const auto inputMesh = TMesh::New();

  const auto inputPoints = inputMesh->GetPoints();
  ASSERT_NE(inputPoints, nullptr);
  inputPoints->assign(points.cbegin(), points.cend());

  const auto writer = itk::MeshFileWriter<TMesh>::New();
  if (writeAsBinary)
  {
    writer->SetFileTypeAsBINARY();
  }
  else
  {
    writer->SetFileTypeAsASCII();
  }
  writer->SetFileName(fileName);
  writer->SetMeshIO(itk::VTKPolyDataMeshIO::New());
  writer->SetInput(inputMesh);
  writer->Update();

  const auto reader = itk::MeshFileReader<TMesh>::New();
  reader->SetFileName(fileName);
  reader->SetMeshIO(itk::VTKPolyDataMeshIO::New());
  reader->Update();

  const auto outputMesh = reader->GetOutput();
  ASSERT_NE(outputMesh, nullptr);
  const auto outputPoints = outputMesh->GetPoints();
  ASSERT_NE(outputPoints, nullptr);

  const size_t expectedNumberOfPoints = points.size();

  EXPECT_EQ(outputPoints->size(), expectedNumberOfPoints);

  auto pointIterator = points.cbegin();

  for (const auto & outputPoint : *outputPoints)
  {
    const auto & expectedPoint = *pointIterator;

    for (size_t i = 0; i < TMesh::PointDimension; ++i)
    {
      const auto expectedCoord = expectedPoint[i];

      if (std::isnan(expectedCoord))
      {
        EXPECT_TRUE(std::isnan(outputPoint[i]));
      }
      else
      {
        EXPECT_EQ(outputPoint[i], expectedCoord);
      }
    }
    ++pointIterator;
  }
}

} // namespace


// Tests that writing points, and then reading them back is lossless (for coordinate values that are not NaN).
TEST(VTKPolyDataMeshIO, LosslessWriteAndReadOfPoints)
{
  using MeshType = itk::Mesh<int>;

  // Generate various input points that have finite coordinate values.
  const auto inputPoints = [] {
    using PointType = typename MeshType::PointType;
    using CoordRepType = typename MeshType::CoordRepType;
    using NumericLimits = std::numeric_limits<MeshType::CoordRepType>;

    std::vector<PointType> points;
    std::mt19937           randomNumberEngine;
    const auto             smallRandomValue = std::uniform_real_distribution<CoordRepType>{}(randomNumberEngine);
    const auto             largeRandomValue =
      std::uniform_real_distribution<CoordRepType>{ CoordRepType{ 1 }, NumericLimits::max() }(randomNumberEngine);

    // Include random and boundary values with the test input.
    for (const CoordRepType coordValue : { smallRandomValue,
                                           largeRandomValue,
                                           CoordRepType{ 0 },
                                           NumericLimits::denorm_min(),
                                           NumericLimits::min(),
                                           NumericLimits::epsilon(),
                                           CoordRepType{ 1 },
                                           NumericLimits::max(),
                                           NumericLimits::infinity() })
    {
      points.push_back(itk::MakeFilled<PointType>(coordValue));
      points.push_back(itk::MakeFilled<PointType>(-coordValue));
    }
    return points;
  }();


  // Test both an ASCII and a binary file type.
  for (const bool writeAsBinary : { false, true })
  {
    Expect_lossless_writing_and_reading_of_points<MeshType>(
      "VTKPolyDataMeshIOGTest_LosslessWriteAndReadOfPoints.vtk", inputPoints, writeAsBinary);
  }
}


// Tests that writing points of NaN ("Not A Number") coordinate values, and then reading them is supported (by reading
// in NaN coordinate values).
TEST(VTKPolyDataMeshIO, SupportWriteAndReadOfNaNCoordValues)
{
  for (const bool writeAsBinary : { false, true })
  {
    using MeshType = itk::Mesh<int>;
    using PointType = typename MeshType::PointType;
    using CoordRepType = typename MeshType::CoordRepType;

    Expect_lossless_writing_and_reading_of_points<MeshType>(
      "VTKPolyDataMeshIOGTest_SupportWriteAndReadOfNaNCoordValues.vtk",
      { itk::MakeFilled<PointType>(std::numeric_limits<CoordRepType>::quiet_NaN()) },
      writeAsBinary);
  }
}


// Tests that a mesh of any `PointDimension` > 1 is properly written and read back, not just 3D (the default).
TEST(VTKPolyDataMeshIO, SupportsPointDimensionsGreaterThanOne)
{
  for (const bool writeAsBinary : { false, true })
  {
    Expect_lossless_writing_and_reading_of_points<itk::Mesh<int, 2>>(
      "VTKPolyDataMeshIOGTest_Supports2D.vtk", { MakePointOfIncreasingCoordValues<2>() }, writeAsBinary);
    Expect_lossless_writing_and_reading_of_points<itk::Mesh<int, 3>>(
      "VTKPolyDataMeshIOGTest_Supports3D.vtk", { MakePointOfIncreasingCoordValues<3>() }, writeAsBinary);
    Expect_lossless_writing_and_reading_of_points<itk::Mesh<int, 4>>(
      "VTKPolyDataMeshIOGTest_Supports4D.vtk", { MakePointOfIncreasingCoordValues<4>() }, writeAsBinary);
  }
}

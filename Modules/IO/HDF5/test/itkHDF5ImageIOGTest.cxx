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
#include "itkHDF5ImageIO.h"
#include "itkMetaDataObject.h"
#include "itk_H5Cpp.h"
#include "itkGTest.h"

#include <algorithm>
#include <cstdint>
#include <string>
#include <vector>

#define _STRING(s) #s
#define TOSTRING(s) _STRING(s)

namespace
{
std::string
TestFilePath(const std::string & name)
{
  return std::string(TOSTRING(ITK_TEST_OUTPUT_DIR)) + "/itkHDF5ImageIOGTest_" + name + ".hdf5";
}

// Writes the geometry datasets of an ITK HDF5 image directly, so that a dataset
// length can disagree with the dimension count the Directions dataset implies.
void
WriteGeometryFixture(const std::string &                path,
                     const std::vector<double> &        origin,
                     const std::vector<double> &        spacing,
                     const std::vector<std::uint64_t> & dimensions,
                     const hsize_t                      directionRows = 2,
                     const hsize_t                      directionColumns = 2,
                     const hsize_t                      numberOfComponents = 1)
{
  H5::H5File file(path, H5F_ACC_TRUNC);
  file.createGroup("/ITKImage");
  file.createGroup("/ITKImage/0");

  const auto writeVector = [&file](const std::string & name, const auto & values, const H5::PredType & type) {
    const hsize_t       count = values.size();
    const H5::DataSpace space(1, &count);
    H5::DataSet         dataSet = file.createDataSet(name, type, space);
    if (count > 0)
    {
      dataSet.write(values.data(), type);
    }
  };

  writeVector("/ITKImage/0/Origin", origin, H5::PredType::NATIVE_DOUBLE);
  writeVector("/ITKImage/0/Spacing", spacing, H5::PredType::NATIVE_DOUBLE);
  writeVector("/ITKImage/0/Dimension", dimensions, H5::PredType::NATIVE_UINT64);

  std::vector<double> directions(directionRows * directionColumns, 0.0);
  for (hsize_t i = 0; i < std::min(directionRows, directionColumns); ++i)
  {
    directions[i * directionRows + i] = 1.0;
  }
  const hsize_t       directionDims[2] = { directionColumns, directionRows };
  const H5::DataSpace directionSpace(2, directionDims);
  H5::DataSet directionSet = file.createDataSet("/ITKImage/0/Directions", H5::PredType::NATIVE_DOUBLE, directionSpace);
  directionSet.write(directions.data(), H5::PredType::NATIVE_DOUBLE);

  file.createGroup("/ITKImage/0/MetaData");

  // HDF5 dimensions are listed slowest moving first (reverse of ITK), with
  // an optional trailing component dimension, matching WriteImageInformation.
  std::vector<hsize_t> voxelDims(dimensions.rbegin(), dimensions.rend());
  if (numberOfComponents > 1)
  {
    voxelDims.push_back(numberOfComponents);
  }
  hsize_t voxelCount = 1;
  for (const hsize_t d : voxelDims)
  {
    voxelCount *= d;
  }
  const std::vector<unsigned char> voxels(voxelCount, 0);
  const H5::DataSpace              voxelSpace(static_cast<int>(voxelDims.size()), voxelDims.data());
  H5::DataSet voxelSet = file.createDataSet("/ITKImage/0/VoxelData", H5::PredType::NATIVE_UCHAR, voxelSpace);
  voxelSet.write(voxels.data(), H5::PredType::NATIVE_UCHAR);
}

// Returns the description of the exception thrown by ReadImageInformation, or an empty string when it does not throw.
std::string
ReadImageInformationDescription(const std::string & path)
{
  auto io = itk::HDF5ImageIO::New();
  io->SetFileName(path);
  try
  {
    io->ReadImageInformation();
  }
  catch (const itk::ExceptionObject & error)
  {
    return error.GetDescription();
  }
  return {};
}
} // namespace

TEST(HDF5ImageIO, ReadImageInformationAcceptsConsistentGeometry)
{
  const std::string path = TestFilePath("consistent");
  ASSERT_NO_THROW(WriteGeometryFixture(path, { 0.0, 0.0 }, { 1.0, 1.0 }, { 2, 2 }));

  auto io = itk::HDF5ImageIO::New();
  io->SetFileName(path);
  ASSERT_NO_THROW(io->ReadImageInformation());
  EXPECT_EQ(io->GetNumberOfDimensions(), 2u);
}

TEST(HDF5ImageIO, ReadImageInformationRejectsTruncatedOrigin)
{
  const std::string path = TestFilePath("short_origin");
  ASSERT_NO_THROW(WriteGeometryFixture(path, { 0.0 }, { 1.0, 1.0 }, { 2, 2 }));

  EXPECT_NE(ReadImageInformationDescription(path).find("Origin has 1 entries"), std::string::npos);
}

TEST(HDF5ImageIO, ReadImageInformationRejectsTruncatedSpacing)
{
  const std::string path = TestFilePath("short_spacing");
  ASSERT_NO_THROW(WriteGeometryFixture(path, { 0.0, 0.0 }, { 1.0 }, { 2, 2 }));

  EXPECT_NE(ReadImageInformationDescription(path).find("Spacing has 1 entries"), std::string::npos);
}

TEST(HDF5ImageIO, ReadImageInformationRejectsTruncatedDimension)
{
  const std::string path = TestFilePath("short_dimension");
  ASSERT_NO_THROW(WriteGeometryFixture(path, { 0.0, 0.0 }, { 1.0, 1.0 }, { 2 }));

  EXPECT_NE(ReadImageInformationDescription(path).find("Dimension has 1 entries"), std::string::npos);
}

TEST(HDF5ImageIO, ReadImageInformationRejectsNonSquareDirections)
{
  const std::string path = TestFilePath("short_directions");
  ASSERT_NO_THROW(WriteGeometryFixture(path, { 0.0, 0.0 }, { 1.0, 1.0 }, { 2, 2 }, 2, 1));

  EXPECT_NE(ReadImageInformationDescription(path).find("Directions row has 1 entries"), std::string::npos);
}

// Regression test for https://github.com/SimpleITK/SimpleITK/issues/2702
TEST(HDF5ImageIO, ReadImageInformationInfersVectorPixelType)
{
  const std::string path = TestFilePath("vector_pixel_type");
  ASSERT_NO_THROW(WriteGeometryFixture(path, { 0.0, 0.0 }, { 1.0, 1.0 }, { 2, 2 }, 2, 2, 3));

  auto io = itk::HDF5ImageIO::New();
  io->SetFileName(path);
  ASSERT_NO_THROW(io->ReadImageInformation());
  EXPECT_EQ(io->GetNumberOfComponents(), 3u);
  EXPECT_EQ(io->GetPixelType(), itk::IOPixelEnum::VECTOR);
}

// Regression test: reusing one HDF5ImageIO instance to read a scalar dataset
// after a vector one must not retain the previous NumberOfComponents/PixelType.
TEST(HDF5ImageIO, ReadImageInformationResetsPixelTypeOnReuse)
{
  const std::string vectorPath = TestFilePath("reuse_vector");
  ASSERT_NO_THROW(WriteGeometryFixture(vectorPath, { 0.0, 0.0 }, { 1.0, 1.0 }, { 2, 2 }, 2, 2, 3));

  const std::string scalarPath = TestFilePath("reuse_scalar");
  ASSERT_NO_THROW(WriteGeometryFixture(scalarPath, { 0.0, 0.0 }, { 1.0, 1.0 }, { 2, 2 }));

  auto io = itk::HDF5ImageIO::New();

  io->SetFileName(vectorPath);
  ASSERT_NO_THROW(io->ReadImageInformation());
  ASSERT_EQ(io->GetNumberOfComponents(), 3u);
  ASSERT_EQ(io->GetPixelType(), itk::IOPixelEnum::VECTOR);

  io->SetFileName(scalarPath);
  ASSERT_NO_THROW(io->ReadImageInformation());
  EXPECT_EQ(io->GetNumberOfComponents(), 1u);
  EXPECT_EQ(io->GetPixelType(), itk::IOPixelEnum::SCALAR);
}

TEST(HDF5ImageIO, WriteImageInformationCStringMetaDataRoundTrips)
{
  const std::string path = TestFilePath("cstring_meta");

  char         value[] = "payload";
  char * const cstr = value;
  const char * constCstr = "const payload";

  {
    auto io = itk::HDF5ImageIO::New();
    io->SetFileName(path);
    io->SetNumberOfDimensions(2);
    io->SetDimensions(0, 2);
    io->SetDimensions(1, 2);
    io->SetNumberOfComponents(1);
    io->SetPixelType(itk::IOPixelEnum::SCALAR);
    io->SetComponentType(itk::IOComponentEnum::UCHAR);

    itk::EncapsulateMetaData<char *>(io->GetMetaDataDictionary(), "CStringMeta", cstr);
    itk::EncapsulateMetaData<const char *>(io->GetMetaDataDictionary(), "ConstCStringMeta", constCstr);

    ASSERT_NO_THROW(io->WriteImageInformation());
  } // io goes out of scope, closing the file it holds open.

  auto readIO = itk::HDF5ImageIO::New();
  readIO->SetFileName(path);
  ASSERT_NO_THROW(readIO->ReadImageInformation());

  std::string readValue;
  ASSERT_TRUE(itk::ExposeMetaData<std::string>(readIO->GetMetaDataDictionary(), "CStringMeta", readValue));
  EXPECT_EQ(readValue, "payload");

  std::string readConstValue;
  ASSERT_TRUE(itk::ExposeMetaData<std::string>(readIO->GetMetaDataDictionary(), "ConstCStringMeta", readConstValue));
  EXPECT_EQ(readConstValue, "const payload");
}

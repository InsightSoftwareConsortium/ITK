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
#include "gtest/gtest.h"
#include "itkMINCImageIO.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionConstIteratorWithIndex.h"

#include "itk_minc2.h"

#include <array>
#include <cstring>
#include <string>
#include <utility>
#include <vector>

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{
using ImageType = itk::Image<float, 4>;

constexpr unsigned int nx = 6;
constexpr unsigned int ny = 5;
constexpr unsigned int nt = 3;
constexpr double       tstart = 10.0;
constexpr double       tstep = 2.5;

std::string
OutputPath(const std::string & name)
{
  return TOSTRING(ITK_TEST_OUTPUT_DIR) + "/" + name;
}

itk::MINCImageIO::Pointer
MakeMINCIO()
{
  auto io = itk::MINCImageIO::New();
  io->RAStoLPSOn();
  return io;
}

// Writes a float volume with voxel values 0, 1, 2, ... in file order; dimensions are listed slowest first.
void
WriteRamp(const std::string & fileName, const std::vector<std::pair<const char *, unsigned int>> & layout)
{
  std::vector<midimhandle_t> dims(layout.size());
  std::vector<misize_t>      start(layout.size(), 0);
  std::vector<misize_t>      count;
  size_t                     numberOfVoxels = 1;
  for (size_t d = 0; d < layout.size(); ++d)
  {
    const char * name = layout[d].first;
    const bool   isTime = !std::strcmp(name, MItime);
    ASSERT_EQ(micreate_dimension(name,
                                 isTime ? MI_DIMCLASS_TIME : MI_DIMCLASS_SPATIAL,
                                 MI_DIMATTR_REGULARLY_SAMPLED,
                                 layout[d].second,
                                 &dims[d]),
              MI_NOERROR);
    if (isTime)
    {
      miset_dimension_start(dims[d], tstart);
      miset_dimension_separation(dims[d], tstep);
    }
    else if (!std::strcmp(name, MIxspace))
    {
      const std::array<double, 3> cosines{ 0.6, 0.8, 0.0 };
      miset_dimension_cosines(dims[d], cosines.data());
    }
    else if (!std::strcmp(name, MIyspace))
    {
      const std::array<double, 3> cosines{ -0.8, 0.6, 0.0 };
      miset_dimension_cosines(dims[d], cosines.data());
    }
    count.push_back(layout[d].second);
    numberOfVoxels *= layout[d].second;
  }

  // The volume takes ownership of the dimension handles.
  mihandle_t volume = nullptr;
  ASSERT_EQ(
    micreate_volume(
      fileName.c_str(), static_cast<int>(dims.size()), dims.data(), MI_TYPE_FLOAT, MI_CLASS_REAL, nullptr, &volume),
    MI_NOERROR);
  ASSERT_EQ(micreate_volume_image(volume), MI_NOERROR);

  std::vector<float> buffer(numberOfVoxels);
  for (size_t i = 0; i < buffer.size(); ++i)
  {
    buffer[i] = static_cast<float>(i);
  }
  miset_volume_range(volume, static_cast<double>(numberOfVoxels - 1), 0.0);
  ASSERT_EQ(miset_real_value_hyperslab(volume, MI_TYPE_FLOAT, start.data(), count.data(), buffer.data()), MI_NOERROR);
  ASSERT_EQ(miclose_volume(volume), MI_NOERROR);
}

template <typename TImage>
typename TImage::Pointer
ReadWithMINCIO(const std::string & fileName)
{
  auto reader = itk::ImageFileReader<TImage>::New();
  reader->SetImageIO(MakeMINCIO());
  reader->SetFileName(fileName);
  reader->Update();
  return reader->GetOutput();
}

void
ExpectTimeSeriesWithSingleSlice(const ImageType * image)
{
  const ImageType::SizeType expectedSize{ { nx, ny, 1, nt } };
  EXPECT_EQ(image->GetLargestPossibleRegion().GetSize(), expectedSize);
  EXPECT_DOUBLE_EQ(image->GetSpacing()[2], 1.0);
  EXPECT_DOUBLE_EQ(image->GetSpacing()[3], tstep);
  EXPECT_DOUBLE_EQ(image->GetOrigin()[3], tstart);

  // The missing z axis is the cross product of x and y.
  const ImageType::DirectionType direction = image->GetDirection();
  EXPECT_NEAR(direction[0][2], 0.0, 1e-12);
  EXPECT_NEAR(direction[1][2], 0.0, 1e-12);
  EXPECT_NEAR(direction[2][2], 1.0, 1e-12);
  EXPECT_NEAR(direction[3][3], 1.0, 1e-12);

  for (itk::ImageRegionConstIteratorWithIndex<ImageType> it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd();
       ++it)
  {
    const auto & index = it.GetIndex();
    ASSERT_EQ(it.Get(), static_cast<float>(index[0] + nx * (index[1] + ny * index[3]))) << "at index " << index;
  }
}
} // namespace

TEST(MINCImageIO, TimeSeriesWithoutZSpaceKeepsTimeOnAxis3)
{
  // Same dimension layout as nii2mnc writes for a single-slice NIfTI time series.
  const std::string fileName = OutputPath("itkMINCImageIOGTest_time_y_x.mnc");
  WriteRamp(fileName, { { MItime, nt }, { MIyspace, ny }, { MIxspace, nx } });

  const ImageType::Pointer image = ReadWithMINCIO<ImageType>(fileName);
  ExpectTimeSeriesWithSingleSlice(image);

  const std::string roundTrip = OutputPath("itkMINCImageIOGTest_time_y_x_roundtrip.mnc");
  auto              writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetImageIO(MakeMINCIO());
  writer->SetInput(image);
  writer->SetFileName(roundTrip);
  ASSERT_NO_THROW(writer->Update());
  ExpectTimeSeriesWithSingleSlice(ReadWithMINCIO<ImageType>(roundTrip));
}

TEST(MINCImageIO, SingleFrameTimeSeriesReadsAs3D)
{
  // Same dimension layout as mincconcat writes for one volume.
  constexpr unsigned int nz = 4;
  const std::string      fileName = OutputPath("itkMINCImageIOGTest_single_frame.mnc");
  WriteRamp(fileName, { { MItime, 1 }, { MIzspace, nz }, { MIyspace, ny }, { MIxspace, nx } });

  auto io = MakeMINCIO();
  io->SetFileName(fileName);
  ASSERT_NO_THROW(io->ReadImageInformation());
  EXPECT_EQ(io->GetNumberOfDimensions(), 3u);

  // ImageFileReader gives an identity direction when the file has more axes than the image.
  using Image3DType = itk::Image<float, 3>;
  const Image3DType::Pointer  image = ReadWithMINCIO<Image3DType>(fileName);
  const Image3DType::SizeType expectedSize{ { nx, ny, nz } };
  EXPECT_EQ(image->GetLargestPossibleRegion().GetSize(), expectedSize);
  const Image3DType::DirectionType direction = image->GetDirection();
  EXPECT_NEAR(direction[0][0], -0.6, 1e-12);
  EXPECT_NEAR(direction[1][0], -0.8, 1e-12);
  EXPECT_NEAR(direction[0][1], 0.8, 1e-12);
  EXPECT_NEAR(direction[1][1], -0.6, 1e-12);
  EXPECT_NEAR(direction[2][2], 1.0, 1e-12);

  for (itk::ImageRegionConstIteratorWithIndex<Image3DType> it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd();
       ++it)
  {
    const auto & index = it.GetIndex();
    ASSERT_EQ(it.Get(), static_cast<float>(index[0] + nx * (index[1] + ny * index[2]))) << "at index " << index;
  }
}

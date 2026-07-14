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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNiftiImageIO.h"
#include "itkVectorImage.h"

#include <string>

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{
std::string
OutputPath(const std::string & name)
{
  return TOSTRING(ITK_TEST_OUTPUT_DIR) + "/" + name;
}

template <typename TComponent>
typename itk::VectorImage<TComponent, 3>::Pointer
MakeVectorImage(const itk::Size<3> & size, unsigned int numComponents)
{
  using ImageType = itk::VectorImage<TComponent, 3>;
  auto image = ImageType::New();
  image->SetNumberOfComponentsPerPixel(numComponents);
  image->SetRegions(typename ImageType::RegionType(size));
  image->Allocate();
  return image;
}

template <typename TImage>
void
WriteWithRescale(TImage * image, const std::string & path, double slope, double intercept)
{
  auto writer = itk::ImageFileWriter<TImage>::New();
  auto io = itk::NiftiImageIO::New();
  io->SetRescaleSlope(slope);
  io->SetRescaleIntercept(intercept);
  writer->SetImageIO(io);
  writer->SetInput(image);
  writer->SetFileName(path);
  ASSERT_NO_THROW(writer->Update());
}

template <typename TImage>
typename TImage::Pointer
ReadVectorImage(const std::string & path)
{
  auto reader = itk::ImageFileReader<TImage>::New();
  reader->SetImageIO(itk::NiftiImageIO::New());
  reader->SetFileName(path);
  EXPECT_NO_THROW(reader->Update());
  return reader->GetOutput();
}
} // namespace

TEST(NiftiImageIO, RescaleCastOfMultiComponentImageStaysWithinCastBuffer)
{
  using OnDiskImageType = itk::VectorImage<short, 3>;
  using TargetImageType = itk::VectorImage<float, 3>;
  constexpr unsigned int numComponents = 4;

  const itk::Size<3>                        size{ { 5, 4, 3 } };
  auto                                      image = MakeVectorImage<short>(size, numComponents);
  itk::ImageRegionIterator<OnDiskImageType> it(image, image->GetLargestPossibleRegion());
  short                                     value = 0;
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    OnDiskImageType::PixelType pixel(numComponents);
    for (unsigned int c = 0; c < numComponents; ++c)
    {
      pixel[c] = value++;
    }
    it.Set(pixel);
  }

  const std::string path = OutputPath("b48_rescale_cast_multicomponent.nii");
  WriteWithRescale(image.GetPointer(), path, 2.0, 0.0);

  const auto readBack = ReadVectorImage<TargetImageType>(path);
  ASSERT_EQ(readBack->GetNumberOfComponentsPerPixel(), numComponents);
  ASSERT_EQ(readBack->GetLargestPossibleRegion().GetSize(), image->GetLargestPossibleRegion().GetSize());
}

TEST(NiftiImageIO, RescaleAppliesToEveryComponentOfEveryVoxel)
{
  using ImageType = itk::VectorImage<float, 3>;
  constexpr unsigned int numComponents = 3;
  constexpr double       slope = 2.0;
  constexpr double       intercept = 1.0;

  const itk::Size<3>                  size{ { 4, 3, 2 } };
  auto                                image = MakeVectorImage<float>(size, numComponents);
  itk::ImageRegionIterator<ImageType> it(image, image->GetLargestPossibleRegion());
  float                               value = 0.0f;
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    ImageType::PixelType pixel(numComponents);
    for (unsigned int c = 0; c < numComponents; ++c)
    {
      pixel[c] = value++;
    }
    it.Set(pixel);
  }

  const std::string path = OutputPath("b49_rescale_all_components.nii");
  WriteWithRescale(image.GetPointer(), path, slope, intercept);

  const auto readBack = ReadVectorImage<ImageType>(path);
  ASSERT_EQ(readBack->GetNumberOfComponentsPerPixel(), numComponents);

  itk::ImageRegionIterator<ImageType> original(image, image->GetLargestPossibleRegion());
  itk::ImageRegionIterator<ImageType> rescaled(readBack, readBack->GetLargestPossibleRegion());
  for (original.GoToBegin(), rescaled.GoToBegin(); !original.IsAtEnd(); ++original, ++rescaled)
  {
    for (unsigned int c = 0; c < numComponents; ++c)
    {
      EXPECT_FLOAT_EQ(rescaled.Get()[c], original.Get()[c] * slope + intercept);
    }
  }
}

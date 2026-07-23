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
#include "itkMetaDataObject.h"
#include "itkNiftiImageIO.h"
#include "itkVectorImage.h"
#include "itkImageRegionIterator.h"

#include <array>
#include <cmath>
#include <limits>
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

TEST(NiftiImageIO, RASConversionRejectsNonThreeComponentVector)
{
  using ImageType = itk::VectorImage<float, 3>;
  constexpr unsigned int numComponents = 2;

  const itk::Size<3>   size{ { 3, 2, 2 } };
  auto                 image = MakeVectorImage<float>(size, numComponents);
  ImageType::PixelType fillPixel(numComponents);
  fillPixel.Fill(1.0f);
  image->FillBuffer(fillPixel);

  const std::string path = OutputPath("b50_ras_non_3_component.nii");
  {
    auto writer = itk::ImageFileWriter<ImageType>::New();
    writer->SetImageIO(itk::NiftiImageIO::New());
    writer->SetInput(image);
    writer->SetFileName(path);
    ASSERT_NO_THROW(writer->Update());
  }

  auto reader = itk::ImageFileReader<ImageType>::New();
  auto io = itk::NiftiImageIO::New();
  io->SetConvertRASVectors(true);
  reader->SetImageIO(io);
  reader->SetFileName(path);
  EXPECT_THROW(reader->Update(), itk::ExceptionObject);
}

// m_ConvertRASDisplacementVectors defaults to true, so a 2-D displacement
// field (a legitimate, common layout) must not throw on a plain read with
// no explicit opt-in: the RAS<->LPS flip only applies to 3-component data
// and must simply not trigger for 2-component data (issue #6575, B50).
TEST(NiftiImageIO, TwoComponentDisplacementFieldReadsWithoutRASConversion)
{
  using ImageType = itk::VectorImage<float, 3>;
  constexpr unsigned int numComponents = 2;

  const itk::Size<3>   size{ { 3, 2, 2 } };
  auto                 image = MakeVectorImage<float>(size, numComponents);
  ImageType::PixelType fillPixel(numComponents);
  fillPixel[0] = 1.0f;
  fillPixel[1] = 2.0f;
  image->FillBuffer(fillPixel);
  itk::EncapsulateMetaData<std::string>(image->GetMetaDataDictionary(), "intent_code", "1006"); // NIFTI_INTENT_DISPVECT

  const std::string path = OutputPath("b50_dispvect_2_component.nii");
  {
    auto writer = itk::ImageFileWriter<ImageType>::New();
    writer->SetImageIO(itk::NiftiImageIO::New());
    writer->SetInput(image);
    writer->SetFileName(path);
    ASSERT_NO_THROW(writer->Update());
  }

  // Default-constructed IO: SetConvertRASDisplacementVectors is never called,
  // exercising the true default.
  auto reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(itk::NiftiImageIO::New());
  reader->SetFileName(path);
  ASSERT_NO_THROW(reader->Update());

  const ImageType::Pointer output = reader->GetOutput();
  ASSERT_EQ(output->GetNumberOfComponentsPerPixel(), numComponents);
  const ImageType::IndexType idx{ { 0, 0, 0 } };
  const ImageType::PixelType readPixel = output->GetPixel(idx);
  EXPECT_FLOAT_EQ(readPixel[0], 1.0f);
  EXPECT_FLOAT_EQ(readPixel[1], 2.0f);
}

// Issue #6575 item B52: nifti_read_buffer() replaced every non-finite value
// with 0.  NaN and +/-Inf are legal IEEE-754 values that carry meaning (e.g.
// out-of-mask voxels of a statistical map), so they must survive a round trip.
template <typename TPixel>
void
ExpectNonFiniteRoundTrip(const std::string & fileName)
{
  using ImageType = itk::Image<TPixel, 3>;
  constexpr auto quietNaN = std::numeric_limits<TPixel>::quiet_NaN();
  constexpr auto infinity = std::numeric_limits<TPixel>::infinity();

  auto image = ImageType::New();
  image->SetRegions(typename ImageType::RegionType(itk::Size<3>{ { 4, 1, 1 } }));
  image->Allocate();

  const std::array<TPixel, 4> written{ TPixel{ 1.5 }, quietNaN, infinity, -infinity };
  {
    itk::ImageRegionIterator<ImageType> it(image, image->GetLargestPossibleRegion());
    for (size_t i = 0; !it.IsAtEnd(); ++it, ++i)
    {
      it.Set(written[i]);
    }
  }

  const std::string path = OutputPath(fileName);
  {
    auto writer = itk::ImageFileWriter<ImageType>::New();
    writer->SetImageIO(itk::NiftiImageIO::New());
    writer->SetInput(image);
    writer->SetFileName(path);
    ASSERT_NO_THROW(writer->Update());
  }

  auto reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(itk::NiftiImageIO::New());
  reader->SetFileName(path);
  ASSERT_NO_THROW(reader->Update());

  const typename ImageType::Pointer        output = reader->GetOutput();
  itk::ImageRegionConstIterator<ImageType> it(output, output->GetLargestPossibleRegion());

  EXPECT_EQ(it.Get(), written[0]);
  ++it;
  EXPECT_TRUE(std::isnan(it.Get())) << "NaN was overwritten with " << it.Get();
  ++it;
  EXPECT_EQ(it.Get(), infinity) << "+Inf was overwritten with " << it.Get();
  ++it;
  EXPECT_EQ(it.Get(), -infinity) << "-Inf was overwritten with " << it.Get();
}

TEST(NiftiImageIO, NonFiniteFloatPixelsSurviveRoundTrip) { ExpectNonFiniteRoundTrip<float>("b52_nonfinite_float.nii"); }

TEST(NiftiImageIO, NonFiniteDoublePixelsSurviveRoundTrip)
{
  ExpectNonFiniteRoundTrip<double>("b52_nonfinite_double.nii");
}

// ZeroNonFinitePixels restores the pre-ITK-6 behavior for callers that relied on it.
TEST(NiftiImageIO, ZeroNonFinitePixelsOverwritesNonFiniteValues)
{
  using ImageType = itk::Image<float, 3>;
  constexpr auto quietNaN = std::numeric_limits<float>::quiet_NaN();
  constexpr auto infinity = std::numeric_limits<float>::infinity();

  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType(itk::Size<3>{ { 4, 1, 1 } }));
  image->Allocate();
  {
    const std::array<float, 4>          written{ 1.5f, quietNaN, infinity, -infinity };
    itk::ImageRegionIterator<ImageType> it(image, image->GetLargestPossibleRegion());
    for (size_t i = 0; !it.IsAtEnd(); ++it, ++i)
    {
      it.Set(written[i]);
    }
  }

  const std::string path = OutputPath("b52_nonfinite_zeroed.nii");
  {
    auto writer = itk::ImageFileWriter<ImageType>::New();
    writer->SetImageIO(itk::NiftiImageIO::New());
    writer->SetInput(image);
    writer->SetFileName(path);
    ASSERT_NO_THROW(writer->Update());
  }

  auto imageIO = itk::NiftiImageIO::New();
  EXPECT_FALSE(imageIO->GetZeroNonFinitePixels()) << "preserving non-finite values must be the default";
  imageIO->ZeroNonFinitePixelsOn();

  auto reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(imageIO);
  reader->SetFileName(path);
  ASSERT_NO_THROW(reader->Update());

  const ImageType::Pointer                 output = reader->GetOutput();
  itk::ImageRegionConstIterator<ImageType> it(output, output->GetLargestPossibleRegion());
  EXPECT_EQ(it.Get(), 1.5f);
  for (++it; !it.IsAtEnd(); ++it)
  {
    EXPECT_EQ(it.Get(), 0.0f) << "opting in should overwrite every non-finite value";
  }
}

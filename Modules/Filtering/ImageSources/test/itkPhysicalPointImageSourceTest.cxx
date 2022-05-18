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

#include "itkPhysicalPointImageSource.h"

#include "itkImageFileWriter.h"

#include "itkVectorImage.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkTestingMacros.h"


namespace
{
template <typename TImageType>
int
itkPhysicalPointImageSourceTest(const std::string &                  fname,
                                typename TImageType::SizeType &      size,
                                typename TImageType::SpacingType &   spacing,
                                typename TImageType::PointType &     origin,
                                typename TImageType::DirectionType & direction)
{

  using ImageType = TImageType;

  using PhysicalPointImageSourceType = itk::PhysicalPointImageSource<ImageType>;

  auto physicalPointImageSource = PhysicalPointImageSourceType::New();

  ITK_TRY_EXPECT_NO_EXCEPTION(physicalPointImageSource->UpdateLargestPossibleRegion());

  physicalPointImageSource->SetSize(size);
  physicalPointImageSource->SetSpacing(spacing);
  physicalPointImageSource->SetOrigin(origin);
  physicalPointImageSource->SetDirection(direction);

  ITK_TRY_EXPECT_NO_EXCEPTION(physicalPointImageSource->UpdateLargestPossibleRegion());

  using ValueImageType =
    itk::Image<typename itk::NumericTraits<typename ImageType::PixelType>::ValueType, ImageType::ImageDimension>;

  using ValueImageCastFilter = itk::VectorIndexSelectionCastImageFilter<ImageType, ValueImageType>;
  auto vif = ValueImageCastFilter::New();
  vif->SetInput(physicalPointImageSource->GetOutput());
  vif->SetIndex(0);

  using WriterType = itk::ImageFileWriter<ValueImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(fname);
  writer->SetInput(vif->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}

} // namespace
int
itkPhysicalPointImageSourceTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputImage whichTest [ theta ]" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 2;
  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, ImageDimension>;

  itk::Size<ImageDimension> size;
  size.Fill(64);

  auto                     spacing = itk::MakeFilled<ImageType::SpacingType>(1.0);
  ImageType::PointType     origin{};
  ImageType::DirectionType direction;
  direction.SetIdentity();

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  using PhysicalPointImageSourceType =
    itk::PhysicalPointImageSource<itk::Image<itk::Point<double, ImageDimension>, ImageDimension>>;

  // Instantiate the filter
  auto physicalPointImageSource = PhysicalPointImageSourceType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(physicalPointImageSource, PhysicalPointImageSource, GenerateImageSource);


  double theta = 0;
  if (argc >= 4)
  {
    theta = std::stod(argv[3]);
  }

  int testStatus = EXIT_SUCCESS;
  if (std::stoi(argv[2]) == 0)
  {
    testStatus = itkPhysicalPointImageSourceTest<itk::Image<itk::Point<double, ImageDimension>, ImageDimension>>(
      std::string(argv[1]), size, spacing, origin, direction);
  }
  else if (std::stoi(argv[2]) == 1)
  {
    testStatus = itkPhysicalPointImageSourceTest<itk::VectorImage<double, ImageDimension>>(
      std::string(argv[1]), size, spacing, origin, direction);
  }
  else if (std::stoi(argv[2]) == 2)
  {
    spacing.Fill(1.123);
    origin.Fill(-0.987);
    testStatus = itkPhysicalPointImageSourceTest<itk::Image<itk::Point<float, ImageDimension>, ImageDimension>>(
      std::string(argv[1]), size, spacing, origin, direction);
  }
  else
  {
    itk::SpacePrecisionType M[] = { std::cos(theta), -std::sin(theta), std::sin(theta), std::cos(theta) };

    direction = vnl_matrix<itk::SpacePrecisionType>(M, 2, 2);
    testStatus = itkPhysicalPointImageSourceTest<itk::VectorImage<float, ImageDimension>>(
      std::string(argv[1]), size, spacing, origin, direction);
  }

  std::cout << "Test finished" << std::endl;
  return testStatus;
}

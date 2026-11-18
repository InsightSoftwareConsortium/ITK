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

#include "itkVariationalRegistrationFilter.h"
#include "itkVariationalRegistrationMultiResolutionFilter.h"
#include "itkVariationalRegistrationDemonsFunction.h"
#include "itkVariationalRegistrationDiffusionRegularizer.h"
#include "itkVariationalRegistrationStopCriterion.h"
#include "itkVariationalRegistrationLogger.h"
#include "itkContinuousBorderWarpImageFilter.h"

#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkCastImageFilter.h"
#include "itkTestingMacros.h"

namespace
{
// Template function to fill in an image with a circle.
template <typename TImage>
void
FillWithCircle(TImage *                   image,
               const double *             center,
               double                     radius,
               typename TImage::PixelType foregnd,
               typename TImage::PixelType backgnd)
{

  using Iterator = itk::ImageRegionIteratorWithIndex<TImage>;
  Iterator it(image, image->GetBufferedRegion());
  it.GoToBegin();

  const double radiusSquared = itk::Math::sqr(radius);

  while (!it.IsAtEnd())
  {
    const auto index = it.GetIndex();
    double     distance = 0;
    for (unsigned int j = 0; j < TImage::ImageDimension; ++j)
    {
      distance += itk::Math::sqr(static_cast<double>(index[j]) - center[j]);
    }
    if (distance <= radiusSquared)
    {
      it.Set(foregnd);
    }
    else
    {
      it.Set(backgnd);
    }
    ++it;
  }
}

} // namespace

int
itkVariationalRegistrationFilterTest(int, char *[])
{
  int testStatus = EXIT_SUCCESS;

  using PixelType = unsigned char;
  constexpr unsigned int ImageDimension = 2;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using VectorType = itk::Vector<float, ImageDimension>;
  using FieldType = itk::Image<VectorType, ImageDimension>;
  using IndexType = ImageType::IndexType;
  using SizeType = ImageType::SizeType;
  using RegionType = ImageType::RegionType;

  //--------------------------------------------------------
  std::cout << "Generate input images and initial deformation field";
  std::cout << std::endl;

  constexpr auto size = SizeType::Filled(128);

  constexpr IndexType index{};

  RegionType region;
  region.SetSize(size);
  region.SetIndex(index);

  auto moving = ImageType::New();
  auto fixed = ImageType::New();
  auto initField = FieldType::New();

  moving->SetLargestPossibleRegion(region);
  moving->SetBufferedRegion(region);
  moving->Allocate();

  fixed->SetLargestPossibleRegion(region);
  fixed->SetBufferedRegion(region);
  fixed->Allocate();

  initField->SetLargestPossibleRegion(region);
  initField->SetBufferedRegion(region);
  initField->Allocate();

  double              center[ImageDimension];
  double              radius;
  constexpr PixelType fgnd = 250;
  constexpr PixelType bgnd = 15;

  // fill moving with circle
  center[0] = 64;
  center[1] = 64;
  radius = 30;
  FillWithCircle<ImageType>(moving, center, radius, fgnd, bgnd);

  // fill fixed with circle
  center[0] = 62;
  center[1] = 64;
  radius = 32;
  FillWithCircle<ImageType>(fixed, center, radius, fgnd, bgnd);

  // fill initial deformation with zero vectors
  VectorType zeroVec;
  zeroVec.Fill(0.0);
  initField->FillBuffer(zeroVec);

  using CasterType = itk::CastImageFilter<FieldType, FieldType>;
  auto caster = CasterType::New();
  caster->SetInput(initField);
  caster->InPlaceOff();

  //-------------------------------------------------------------
  std::cout << "Run registration and warp moving" << std::endl;

  // Setup registration function
  using DemonsFunctionType = itk::VariationalRegistrationDemonsFunction<ImageType, ImageType, FieldType>;
  auto demonsFunction = DemonsFunctionType::New();
  demonsFunction->SetGradientTypeToFixedImage();
  demonsFunction->SetIntensityDifferenceThreshold(0.001);
  demonsFunction->SetTimeStep(1.0);

  // Setup regularizer
  using DiffusionRegularizerType = itk::VariationalRegistrationDiffusionRegularizer<FieldType>;
  auto diffRegularizer = DiffusionRegularizerType::New();
  diffRegularizer->SetAlpha(0.1);

  // Setup registration filter
  using RegistrationFilterType = itk::VariationalRegistrationFilter<ImageType, ImageType, FieldType>;
  auto regFilter = RegistrationFilterType::New();
  regFilter->SetRegularizer(diffRegularizer);
  regFilter->SetDifferenceFunction(demonsFunction);
  regFilter->SetMovingImage(moving);
  regFilter->SetFixedImage(fixed);
  regFilter->SetNumberOfIterations(200);
  regFilter->SetInitialDisplacementField(caster->GetOutput());

  // type needed for stop criterion
  using MRRegistrationFilterType = itk::VariationalRegistrationMultiResolutionFilter<ImageType, ImageType, FieldType>;

  // Setup stop criterion
  using StopCriterionType = itk::VariationalRegistrationStopCriterion<RegistrationFilterType, MRRegistrationFilterType>;
  auto stopCriterion = StopCriterionType::New();
  stopCriterion->SetRegressionLineSlopeThreshold(0.0001);
  stopCriterion->PerformLineFittingMaxDistanceCheckOn();
  stopCriterion->SetMultiResolutionPolicyToDefault();

  regFilter->AddObserver(itk::IterationEvent(), stopCriterion);

  // Setup logger
  using LoggerType = itk::VariationalRegistrationLogger<RegistrationFilterType, MRRegistrationFilterType>;
  auto logger = LoggerType::New();

  regFilter->AddObserver(itk::IterationEvent(), logger);

  // warp moving image
  using WarperType = itk::ContinuousBorderWarpImageFilter<ImageType, ImageType, FieldType>;
  auto warper = WarperType::New();

  using CoordinateType = WarperType::CoordinateType;
  using InterpolatorType = itk::NearestNeighborInterpolateImageFunction<ImageType, CoordinateType>;
  auto interpolator = InterpolatorType::New();

  warper->SetInput(moving);
  warper->SetDisplacementField(regFilter->GetOutput());
  warper->SetInterpolator(interpolator);
  warper->SetOutputSpacing(fixed->GetSpacing());
  warper->SetOutputOrigin(fixed->GetOrigin());
  warper->SetOutputDirection(fixed->GetDirection());
  warper->SetEdgePaddingValue(bgnd);

  testStatus |= [&]() -> int {
    ITK_TRY_EXPECT_NO_EXCEPTION(warper->Update());

    std::cout << "Compare warped moving and fixed." << std::endl;

    // compare the warp and fixed images
    itk::ImageRegionIterator<ImageType> fixedIter(fixed, fixed->GetBufferedRegion());
    itk::ImageRegionIterator<ImageType> warpedIter(warper->GetOutput(), fixed->GetBufferedRegion());

    unsigned int numPixelsDifferent = 0;
    while (!fixedIter.IsAtEnd())
    {
      if (fixedIter.Get() != warpedIter.Get())
      {
        ++numPixelsDifferent;
      }
      ++fixedIter;
      ++warpedIter;
    }

    std::cout << "Number of pixels different: " << numPixelsDifferent;
    std::cout << std::endl;

    ITK_TEST_EXPECT_TRUE(numPixelsDifferent <= 10);
    return EXIT_SUCCESS;
  }();

  std::cout << "Test basic object methods.";
  std::cout << std::endl;

  testStatus |= [&]() -> int {
    ITK_EXERCISE_BASIC_OBJECT_METHODS(
      demonsFunction, VariationalRegistrationDemonsFunction, VariationalRegistrationFunction);
    return EXIT_SUCCESS;
  }();
  testStatus |= [&]() -> int {
    ITK_EXERCISE_BASIC_OBJECT_METHODS(
      diffRegularizer, VariationalRegistrationDiffusionRegularizer, VariationalRegistrationRegularizer);
    return EXIT_SUCCESS;
  }();
  testStatus |= [&]() -> int {
    ITK_EXERCISE_BASIC_OBJECT_METHODS(regFilter, VariationalRegistrationFilter, DenseFiniteDifferenceImageFilter);
    return EXIT_SUCCESS;
  }();
  testStatus |= [&]() -> int {
    ITK_EXERCISE_BASIC_OBJECT_METHODS(warper, ContinuousBorderWarpImageFilter, WarpImageFilter);
    return EXIT_SUCCESS;
  }();

  std::cout << "Test running registration without initial deformation field.";
  std::cout << std::endl;

  regFilter->ResetPipeline();
  regFilter->SetInput(nullptr);
  regFilter->SetNumberOfIterations(2);
  testStatus |= [&]() -> int {
    ITK_TRY_EXPECT_NO_EXCEPTION(regFilter->Update());
    return EXIT_SUCCESS;
  }();

  std::cout << "Test null moving image." << std::endl;
  regFilter->ResetPipeline();
  regFilter->SetInput(caster->GetOutput());
  regFilter->SetMovingImage(nullptr);
  testStatus |= [&]() -> int {
    ITK_TRY_EXPECT_EXCEPTION(regFilter->Update());
    return EXIT_SUCCESS;
  }();
  regFilter->SetMovingImage(moving);
  regFilter->ResetPipeline();

  if (testStatus == EXIT_SUCCESS)
  {
    std::cout << "Test passed" << std::endl;
  }
  return testStatus;
}

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

#include "itkFastSymmetricForcesDemonsRegistrationFilter.h"

#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkCommand.h"
#include "itkCastImageFilter.h"
#include "itkTestingMacros.h"


namespace
{
// The following class is used to support callbacks
// on the filter in the pipeline that follows later
template <typename TRegistration>
class ShowProgressObject
{
public:
  ShowProgressObject(TRegistration * o) { m_Process = o; }
  void
  ShowProgress()
  {
    std::cout << "Progress: " << m_Process->GetProgress() << "  ";
    std::cout << "Iter: " << m_Process->GetElapsedIterations() << "  ";
    std::cout << "Metric: " << m_Process->GetMetric() << "  ";
    std::cout << "RMSChange: " << m_Process->GetRMSChange() << "  ";
    std::cout << std::endl;
    if (m_Process->GetElapsedIterations() == 150)
    {
      m_Process->StopRegistration();
    }
  }
  typename TRegistration::Pointer m_Process;
};
} // namespace

// Template function to fill in an image with a circle.
template <typename TImage>
void
FillWithCircle(TImage *                   image,
               double *                   center,
               double                     radius,
               typename TImage::PixelType foregnd,
               typename TImage::PixelType backgnd)
{

  using Iterator = itk::ImageRegionIteratorWithIndex<TImage>;
  Iterator it(image, image->GetBufferedRegion());
  it.GoToBegin();

  typename TImage::IndexType index;
  double                     r2 = itk::Math::sqr(radius);

  for (; !it.IsAtEnd(); ++it)
  {
    index = it.GetIndex();
    double distance = 0;
    for (unsigned int j = 0; j < TImage::ImageDimension; ++j)
    {
      distance += itk::Math::sqr(static_cast<double>(index[j]) - center[j]);
    }
    if (distance <= r2)
    {
      it.Set(foregnd);
    }
    else
    {
      it.Set(backgnd);
    }
  }
}


// Template function to copy image regions
template <typename TImage>
void
CopyImageBuffer(TImage * input, TImage * output)
{
  using Iterator = itk::ImageRegionIteratorWithIndex<TImage>;
  Iterator inIt(input, output->GetBufferedRegion());
  Iterator outIt(output, output->GetBufferedRegion());
  for (; !inIt.IsAtEnd(); ++inIt, ++outIt)
  {
    outIt.Set(inIt.Get());
  }
}

int
itkFastSymmetricForcesDemonsRegistrationFilterTest(int, char *[])
{

  using PixelType = unsigned char;
  enum
  {
    ImageDimension = 2
  };
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using VectorType = itk::Vector<float, ImageDimension>;
  using FieldType = itk::Image<VectorType, ImageDimension>;
  using IndexType = ImageType::IndexType;
  using SizeType = ImageType::SizeType;
  using RegionType = ImageType::RegionType;

  std::cout << "Generate input images and initial deformation field" << std::endl;

  ImageType::SizeValueType sizeArray[ImageDimension] = { 128, 128 };
  SizeType                 size;
  size.SetSize(sizeArray);

  IndexType index;
  index.Fill(0);

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

  double    center[ImageDimension];
  double    radius;
  PixelType fgnd = 250;
  PixelType bgnd = 15;

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

  std::cout << "Run registration and warp moving" << std::endl;

  using RegistrationType = itk::FastSymmetricForcesDemonsRegistrationFilter<ImageType, ImageType, FieldType>;
  auto registrator = RegistrationType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    registrator, FastSymmetricForcesDemonsRegistrationFilter, PDEDeformableRegistrationFilter);


  registrator->SetInitialDisplacementField(caster->GetOutput());
  ITK_TEST_SET_GET_VALUE(caster->GetOutput(), registrator->GetInitialDisplacementField());

  registrator->SetMovingImage(moving);
  ITK_TEST_SET_GET_VALUE(moving, registrator->GetMovingImage());

  registrator->SetFixedImage(fixed);
  ITK_TEST_SET_GET_VALUE(fixed, registrator->GetFixedImage());

  auto numberOfIterations = static_cast<itk::IdentifierType>(200);
  registrator->SetNumberOfIterations(numberOfIterations);
  ITK_TEST_SET_GET_VALUE(numberOfIterations, registrator->GetNumberOfIterations());

  double                                   standardDeviationsVal = 1.0;
  RegistrationType::StandardDeviationsType standardDeviations{ standardDeviationsVal };
  registrator->SetStandardDeviations(standardDeviationsVal);
  ITK_TEST_SET_GET_VALUE(standardDeviations, registrator->GetStandardDeviations());

  registrator->SetStandardDeviations(standardDeviations);
  ITK_TEST_SET_GET_VALUE(standardDeviations, registrator->GetStandardDeviations());

  auto maximumError = 0.08;
  registrator->SetMaximumError(maximumError);
  ITK_TEST_SET_GET_VALUE(maximumError, registrator->GetMaximumError());

  unsigned int maximumKernelWidth = 10;
  registrator->SetMaximumKernelWidth(maximumKernelWidth);
  ITK_TEST_SET_GET_VALUE(maximumKernelWidth, registrator->GetMaximumKernelWidth());

  auto intensityDifferenceThreshold = 0.001;
  registrator->SetIntensityDifferenceThreshold(intensityDifferenceThreshold);
  ITK_TEST_SET_GET_VALUE(intensityDifferenceThreshold, registrator->GetIntensityDifferenceThreshold());

  auto smoothDisplacementField = true;
  ITK_TEST_SET_GET_BOOLEAN(registrator, SmoothDisplacementField, smoothDisplacementField);

  auto smoothUpdateField = false;
  ITK_TEST_SET_GET_BOOLEAN(registrator, SmoothUpdateField, smoothUpdateField);

  double                                   updateFieldStandardDeviationsVal = 1.0;
  RegistrationType::StandardDeviationsType updateFieldStandardDeviations{ updateFieldStandardDeviationsVal };
  registrator->SetUpdateFieldStandardDeviations(updateFieldStandardDeviationsVal);
  ITK_TEST_SET_GET_VALUE(updateFieldStandardDeviations, registrator->GetUpdateFieldStandardDeviations());

  registrator->SetUpdateFieldStandardDeviations(updateFieldStandardDeviations);
  ITK_TEST_SET_GET_VALUE(updateFieldStandardDeviations, registrator->GetUpdateFieldStandardDeviations());

  // turn on inplace execution
  auto inPlace = true;
  ITK_TEST_SET_GET_BOOLEAN(registrator, InPlace, inPlace);

  using FunctionType = RegistrationType::DemonsRegistrationFunctionType;
  FunctionType * fptr;
  fptr = dynamic_cast<FunctionType *>(registrator->GetDifferenceFunction().GetPointer());
  fptr->Print(std::cout);

  using ProgressType = ShowProgressObject<RegistrationType>;
  ProgressType                                    progressWatch(registrator);
  itk::SimpleMemberCommand<ProgressType>::Pointer command;
  command = itk::SimpleMemberCommand<ProgressType>::New();
  command->SetCallbackFunction(&progressWatch, &ProgressType::ShowProgress);
  registrator->AddObserver(itk::ProgressEvent(), command);

  // warp moving image
  using WarperType = itk::WarpImageFilter<ImageType, ImageType, FieldType>;
  auto warper = WarperType::New();

  using CoordRepType = WarperType::CoordRepType;
  using InterpolatorType = itk::NearestNeighborInterpolateImageFunction<ImageType, CoordRepType>;
  auto interpolator = InterpolatorType::New();


  warper->SetInput(moving);
  warper->SetDisplacementField(registrator->GetOutput());
  warper->SetInterpolator(interpolator);
  warper->SetOutputSpacing(fixed->GetSpacing());
  warper->SetOutputOrigin(fixed->GetOrigin());
  warper->SetOutputDirection(fixed->GetDirection());
  warper->SetEdgePaddingValue(bgnd);

  warper->Print(std::cout);

  warper->Update();

  std::cout << "Compare warped moving and fixed." << std::endl;

  // compare the warp and fixed images
  itk::ImageRegionIterator<ImageType> fixedIter(fixed, fixed->GetBufferedRegion());
  itk::ImageRegionIterator<ImageType> warpedIter(warper->GetOutput(), fixed->GetBufferedRegion());

  unsigned int numPixelsDifferent = 0;
  while (!fixedIter.IsAtEnd())
  {
    if (fixedIter.Get() != warpedIter.Get())
    {
      numPixelsDifferent++;
    }
    ++fixedIter;
    ++warpedIter;
  }

  std::cout << "Number of pixels different: " << numPixelsDifferent << std::endl;

  if (numPixelsDifferent > 45)
  {
    std::cout << "Test failed - too many pixels different." << std::endl;
    return EXIT_FAILURE;
  }

  registrator->Print(std::cout);


  std::cout << "Test running registrator without initial deformation field." << std::endl;

  registrator->SetInput(nullptr);
  registrator->SetNumberOfIterations(2);

  ITK_TRY_EXPECT_NO_EXCEPTION(registrator->Update());


  // Test exceptions
  std::cout << "Test exception handling." << std::endl;

  std::cout << "Test nullptr moving image. " << std::endl;

  registrator->SetInput(caster->GetOutput());
  registrator->SetMovingImage(nullptr);

  ITK_TRY_EXPECT_EXCEPTION(registrator->Update());


  registrator->SetMovingImage(moving);
  registrator->ResetPipeline();

  std::cout << "Test nullptr moving image interpolator. " << std::endl;

  fptr = dynamic_cast<FunctionType *>(registrator->GetDifferenceFunction().GetPointer());
  fptr->SetMovingImageInterpolator(nullptr);
  registrator->SetInput(initField);

  ITK_TRY_EXPECT_EXCEPTION(registrator->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}

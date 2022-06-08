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

#include "itkMultiResolutionPDEDeformableRegistration.h"
#include "itkWarpImageFilter.h"
#include "itkNearestNeighborInterpolateImageFunction.h"

#include "itkImageFileWriter.h"

#include <iostream>
#include "itkTestingMacros.h"

namespace
{

// The following class is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressPDEObject
{
public:
  ShowProgressPDEObject(itk::ProcessObject * o)
    : m_Process(o)
    , m_Prefix("")
  {}
  void
  ShowProgress()
  {
    std::cout << m_Prefix;
    std::cout << "Progress " << m_Process->GetProgress() << std::endl;
  }
  void
  ShowIteration()
  {
    std::cout << "Level Completed" << std::endl;
  }
  itk::ProcessObject::Pointer m_Process;
  std::string                 m_Prefix;
};

template <typename TRegistration>
class PDERegistrationController
{
public:
  PDERegistrationController(TRegistration * o) { m_Process = o; }
  void
  ShowProgress()
  {
    if (m_Process->GetCurrentLevel() == 3)
    {
      m_Process->StopRegistration();
    }
  }
  typename TRegistration::Pointer m_Process;
};

} // namespace


// Template function to fill in an image with a value
template <typename TImage>
void
FillImage(TImage * image, typename TImage::PixelType value)
{

  using Iterator = itk::ImageRegionIteratorWithIndex<TImage>;
  Iterator it(image, image->GetBufferedRegion());
  it.GoToBegin();

  while (!it.IsAtEnd())
  {
    it.Set(value);
    ++it;
  }
}


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

  while (!it.IsAtEnd())
  {
    index = it.GetIndex();
    double distance = 0;
    for (unsigned int j = 0; j < TImage::ImageDimension; ++j)
    {
      distance += itk::Math::sqr(static_cast<double>(index[j]) - center[j]);
    }
    if (distance <= r2)
      it.Set(foregnd);
    else
      it.Set(backgnd);
    ++it;
  }
}


// Template function to copy image regions
template <typename TImage>
void
CopyImageBuffer(TImage * input, TImage * output)
{
  using Iterator = itk::ImageRegionIteratorWithIndex<TImage>;
  Iterator outIt(output, output->GetBufferedRegion());
  for (Iterator inIt(input, output->GetBufferedRegion()); !inIt.IsAtEnd(); ++inIt, ++outIt)
  {
    outIt.Set(inIt.Get());
  }
}

int
itkMultiResolutionPDEDeformableRegistrationTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Missing parametes." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " WarpedImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 2;
  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, ImageDimension>;
  using VectorType = itk::Vector<float, ImageDimension>;
  using FieldType = itk::Image<VectorType, ImageDimension>;
  using IndexType = ImageType::IndexType;
  using SizeType = ImageType::SizeType;
  using RegionType = ImageType::RegionType;

  //--------------------------------------------------------
  std::cout << "Generate input images and initial field";
  std::cout << std::endl;

  SizeType size;
  size.Fill(256);
  size[1] = 251;

  IndexType index;
  index.Fill(0);
  index[0] = 3;

  RegionType region;
  region.SetSize(size);
  region.SetIndex(index);

  ImageType::PointType origin;
  origin.Fill(0.0);
  origin[0] = 0.8;

  ImageType::SpacingType spacing;
  spacing.Fill(1.0);
  spacing[1] = 1.2;

  auto moving = ImageType::New();
  auto fixed = ImageType::New();
  auto initField = FieldType::New();

  moving->SetLargestPossibleRegion(region);
  moving->SetBufferedRegion(region);
  moving->Allocate();
  moving->SetOrigin(origin);
  moving->SetSpacing(spacing);

  fixed->SetLargestPossibleRegion(region);
  fixed->SetBufferedRegion(region);
  fixed->Allocate();
  fixed->SetOrigin(origin);
  fixed->SetSpacing(spacing);

  initField->SetLargestPossibleRegion(region);
  initField->SetBufferedRegion(region);
  initField->Allocate();
  initField->SetOrigin(origin);
  initField->SetSpacing(spacing);

  double    center[ImageDimension];
  double    radius;
  PixelType fgnd = 250;
  PixelType bgnd = 15;

  // fill moving with circle
  center[0] = 128;
  center[1] = 128;
  radius = 60;
  FillWithCircle<ImageType>(moving, center, radius, fgnd, bgnd);

  // fill fixed with circle
  center[0] = 115;
  center[1] = 120;
  radius = 65;
  FillWithCircle<ImageType>(fixed, center, radius, fgnd, bgnd);

  // fill initial deformation with zero vectors
  VectorType zeroVec;
  zeroVec.Fill(0.0);
  FillImage<FieldType>(initField, zeroVec);

  //----------------------------------------------------------------
  std::cout << "Run registration." << std::endl;

  using RegistrationType = itk::MultiResolutionPDEDeformableRegistration<ImageType, ImageType, FieldType>;

  auto registrator = RegistrationType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(registrator, MultiResolutionPDEDeformableRegistration, ImageToImageFilter);


  registrator->SetMovingImage(moving);
  ITK_TEST_SET_GET_VALUE(moving, registrator->GetMovingImage());

  registrator->SetFixedImage(fixed);
  ITK_TEST_SET_GET_VALUE(fixed, registrator->GetFixedImage());

  registrator->GetModifiableFixedImagePyramid()->UseShrinkImageFilterOn();
  registrator->GetModifiableMovingImagePyramid()->UseShrinkImageFilterOn();

  constexpr unsigned int numLevel = 3;
  unsigned int           numIterations[numLevel];
  numIterations[0] = 64;

  unsigned int ilevel;
  for (ilevel = 1; ilevel < numLevel; ++ilevel)
  {
    numIterations[ilevel] = numIterations[ilevel - 1] / 2;
  }

  registrator->SetNumberOfLevels(numLevel);
  ITK_TEST_SET_GET_VALUE(numLevel, registrator->GetNumberOfLevels());

  registrator->SetNumberOfIterations(numIterations);
  RegistrationType::NumberOfIterationsType numIterationsArr;
  numIterationsArr.SetData(numIterations, numLevel);
  ITK_TEST_SET_GET_VALUE(numIterationsArr, registrator->GetNumberOfIterations());

  // Set the number of iterations to a different value and try the overloaded method with the desired value
  unsigned int numIterations2[numLevel];
  numIterations2[0] = 8;
  for (ilevel = 1; ilevel < numLevel; ++ilevel)
  {
    numIterations2[ilevel] = numIterations2[ilevel - 1] / 2;
  }
  registrator->SetNumberOfIterations(numIterations2);
  RegistrationType::NumberOfIterationsType numIterationsArr2;
  numIterationsArr2.SetData(numIterations2, numLevel);
  ITK_TEST_SET_GET_VALUE(numIterationsArr2, registrator->GetNumberOfIterations());

  registrator->SetNumberOfIterations(numIterationsArr);
  ITK_TEST_SET_GET_VALUE(numIterationsArr, registrator->GetNumberOfIterations());

  using CommandType = itk::SimpleMemberCommand<ShowProgressPDEObject>;

  ShowProgressPDEObject progressWatch(registrator);
  auto                  command = CommandType::New();
  command->SetCallbackFunction(&progressWatch, &ShowProgressPDEObject::ShowIteration);
  registrator->AddObserver(itk::IterationEvent(), command);

  PDERegistrationController<RegistrationType> controller(registrator);
  using ControllerType = itk::SimpleMemberCommand<PDERegistrationController<RegistrationType>>;
  auto controllerCommand = ControllerType::New();
  controllerCommand->SetCallbackFunction(&controller, &PDERegistrationController<RegistrationType>::ShowProgress);
  registrator->AddObserver(itk::ProgressEvent(), controllerCommand);

  ShowProgressPDEObject innerWatch(registrator->GetModifiableRegistrationFilter());
  innerWatch.m_Prefix = "    ";
  auto innerCommand = CommandType::New();
  innerCommand->SetCallbackFunction(&innerWatch, &ShowProgressPDEObject::ShowProgress);
  registrator->GetRegistrationFilter()->AddObserver(itk::ProgressEvent(), innerCommand);

  // make registration inplace
  registrator->GetModifiableRegistrationFilter()->InPlaceOn();

  registrator->Update();


  // -------------------------------------------------------
  std::cout << "Warp moving image" << std::endl;

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

  warper->Update();

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(warper->GetOutput());
  writer->SetFileName(argv[1]);
  writer->Update();

  // ---------------------------------------------------------
  std::cout << "Compare warped moving and fixed." << std::endl;

  // compare the warp and fixed images
  itk::ImageRegionIterator<ImageType> fixedIter(fixed, fixed->GetBufferedRegion());
  itk::ImageRegionIterator<ImageType> warpedIter(warper->GetOutput(), warper->GetOutput()->GetBufferedRegion());

  unsigned int numPixelsDifferent = 0;
  while (!fixedIter.IsAtEnd())
  {
    if (itk::Math::abs(fixedIter.Get() - warpedIter.Get()) > 0.1 * itk::Math::abs(fgnd - bgnd))
    {
      numPixelsDifferent++;
    }
    ++fixedIter;
    ++warpedIter;
  }

  std::cout << "Number of pixels different: " << numPixelsDifferent;
  std::cout << std::endl;

  //-------------------------------------------------------------
  std::cout << "Test when last shrink factors are not ones." << std::endl;

  registrator->SetNumberOfLevels(1);
  registrator->GetModifiableFixedImagePyramid()->SetStartingShrinkFactors(2);

  unsigned int n = 5;
  registrator->SetNumberOfIterations(&n);

  registrator->Update();

  if (registrator->GetOutput()->GetBufferedRegion() != fixed->GetBufferedRegion())
  {
    std::cout << "Deformation field should be the same size as fixed";
    std::cout << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise error handling
  bool passed;

  using InternalRegistrationType = RegistrationType::RegistrationType;
  InternalRegistrationType::Pointer demons = registrator->GetModifiableRegistrationFilter();

  try
  {
    passed = false;
    std::cout << "Set RegistrationFilter to nullptr" << std::endl;
    registrator->SetRegistrationFilter(nullptr);
    registrator->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
    passed = true;
    registrator->ResetPipeline();
    registrator->SetRegistrationFilter(demons);
    ITK_TEST_SET_GET_VALUE(demons, registrator->GetRegistrationFilter());
  }

  if (!passed)
  {
    std::cout << "Test failed" << std::endl;
  }

  using FixedImagePyramidType = RegistrationType::FixedImagePyramidType;
  FixedImagePyramidType::Pointer fixedPyramid = registrator->GetModifiableFixedImagePyramid();

  try
  {
    passed = false;
    std::cout << "Set FixedImagePyramid to nullptr" << std::endl;
    registrator->SetFixedImagePyramid(nullptr);
    registrator->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
    passed = true;
    registrator->ResetPipeline();
    registrator->SetFixedImagePyramid(fixedPyramid);
    ITK_TEST_SET_GET_VALUE(fixedPyramid, registrator->GetFixedImagePyramid());
  }

  if (!passed)
  {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  using MovingImagePyramidType = RegistrationType::MovingImagePyramidType;
  MovingImagePyramidType::Pointer movingPyramid = registrator->GetModifiableMovingImagePyramid();

  try
  {
    passed = false;
    std::cout << "Set MovingImagePyramid to nullptr" << std::endl;
    registrator->SetMovingImagePyramid(nullptr);
    registrator->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
    passed = true;
    registrator->ResetPipeline();
    registrator->SetMovingImagePyramid(movingPyramid);
    ITK_TEST_SET_GET_VALUE(movingPyramid, registrator->GetMovingImagePyramid());
  }

  if (!passed)
  {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }


  try
  {
    passed = false;
    std::cout << "Set FixedImage to nullptr" << std::endl;
    registrator->SetFixedImage(nullptr);
    registrator->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
    passed = true;
    registrator->ResetPipeline();
    registrator->SetFixedImage(fixed);
  }

  if (!passed)
  {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}

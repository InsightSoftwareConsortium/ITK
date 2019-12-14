/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkConfigure.h"

#if defined(ITK_USE_FFTWD)

#  include "itkCurvatureRegistrationFilter.h"
#  include "itkFastSymmetricForcesDemonsRegistrationFunction.h"

#  include "itkIndex.h"
#  include "itkImageRegionIteratorWithIndex.h"
#  include "itkNearestNeighborInterpolateImageFunction.h"
#  include "itkCommand.h"
#  include "itkMath.h"

#  include "itkImageFileWriter.h"

namespace
{
// The following class is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject * o) { m_Process = o; }
  void
  ShowProgress()
  {
    std::cout << "Progress " << m_Process->GetProgress() << std::endl;
  }
  itk::ProcessObject::Pointer m_Process;
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
  it.Begin();

  typename TImage::IndexType index;
  double                     r2 = itk::Math::sqr(radius);

  while (!it.IsAtEnd())
  {
    index = it.GetIndex();
    double distance = 0;
    for (unsigned int j = 0; j < TImage::ImageDimension; j++)
    {
      distance += itk::Math::sqr((double)index[j] - center[j]);
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
itkCurvatureRegistrationFilterTest(int, char *[])
{

  using PixelType = unsigned char;
  enum
  {
    ImageDimension = 2
  };
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using VectorType = itk::Vector<float, ImageDimension>;
  using FieldType = itk::Image<VectorType, ImageDimension>;
  using ForcesType = itk::FastSymmetricForcesDemonsRegistrationFunction<ImageType, ImageType, FieldType>;
  using FloatImageType = itk::Image<VectorType::ValueType, ImageDimension>;
  using IndexType = ImageType::IndexType;
  using SizeType = ImageType::SizeType;
  using RegionType = ImageType::RegionType;

  //--------------------------------------------------------
  std::cout << "Generate input images and initial deformation field";
  std::cout << std::endl;

  ImageType::SizeValueType sizeArray[ImageDimension] = { 128, 128 };
  SizeType                 size;
  size.SetSize(sizeArray);

  IndexType index;
  index.Fill(0);

  RegionType region;
  region.SetSize(size);
  region.SetIndex(index);

  ImageType::Pointer moving = ImageType::New();
  ImageType::Pointer fixed = ImageType::New();
  FieldType::Pointer initField = FieldType::New();

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
  PixelType bgnd = 0;

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

  //-------------------------------------------------------------
  std::cout << "Run registration and warp moving" << std::endl;

  using RegistrationType = itk::CurvatureRegistrationFilter<ImageType, ImageType, FieldType, ForcesType>;
  RegistrationType::Pointer registrator = RegistrationType::New();

  registrator->SetInitialDisplacementField(initField);
  registrator->SetMovingImage(moving);
  registrator->SetFixedImage(fixed);
  registrator->SetNumberOfIterations(100);
  registrator->SetTimeStep(1);
  registrator->SetConstraintWeight(0.01);
  registrator->Print(std::cout);

  std::cout << "\n\n\nPrinting function" << std::endl;
  using FunctionType = RegistrationType::RegistrationFunctionType;
  FunctionType * fptr;
  fptr = dynamic_cast<FunctionType *>(registrator->GetDifferenceFunction().GetPointer());
  fptr->Print(std::cout);

  // exercise other member variables
  std::cout << "No. Iterations: " << registrator->GetNumberOfIterations() << std::endl;

  ShowProgressObject                                    progressWatch(registrator);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch, &ShowProgressObject::ShowProgress);
  registrator->AddObserver(itk::ProgressEvent(), command);

  // warp moving image
  using WarperType = itk::WarpImageFilter<ImageType, ImageType, FieldType>;
  WarperType::Pointer warper = WarperType::New();

  using CoordRepType = WarperType::CoordRepType;
  using InterpolatorType = itk::NearestNeighborInterpolateImageFunction<ImageType, CoordRepType>;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();


  warper->SetInput(moving);
  warper->SetDisplacementField(registrator->GetOutput());
  warper->SetInterpolator(interpolator);
  warper->SetOutputSpacing(fixed->GetSpacing());
  warper->SetOutputOrigin(fixed->GetOrigin());

  warper->Print(std::cout);

  warper->Update();

  // ---------------------------------------------------------
  std::cout << "Compare warped moving and fixed." << std::endl;

  // compare the warp and fixed images
  itk::ImageRegionIterator<ImageType> fixedIter(fixed, fixed->GetBufferedRegion());
  itk::ImageRegionIterator<ImageType> warpedIter(warper->GetOutput(), fixed->GetBufferedRegion());

  size_t       ofs = 0;
  unsigned int numPixelsDifferent = 0;
  while (!fixedIter.IsAtEnd())
  {
    if (fixedIter.Get() != warpedIter.Get())
    {
      numPixelsDifferent++;
    }
    ++fixedIter;
    ++warpedIter;
    ++ofs;
  }

  std::cout << "Number of pixels different: " << numPixelsDifferent;
  std::cout << std::endl;

  if (numPixelsDifferent > 10)
  {
    std::cout << "Test failed - too many pixels different." << std::endl;

    using WriterType = itk::ImageFileWriter<ImageType>;
    WriterType::Pointer writer = WriterType::New();
    writer->SetInput(warper->GetOutput());
    writer->SetFileName("warped.png");
    writer->Update();

    writer->SetInput(fixed);
    writer->SetFileName("fixed.png");
    writer->Update();

    writer->SetInput(moving);
    writer->SetFileName("moving.png");
    writer->Update();

    return EXIT_FAILURE;
  }

  // -----------------------------------------------------------
  std::cout << "Test running registrator without initial deformation field.";
  std::cout << std::endl;

  bool passed = true;
  try
  {
    registrator->SetInput(nullptr);
    registrator->SetNumberOfIterations(2);
    registrator->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Unexpected error." << std::endl;
    std::cout << err << std::endl;
    passed = false;
  }

  if (!passed)
  {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  //--------------------------------------------------------------
  std::cout << "Test exception handling." << std::endl;

  std::cout << "Test nullptr moving image. " << std::endl;
  passed = false;
  try
  {
    registrator->SetInput(initField);
    registrator->SetMovingImage(nullptr);
    registrator->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught expected error." << std::endl;
    std::cout << err << std::endl;
    passed = true;
  }

  if (!passed)
  {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }
  registrator->SetMovingImage(moving);
  registrator->ResetPipeline();

  std::cout << "Test nullptr moving image interpolator. " << std::endl;
  passed = false;
  try
  {
    fptr = dynamic_cast<FunctionType *>(registrator->GetDifferenceFunction().GetPointer());
    fptr->SetMovingImageInterpolator(nullptr);
    registrator->SetInput(initField);
    registrator->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught expected error." << std::endl;
    std::cout << err << std::endl;
    passed = true;
  }

  if (!passed)
  {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}

#endif

/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef ITK_USE_FFTWD
#  error "This program needs FFTWD to work."
#endif


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkCurvatureRegistrationFilter.h"
#include "itkFastSymmetricForcesDemonsRegistrationFunction.h"
#include "itkMultiResolutionPDEDeformableRegistration.h"

#include "itkHistogramMatchingImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"

constexpr unsigned int Dimension = 3;

//  The following section of code implements a Command observer
//  that will monitor the evolution of the registration process.
//
class CommandIterationUpdate : public itk::Command
{
public:
  using Self = CommandIterationUpdate;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<CommandIterationUpdate>;
  itkNewMacro(CommandIterationUpdate);

protected:
  CommandIterationUpdate() = default;

  using InternalImageType = itk::Image<float, Dimension>;
  using VectorPixelType = itk::Vector<float, Dimension>;
  using DisplacementFieldType = itk::Image<VectorPixelType, Dimension>;

  using RegistrationFilterType = itk::CurvatureRegistrationFilter<
    InternalImageType,
    InternalImageType,
    DisplacementFieldType,
    itk::FastSymmetricForcesDemonsRegistrationFunction<InternalImageType, InternalImageType, DisplacementFieldType>>;

public:
  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    const auto * filter = static_cast<const RegistrationFilterType *>(object);
    if (!(itk::IterationEvent().CheckEvent(&event)))
    {
      return;
    }
    std::cout << filter->GetMetric() << std::endl;
  }
};


#include "itkTestDriverIncludeRequiredIOFactories.h"
int
main(int argc, char * argv[])
{
  RegisterRequiredFactories();
  if (argc < 4)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile " << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = short;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  FixedImageReaderType::Pointer  fixedImageReader = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  using InternalPixelType = float;
  using InternalImageType = itk::Image<InternalPixelType, Dimension>;
  using FixedImageCasterType = itk::CastImageFilter<FixedImageType, InternalImageType>;
  using MovingImageCasterType = itk::CastImageFilter<MovingImageType, InternalImageType>;

  FixedImageCasterType::Pointer  fixedImageCaster = FixedImageCasterType::New();
  MovingImageCasterType::Pointer movingImageCaster = MovingImageCasterType::New();

  fixedImageCaster->SetInput(fixedImageReader->GetOutput());
  movingImageCaster->SetInput(movingImageReader->GetOutput());

  using MatchingFilterType = itk::HistogramMatchingImageFilter<InternalImageType, InternalImageType>;
  MatchingFilterType::Pointer matcher = MatchingFilterType::New();

  matcher->SetInput(movingImageCaster->GetOutput());
  matcher->SetReferenceImage(fixedImageCaster->GetOutput());
  matcher->SetNumberOfHistogramLevels(1024);
  matcher->SetNumberOfMatchPoints(7);
  matcher->ThresholdAtMeanIntensityOn();

  using VectorPixelType = itk::Vector<float, Dimension>;
  using DisplacementFieldType = itk::Image<VectorPixelType, Dimension>;
  using RegistrationFilterType = itk::CurvatureRegistrationFilter<
    InternalImageType,
    InternalImageType,
    DisplacementFieldType,
    itk::FastSymmetricForcesDemonsRegistrationFunction<InternalImageType, InternalImageType, DisplacementFieldType>>;

  RegistrationFilterType::Pointer filter = RegistrationFilterType::New();
  filter->SetTimeStep(1);
  filter->SetConstraintWeight(0.1);

  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  filter->AddObserver(itk::IterationEvent(), observer);

  using MultiResRegistrationFilterType =
    itk::MultiResolutionPDEDeformableRegistration<InternalImageType, InternalImageType, DisplacementFieldType>;
  MultiResRegistrationFilterType::Pointer multires = MultiResRegistrationFilterType::New();
  multires->SetRegistrationFilter(filter);
  multires->SetNumberOfLevels(3);
  multires->SetFixedImage(fixedImageCaster->GetOutput());
  multires->SetMovingImage(matcher->GetOutput());

  unsigned int nIterations[4] = { 10, 20, 50, 50 };
  multires->SetNumberOfIterations(nIterations);
  multires->Update();

  using WarperType = itk::WarpImageFilter<MovingImageType, MovingImageType, DisplacementFieldType>;
  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;
  WarperType::Pointer       warper = WarperType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  FixedImageType::Pointer   fixedImage = fixedImageReader->GetOutput();

  warper->SetInput(movingImageReader->GetOutput());
  warper->SetInterpolator(interpolator);
  warper->SetOutputSpacing(fixedImage->GetSpacing());
  warper->SetOutputOrigin(fixedImage->GetOrigin());
  warper->SetOutputDirection(fixedImage->GetDirection());
  warper->SetDisplacementField(multires->GetOutput());

  // Write warped image out to file
  using OutputPixelType = unsigned short;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using CastFilterType = itk::CastImageFilter<MovingImageType, OutputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  WriterType::Pointer     writer = WriterType::New();
  CastFilterType::Pointer caster = CastFilterType::New();

  writer->SetFileName(argv[3]);

  caster->SetInput(warper->GetOutput());
  writer->SetInput(caster->GetOutput());
  writer->Update();

  if (argc > 4) // if a fourth line argument has been provided...
  {

    using FieldWriterType = itk::ImageFileWriter<DisplacementFieldType>;

    FieldWriterType::Pointer fieldWriter = FieldWriterType::New();
    fieldWriter->SetFileName(argv[4]);
    fieldWriter->SetInput(multires->GetOutput());

    try
    {
      fieldWriter->Update();
    }
    catch (const itk::ExceptionObject & e)
    {
      e.Print(std::cerr);
    }
  }

  return EXIT_SUCCESS;
}

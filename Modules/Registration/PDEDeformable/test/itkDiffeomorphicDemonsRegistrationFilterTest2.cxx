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

#include "itkDiffeomorphicDemonsRegistrationFilter.h"

#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


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


int
itkDiffeomorphicDemonsRegistrationFilterTest2(int argc, char * argv[])
{

  if (argc < 8)
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << argv[0] << std::endl;
    std::cerr << "fixedImage movingImage resampledImage" << std::endl;
    std::cerr << "GradientType [0=Symmetric,1=Fixed,2=WarpedMoving,3=MappedMoving]" << std::endl;
    std::cerr << "UseFirstOrderExp [0=No,1=Yes]" << std::endl;
    std::cerr << "Intensity Difference Threshold (double)" << std::endl;
    std::cerr << "Maximum Update step length (double)" << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = float;
  constexpr unsigned int ImageDimension = 2;

  using ImageType = itk::Image<PixelType, ImageDimension>;
  using VectorType = itk::Vector<float, ImageDimension>;
  using FieldType = itk::Image<VectorType, ImageDimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  ReaderType::Pointer fixedReader = ReaderType::New();
  ReaderType::Pointer movingReader = ReaderType::New();

  fixedReader->SetFileName(argv[1]);
  movingReader->SetFileName(argv[2]);

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName(argv[3]);

  try
  {
    fixedReader->Update();
    movingReader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  //-------------------------------------------------------------
  std::cout << "Run registration and warp moving" << std::endl;

  using RegistrationType = itk::DiffeomorphicDemonsRegistrationFilter<ImageType, ImageType, FieldType>;

  RegistrationType::Pointer registrator = RegistrationType::New();

  registrator->SetMovingImage(movingReader->GetOutput());
  registrator->SetFixedImage(fixedReader->GetOutput());
  registrator->SetNumberOfIterations(200);
  registrator->SetStandardDeviations(1.0);
  registrator->SetMaximumError(0.08);
  registrator->SetMaximumKernelWidth(10);


  const double intensityDifferenceThreshold = std::stod(argv[6]);

  registrator->SetIntensityDifferenceThreshold(intensityDifferenceThreshold);

  const double maximumUpdateStepLength = std::stod(argv[7]);

  registrator->SetMaximumUpdateStepLength(maximumUpdateStepLength);


  const std::string gradientTypeString{ argv[4] };
  const int         gradientType = std::stoi(gradientTypeString);

  using FunctionType = RegistrationType::DemonsRegistrationFunctionType;

  switch (gradientType)
  {
    case 0:
      registrator->SetUseGradientType(FunctionType::Symmetric);
      break;
    case 1:
      registrator->SetUseGradientType(FunctionType::Fixed);
      break;
    case 2:
      registrator->SetUseGradientType(FunctionType::WarpedMoving);
      break;
    case 3:
      registrator->SetUseGradientType(FunctionType::MappedMoving);
      break;
  }

  std::cout << "GradientType = " << registrator->GetUseGradientType() << std::endl;

  const std::string useFirstOrderExponentialString{ argv[5] };
  const int         useFirstOrderExponential = std::stoi(useFirstOrderExponentialString);

  if (useFirstOrderExponential == 0)
  {
    registrator->SetUseFirstOrderExp(false);
  }
  else
  {
    registrator->SetUseFirstOrderExp(true);
  }


  // turn on inplace execution
  registrator->InPlaceOn();


  FunctionType * fptr;
  fptr = dynamic_cast<FunctionType *>(registrator->GetDifferenceFunction().GetPointer());
  fptr->Print(std::cout);

  // exercise other member variables
  std::cout << "No. Iterations: " << registrator->GetNumberOfIterations() << std::endl;
  std::cout << "Max. kernel error: " << registrator->GetMaximumError() << std::endl;
  std::cout << "Max. kernel width: " << registrator->GetMaximumKernelWidth() << std::endl;

  double v[ImageDimension];
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    v[j] = registrator->GetStandardDeviations()[j];
  }
  registrator->SetStandardDeviations(v);

  using ProgressType = ShowProgressObject<RegistrationType>;
  ProgressType                                    progressWatch(registrator);
  itk::SimpleMemberCommand<ProgressType>::Pointer command;
  command = itk::SimpleMemberCommand<ProgressType>::New();
  command->SetCallbackFunction(&progressWatch, &ProgressType::ShowProgress);
  registrator->AddObserver(itk::ProgressEvent(), command);

  // warp moving image
  using WarperType = itk::WarpImageFilter<ImageType, ImageType, FieldType>;
  WarperType::Pointer warper = WarperType::New();

  using CoordRepType = WarperType::CoordRepType;
  using InterpolatorType = itk::NearestNeighborInterpolateImageFunction<ImageType, CoordRepType>;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  const ImageType * fixed = fixedReader->GetOutput();
  const ImageType * moving = movingReader->GetOutput();

  warper->SetInput(moving);
  warper->SetDisplacementField(registrator->GetOutput());
  warper->SetInterpolator(interpolator);
  warper->SetOutputSpacing(fixed->GetSpacing());
  warper->SetOutputOrigin(fixed->GetOrigin());
  warper->SetOutputDirection(fixed->GetDirection());

  warper->Print(std::cout);

  warper->Update();

  writer->SetInput(warper->GetOutput());
  writer->UseCompressionOn();

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}

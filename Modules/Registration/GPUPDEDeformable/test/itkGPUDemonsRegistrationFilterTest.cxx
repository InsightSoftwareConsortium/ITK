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

/**
 * Test program for itkGPUDemonsRegistrationFilter class
 *
 * This program creates a GPU Mean filter and a CPU threshold filter using
 * object factory framework and test pipelining of GPU and CPU filters.
 */
// #include "pathToOpenCLSourceCode.h"

#include "itkGPUDemonsRegistrationFilter.h"
#include "itkHistogramMatchingImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkWarpImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itkSmartPointer.h"
#include "itkTimeProbe.h"

#include "itkGPUImage.h"
#include "itkGPUKernelManager.h"
#include "itkGPUContextManager.h"
#include "itkMacro.h"
#include "itkTestingMacros.h"

namespace
{
// The following class is used to support callbacks
// on the filter in the pipeline that follows later
template <typename TRegistration>
class ShowProgressObject : public itk::Object
{
public:
  ShowProgressObject(TRegistration * o) { m_Process = o; }

  void
  ShowProgress()
  {
    // std::cout
    itkDebugMacro("Progress: " << m_Process->GetProgress() << "  "
                               << "Iter: " << m_Process->GetElapsedIterations() << "  "
                               << "Metric: " << m_Process->GetMetric() << "  "
                               << "RMSChange: " << m_Process->GetRMSChange() << "  ");
    //               << std::endl;

    if (m_Process->GetElapsedIterations() == 10000)
    {
      m_Process->StopRegistration();
    }
  }

  typename TRegistration::Pointer m_Process;
};

constexpr unsigned int numberOfRepeatedTests{ 1 };
constexpr float        displacementFieldSmoothingSigma{ 1.0 };
constexpr float        updateFieldSmoothingSigma{ 1.0 };
constexpr float        maximumRMSError{ 0.01 };
const bool             smoothUpdateField = true;

itk::TimeProbe m_GPUTime;
itk::TimeProbe m_CPUTime;

template <unsigned int VDimension>
int
GPUDemonsRegistrationFilterTestTemplate(int argc, char * argv[]);

template <unsigned int VDimension, typename TDisplacementFieldPointer>
TDisplacementFieldPointer
itkGPUDemons(int argc, char * argv[]);
template <unsigned int VDimension, typename TDisplacementFieldPointer>
TDisplacementFieldPointer
itkCPUDemons(int argc, char * argv[]);

char *
AppendFileName(char * src, const char * postfix)
{
  size_t destLength = strlen(src) + strlen(postfix) + 1;
  char * dest = new char[destLength];
  char * pos = strrchr(src, '.');
  int    skip = pos - src;

  snprintf(dest, destLength, "%s", src);
  snprintf(dest + skip, destLength - skip, "%s%s", postfix, pos);
  return dest;
}

} // namespace

int
itkGPUDemonsRegistrationFilterTest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " imageDimension numOfIterations ";
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile " << std::endl;
    // std::cerr << " [outputDisplacementFieldFile] " << std::endl;
    return EXIT_SUCCESS;
  }
  int returnValue = EXIT_SUCCESS;

  switch (std::stoi(argv[1]))
  {
    case 2:
      returnValue = GPUDemonsRegistrationFilterTestTemplate<2>(argc, argv);
      break;
    case 3:
      returnValue = GPUDemonsRegistrationFilterTestTemplate<3>(argc, argv);
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      returnValue = EXIT_FAILURE;
  }
  return returnValue;
}

namespace
{
template <unsigned int VDimension>
int
GPUDemonsRegistrationFilterTestTemplate(int argc, char * argv[])
{
  bool         passed;
  unsigned int size1 = 0, size2 = 0;

  using InternalPixelType = float;
  using VectorPixelType = itk::Vector<float, VDimension>;
  using GPUDisplacementFieldType = itk::GPUImage<VectorPixelType, VDimension>;
  using CPUDisplacementFieldType = itk::Image<VectorPixelType, VDimension>;
  using GPUDisplacementFieldPointer = typename GPUDisplacementFieldType::Pointer;
  using CPUDisplacementFieldPointer = typename CPUDisplacementFieldType::Pointer;

  GPUDisplacementFieldPointer gpuOut;
  CPUDisplacementFieldPointer cpuOut;
  for (unsigned int i = 0; i < numberOfRepeatedTests; ++i)
  {
    std::cout << "---------------------------------------------------" << std::endl;
    std::cout << "Starting GPU Demons" << std::endl;
    gpuOut = (itkGPUDemons<VDimension, GPUDisplacementFieldPointer>(argc, argv));
    std::cout << "Finished GPU Demons" << std::endl;

    std::cout << "---------------------------------------------------" << std::endl;
    std::cout << "Starting CPU Demons" << std::endl;
    cpuOut = (itkCPUDemons<VDimension, CPUDisplacementFieldPointer>(argc, argv));
    std::cout << "Finished CPU Demons" << std::endl;
  }
  std::cout << "Average GPU registration time in seconds = " << m_GPUTime.GetMean() << std::endl;
  std::cout << "Average CPU registration time in seconds = " << m_CPUTime.GetMean() << std::endl;
  InternalPixelType maxDiff = 0, avgDiff = 0, diff, tmp;

  InternalPixelType *gpuBuf, *cpuBuf;
  gpuBuf = (InternalPixelType *)gpuOut->GetBufferPointer();
  cpuBuf = (InternalPixelType *)cpuOut->GetBufferPointer();
  size1 = gpuOut->GetLargestPossibleRegion().GetNumberOfPixels();
  size2 = cpuOut->GetLargestPossibleRegion().GetNumberOfPixels();

  for (unsigned int i = 0; (i < size1) && (i < size2); ++i)
  {
    diff = 0;
    for (unsigned int d = 0; d < VDimension; ++d)
    {
      tmp = gpuBuf[i * VDimension + d] - cpuBuf[i * VDimension + d];
      diff += tmp * tmp;
    }
    diff = std::sqrt(diff);
    avgDiff += diff;
    if (diff > maxDiff)
    {
      maxDiff = diff;
    }
  }
  avgDiff /= size1;
  std::cout << "Maximum displacement difference = " << maxDiff << std::endl;
  std::cout << "Average displacement difference = " << avgDiff << std::endl;

  // std::cout << "Total GPU time in seconds = " << m_GPUTime.GetMean() <<
  // std::endl;
  // std::cout << "Initial GPU time in seconds = " << gpuInitTime.GetMean() <<
  // std::endl;
  // std::cout << "Total CPU time in seconds = " << m_CPUTime.GetMean() <<
  // std::endl;
  passed = avgDiff < 2;

  if (!passed)
  {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}

template <unsigned int VDimension, typename TDisplacementFieldPointer>
TDisplacementFieldPointer
itkGPUDemons(int, char * argv[])
{
  const unsigned int Dimension = VDimension;
  unsigned int       numOfIterations = std::stoi(argv[2]);

  using PixelType = unsigned short;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  auto fixedImageReader = FixedImageReaderType::New();
  auto movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[3]);
  movingImageReader->SetFileName(argv[4]);

  // casting pixel type from short to float
  using InternalPixelType = float;
  using InternalImageType = itk::GPUImage<InternalPixelType, Dimension>;
  using FixedImageCasterType = itk::CastImageFilter<FixedImageType, InternalImageType>;
  using MovingImageCasterType = itk::CastImageFilter<MovingImageType, InternalImageType>;

  auto fixedImageCaster = FixedImageCasterType::New();
  auto movingImageCaster = MovingImageCasterType::New();

  fixedImageCaster->SetInput(fixedImageReader->GetOutput());
  movingImageCaster->SetInput(movingImageReader->GetOutput());

  // matching intensity histogram
  using MatchingFilterType = itk::HistogramMatchingImageFilter<InternalImageType, InternalImageType>;
  auto matcher = MatchingFilterType::New();

  matcher->SetInput(movingImageCaster->GetOutput());
  matcher->SetReferenceImage(fixedImageCaster->GetOutput());

  matcher->SetNumberOfHistogramLevels(1024);
  matcher->SetNumberOfMatchPoints(7);

  matcher->ThresholdAtMeanIntensityOn();

  // demons registration
  using DisplacementFieldType = typename TDisplacementFieldPointer::ObjectType;
  using RegistrationFilterType =
    itk::GPUDemonsRegistrationFilter<InternalImageType, InternalImageType, DisplacementFieldType>;

  auto filter = RegistrationFilterType::New();

  using ProgressType = ShowProgressObject<RegistrationFilterType>;
  ProgressType progressWatch(filter);
  progressWatch.DebugOn();
  typename itk::SimpleMemberCommand<ProgressType>::Pointer command = itk::SimpleMemberCommand<ProgressType>::New();
  command->SetCallbackFunction(&progressWatch, &ProgressType::ShowProgress);
  filter->AddObserver(itk::ProgressEvent(), command);

  filter->SetFixedImage(fixedImageCaster->GetOutput());
  filter->SetMovingImage(matcher->GetOutput());

  filter->SetNumberOfIterations(numOfIterations);
  filter->SetStandardDeviations(displacementFieldSmoothingSigma);
  filter->SetUpdateFieldStandardDeviations(updateFieldSmoothingSigma);
  filter->SetSmoothUpdateField(smoothUpdateField);
  filter->SetMaximumRMSError(maximumRMSError);

  m_GPUTime.Start();
  filter->Update();
  m_GPUTime.Stop();

  std::cout << "GPU InitTime in seconds = " << filter->GetInitTime().GetTotal() << std::endl;
  std::cout << "GPU ComputeUpdateTime in seconds = " << filter->GetComputeUpdateTime().GetTotal() << std::endl;
  std::cout << "GPU ApplyUpdateTime in seconds = " << filter->GetApplyUpdateTime().GetTotal() << std::endl;
  std::cout << "GPU SmoothFieldTime in seconds = " << filter->GetSmoothFieldTime().GetTotal() << std::endl;

  // warp the image with the deformation field
  using WarperType = itk::WarpImageFilter<MovingImageType, MovingImageType, DisplacementFieldType>;
  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;
  auto                             warper = WarperType::New();
  auto                             interpolator = InterpolatorType::New();
  typename FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  warper->SetInput(movingImageReader->GetOutput());
  warper->SetInterpolator(interpolator);
  warper->SetOutputSpacing(fixedImage->GetSpacing());
  warper->SetOutputOrigin(fixedImage->GetOrigin());
  warper->SetOutputDirection(fixedImage->GetDirection());

  warper->SetDisplacementField(filter->GetOutput());

  // write the warped image into a file
  using OutputPixelType = unsigned char;

  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using CastFilterType = itk::CastImageFilter<MovingImageType, OutputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto writer = WriterType::New();
  auto caster = CastFilterType::New();

  char * outName = AppendFileName(argv[5], "_gpu");
  writer->SetFileName(outName);

  caster->SetInput(warper->GetOutput());
  writer->SetInput(caster->GetOutput());
  writer->Update();

  TDisplacementFieldPointer ret = filter->GetOutput();

  return ret;
}

template <unsigned int VDimension, typename TDisplacementFieldPointer>
TDisplacementFieldPointer
itkCPUDemons(int, char * argv[])
{
  const unsigned int Dimension = VDimension;
  unsigned int       numOfIterations = std::stoi(argv[2]);

  using PixelType = unsigned short;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  auto fixedImageReader = FixedImageReaderType::New();
  auto movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[3]);
  movingImageReader->SetFileName(argv[4]);

  // casting pixel type from short to float
  using InternalPixelType = float;
  using InternalImageType = itk::Image<InternalPixelType, Dimension>;
  using FixedImageCasterType = itk::CastImageFilter<FixedImageType, InternalImageType>;
  using MovingImageCasterType = itk::CastImageFilter<MovingImageType, InternalImageType>;

  auto fixedImageCaster = FixedImageCasterType::New();
  auto movingImageCaster = MovingImageCasterType::New();

  fixedImageCaster->SetInput(fixedImageReader->GetOutput());
  movingImageCaster->SetInput(movingImageReader->GetOutput());

  // matching intensity histogram
  using MatchingFilterType = itk::HistogramMatchingImageFilter<InternalImageType, InternalImageType>;
  auto matcher = MatchingFilterType::New();

  matcher->SetInput(movingImageCaster->GetOutput());
  matcher->SetReferenceImage(fixedImageCaster->GetOutput());

  matcher->SetNumberOfHistogramLevels(1024);
  matcher->SetNumberOfMatchPoints(7);

  matcher->ThresholdAtMeanIntensityOn();

  // demons registration
  using DisplacementFieldType = typename TDisplacementFieldPointer::ObjectType;
  using RegistrationFilterType =
    itk::DemonsRegistrationFilter<InternalImageType, InternalImageType, DisplacementFieldType>;

  auto filter = RegistrationFilterType::New();

  using ProgressType = ShowProgressObject<RegistrationFilterType>;
  ProgressType                                             progressWatch(filter);
  typename itk::SimpleMemberCommand<ProgressType>::Pointer command = itk::SimpleMemberCommand<ProgressType>::New();
  command->SetCallbackFunction(&progressWatch, &ProgressType::ShowProgress);
  filter->AddObserver(itk::ProgressEvent(), command);

  filter->SetFixedImage(fixedImageCaster->GetOutput());
  filter->SetMovingImage(matcher->GetOutput());

  filter->SetNumberOfIterations(numOfIterations);
  filter->SetStandardDeviations(displacementFieldSmoothingSigma);
  filter->SetUpdateFieldStandardDeviations(updateFieldSmoothingSigma);
  filter->SetSmoothUpdateField(smoothUpdateField);
  filter->SetMaximumRMSError(maximumRMSError);

  m_CPUTime.Start();
  filter->Update();
  m_CPUTime.Stop();

  // warp the image with the deformation field
  using WarperType = itk::WarpImageFilter<MovingImageType, MovingImageType, DisplacementFieldType>;
  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;
  auto                             warper = WarperType::New();
  auto                             interpolator = InterpolatorType::New();
  typename FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  warper->SetInput(movingImageReader->GetOutput());
  warper->SetInterpolator(interpolator);
  warper->SetOutputSpacing(fixedImage->GetSpacing());
  warper->SetOutputOrigin(fixedImage->GetOrigin());
  warper->SetOutputDirection(fixedImage->GetDirection());

  warper->SetDisplacementField(filter->GetOutput());

  // write the warped image into a file
  using OutputPixelType = unsigned char;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using CastFilterType = itk::CastImageFilter<MovingImageType, OutputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto writer = WriterType::New();
  auto caster = CastFilterType::New();

  char * outName = AppendFileName(argv[5], "_cpu");
  writer->SetFileName(outName);

  caster->SetInput(warper->GetOutput());
  writer->SetInput(caster->GetOutput());
  writer->Update();

  TDisplacementFieldPointer ret = filter->GetOutput();

  return ret;
}

} // namespace

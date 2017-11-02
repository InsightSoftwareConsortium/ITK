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
#include "itkGPUDemonsRegistrationFilter.h"
#include "itkMacro.h"

namespace
{
// The following class is used to support callbacks
// on the filter in the pipeline that follows later
template <typename TRegistration>
class ShowProgressObject : public itk::Object
{
public:
  ShowProgressObject(TRegistration* o)
  {
    m_Process = o;
  }

  void ShowProgress()
  {
    //std::cout
    itkDebugMacro(
                   << "Progress: " << m_Process->GetProgress() << "  "
                   << "Iter: " << m_Process->GetElapsedIterations() << "  "
                   << "Metric: "   << m_Process->GetMetric()   << "  "
                   << "RMSChange: " << m_Process->GetRMSChange() << "  "
                 );
    //               << std::endl;

    if( m_Process->GetElapsedIterations() == 10000 )
      {
      m_Process->StopRegistration();
      }
  }

  typename TRegistration::Pointer m_Process;
};

const unsigned int numberOfRepeatedTests = 1;
const float        displacementFieldSmoothingSigma = 1.0;
const float        updateFieldSmoothingSigma = 1.0;
const float        maximumRMSError = 0.01;
const bool         smoothUpdateField = true;

itk::TimeProbe m_GPUTime;
itk::TimeProbe m_CPUTime;

template <unsigned VDimension>
int GPUDemonsRegistrationFilterTestTemplate(int argc, char *argv[]);

template <unsigned VDimension, typename TDisplacementFieldPointer>
TDisplacementFieldPointer itkGPUDemons(int argc, char *argv[]);
template <unsigned VDimension, typename TDisplacementFieldPointer>
TDisplacementFieldPointer itkCPUDemons(int argc, char *argv[]);

char * AppendFileName(char *src, const char *postfix)
{
  char *dest = new char[strlen(src) + strlen(postfix) + 1];
  char *pos = strrchr(src, '.');
  int   skip = pos - src;

  sprintf(dest, "%s", src);
  sprintf(dest + skip, "%s", postfix);
  sprintf(dest + skip + strlen(postfix), "%s", pos);
  return dest;
}

}

int itkGPUDemonsRegistrationFilterTest(int argc, char *argv[])
{
  if( argc < 6 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " imageDimension numOfIterations ";
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile " << std::endl;
    //std::cerr << " [outputDisplacementFieldFile] " << std::endl;
    return EXIT_SUCCESS;
    }
  int returnValue = EXIT_SUCCESS;

  switch( atoi( argv[1] ) )
   {
   case 2:
     returnValue = GPUDemonsRegistrationFilterTestTemplate<2>( argc, argv );
     break;
   case 3:
     returnValue = GPUDemonsRegistrationFilterTestTemplate<3>( argc, argv );
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
int GPUDemonsRegistrationFilterTestTemplate(int argc, char *argv[])
{
  const unsigned int ImageDimension = VDimension;
  bool passed;
  unsigned int size1 = 0, size2 = 0;

  typedef float                                          InternalPixelType;
  typedef itk::Vector<float, ImageDimension>             VectorPixelType;
  typedef itk::GPUImage<VectorPixelType, ImageDimension> GPUDisplacementFieldType;
  typedef itk::Image<VectorPixelType, ImageDimension>    CPUDisplacementFieldType;
  typedef typename GPUDisplacementFieldType::Pointer     GPUDisplacementFieldPointer;
  typedef typename CPUDisplacementFieldType::Pointer     CPUDisplacementFieldPointer;

  GPUDisplacementFieldPointer gpuOut;
  CPUDisplacementFieldPointer cpuOut;
  for( unsigned int i = 0; i < numberOfRepeatedTests; i++ )
    {
    std::cout << "---------------------------------------------------" << std::endl;
    std::cout << "Starting GPU Demons" << std::endl;
    gpuOut = (itkGPUDemons<ImageDimension, GPUDisplacementFieldPointer>(argc, argv) );
    std::cout << "Finished GPU Demons" << std::endl;

    std::cout << "---------------------------------------------------" << std::endl;
    std::cout << "Starting CPU Demons" << std::endl;
    cpuOut = (itkCPUDemons<ImageDimension, CPUDisplacementFieldPointer>(argc, argv) );
    std::cout << "Finished CPU Demons" << std::endl;

    }
  std::cout << "Average GPU registration time in seconds = " << m_GPUTime.GetMean() << std::endl;
  std::cout << "Average CPU registration time in seconds = " << m_CPUTime.GetMean() << std::endl;
  InternalPixelType maxDiff = 0, avgDiff = 0, diff, tmp;

  InternalPixelType *gpuBuf, *cpuBuf;
  gpuBuf = (InternalPixelType *) gpuOut->GetBufferPointer();
  cpuBuf = (InternalPixelType *) cpuOut->GetBufferPointer();
  size1 = gpuOut->GetLargestPossibleRegion().GetNumberOfPixels();
  size2 = cpuOut->GetLargestPossibleRegion().GetNumberOfPixels();

  for( unsigned int i = 0; ( i < size1 ) && ( i < size2 ); i++ )
    {
    diff = 0;
    for( unsigned int d = 0; d < ImageDimension; d++ )
      {
      tmp = gpuBuf[i * ImageDimension + d] - cpuBuf[i * ImageDimension + d];
      diff += tmp * tmp;
      }
    diff = std::sqrt(diff);
    avgDiff += diff;
    if( diff > maxDiff )
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
  if( avgDiff < 2 )
    {
    passed = true;
    }
  else
    {
    passed = false;
    }

  if( !passed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;

}

template <unsigned VDimension, typename TDisplacementFieldPointer>
TDisplacementFieldPointer itkGPUDemons(int, char *argv[])
{
  const unsigned int Dimension = VDimension;
  unsigned int numOfIterations = atoi( argv[2] );

  typedef unsigned short PixelType;

  typedef itk::Image<PixelType, Dimension> FixedImageType;
  typedef itk::Image<PixelType, Dimension> MovingImageType;

  typedef itk::ImageFileReader<FixedImageType>  FixedImageReaderType;
  typedef itk::ImageFileReader<MovingImageType> MovingImageReaderType;

  typename FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  typename MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName( argv[3] );
  movingImageReader->SetFileName( argv[4] );

  // casting pixel type from short to float
  typedef float                                       InternalPixelType;
  typedef itk::GPUImage<InternalPixelType, Dimension> InternalImageType;
  typedef itk::CastImageFilter<FixedImageType,
                               InternalImageType>     FixedImageCasterType;
  typedef itk::CastImageFilter<MovingImageType,
                               InternalImageType>     MovingImageCasterType;

  typename FixedImageCasterType::Pointer  fixedImageCaster  = FixedImageCasterType::New();
  typename MovingImageCasterType::Pointer movingImageCaster = MovingImageCasterType::New();

  fixedImageCaster->SetInput( fixedImageReader->GetOutput() );
  movingImageCaster->SetInput( movingImageReader->GetOutput() );

  // maching intensity histogram
  typedef itk::HistogramMatchingImageFilter<
    InternalImageType,
    InternalImageType>   MatchingFilterType;
  typename MatchingFilterType::Pointer matcher = MatchingFilterType::New();

  matcher->SetInput( movingImageCaster->GetOutput() );
  matcher->SetReferenceImage( fixedImageCaster->GetOutput() );

  matcher->SetNumberOfHistogramLevels( 1024 );
  matcher->SetNumberOfMatchPoints( 7 );

  matcher->ThresholdAtMeanIntensityOn();

  // demons registration
  typedef typename TDisplacementFieldPointer::ObjectType     DisplacementFieldType;
  typedef itk::GPUDemonsRegistrationFilter<
    InternalImageType,
    InternalImageType,
    DisplacementFieldType> RegistrationFilterType;

  typename RegistrationFilterType::Pointer filter = RegistrationFilterType::New();

  typedef ShowProgressObject<RegistrationFilterType> ProgressType;
  ProgressType progressWatch(filter);
  progressWatch.DebugOn();
  typename itk::SimpleMemberCommand<ProgressType>::Pointer command;
  command = itk::SimpleMemberCommand<ProgressType>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ProgressType::ShowProgress);
  filter->AddObserver( itk::ProgressEvent(), command);

  filter->SetFixedImage( fixedImageCaster->GetOutput() );
  filter->SetMovingImage( matcher->GetOutput() );

  filter->SetNumberOfIterations( numOfIterations );
  filter->SetStandardDeviations( displacementFieldSmoothingSigma );
  filter->SetUpdateFieldStandardDeviations( updateFieldSmoothingSigma );
  filter->SetSmoothUpdateField(smoothUpdateField);
  filter->SetMaximumRMSError(maximumRMSError);

  m_GPUTime.Start();
  filter->Update();
  m_GPUTime.Stop();

  std::cout << "GPU InitTime in seconds = "          << filter->GetInitTime().GetTotal() << std::endl;
  std::cout << "GPU ComputeUpdateTime in seconds = " << filter->GetComputeUpdateTime().GetTotal() << std::endl;
  std::cout << "GPU ApplyUpdateTime in seconds = "   << filter->GetApplyUpdateTime().GetTotal() << std::endl;
  std::cout << "GPU SmoothFieldTime in seconds = "   << filter->GetSmoothFieldTime().GetTotal() << std::endl;

  // warp the image with the deformation field
  typedef itk::WarpImageFilter<
    MovingImageType,
    MovingImageType,
    DisplacementFieldType>     WarperType;
  typedef itk::LinearInterpolateImageFunction<
    MovingImageType,
    double>                    InterpolatorType;
  typename WarperType::Pointer       warper = WarperType::New();
  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();
  typename FixedImageType::Pointer   fixedImage = fixedImageReader->GetOutput();

  warper->SetInput( movingImageReader->GetOutput() );
  warper->SetInterpolator( interpolator );
  warper->SetOutputSpacing( fixedImage->GetSpacing() );
  warper->SetOutputOrigin( fixedImage->GetOrigin() );
  warper->SetOutputDirection( fixedImage->GetDirection() );

  warper->SetDisplacementField( filter->GetOutput() );

  // write the warped image into a file
  typedef  unsigned char OutputPixelType;

  typedef itk::Image<OutputPixelType, Dimension> OutputImageType;
  typedef itk::CastImageFilter<
    MovingImageType, OutputImageType>            CastFilterType;
  typedef itk::ImageFileWriter<OutputImageType>  WriterType;

  typename WriterType::Pointer     writer =  WriterType::New();
  typename CastFilterType::Pointer caster =  CastFilterType::New();

  char *outName = AppendFileName(argv[5], "_gpu");
  writer->SetFileName( outName );

  caster->SetInput( warper->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  writer->Update();

  TDisplacementFieldPointer ret = filter->GetOutput();

  return ret;
}

template <unsigned VDimension, typename TDisplacementFieldPointer>
TDisplacementFieldPointer itkCPUDemons(int, char *argv[])
{
  const unsigned int Dimension = VDimension;
  unsigned int numOfIterations = atoi( argv[2] );

  typedef unsigned short PixelType;

  typedef itk::Image<PixelType, Dimension> FixedImageType;
  typedef itk::Image<PixelType, Dimension> MovingImageType;

  typedef itk::ImageFileReader<FixedImageType>  FixedImageReaderType;
  typedef itk::ImageFileReader<MovingImageType> MovingImageReaderType;

  typename FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  typename MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName( argv[3] );
  movingImageReader->SetFileName( argv[4] );

  // casting pixel type from short to float
  typedef float                                    InternalPixelType;
  typedef itk::Image<InternalPixelType, Dimension> InternalImageType;
  typedef itk::CastImageFilter<FixedImageType,
                               InternalImageType>  FixedImageCasterType;
  typedef itk::CastImageFilter<MovingImageType,
                               InternalImageType>  MovingImageCasterType;

  typename FixedImageCasterType::Pointer  fixedImageCaster  = FixedImageCasterType::New();
  typename MovingImageCasterType::Pointer movingImageCaster = MovingImageCasterType::New();

  fixedImageCaster->SetInput( fixedImageReader->GetOutput() );
  movingImageCaster->SetInput( movingImageReader->GetOutput() );

  // maching intensity histogram
  typedef itk::HistogramMatchingImageFilter<
    InternalImageType,
    InternalImageType>   MatchingFilterType;
  typename MatchingFilterType::Pointer matcher = MatchingFilterType::New();

  matcher->SetInput( movingImageCaster->GetOutput() );
  matcher->SetReferenceImage( fixedImageCaster->GetOutput() );

  matcher->SetNumberOfHistogramLevels( 1024 );
  matcher->SetNumberOfMatchPoints( 7 );

  matcher->ThresholdAtMeanIntensityOn();

  // demons registration
  typedef typename TDisplacementFieldPointer::ObjectType     DisplacementFieldType;
  typedef itk::DemonsRegistrationFilter<InternalImageType,
    InternalImageType, DisplacementFieldType>       RegistrationFilterType;

  typename RegistrationFilterType::Pointer filter = RegistrationFilterType::New();

  typedef ShowProgressObject<RegistrationFilterType> ProgressType;
  ProgressType                                    progressWatch(filter);
  typename itk::SimpleMemberCommand<ProgressType>::Pointer command;
  command = itk::SimpleMemberCommand<ProgressType>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ProgressType::ShowProgress);
  filter->AddObserver( itk::ProgressEvent(), command);

  filter->SetFixedImage( fixedImageCaster->GetOutput() );
  filter->SetMovingImage( matcher->GetOutput() );

  filter->SetNumberOfIterations( numOfIterations );
  filter->SetStandardDeviations( displacementFieldSmoothingSigma );
  filter->SetUpdateFieldStandardDeviations( updateFieldSmoothingSigma );
  filter->SetSmoothUpdateField(smoothUpdateField);
  filter->SetMaximumRMSError(maximumRMSError);

  m_CPUTime.Start();
  filter->Update();
  m_CPUTime.Stop();

  // warp the image with the deformation field
  typedef itk::WarpImageFilter<
    MovingImageType,
    MovingImageType,
    DisplacementFieldType>     WarperType;
  typedef itk::LinearInterpolateImageFunction<
    MovingImageType, double>   InterpolatorType;
  typename WarperType::Pointer       warper = WarperType::New();
  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();
  typename FixedImageType::Pointer   fixedImage = fixedImageReader->GetOutput();

  warper->SetInput( movingImageReader->GetOutput() );
  warper->SetInterpolator( interpolator );
  warper->SetOutputSpacing( fixedImage->GetSpacing() );
  warper->SetOutputOrigin( fixedImage->GetOrigin() );
  warper->SetOutputDirection( fixedImage->GetDirection() );

  warper->SetDisplacementField( filter->GetOutput() );

  // write the warped image into a file
  typedef  unsigned char                         OutputPixelType;
  typedef itk::Image<OutputPixelType, Dimension> OutputImageType;
  typedef itk::CastImageFilter<
    MovingImageType, OutputImageType>            CastFilterType;
  typedef itk::ImageFileWriter<OutputImageType>  WriterType;

  typename WriterType::Pointer     writer =  WriterType::New();
  typename CastFilterType::Pointer caster =  CastFilterType::New();

  char *outName = AppendFileName(argv[5], "_cpu");
  writer->SetFileName( outName );

  caster->SetInput( warper->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  writer->Update();

  TDisplacementFieldPointer ret = filter->GetOutput();

  return ret;
}

}

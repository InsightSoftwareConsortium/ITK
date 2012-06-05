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
//#include "pathToOpenCLSourceCode.h"

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

namespace{
// The following class is used to support callbacks
// on the filter in the pipeline that follows later
template<typename TRegistration>
class ShowProgressObject
{
public:
  ShowProgressObject(TRegistration* o)
    {m_Process = o;}
  void ShowProgress()
    {
    std::cout << "Progress: " << m_Process->GetProgress() << "  ";
    std::cout << "Iter: " << m_Process->GetElapsedIterations() << "  ";
    std::cout << "Metric: "   << m_Process->GetMetric()   << "  ";
    std::cout << "RMSChange: " << m_Process->GetRMSChange() << "  ";
    std::cout << std::endl;
    if ( m_Process->GetElapsedIterations() == 10000 )
      { m_Process->StopRegistration(); }
    }
  typename TRegistration::Pointer m_Process;
};

const unsigned int ImageDimension = 2;
const unsigned int testIterations = 1;
const unsigned int numOfIterations = 500;

itk::TimeProbe gpuTime, cpuTime;

typedef float                                             InternalPixelType;
typedef itk::Vector< float, ImageDimension >              VectorPixelType;
typedef itk::GPUImage<  VectorPixelType, ImageDimension > GPUDeformationFieldType;
typedef itk::Image<  VectorPixelType, ImageDimension >    CPUDeformationFieldType;

itk::SmartPointer<GPUDeformationFieldType> itkGPUDemons(int argc, char *argv[], unsigned int &size);
itk::SmartPointer<CPUDeformationFieldType> itkCPUDemons(int argc, char *argv[], unsigned int &size);

char *AppendFileName(char *src, char *postfix)
{
  char *dest = new char[strlen(src) + strlen(postfix) + 1];
  char *pos = strrchr(src, '.');
  int skip = pos - src;
  sprintf(dest, "%s", src);
  sprintf(dest + skip, "%s", postfix);
  sprintf(dest + skip + strlen(postfix), "%s", pos);
  return dest;
}
}

int itkGPUDemonsRegistrationFilterTest(int argc, char *argv[])
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile " << std::endl;
    std::cerr << " [outputDeformationFieldFile] " << std::endl;
    return EXIT_SUCCESS;
    }

  bool passed;

  unsigned int size1 = 0, size2 = 0;

  GPUDeformationFieldType::Pointer gpuOut;
  CPUDeformationFieldType::Pointer cpuOut;

  for (unsigned int i=0; i<testIterations; i++)
    {
    std:: cout << "---------------------------------------------------" << std::endl;
    std::cout << "Starting GPU Demons" << std::endl;
    gpuOut = (itkGPUDemons(argc, argv, size1));
    std::cout << "Finished GPU Demons" << std::endl;

    std:: cout << "---------------------------------------------------" << std::endl;
    std::cout << "Starting CPU Demons" << std::endl;
    cpuOut = (itkCPUDemons(argc, argv, size2));
    std::cout << "Finished CPU Demons" << std::endl;

    }
  std::cout << "Average GPU registration time in seconds = " << gpuTime.GetMeanTime() << std::endl;
  std::cout << "Average CPU registration time in seconds = " << cpuTime.GetMeanTime() << std::endl;
  InternalPixelType maxDiff = 0, avgDiff = 0, diff, tmp;

  InternalPixelType *gpuBuf, *cpuBuf;
  gpuBuf = (InternalPixelType *) gpuOut->GetBufferPointer();
  cpuBuf = (InternalPixelType *) cpuOut->GetBufferPointer();

  for (unsigned int i=0; i<size1; i++)
    {
    diff = 0;
    for (unsigned int d=0; d<ImageDimension; d++)
      {
      tmp = gpuBuf[i*ImageDimension+d] - cpuBuf[i*ImageDimension+d];
      diff += tmp * tmp;
      }
    diff = vcl_sqrt(diff);
    avgDiff += diff;
    if (diff > maxDiff)
      {
      maxDiff = diff;
      }
    }
  avgDiff /= size1;
  std::cout << "Maximum displacement difference = " << maxDiff << std::endl;
  std::cout << "Average displacement difference = " << avgDiff << std::endl;

  //std::cout << "Total GPU time in seconds = " << gpuTime.GetMeanTime() << std::endl;
  //std::cout << "Initial GPU time in seconds = " << gpuInitTime.GetMeanTime() << std::endl;
  //std::cout << "Total CPU time in seconds = " << cpuTime.GetMeanTime() << std::endl;
  if (avgDiff < 2)
    {
    passed = true;
    }
  else
    {
    passed = false;
    }

  if ( !passed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;

}

namespace{

itk::SmartPointer<GPUDeformationFieldType> itkGPUDemons(int, char *argv[], unsigned int &size)
{
  const unsigned int Dimension = ImageDimension;
  typedef unsigned short PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer fixedImageReader   = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName( argv[1] );
  movingImageReader->SetFileName( argv[2] );

  //casting pixel type from short to float
  typedef float                                      InternalPixelType;
  typedef itk::GPUImage< InternalPixelType, Dimension > InternalImageType;
  typedef itk::CastImageFilter< FixedImageType,
                                InternalImageType >  FixedImageCasterType;
  typedef itk::CastImageFilter< MovingImageType,
                                InternalImageType >  MovingImageCasterType;

  FixedImageCasterType::Pointer fixedImageCaster   = FixedImageCasterType::New();
  MovingImageCasterType::Pointer movingImageCaster = MovingImageCasterType::New();

  fixedImageCaster->SetInput( fixedImageReader->GetOutput() );
  movingImageCaster->SetInput( movingImageReader->GetOutput() );

  //maching intensity histogram
  typedef itk::HistogramMatchingImageFilter<
                                    InternalImageType,
                                    InternalImageType >   MatchingFilterType;
  MatchingFilterType::Pointer matcher = MatchingFilterType::New();


  matcher->SetInput( movingImageCaster->GetOutput() );
  matcher->SetReferenceImage( fixedImageCaster->GetOutput() );

  matcher->SetNumberOfHistogramLevels( 1024 );
  matcher->SetNumberOfMatchPoints( 7 );

  matcher->ThresholdAtMeanIntensityOn();

  //demons registration
  typedef itk::Vector< float, Dimension >             VectorPixelType;
  typedef itk::GPUImage<  VectorPixelType, Dimension >   DeformationFieldType;
  typedef itk::GPUDemonsRegistrationFilter<
                                InternalImageType,
                                InternalImageType,
                                DeformationFieldType> RegistrationFilterType;

  RegistrationFilterType::Pointer filter = RegistrationFilterType::New();

  typedef ShowProgressObject<RegistrationFilterType> ProgressType;
  ProgressType progressWatch(filter);
  itk::SimpleMemberCommand<ProgressType>::Pointer command;
  command = itk::SimpleMemberCommand<ProgressType>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ProgressType::ShowProgress);
  filter->AddObserver( itk::ProgressEvent(), command);

  filter->SetFixedImage( fixedImageCaster->GetOutput() );
  filter->SetMovingImage( matcher->GetOutput() );

  filter->SetNumberOfIterations( numOfIterations );
  filter->SetStandardDeviations( 1.0 );

  gpuTime.Start();
  filter->Update();
  gpuTime.Stop();

  std::cout << "GPU InitTime in seconds = "          << filter->GetInitTime().GetTotal() << std::endl;
  std::cout << "GPU ComputeUpdateTime in seconds = " << filter->GetComputeUpdateTime().GetTotal() << std::endl;
  std::cout << "GPU ApplyUpdateTime in seconds = "   << filter->GetApplyUpdateTime().GetTotal() << std::endl;
  std::cout << "GPU SmoothFieldTime in seconds = "   << filter->GetSmoothFieldTime().GetTotal() << std::endl;

  //warp the image with the deformation field
  typedef itk::WarpImageFilter<
                          MovingImageType,
                          MovingImageType,
                          DeformationFieldType  >     WarperType;
  typedef itk::LinearInterpolateImageFunction<
                                   MovingImageType,
                                   double          >  InterpolatorType;
  WarperType::Pointer warper = WarperType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  warper->SetInput( movingImageReader->GetOutput() );
  warper->SetInterpolator( interpolator );
  warper->SetOutputSpacing( fixedImage->GetSpacing() );
  warper->SetOutputOrigin( fixedImage->GetOrigin() );
  warper->SetOutputDirection( fixedImage->GetDirection() );

  warper->SetDisplacementField( filter->GetOutput() );

  //write the warped image into a file
  typedef  unsigned char                           OutputPixelType;

  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  typedef itk::CastImageFilter<
                        MovingImageType,
                        OutputImageType > CastFilterType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();

  char *outName = AppendFileName(argv[3], (char *)"_gpu");
  writer->SetFileName( outName );

  caster->SetInput( warper->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  writer->Update();

  size = (unsigned int)(filter->GetFixedImage()->GetLargestPossibleRegion().GetNumberOfPixels());
  GPUDeformationFieldType::Pointer ret = filter->GetOutput();

  return ret;
}

itk::SmartPointer<CPUDeformationFieldType> itkCPUDemons(int, char *argv[], unsigned int &size)
{
  const unsigned int Dimension = ImageDimension;
  typedef unsigned short PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer fixedImageReader   = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName( argv[1] );
  movingImageReader->SetFileName( argv[2] );

  //casting pixel type from short to float
  typedef float                                      InternalPixelType;
  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;
  typedef itk::CastImageFilter< FixedImageType,
                                InternalImageType >  FixedImageCasterType;
  typedef itk::CastImageFilter< MovingImageType,
                                InternalImageType >  MovingImageCasterType;

  FixedImageCasterType::Pointer fixedImageCaster   = FixedImageCasterType::New();
  MovingImageCasterType::Pointer movingImageCaster = MovingImageCasterType::New();

  fixedImageCaster->SetInput( fixedImageReader->GetOutput() );
  movingImageCaster->SetInput( movingImageReader->GetOutput() );

  //maching intensity histogram
  typedef itk::HistogramMatchingImageFilter<
                                    InternalImageType,
                                    InternalImageType >   MatchingFilterType;
  MatchingFilterType::Pointer matcher = MatchingFilterType::New();


  matcher->SetInput( movingImageCaster->GetOutput() );
  matcher->SetReferenceImage( fixedImageCaster->GetOutput() );

  matcher->SetNumberOfHistogramLevels( 1024 );
  matcher->SetNumberOfMatchPoints( 7 );

  matcher->ThresholdAtMeanIntensityOn();

  //demons registration
  typedef itk::Vector< float, Dimension >             VectorPixelType;
  typedef itk::Image<  VectorPixelType, Dimension >   DeformationFieldType;
  typedef itk::DemonsRegistrationFilter<
                                InternalImageType,
                                InternalImageType,
                                DeformationFieldType> RegistrationFilterType;

  RegistrationFilterType::Pointer filter = RegistrationFilterType::New();

  typedef ShowProgressObject<RegistrationFilterType> ProgressType;
  ProgressType progressWatch(filter);
  itk::SimpleMemberCommand<ProgressType>::Pointer command;
  command = itk::SimpleMemberCommand<ProgressType>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ProgressType::ShowProgress);
  filter->AddObserver( itk::ProgressEvent(), command);

  filter->SetFixedImage( fixedImageCaster->GetOutput() );
  filter->SetMovingImage( matcher->GetOutput() );

  filter->SetNumberOfIterations( numOfIterations );
  filter->SetStandardDeviations( 1.0 );

  cpuTime.Start();
  filter->Update();
  cpuTime.Stop();


  //warp the image with the deformation field
  typedef itk::WarpImageFilter<
                          MovingImageType,
                          MovingImageType,
                          DeformationFieldType  >     WarperType;
  typedef itk::LinearInterpolateImageFunction<
                                   MovingImageType,
                                   double          >  InterpolatorType;
  WarperType::Pointer warper = WarperType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  warper->SetInput( movingImageReader->GetOutput() );
  warper->SetInterpolator( interpolator );
  warper->SetOutputSpacing( fixedImage->GetSpacing() );
  warper->SetOutputOrigin( fixedImage->GetOrigin() );
  warper->SetOutputDirection( fixedImage->GetDirection() );

  warper->SetDisplacementField( filter->GetOutput() );

  //write the warped image into a file
  typedef  unsigned char                           OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  typedef itk::CastImageFilter<
                        MovingImageType,
                        OutputImageType > CastFilterType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();

  char *outName = AppendFileName(argv[3], (char *)"_cpu");
  writer->SetFileName( outName );

  caster->SetInput( warper->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  writer->Update();

  size = (unsigned int)(filter->GetFixedImage()->GetLargestPossibleRegion().GetNumberOfPixels());
  CPUDeformationFieldType::Pointer ret = filter->GetOutput();

  return ret;
}
}

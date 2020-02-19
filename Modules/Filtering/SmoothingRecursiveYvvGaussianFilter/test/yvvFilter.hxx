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

#include "itkCastImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSmoothingRecursiveGaussianImageFilter.h"
#include "itkSmoothingRecursiveYvvGaussianImageFilter.h"
#include "itkTimeProbesCollectorBase.h"

#ifdef GPU
#  include "itkGPUImage.h"
#  include "itkGPUSmoothingRecursiveYvvGaussianImageFilter.h"
#endif


template <typename ImageType>
void
writeImage(std::string filterLabel, ImageType * result)
{
  using UnsignedCharImageType = itk::Image<unsigned char, ImageType::ImageDimension>;
  using CastFilterType = itk::CastImageFilter<ImageType, UnsignedCharImageType>;
  using WriterType = itk::ImageFileWriter<UnsignedCharImageType>;

  std::string outputFilename = filterLabel;

  typename CastFilterType::Pointer castFilter = CastFilterType::New();
  castFilter->SetInput(result);
  castFilter->Update();

  typename WriterType::Pointer writer = WriterType::New();

  if (ImageType::ImageDimension == 2)
  {
    outputFilename += ".jpg";
  }
  else
  {
    outputFilename += ".tif";
  }
  writer->SetFileName(outputFilename);
  writer->SetInput(castFilter->GetOutput());
  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & e)
  {
    std::cerr << e << std::endl;
  }
}


template <typename ImageType>
int
getSourceImage(void * sourceImagePtr, std::string inputFilename)
{
  using ReaderType = itk::ImageFileReader<ImageType>;
  typename ReaderType::Pointer readerGPU = ReaderType::New();
  readerGPU->SetFileName(inputFilename);
  try
  {
    readerGPU->Update();
  }
  catch (const itk::ImageFileReaderException & e)
  {
    std::cout << "Error : " << e.GetDescription() << std::endl;
    return -1;
  }

  auto * kimg = (typename ImageType::Pointer *)sourceImagePtr;
  *kimg = readerGPU->GetOutput();

  (*kimg)->DisconnectPipeline();
  return EXIT_SUCCESS;
}


template <typename ImageType>
int
createWhiteImage(void * sourceImagePtr, typename ImageType::SizeType size)
{
  auto *                         kimg = (typename ImageType::Pointer *)sourceImagePtr;
  typename ImageType::RegionType region;
  region.SetSize(size);

  // create
  *kimg = ImageType::New();
  (*kimg)->SetRegions(region);
  (*kimg)->Allocate();
  (*kimg)->FillBuffer(255.0);

  return EXIT_SUCCESS;
}


template <typename FilterType>
int
testCpuFilter(std::string &                                 filterLabel,
              std::string &                                 inputFilename,
              typename FilterType::InputImageType::SizeType size,
              float                                         sigma,
              std::string                                   parameters,
              itk::TimeProbesCollectorBase *                timeCollector)
{
  using InputImage = typename FilterType::InputImageType;
  typename InputImage::Pointer src;
  void *                       imgPtr = &src;

  if (inputFilename.empty())
  {
    createWhiteImage<InputImage>(imgPtr, size);
    // createStepImage< InputImage >( imgPtr, size );

    std::ostringstream sizeStream;

    sizeStream << size[0];
    for (unsigned int i = 1; i < InputImage::ImageDimension; ++i)
    {
      sizeStream << "x" << size[i];
    }
    parameters += sizeStream.str();
  }
  else
  {
    getSourceImage<InputImage>(imgPtr, inputFilename);
  }

  typename FilterType::Pointer filter = FilterType::New();
  filter->SetSigma(sigma);
  filter->SetInput(src);
  filter->Update();

  {
    src->Modified();
    filter->Modified();

    timeCollector->Start(filterLabel.c_str());
    filter->Update();
    filter->GetOutput();
    timeCollector->Stop(filterLabel.c_str());

    writeImage<typename FilterType::InputImageType>(filterLabel + parameters, filter->GetOutput());
  }

  src->DisconnectPipeline();
  src = nullptr;
  filter = nullptr;
  imgPtr = nullptr;
  return EXIT_SUCCESS;
}


#ifdef GPU

template <typename FilterType>
int
testGpuFilter(std::string &                                 filterLabel,
              std::string &                                 inputFilename,
              typename FilterType::InputImageType::SizeType size,
              float                                         sigma,
              std::string                                   parameters,
              itk::TimeProbesCollectorBase *                timeCollector,
              bool                                          measureWithSync)
{
  using InputImage = typename FilterType::InputImageType;

  typename InputImage::Pointer src;
  void *                       imgPtr = &src;

  if (inputFilename.empty())
  {
    createWhiteImage<InputImage>(imgPtr, size);
    // createStepImage<InputImage>(imgPtr, size);

    std::ostringstream sizeStream;

    sizeStream << size[0];
    for (int i = 1; i < InputImage::ImageDimension; ++i)
    {
      sizeStream << "x" << size[i];
    }
    parameters += sizeStream.str();
  }
  else
  {
    getSourceImage<InputImage>(imgPtr, inputFilename);
  }

  typename FilterType::Pointer filter = FilterType::New();
  filter->SetSigma(sigma);
  filter->SetInput(src);
  filter->Update();
  filter->GetOutput()->UpdateBuffers(); // force first CPU-GPU synchronisation

  {
    // src->Modified(); //No need, filter->Update forces call to GPUGenerateData()
    filter->Modified();
    if (measureWithSync)
    {
      timeCollector->Start(filterLabel.c_str());
      filter->Update();
      filter->GetOutput()->UpdateBuffers();
      timeCollector->Stop(filterLabel.c_str());
    }
    else
    {
      timeCollector->Start(filterLabel.c_str());
      filter->Update();
      timeCollector->Stop(filterLabel.c_str());
      filter->GetOutput()->UpdateBuffers();
    }
    writeImage<InputImage>(filterLabel + parameters, filter->GetOutput());
  }

  src->DisconnectPipeline();
  src = nullptr;
  filter = nullptr;
  imgPtr = nullptr;
  return EXIT_SUCCESS;
}

#endif


template <typename ImageType>
int
testImage(std::string inputFilename, float sigma, itk::TimeProbesCollectorBase * timeCollector, unsigned int ntests)
{
#ifdef GPU
  using GPUImageType = itk::GPUImage<typename ImageType::PixelType, ImageType::ImageDimension>;
  using GPUrecursiveYVVFilterType = itk::GPUSmoothingRecursiveYvvGaussianImageFilter<GPUImageType, GPUImageType>;
#endif
  using CPUImageType = ImageType;
  using RecursiveYVVFilterType = itk::SmoothingRecursiveYvvGaussianImageFilter<CPUImageType, CPUImageType>;
  using DericheFilterType = itk::SmoothingRecursiveGaussianImageFilter<CPUImageType, CPUImageType>;

  std::cout << ":::: Testing on " << inputFilename << ", using sigma = " << sigma << "   ::::" << std::endl;
  typename ImageType::SizeType size;
  size.Fill(0); // empty, since we'll be using an actual file
  std::ostringstream parameterStream;
  parameterStream << "_s" << sigma << "_";

  ////////////////////////////////
  std::string dericheLabel = "cpu_Deriche";
  for (unsigned int i = 0; i < ntests; ++i)
  {
    testCpuFilter<DericheFilterType>(dericheLabel, inputFilename, size, sigma, parameterStream.str(), timeCollector);
  }
  ////////////////////////////////
  std::string cpuYvvLabel = "cpu_Yvv";
  for (unsigned int i = 0; i < ntests; ++i)
  {
    testCpuFilter<RecursiveYVVFilterType>(
      cpuYvvLabel, inputFilename, size, sigma, parameterStream.str(), timeCollector);
  }

//////////////////////////////
#ifdef GPU
  std::string gpuYvvLabel = "gpu_Yvv_wSync";
  for (unsigned int i = 0; i < ntests; ++i)
  {
    testGpuFilter<GPUrecursiveYVVFilterType>(
      gpuYvvLabel, inputFilename, size, sigma, parameterStream.str(), timeCollector, true);
    /* Stress tests (e.g. high ntests) may cause CL_MEM_OBJECT_ALLOCATION_FAILURE,
     * even when re-instantiating input _and_ filter. Not all GPUs support resetting, so adding a sleep helps.*/
    sleep(1);
  }

  std::string gpuYvvLabelNoSync = "gpu_Yvv_NoSyn";
  for (unsigned int i = 0; i < ntests; ++i)
  {
    testGpuFilter<GPUrecursiveYVVFilterType>(
      gpuYvvLabelNoSync, inputFilename, size, sigma, parameterStream.str(), timeCollector, false);
    sleep(1); // helps for stress tests
  }

#endif
  return EXIT_SUCCESS;
}


template <typename ImageType>
int
testWhite(typename ImageType::SizeType   size,
          float                          sigma,
          itk::TimeProbesCollectorBase * timeCollector,
          unsigned int                   ntests)
{
  std::cout << "Testing: " << size << " with sigma = " << sigma << ". Average over " << ntests << " runs." << std::endl;

#ifdef GPU
  using GPUImageType = itk::GPUImage<typename ImageType::PixelType, ImageType::ImageDimension>;
  using GPUrecursiveYVVFilterType = itk::GPUSmoothingRecursiveYvvGaussianImageFilter<GPUImageType, GPUImageType>;
#endif
  using CPUImageType = ImageType;
  using RecursiveYVVFilterType = itk::SmoothingRecursiveYvvGaussianImageFilter<CPUImageType, CPUImageType>;
  using DericheFilterType = itk::SmoothingRecursiveGaussianImageFilter<CPUImageType, CPUImageType>;

  std::string        emptyFilename;
  std::ostringstream parameterStream;
  parameterStream << "_s" << sigma << "_";

  std::cout << ":::: Testing on white image: " << size << ", using sigma = " << sigma << "   ::::" << std::endl;

  ////////////////////////////////
  std::string dericheLabel = "cpu_Deriche";
  for (unsigned int i = 0; i < ntests; ++i)
  {
    testCpuFilter<DericheFilterType>(dericheLabel, emptyFilename, size, sigma, parameterStream.str(), timeCollector);
  }
  ////////////////////////////////
  std::string cpuYvvLabel = "cpu_Yvv";
  for (unsigned int i = 0; i < ntests; ++i)
  {
    testCpuFilter<RecursiveYVVFilterType>(
      cpuYvvLabel, emptyFilename, size, sigma, parameterStream.str(), timeCollector);
  }

//////////////////////////////
#ifdef GPU
  std::string gpuYvvLabelSync = "gpu_Yvv_wSync";
  for (unsigned int i = 0; i < ntests; ++i)
  {
    testGpuFilter<GPUrecursiveYVVFilterType>(
      gpuYvvLabelSync, emptyFilename, size, sigma, parameterStream.str(), timeCollector, true);
    /* Stress tests (e.g. high ntests) may cause CL_MEM_OBJECT_ALLOCATION_FAILURE,
     * even when re-instantiating input _and_ filter. Not all GPUs support resetting, so adding a sleep helps.*/
    sleep(1);
  }

  std::string gpuYvvLabelNoSync = "gpu_Yvv_NoSyn";
  for (unsigned int i = 0; i < ntests; ++i)
  {
    testGpuFilter<GPUrecursiveYVVFilterType>(
      gpuYvvLabelNoSync, emptyFilename, size, sigma, parameterStream.str(), timeCollector, false);
    sleep(1); // helps for stress tests
  }
#endif
  return EXIT_SUCCESS;
}

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

#include "itkZeroDCImageFilter.h"
#include <string>
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
using namespace std;
using namespace itk;

template <unsigned int N>
int
runZeroDCImageFilterTest(const std::string & inputImage)
{
  const unsigned int dimension = N;
  // TODO massive difference (-1,4, 3^-10) of 0 freq pixel between (float, double) I guess it is because FFT algorithm
  // not ZeroDC. Maybe because odd size using FFTW.
  typedef float                            PixelType;
  typedef itk::Image<PixelType, dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;
  typename ReaderType::Pointer             reader = ReaderType::New();
  reader->SetFileName(inputImage);
  typedef itk::ZeroDCImageFilter<ImageType> ZeroDCFilterType;
  typename ZeroDCFilterType::Pointer        zeroDCFilter = ZeroDCFilterType::New();
  zeroDCFilter->SetInput(reader->GetOutput());

  // Perform FFT on zeroDC image and check freq bin 0 has zero value.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTForwardFilterType;
  typename FFTForwardFilterType::Pointer        fftForwardFilter = FFTForwardFilterType::New();
  fftForwardFilter->SetInput(zeroDCFilter->GetOutput());
  fftForwardFilter->Update();
  typedef typename FFTForwardFilterType::OutputImageType ComplexImageType;
  typename ComplexImageType::IndexType                   zeroIndex;
  zeroIndex.Fill(0);

  typename ComplexImageType::Pointer filteredImg = fftForwardFilter->GetOutput();
  filteredImg->DisconnectPipeline();
  typename ComplexImageType::PixelType filteredZeroFreqPixelValue = filteredImg->GetPixel(zeroIndex);
  typename ZeroDCFilterType::RealType  mean = zeroDCFilter->GetMean();

  fftForwardFilter->SetInput(reader->GetOutput());
  fftForwardFilter->Update();
  typename ComplexImageType::Pointer   originalImg = fftForwardFilter->GetOutput();
  typename ComplexImageType::PixelType originalZeroFreqPixelValue = originalImg->GetPixel(zeroIndex);

  if (itk::Math::NotAlmostEquals(filteredZeroFreqPixelValue.real(), 0.0))
  {
    std::cerr << "DC Component (Index = " << zeroIndex << ") is not zero: " << filteredZeroFreqPixelValue.real()
              << std::endl;
    std::cerr << "DC Component (Index = " << zeroIndex
              << ") of original image is: " << originalZeroFreqPixelValue.real() << std::endl;
    std::cerr << "Mean value of original image (spatial domain) is: " << mean << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
int
itkZeroDCImageFilterTest(int argc, char * argv[])
{
  if (argc < 2 || argc > 3)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage [dimension]" << std::endl;
    return EXIT_FAILURE;
  }
  const string inputImage = argv[1];
  unsigned int dimension = 3;
  if (argc == 3)
  {
    dimension = atoi(argv[2]);
  }

  if (dimension == 2)
    return runZeroDCImageFilterTest<2>(inputImage);
  else if (dimension == 3)
    return runZeroDCImageFilterTest<3>(inputImage);
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}

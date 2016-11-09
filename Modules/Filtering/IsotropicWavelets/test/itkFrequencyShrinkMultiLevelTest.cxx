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
#include <memory>
#include <string>
#include <cmath>
#include <iomanip>
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include <itkComplexToRealImageFilter.h>
#include <itkComplexToImaginaryImageFilter.h>
#include <itkFrequencyShrinkImageFilter.h>
#include <itkShrinkImageFilter.h>
#include <itkZeroDCImageFilter.h>
#include <itkComplexToComplexFFTImageFilter.h>
#include <itkImageRegionConstIterator.h>
#include <itkImageRegionConstIteratorWithIndex.h>
#include <itkCastImageFilter.h>
#include <itkNumberToString.h>
#include "itkIsotropicWaveletTestUtilities.h"
using namespace std;
using namespace itk;

// Visualize for dev/debug purposes. Set in cmake file. Require VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

template <unsigned int N>
int
runFrequencyShrinkMultiLevelTest(const std::string & inputImage, const std::string & outputImage, unsigned int levels)
{
  const unsigned int                       dimension = N;
  typedef double                           PixelType;
  typedef itk::Image<PixelType, dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;
  typename ReaderType::Pointer             reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();
  reader->UpdateLargestPossibleRegion();

  /***** Calculate mean value and substract: zeroDCFilter ****/
  typedef itk::ZeroDCImageFilter<ImageType> ZeroDCFilterType;
  typename ZeroDCFilterType::Pointer        zeroDCFilter = ZeroDCFilterType::New();
  zeroDCFilter->SetInput(reader->GetOutput());
  /**********************************************/

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  typename FFTFilterType::Pointer               fftFilter = FFTFilterType::New();
  fftFilter->SetInput(zeroDCFilter->GetOutput());
  typedef typename FFTFilterType::OutputImageType ComplexImageType;

  /*********** SHRINK ***************/
  typedef itk::FrequencyShrinkImageFilter<ComplexImageType>       ShrinkType;
  typedef itk::InverseFFTImageFilter<ComplexImageType, ImageType> InverseFFTFilterType;

  typename InverseFFTFilterType::Pointer inverseFFT = InverseFFTFilterType::New();
  fftFilter->Update();
  typename ComplexImageType::Pointer shrinkedImage = fftFilter->GetOutput();
  shrinkedImage->DisconnectPipeline();
  typedef itk::ShrinkImageFilter<ImageType, ImageType> RegularShrinkType;
  typename ImageType::Pointer                          shrinkedImageRegular = zeroDCFilter->GetOutput();
  shrinkedImageRegular->DisconnectPipeline();
  for (unsigned int l = 0; l < levels; ++l)
  {
    typename ShrinkType::Pointer shrinkFilterLevel = ShrinkType::New();
    shrinkFilterLevel->SetInput(shrinkedImage);
    shrinkFilterLevel->SetShrinkFactors(2);
    shrinkFilterLevel->Update();
    shrinkedImage = shrinkFilterLevel->GetOutput();
    shrinkedImage->DisconnectPipeline();

    inverseFFT->SetInput(shrinkedImage);
    inverseFFT->Update();
#ifdef ITK_VISUALIZE_TESTS
    itk::NumberToString<unsigned int> n2s;
    itk::Testing::ViewImage(inverseFFT->GetOutput(), "FrequencyShrink, level:" + n2s(l));
#endif
    // Regular shrinker (center is equal in original and downsampled)
    typename RegularShrinkType::Pointer shrinkFilterLevelRegular = RegularShrinkType::New();
    shrinkFilterLevelRegular->SetInput(shrinkedImageRegular);
    shrinkFilterLevelRegular->SetShrinkFactors(2);
    shrinkFilterLevelRegular->Update();
    shrinkedImageRegular = shrinkFilterLevelRegular->GetOutput();
    shrinkedImageRegular->DisconnectPipeline();
#ifdef ITK_VISUALIZE_TESTS
    itk::Testing::ViewImage(shrinkedImageRegular.GetPointer(), "RegularShrink, level:" + n2s(l));
#endif
  }

  // Write last output for comparisson
  typedef itk::Image<float, dimension>                    FloatImageType;
  typedef itk::CastImageFilter<ImageType, FloatImageType> CastType;
  typename CastType::Pointer                              castFilter = CastType::New();
  castFilter->SetInput(inverseFFT->GetOutput());

  typedef itk::ImageFileWriter<FloatImageType> WriterType;
  typename WriterType::Pointer                 writer = WriterType::New();
  writer->SetFileName(outputImage);
  writer->SetInput(castFilter->GetOutput());

  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error writing the FrequencyShrink image: " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
  }

#ifdef ITK_VISUALIZE_TESTS
  Testing::ViewImage(zeroDCFilter->GetOutput(), "Original");
#endif

  return EXIT_SUCCESS;
}

int
itkFrequencyShrinkMultiLevelTest(int argc, char * argv[])
{
  if (argc < 4 || argc > 5)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage levels [dimension]" << std::endl;
    return EXIT_FAILURE;
  }
  const string       inputImage = argv[1];
  const string       outputImage = argv[2];
  const unsigned int levels = atoi(argv[3]);

  unsigned int dimension = 3;
  if (argc == 5)
    dimension = atoi(argv[4]);

  if (dimension == 2)
    return runFrequencyShrinkMultiLevelTest<2>(inputImage, outputImage, levels);
  else if (dimension == 3)
    return runFrequencyShrinkMultiLevelTest<3>(inputImage, outputImage, levels);
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}

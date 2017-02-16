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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkComplexToRealImageFilter.h"
#include "itkComplexToImaginaryImageFilter.h"
#include "itkFrequencyShrinkImageFilter.h"
#include "itkFrequencyExpandImageFilter.h"
#include "itkFrequencyShrinkViaInverseFFTImageFilter.h"
#include "itkFrequencyExpandViaInverseFFTImageFilter.h"
#include "itkZeroDCImageFilter.h"
#include "itkComplexToComplexFFTImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkCastImageFilter.h"
#include "itkNumberToString.h"
#include "itkIsotropicWaveletTestUtilities.h"
#include "itkTestingMacros.h"

#include <memory>
#include <string>
#include <cmath>
#include <iomanip>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif


template <unsigned int VDimension>
int
runFrequencyExpandAndShrinkTest(const std::string & inputImage, const std::string & outputImage)
{
  const unsigned int Dimension = VDimension;

  typedef double                           PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;

  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputImage);

  reader->Update();

  // Calculate mean value and subtract
  typedef itk::ZeroDCImageFilter<ImageType> ZeroDCFilterType;
  typename ZeroDCFilterType::Pointer        zeroDCFilter = ZeroDCFilterType::New();

  zeroDCFilter->SetInput(reader->GetOutput());

  zeroDCFilter->Update();

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  typename FFTFilterType::Pointer               fftFilter = FFTFilterType::New();

  fftFilter->SetInput(zeroDCFilter->GetOutput());

  fftFilter->Update();

  typedef typename FFTFilterType::OutputImageType                 ComplexImageType;
  typedef itk::InverseFFTImageFilter<ComplexImageType, ImageType> InverseFFTFilterType;
  /*********** EXPAND ***************/
  typedef itk::FrequencyExpandImageFilter<ComplexImageType> ExpandType;
  typename ExpandType::Pointer                              expandFilter = ExpandType::New();
  expandFilter->SetInput(fftFilter->GetOutput());
  expandFilter->SetExpandFactors(2);
  expandFilter->Update();

  typedef itk::FrequencyExpandViaInverseFFTImageFilter<ComplexImageType> ExpandViaInverseFFTType;
  typename ExpandViaInverseFFTType::Pointer expandViaInverseFFTFilter = ExpandViaInverseFFTType::New();
  expandViaInverseFFTFilter->SetInput(fftFilter->GetOutput());
  expandViaInverseFFTFilter->SetExpandFactors(2);
  expandViaInverseFFTFilter->Update();

  // #ifdef ITK_VISUALIZE_TESTS
  //   typename InverseFFTFilterType::Pointer inverseFFTExpand1 = InverseFFTFilterType::New();
  //   inverseFFTExpand1->SetInput(expandFilter->GetOutput());
  //   inverseFFTExpand1->Update();
  //   typename InverseFFTFilterType::Pointer inverseFFTExpand2 = InverseFFTFilterType::New();
  //   inverseFFTExpand2->SetInput(expandViaInverseFFTFilter->GetOutput());
  //   inverseFFTExpand2->Update();
  //   itk::Testing::ViewImage(inverseFFTExpand1->GetOutput(), "Expand via frequency manipulation");
  //   itk::Testing::ViewImage(inverseFFTExpand2->GetOutput(), "Expand ViaInverseFFT");
  // #endif

  /*********** SHRINK ***************/
  typedef itk::FrequencyShrinkImageFilter<ComplexImageType> ShrinkType;
  typename ShrinkType::Pointer                              shrinkFilter = ShrinkType::New();
  shrinkFilter->SetInput(expandFilter->GetOutput());
  shrinkFilter->SetShrinkFactors(2);
  shrinkFilter->Update();

  typedef itk::FrequencyShrinkViaInverseFFTImageFilter<ComplexImageType> ShrinkViaInverseFFTType;
  typename ShrinkViaInverseFFTType::Pointer shrinkViaInverseFFTFilter = ShrinkViaInverseFFTType::New();
  shrinkViaInverseFFTFilter->SetInput(expandViaInverseFFTFilter->GetOutput());
  shrinkViaInverseFFTFilter->SetShrinkFactors(2);
  shrinkViaInverseFFTFilter->Update();

  /*********** InverseFFT ***************/

  typename InverseFFTFilterType::Pointer inverseFFT1 = InverseFFTFilterType::New();
  inverseFFT1->SetInput(shrinkFilter->GetOutput());
  inverseFFT1->Update();
  typename InverseFFTFilterType::Pointer inverseFFT2 = InverseFFTFilterType::New();
  inverseFFT2->SetInput(shrinkViaInverseFFTFilter->GetOutput());

  inverseFFT2->Update();

#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(inverseFFT1->GetOutput(), "ExpandAndShrink via frequency manipulation");
  itk::Testing::ViewImage(inverseFFT2->GetOutput(), "ExpandAndShrink ViaInverseFFT");
#endif

  // Write output
  typedef itk::Image<float, Dimension>                    FloatImageType;
  typedef itk::CastImageFilter<ImageType, FloatImageType> CastType;
  typename CastType::Pointer                              castFilter = CastType::New();
  castFilter->SetInput(inverseFFT2->GetOutput());

  typedef itk::ImageFileWriter<FloatImageType> WriterType;
  typename WriterType::Pointer                 writer = WriterType::New();
  writer->SetFileName(outputImage);
  writer->SetInput(castFilter->GetOutput());

  TRY_EXPECT_NO_EXCEPTION(writer->Update());


#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(zeroDCFilter->GetOutput(), "Original");
#endif

  return EXIT_SUCCESS;
}

int
itkFrequencyExpandAndShrinkTest(int argc, char * argv[])
{
  if (argc < 3 || argc > 4)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage [dimension]" << std::endl;
    return EXIT_FAILURE;
  }
  const std::string inputImage = argv[1];
  const std::string outputImage = argv[2];

  const unsigned int                            ImageDimension = 3;
  typedef double                                PixelType;
  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  typedef FFTFilterType::OutputImageType        ComplexImageType;

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  typedef itk::FrequencyExpandViaInverseFFTImageFilter<ComplexImageType> ExpandViaInverseFFTType;
  ExpandViaInverseFFTType::Pointer expandViaInverseFFTFilter = ExpandViaInverseFFTType::New();

  EXERCISE_BASIC_OBJECT_METHODS(expandViaInverseFFTFilter, FrequencyExpandViaInverseFFTImageFilter, ImageToImageFilter);

  typedef itk::FrequencyShrinkViaInverseFFTImageFilter<ComplexImageType> ShrinkViaInverseFFTType;
  ShrinkViaInverseFFTType::Pointer shrinkViaInverseFFTFilter = ShrinkViaInverseFFTType::New();

  EXERCISE_BASIC_OBJECT_METHODS(shrinkViaInverseFFTFilter, FrequencyShrinkViaInverseFFTImageFilter, ImageToImageFilter);


  unsigned int dimension = 3;
  if (argc == 4)
  {
    dimension = atoi(argv[3]);
  }

  if (dimension == 2)
  {
    return runFrequencyExpandAndShrinkTest<2>(inputImage, outputImage);
  }
  else if (dimension == 3)
  {
    return runFrequencyExpandAndShrinkTest<3>(inputImage, outputImage);
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}

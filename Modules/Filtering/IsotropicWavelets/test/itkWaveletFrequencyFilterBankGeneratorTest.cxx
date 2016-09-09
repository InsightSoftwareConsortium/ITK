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
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkWaveletFrequencyFilterBankGenerator.h"
#include "itkHeldIsotropicWavelet.h"
#include "itkVowIsotropicWavelet.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include <itkComplexToRealImageFilter.h>
#include <itkImageRegionConstIterator.h>
// Visualize for dev/debug purposes. Set in cmake file. Require VTK
#if ITK_VISUALIZE_TESTS != 0
#  include "itkView3DImage.h"
#endif
using namespace std;
using namespace itk;

int
itkWaveletFrequencyFilterBankGeneratorTest(int argc, char ** argv)
{
  if (argc != 4)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage inputBands" << std::endl;
    return EXIT_FAILURE;
  }
  const string inputImage = argv[1];
  const string outputImage = argv[2];
  unsigned int inputBands = atoi(argv[3]);

  const unsigned int                       dimension = 3;
  typedef float                            PixelType;
  typedef itk::Image<PixelType, dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;
  typename ReaderType::Pointer             reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();
  reader->UpdateLargestPossibleRegion();

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  typename FFTFilterType::Pointer               fftFilter = FFTFilterType::New();
  fftFilter->SetInput(reader->GetOutput());
  fftFilter->Update();
  typedef typename FFTFilterType::OutputImageType ComplexImageType;

  // Set the WaveletFunctionType and the WaveletFilterBank
  // typedef itk::HeldIsotropicWavelet<PixelType> WaveletFunctionType;
  typedef itk::VowIsotropicWavelet<PixelType>                                             WaveletFunctionType;
  typedef itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, WaveletFunctionType> WaveletFilterBankType;
  typename WaveletFilterBankType::Pointer forwardFilterBank = WaveletFilterBankType::New();
  unsigned int                            high_sub_bands = inputBands;
  forwardFilterBank->SetHighPassSubBands(high_sub_bands);
  forwardFilterBank->SetSize(fftFilter->GetOutput()->GetLargestPossibleRegion().GetSize());
  forwardFilterBank->Update();
  // forwardFilterBank->Print(std::cout);

  // Get real part of complex image for visualization
  typedef itk::ComplexToRealImageFilter<ComplexImageType, ImageType> ComplexToRealFilter;
  typename ComplexToRealFilter::Pointer                              complexToRealFilter = ComplexToRealFilter::New();
  std::cout << "Real Part of ComplexImage:" << std::endl;
  for (unsigned int i = 0; i < high_sub_bands + 1; ++i)
  {
    std::cout << "Band: " << i << " / " << forwardFilterBank->GetHighPassSubBands() << std::endl;
    // std::cout << "Largest Region: " << forwardFilterBank->GetOutput(i)->GetLargestPossibleRegion() << std::endl;

    complexToRealFilter->SetInput(forwardFilterBank->GetOutput(i));
    complexToRealFilter->Update();
#if ITK_VISUALIZE_TESTS != 0
    View3DImage(complexToRealFilter->GetOutput());
#endif
  }
  // Write only the last band.
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer                     writer = WriterType::New();
  writer->SetFileName(outputImage);
  writer->SetInput(complexToRealFilter->GetOutput());
  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error writing the last band of WaveletFrequencyFilterBankGeneratorTest: " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
  }

  // Inverse FFT Transform
  typedef itk::InverseFFTImageFilter<ComplexImageType, ImageType> InverseFFTFilterType;
  typename InverseFFTFilterType::Pointer                          inverseFFT = InverseFFTFilterType::New();
  std::cout << "InverseFFT:" << std::endl;
  for (unsigned int i = 0; i < high_sub_bands + 1; ++i)
  {
    std::cout << "Band: " << i << " / " << forwardFilterBank->GetHighPassSubBands() << std::endl;
    inverseFFT->SetInput(forwardFilterBank->GetOutput(i));
    inverseFFT->Update();
#if ITK_VISUALIZE_TESTS != 0
    View3DImage(inverseFFT->GetOutput());
#endif
  }


  // Create a new filter for the inverse Filter Bank
  // TODO if you just change the InverseFlag, the output already generated by the filter will get overriden, and trigger
  // the pipeline.
  typename WaveletFilterBankType::Pointer inverseFilterBank = WaveletFilterBankType::New();
  inverseFilterBank->SetInverseBank(true);
  inverseFilterBank->SetHighPassSubBands(high_sub_bands);
  inverseFilterBank->SetSize(fftFilter->GetOutput()->GetLargestPossibleRegion().GetSize());
  inverseFilterBank->Update();

  // Compare images: TODO use itk test facilities instead of region iterators?
  //  itk::Testing::ComparisonImageFilter does not work with complex
  typedef typename itk::ImageRegionConstIterator<ComplexImageType> ComplexConstRegionIterator;
  unsigned int                                                     ne = 0;
  for (unsigned int i = 0; i < high_sub_bands + 1; ++i)
  {
    typename ComplexImageType::Pointer outForward = forwardFilterBank->GetOutput(i);
    typename ComplexImageType::Pointer outInverse = inverseFilterBank->GetOutput(i);
    ComplexConstRegionIterator         itForward(outForward, outForward->GetLargestPossibleRegion());
    ComplexConstRegionIterator         itInverse(outInverse, outInverse->GetLargestPossibleRegion());
    itForward.GoToBegin();
    itInverse.GoToBegin();
    unsigned int ne_per_band = 0;
    while (!itForward.IsAtEnd() || !itInverse.IsAtEnd())
    {
      if (itForward.Get() != itInverse.Get())
        ++ne_per_band;
      ++itForward;
      ++itInverse;
    }
    ne += ne_per_band;
  }
  if (ne > 0)
    std::cout << "Comparison Error, num of errors: " << ne << '\n';
  else
    std::cout << "Pass! no comparison errors: " << ne << '\n';
  return EXIT_SUCCESS;
}

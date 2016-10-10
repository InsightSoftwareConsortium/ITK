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
#include "itkSimoncelliIsotropicWavelet.h"
#include "itkShannonIsotropicWavelet.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include <itkComplexToRealImageFilter.h>
#include <itkImageRegionConstIterator.h>
#include <itkNumberToString.h>
// Visualize for dev/debug purposes. Set in cmake file. Require VTK
#if ITK_VISUALIZE_TESTS != 0
#  include "itkViewImage.h"
#endif
using namespace std;
using namespace itk;

template <unsigned int N, typename TWaveletFunction>
int
runWaveletFrequencyFilterBankGeneratorTest(const std::string &  inputImage,
                                           const std::string &  outputImage,
                                           const unsigned int & inputBands)
{
  const unsigned int                       dimension = N;
  typedef float                            PixelType;
  typedef itk::Image<PixelType, dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;
  typename ReaderType::Pointer             reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();
  reader->UpdateLargestPossibleRegion();

  itk::NumberToString<unsigned int> n2s;
  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  typename FFTFilterType::Pointer               fftFilter = FFTFilterType::New();
  fftFilter->SetInput(reader->GetOutput());
  fftFilter->Update();
  typedef typename FFTFilterType::OutputImageType ComplexImageType;

  typedef TWaveletFunction WaveletFunctionType;
  // TODO remove or move to WaveletFunctionType test.
  // Check profile of subbands.
  size_t              points = 10000;
  std::vector<double> w_array(points);
  double              init = 0;
  double              end = +2 * itk::Math::pi;
  double              interval = (end - init) / (points - 1);
  for (unsigned int i = 0; i < points; ++i)
  {
    w_array[i] = init + interval * i;
  }
  typename WaveletFunctionType::Pointer motherWavelet = WaveletFunctionType::New();
  motherWavelet->SetHighPassSubBands(inputBands);
  // Write profile.
  std::vector<std::vector<double>> subBandsResults;
  for (unsigned int k = 0; k < inputBands + 1; ++k)
  {
    std::ofstream ofs("/home/phc/tmp/wavelet_subband_" + n2s(k) + "_" + n2s(inputBands) + ".txt", std::ofstream::out);
    std::vector<double> bandResults;
    for (unsigned int i = 0; i < points; ++i)
    {
      bandResults.push_back(motherWavelet->EvaluateForwardSubBand(motherWavelet->RadPerSecToHertz(w_array[i]), k));
      ofs << w_array[i] << "," << bandResults.back() << "\n";
    }
    ofs.close();
    subBandsResults.push_back(bandResults);
  }

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
    Testing::ViewImage(complexToRealFilter->GetOutput(),
                       "RealPart of Complex. Band: " + n2s(i) + "/" + n2s(high_sub_bands));
#endif
  }
  // Write only the last band.
  typedef itk::ImageFileWriter<ImageType> WriterType;
  typename WriterType::Pointer            writer = WriterType::New();
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
    Testing::ViewImage(inverseFFT->GetOutput(), "InverseFFT. Band: " + n2s(i) + "/" + n2s(high_sub_bands));
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
  typedef itk::ImageRegionConstIterator<ComplexImageType> ComplexConstRegionIterator;
  unsigned int                                            ne = 0;
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

int
itkWaveletFrequencyFilterBankGeneratorTest(int argc, char * argv[])
{
  if (argc < 5 || argc > 6)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage inputBands waveletFunction [dimension]" << std::endl;
    return EXIT_FAILURE;
  }
  const string       inputImage = argv[1];
  const string       outputImage = argv[2];
  const unsigned int inputBands = atoi(argv[3]);
  const string       waveletFunction = argv[4];

  unsigned int dimension = 3;
  if (argc == 6)
  {
    dimension = atoi(argv[5]);
  }
  typedef itk::HeldIsotropicWavelet<>       HeldWavelet;
  typedef itk::VowIsotropicWavelet<>        VowWavelet;
  typedef itk::SimoncelliIsotropicWavelet<> SimoncelliWavelet;
  typedef itk::ShannonIsotropicWavelet<>    ShannonWavelet;

  if (dimension == 2)
  {
    if (waveletFunction == "Held")
      return runWaveletFrequencyFilterBankGeneratorTest<2, HeldWavelet>(inputImage, outputImage, inputBands);
    else if (waveletFunction == "Vow")
      return runWaveletFrequencyFilterBankGeneratorTest<2, VowWavelet>(inputImage, outputImage, inputBands);
    else if (waveletFunction == "Simoncelli")
      return runWaveletFrequencyFilterBankGeneratorTest<2, SimoncelliWavelet>(inputImage, outputImage, inputBands);
    else if (waveletFunction == "Shannon")
      return runWaveletFrequencyFilterBankGeneratorTest<2, ShannonWavelet>(inputImage, outputImage, inputBands);
    else
    {
      std::cerr << argv[4] << " is an unknown wavelet type " << std::endl;
      return EXIT_FAILURE;
    }
  }
  else if (dimension == 3)
  {
    if (waveletFunction == "Held")
      return runWaveletFrequencyFilterBankGeneratorTest<3, HeldWavelet>(inputImage, outputImage, inputBands);
    else if (waveletFunction == "Vow")
      return runWaveletFrequencyFilterBankGeneratorTest<3, VowWavelet>(inputImage, outputImage, inputBands);
    else if (waveletFunction == "Simoncelli")
      return runWaveletFrequencyFilterBankGeneratorTest<3, SimoncelliWavelet>(inputImage, outputImage, inputBands);
    else if (waveletFunction == "Shannon")
      return runWaveletFrequencyFilterBankGeneratorTest<3, ShannonWavelet>(inputImage, outputImage, inputBands);
    else
    {
      std::cerr << argv[4] << " is an unknown wavelet type " << std::endl;
      return EXIT_FAILURE;
    }
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}

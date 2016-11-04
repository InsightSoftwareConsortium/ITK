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
#include "itkWaveletFrequencyForward.h"
#include "itkWaveletFrequencyFilterBankGenerator.h"
#include "itkHeldIsotropicWavelet.h"
#include "itkVowIsotropicWavelet.h"
#include "itkSimoncelliIsotropicWavelet.h"
#include "itkShannonIsotropicWavelet.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include <itkComplexToRealImageFilter.h>
#include "itkNumberToString.h"
// Visualize for dev/debug purposes. Set in cmake file. Require VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif
using namespace std;
using namespace itk;

std::string
append_to_filename(const std::string & filename, const std::string & appendix)
{
  std::size_t found_dot = filename.find_last_of('.');
  return filename.substr(0, found_dot) + appendix + filename.substr(found_dot);
}

template <unsigned int N, typename TWaveletFunction>
int
runWaveletFrequencyForwardTest(const std::string &  inputImage,
                               const std::string &  outputImage,
                               const unsigned int & inputLevels,
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

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  typename FFTFilterType::Pointer               fftFilter = FFTFilterType::New();
  fftFilter->SetInput(reader->GetOutput());
  typedef typename FFTFilterType::OutputImageType ComplexImageType;

  // Set the WaveletFunctionType and the WaveletFilterBank
  typedef TWaveletFunction                                                                        WaveletFunctionType;
  typedef itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, WaveletFunctionType>         WaveletFilterBankType;
  typedef itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, WaveletFilterBankType> ForwardWaveletType;
  typename ForwardWaveletType::Pointer forwardWavelet = ForwardWaveletType::New();
  unsigned int                         high_sub_bands = inputBands;
  unsigned int                         levels = inputLevels;
  forwardWavelet->SetHighPassSubBands(high_sub_bands);
  forwardWavelet->SetLevels(levels);
  forwardWavelet->SetInput(fftFilter->GetOutput());
  forwardWavelet->Print(std::cout);
  forwardWavelet->Update();


  unsigned int ne = 0;
  // TEST INTERFACE:
  typedef typename ForwardWaveletType::OutputsType OutputsType;
  OutputsType                                      all_outputs = forwardWavelet->GetOutputs();
  if (all_outputs.size() != forwardWavelet->GetTotalOutputs())
  {
    std::cout << "Error all_outputs" << '\n';
    ++ne;
  }

  typename ForwardWaveletType::OutputImagePointer low_pass = forwardWavelet->GetOutputLowPass();
  OutputsType                                     all_high_sub_bands = forwardWavelet->GetOutputsHighPass();
  if (all_high_sub_bands.size() != forwardWavelet->GetTotalOutputs() - 1)
  {
    std::cout << "Error all_high_sub_bands" << '\n';
    ++ne;
  }
  OutputsType high_sub_bands_per_level = forwardWavelet->GetOutputsHighPassByLevel(0);
  if (high_sub_bands_per_level.size() != forwardWavelet->GetHighPassSubBands())
  {
    std::cout << "Error high_sub_bands_per_level" << '\n';
    ++ne;
  }

  for (unsigned int nout = 0; nout < forwardWavelet->GetNumberOfOutputs(); ++nout)
  {
    std::pair<unsigned int, unsigned int> pairLvBand = forwardWavelet->OutputIndexToLevelBand(nout);
    unsigned int                          lv = pairLvBand.first;
    unsigned int                          b = pairLvBand.second;
    std::cout << "InputIndex: " << nout << " --> lv:" << lv << " b:" << b << std::endl;
  }

  /* test OutputIndexToLevelBand */
  {
    std::pair<unsigned int, unsigned int> pairLvBand = forwardWavelet->OutputIndexToLevelBand(0);
    unsigned int                          lv = pairLvBand.first;
    unsigned int                          b = pairLvBand.second;
    std::cout << "inputindex: " << 0 << " lv:" << lv << " b:" << b << std::endl;
    if (lv != levels || b != 0)
      ++ne;
  }
  {
    std::pair<unsigned int, unsigned int> pairLvBand = forwardWavelet->OutputIndexToLevelBand(high_sub_bands);
    unsigned int                          lv = pairLvBand.first;
    unsigned int                          b = pairLvBand.second;
    std::cout << "inputindex: " << high_sub_bands << " lv:" << lv << " b:" << b << std::endl;
    if (lv != 1 || b != high_sub_bands)
      ++ne;
  }
  if (ne != 0)
  {
    std::cout << "ERROR in OutputIndexToLevelBand" << std::endl;
    return EXIT_FAILURE;
  }

  // Inverse FFT Transform (Multilevel)
  typedef itk::InverseFFTImageFilter<ComplexImageType, ImageType>              InverseFFTFilterType;
  typename InverseFFTFilterType::Pointer                                       inverseFFT = InverseFFTFilterType::New();
  typedef itk::ImageFileWriter<typename InverseFFTFilterType::OutputImageType> WriterType;
  typename WriterType::Pointer                                                 writer = WriterType::New();
  itk::NumberToString<unsigned int>                                            n2s;
  for (unsigned int level = 0; level < levels; ++level)
  {
    for (unsigned int band = 0; band < high_sub_bands; ++band)
    {
      if (level == 0 && band == 0) // Low pass
      {
        unsigned int n_output = 0;
        std::cout << "OutputIndex : " << n_output << std::endl;
        std::cout << "Level: " << level + 1 << " / " << forwardWavelet->GetLevels() << std::endl;
        std::cout << "Band: " << band << " / " << forwardWavelet->GetHighPassSubBands() << std::endl;
        std::cout << "Largest Region: " << forwardWavelet->GetOutput(n_output)->GetLargestPossibleRegion() << std::endl;
        std::cout << "Origin: " << forwardWavelet->GetOutput(n_output)->GetOrigin() << std::endl;
        std::cout << "Spacing: " << forwardWavelet->GetOutput(n_output)->GetSpacing() << std::endl;

        inverseFFT->SetInput(forwardWavelet->GetOutput(n_output));
        inverseFFT->Update();
#ifdef ITK_VISUALIZE_TESTS
        Testing::ViewImage(inverseFFT->GetOutput(),
                           "Approx coef. n_out: " + n2s(n_output) + " level: " + n2s(levels) + ", band:0/" +
                             n2s(inputBands));
#endif
        writer->SetFileName(append_to_filename(outputImage, n2s(n_output)));
        writer->SetInput(inverseFFT->GetOutput());
        try
        {
          writer->Update();
        }
        catch (itk::ExceptionObject & error)
        {
          std::cerr << "Error writing the WaveletForward image: " << std::endl;
          std::cerr << error << std::endl;
          return EXIT_FAILURE;
        }
      }
      unsigned int n_output = 1 + level * forwardWavelet->GetHighPassSubBands() + band;
      std::cout << "OutputIndex : " << n_output << std::endl;
      std::cout << "Level: " << level + 1 << " / " << forwardWavelet->GetLevels() << std::endl;
      std::cout << "Band: " << band + 1 << " / " << forwardWavelet->GetHighPassSubBands() << std::endl;
      std::cout << "Largest Region: " << forwardWavelet->GetOutput(n_output)->GetLargestPossibleRegion() << std::endl;
      std::cout << "Origin: " << forwardWavelet->GetOutput(n_output)->GetOrigin() << std::endl;
      std::cout << "Spacing: " << forwardWavelet->GetOutput(n_output)->GetSpacing() << std::endl;

      inverseFFT->SetInput(forwardWavelet->GetOutput(n_output));
      inverseFFT->Update();
#ifdef ITK_VISUALIZE_TESTS
      std::pair<unsigned int, unsigned int> pairLvBand = forwardWavelet->OutputIndexToLevelBand(n_output);
      Testing::ViewImage(inverseFFT->GetOutput(),
                         "Wavelet coef. n_out: " + n2s(n_output) + " level: " + n2s(pairLvBand.first) +
                           " , band: " + n2s(pairLvBand.second) + "/" + n2s(inputBands));

#endif
      writer->SetFileName(append_to_filename(outputImage, n2s(n_output)));
      writer->SetInput(inverseFFT->GetOutput());

      try
      {
        writer->Update();
      }
      catch (itk::ExceptionObject & error)
      {
        std::cerr << "Error writing the WaveletForward image: " << std::endl;
        std::cerr << error << std::endl;
        return EXIT_FAILURE;
      }
    }
  }

  return EXIT_SUCCESS;
}

int
itkWaveletFrequencyForwardTest(int argc, char * argv[])
{
  if (argc < 6 || argc > 7)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage inputLevels inputBands waveletFunction [dimension]"
              << std::endl;
    return EXIT_FAILURE;
  }
  const string       inputImage = argv[1];
  const string       outputImage = argv[2];
  const unsigned int inputLevels = atoi(argv[3]);
  const unsigned int inputBands = atoi(argv[4]);
  const string       waveletFunction = argv[5];
  unsigned int       dimension = 3;
  if (argc == 7)
  {
    dimension = atoi(argv[6]);
  }

  typedef itk::HeldIsotropicWavelet<>       HeldWavelet;
  typedef itk::VowIsotropicWavelet<>        VowWavelet;
  typedef itk::SimoncelliIsotropicWavelet<> SimoncelliWavelet;
  typedef itk::ShannonIsotropicWavelet<>    ShannonWavelet;
  if (dimension == 2)
  {
    if (waveletFunction == "Held")
      return runWaveletFrequencyForwardTest<2, HeldWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else if (waveletFunction == "Vow")
      return runWaveletFrequencyForwardTest<2, VowWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else if (waveletFunction == "Simoncelli")
      return runWaveletFrequencyForwardTest<2, SimoncelliWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else if (waveletFunction == "Shannon")
      return runWaveletFrequencyForwardTest<2, ShannonWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else
    {
      std::cerr << argv[5] << " is an unknown wavelet type " << std::endl;
      return EXIT_FAILURE;
    }
  }
  else if (dimension == 3)
  {
    if (waveletFunction == "Held")
      return runWaveletFrequencyForwardTest<3, HeldWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else if (waveletFunction == "Vow")
      return runWaveletFrequencyForwardTest<3, VowWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else if (waveletFunction == "Simoncelli")
      return runWaveletFrequencyForwardTest<3, SimoncelliWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else if (waveletFunction == "Shannon")
      return runWaveletFrequencyForwardTest<3, ShannonWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else
    {
      std::cerr << argv[5] << " is an unknown wavelet type " << std::endl;
      return EXIT_FAILURE;
    }
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}

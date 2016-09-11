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
#include "itkWaveletFrequencyForward.h"
#include "itkWaveletFrequencyFilterBankGenerator.h"
#include "itkHeldIsotropicWavelet.h"
#include "itkVowIsotropicWavelet.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include <itkComplexToRealImageFilter.h>
// Visualize for dev/debug purposes. Set in cmake file. Require VTK
#if ITK_VISUALIZE_TESTS != 0
#  include "itkView3DImage.h"
#endif
using namespace std;
using namespace itk;

int
itkWaveletFrequencyForwardTest(int argc, char ** argv)
{
  if (argc != 5)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage inputLevels inputBands" << std::endl;
    return EXIT_FAILURE;
  }
  const string inputImage = argv[1];
  const string outputImage = argv[2];
  unsigned int inputBands = atoi(argv[3]);
  unsigned int inputLevels = atoi(argv[4]);

  const unsigned int                       dimension = 3;
  typedef float                            PixelType;
  typedef itk::Image<PixelType, dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;
  ReaderType::Pointer                      reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();
  reader->UpdateLargestPossibleRegion();

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  FFTFilterType::Pointer                        fftFilter = FFTFilterType::New();
  fftFilter->SetInput(reader->GetOutput());
  typedef FFTFilterType::OutputImageType ComplexImageType;

  // Set the WaveletFunctionType and the WaveletFilterBank
  // typedef itk::HeldIsotropicWavelet<PixelType> WaveletFunctionType;
  typedef itk::VowIsotropicWavelet<PixelType>                                                     WaveletFunctionType;
  typedef itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, WaveletFunctionType>         WaveletFilterBankType;
  typedef itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, WaveletFilterBankType> ForwardWaveletType;
  ForwardWaveletType::Pointer forwardWavelet = ForwardWaveletType::New();
  unsigned int                high_sub_bands = inputBands;
  unsigned int                levels = inputLevels;
  forwardWavelet->SetHighPassSubBands(high_sub_bands);
  forwardWavelet->SetLevels(levels);
  forwardWavelet->SetInput(fftFilter->GetOutput());
  forwardWavelet->Print(std::cout);
  forwardWavelet->Update();


  unsigned int ne = 0;
  // TEST INTERFACE:
  typedef ForwardWaveletType::OutputsType OutputsType;
  OutputsType                             all_outputs = forwardWavelet->GetOutputs();
  if (all_outputs.size() != forwardWavelet->GetTotalOutputs())
  {
    std::cout << "Error all_outputs" << '\n';
    ++ne;
  }

  ForwardWaveletType::OutputImagePointer low_pass = forwardWavelet->GetOutputLowPass();
  OutputsType                            all_high_sub_bands = forwardWavelet->GetOutputsHighPass();
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
  typedef itk::InverseFFTImageFilter<ComplexImageType, ImageType> InverseFFTFilterType;
  InverseFFTFilterType::Pointer                                   inverseFFT = InverseFFTFilterType::New();
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
#if ITK_VISUALIZE_TESTS != 0
        View3DImage(inverseFFT->GetOutput());
#endif
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
#if ITK_VISUALIZE_TESTS != 0
      View3DImage(inverseFFT->GetOutput());
#endif
    }
  }

  // Get real part of complex image for visualization
  typedef itk::ComplexToRealImageFilter<ComplexImageType, ImageType> ComplexToRealFilter;
  ComplexToRealFilter::Pointer                                       complexToRealFilter = ComplexToRealFilter::New();
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

        complexToRealFilter->SetInput(forwardWavelet->GetOutput(n_output));
        complexToRealFilter->Update();
        // #if ITK_VISUALIZE_TESTS != 0
        //                 View3DImage(complexToRealFilter->GetOutput());
        // #endif
      }
      unsigned int n_output = 1 + level * forwardWavelet->GetHighPassSubBands() + band;
      std::cout << "OutputIndex : " << n_output << std::endl;
      std::cout << "Level: " << level + 1 << " / " << forwardWavelet->GetLevels() << std::endl;
      std::cout << "Band: " << band + 1 << " / " << forwardWavelet->GetHighPassSubBands() << std::endl;
      std::cout << "Largest Region: " << forwardWavelet->GetOutput(n_output)->GetLargestPossibleRegion() << std::endl;
      std::cout << "Origin: " << forwardWavelet->GetOutput(n_output)->GetOrigin() << std::endl;
      std::cout << "Spacing: " << forwardWavelet->GetOutput(n_output)->GetSpacing() << std::endl;

      complexToRealFilter->SetInput(forwardWavelet->GetOutput(n_output));
      complexToRealFilter->Update();
      // #if ITK_VISUALIZE_TESTS != 0
      //             View3DImage(complexToRealFilter->GetOutput());
      // #endif
    }
  }

  return EXIT_SUCCESS;
}

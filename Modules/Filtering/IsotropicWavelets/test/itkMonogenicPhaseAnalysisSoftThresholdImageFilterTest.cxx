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
#include <string>
#include <cmath>
#include "itkMonogenicPhaseAnalysisSoftThresholdImageFilter.h"
#include "itkMonogenicSignalFrequencyImageFilter.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"

#include "itkVectorInverseFFTImageFilter.h"
// Visualize for dev/debug purposes. Set in cmake file. Require VTK
#if ITK_VISUALIZE_TESTS != 0
#  include "itkViewImage.h"
#endif
using namespace std;
using namespace itk;

int
itkMonogenicPhaseAnalysisSoftThresholdImageFilterTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage" << std::endl;
    return EXIT_FAILURE;
  }
  const string inputImage = argv[1];
  const string outputImage = argv[2];

  const unsigned int                       dimension = 3;
  typedef float                            PixelType;
  typedef itk::Image<PixelType, dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;
  ReaderType::Pointer                      reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();
  reader->UpdateLargestPossibleRegion();

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTForwardFilterType;
  FFTForwardFilterType::Pointer                 fftForwardFilter = FFTForwardFilterType::New();
  fftForwardFilter->SetInput(reader->GetOutput());
  fftForwardFilter->Update();
  typedef FFTForwardFilterType::OutputImageType ComplexImageType;

  typedef itk::MonogenicSignalFrequencyImageFilter<ComplexImageType> MonogenicSignalFrequencyFilterType;
  MonogenicSignalFrequencyFilterType::Pointer monoFilter = MonogenicSignalFrequencyFilterType::New();
  monoFilter->SetInput(fftForwardFilter->GetOutput());
  monoFilter->Update();

  typedef MonogenicSignalFrequencyFilterType::OutputImageType VectorMonoOutputType;

  typedef itk::VectorInverseFFTImageFilter<VectorMonoOutputType> VectorInverseFFTType;
  VectorInverseFFTType::Pointer                                  vecInverseFFT = VectorInverseFFTType::New();
  vecInverseFFT->SetInput(monoFilter->GetOutput());
  vecInverseFFT->Update();
  // Input to the PhaseAnalysisSoftThreshold
  typedef MonogenicPhaseAnalysisSoftThresholdImageFilter<VectorInverseFFTType::OutputImageType>
                                            PhaseAnalysisSoftThresholdFilter;
  PhaseAnalysisSoftThresholdFilter::Pointer phaseAnalyzer = PhaseAnalysisSoftThresholdFilter::New();
  phaseAnalyzer->SetInput(vecInverseFFT->GetOutput());
  phaseAnalyzer->Update();

#if ITK_VISUALIZE_TESTS != 0
  Testing::ViewImage(phaseAnalyzer->GetOutput(), "PhaseAnalyzer(Soft) output:");
#endif


  return EXIT_SUCCESS;
}

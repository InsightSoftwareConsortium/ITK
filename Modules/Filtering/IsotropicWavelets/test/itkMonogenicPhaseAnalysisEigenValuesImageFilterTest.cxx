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
#include "itkMonogenicPhaseAnalysisEigenValuesImageFilter.h"
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

template <unsigned int N>
int
runMonogenicPhaseAnalysisEigenValuesImageFilterTest(const std::string & inputImage, const std::string & outputImage)
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
  typedef itk::ForwardFFTImageFilter<ImageType> FFTForwardFilterType;
  typename FFTForwardFilterType::Pointer        fftForwardFilter = FFTForwardFilterType::New();
  fftForwardFilter->SetInput(reader->GetOutput());
  fftForwardFilter->Update();
  typedef typename FFTForwardFilterType::OutputImageType ComplexImageType;

  typedef itk::MonogenicSignalFrequencyImageFilter<ComplexImageType> MonogenicSignalFrequencyFilterType;
  typename MonogenicSignalFrequencyFilterType::Pointer monoFilter = MonogenicSignalFrequencyFilterType::New();
  monoFilter->SetInput(fftForwardFilter->GetOutput());
  monoFilter->Update();

  typedef typename MonogenicSignalFrequencyFilterType::OutputImageType VectorMonoOutputType;

  typedef itk::VectorInverseFFTImageFilter<VectorMonoOutputType> VectorInverseFFTType;
  typename VectorInverseFFTType::Pointer                         vecInverseFFT = VectorInverseFFTType::New();
  vecInverseFFT->SetInput(monoFilter->GetOutput());
  vecInverseFFT->Update();

  // Input to the PhaseAnalysisEigenValues
  typedef MonogenicPhaseAnalysisEigenValuesImageFilter<typename VectorInverseFFTType::OutputImageType>
                                        PhaseAnalysisFilter;
  typename PhaseAnalysisFilter::Pointer phaseAnalyzer = PhaseAnalysisFilter::New();
  phaseAnalyzer->SetInput(vecInverseFFT->GetOutput());
  phaseAnalyzer->SetApplySoftThreshold(true);
  phaseAnalyzer->Update();

#if ITK_VISUALIZE_TESTS != 0
  // Testing::ViewImage(reader->GetOutput(), "Input Image");
  Testing::ViewImage(phaseAnalyzer->GetOutput(1), "PhaseAnalyzer(Eigen) Amplitude:");
  Testing::ViewImage(phaseAnalyzer->GetOutput(2), "PhaseAnalyzer(Eigen) Phase:");
  Testing::ViewImage(phaseAnalyzer->GetOutput(0), "PhaseAnalyzer(Eigen) output:");
#endif

  typedef itk::ImageFileWriter<typename PhaseAnalysisFilter::OutputImageType> WriterType;
  typename WriterType::Pointer                                                writer = WriterType::New();
  writer->SetFileName(outputImage);
  writer->SetInput(phaseAnalyzer->GetOutput(0));

  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error writing the image: " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkMonogenicPhaseAnalysisEigenValuesImageFilterTest(int argc, char * argv[])
{
  if (argc < 3 || argc > 4)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage [dimension]" << std::endl;
    return EXIT_FAILURE;
  }
  const string inputImage = argv[1];
  const string outputImage = argv[2];
  unsigned int dimension = 3;
  if (argc == 4)
  {
    dimension = atoi(argv[3]);
  }

  if (dimension == 2)
  {
    return runMonogenicPhaseAnalysisEigenValuesImageFilterTest<2>(inputImage, outputImage);
  }
  else if (dimension == 3)
  {
    return runMonogenicPhaseAnalysisEigenValuesImageFilterTest<3>(inputImage, outputImage);
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}

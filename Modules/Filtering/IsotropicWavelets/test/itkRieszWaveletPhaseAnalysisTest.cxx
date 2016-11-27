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
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkWaveletFrequencyForward.h"
#include "itkWaveletFrequencyInverse.h"
#include "itkWaveletFrequencyFilterBankGenerator.h"
#include "itkHeldIsotropicWavelet.h"
#include "itkVowIsotropicWavelet.h"
#include "itkSimoncelliIsotropicWavelet.h"
#include "itkShannonIsotropicWavelet.h"

#include "itkMonogenicSignalFrequencyImageFilter.h"
#include "itkVectorInverseFFTImageFilter.h"
#include "itkMonogenicPhaseAnalysisEigenValuesImageFilter.h"
#include "itkMonogenicPhaseAnalysisSoftThresholdImageFilter.h"
#include "itkZeroDCImageFilter.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNumberToString.h"

#include <itkGaussianSpatialFunction.h>
#include <itkFrequencyImageRegionIteratorWithIndex.h>
// Visualize for dev/debug purposes. Set in cmake file. Require VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif
using namespace std;
using namespace itk;

template <unsigned int N, typename TWaveletFunction>
int
runRieszWaveletPhaseAnalysisTest(const std::string &  inputImage,
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
  // Wavelet analysis (forward)
  // Monogenic Analysis on each output. (In-place style)
  // Wavelet reconstruction (inverse) from PhaseAnalysis output
  // Profit.

  typedef itk::ZeroDCImageFilter<ImageType> ZeroDCFilterType;
  typename ZeroDCFilterType::Pointer        zeroDCFilter = ZeroDCFilterType::New();
  zeroDCFilter->SetInput(reader->GetOutput());
  zeroDCFilter->Update();

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<typename ZeroDCFilterType::OutputImageType> FFTForwardFilterType;
  typename FFTForwardFilterType::Pointer fftForwardFilter = FFTForwardFilterType::New();
  fftForwardFilter->SetInput(zeroDCFilter->GetOutput());
  fftForwardFilter->Update();
  typedef typename FFTForwardFilterType::OutputImageType ComplexImageType;

  // Forward Wavelet
  typedef TWaveletFunction                                                                        WaveletFunctionType;
  typedef itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, WaveletFunctionType>         WaveletFilterBankType;
  typedef itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, WaveletFilterBankType> ForwardWaveletType;
  typename ForwardWaveletType::Pointer forwardWavelet = ForwardWaveletType::New();
  unsigned int                         high_sub_bands = inputBands;
  unsigned int                         levels = inputLevels;
  forwardWavelet->SetHighPassSubBands(high_sub_bands);
  forwardWavelet->SetLevels(levels);
  forwardWavelet->SetInput(fftForwardFilter->GetOutput());
  forwardWavelet->Update();

  typedef itk::MonogenicSignalFrequencyImageFilter<ComplexImageType> MonogenicSignalFrequencyFilterType;

  typedef typename MonogenicSignalFrequencyFilterType::OutputImageType VectorMonoOutputType;
  typedef itk::VectorInverseFFTImageFilter<VectorMonoOutputType>       VectorInverseFFTType;

  // Input to the PhaseAnalysisEigenValues
  typedef MonogenicPhaseAnalysisSoftThresholdImageFilter<typename VectorInverseFFTType::OutputImageType>
    PhaseAnalysisFilter;
  // typedef MonogenicPhaseAnalysisEigenValuesImageFilter<typename VectorInverseFFTType::OutputImageType>
  // PhaseAnalysisFilter;

  typename ForwardWaveletType::OutputsType analysisWavelets = forwardWavelet->GetOutputs();
  typename ForwardWaveletType::OutputsType modifiedWavelets;
  unsigned int                             noutputs = forwardWavelet->GetNumberOfOutputs();
  for (unsigned int nout = 0; nout < forwardWavelet->GetNumberOfOutputs(); ++nout)
  {
    std::cout << "************Nout: " << nout << " / " << noutputs << std::endl;
    if (nout == 0) // TODO check this. avoid phase analysis in last approximation (low_pass).
    {
      modifiedWavelets.push_back(analysisWavelets[nout]);
      // TODO remove this visualize
#ifdef ITK_VISUALIZE_TESTS
      typedef itk::InverseFFTImageFilter<ComplexImageType> FFTInverseFilterType;
      typename FFTInverseFilterType::Pointer               fftInv = FFTInverseFilterType::New();
      fftInv->SetInput(analysisWavelets[nout]);
      fftInv->Update();
      Testing::ViewImage(fftInv->GetOutput(), "Wavelet coef 0 (LowPass) Original");
#endif
      continue;
    }
    typename MonogenicSignalFrequencyFilterType::Pointer monoFilter = MonogenicSignalFrequencyFilterType::New();
    typename VectorInverseFFTType::Pointer               vecInverseFFT = VectorInverseFFTType::New();
    typename PhaseAnalysisFilter::Pointer                phaseAnalyzer = PhaseAnalysisFilter::New();
    typename FFTForwardFilterType::Pointer               fftForwardPhaseFilter = FFTForwardFilterType::New();
    monoFilter->SetInput(analysisWavelets[nout]);
    monoFilter->Update();
    vecInverseFFT->SetInput(monoFilter->GetOutput());
    vecInverseFFT->Update();
    phaseAnalyzer->SetInput(vecInverseFFT->GetOutput());
    phaseAnalyzer->SetApplySoftThreshold(true);
    phaseAnalyzer->Update();
    fftForwardPhaseFilter->SetInput(phaseAnalyzer->GetOutput(0));
    fftForwardPhaseFilter->Update();
    modifiedWavelets.push_back(fftForwardPhaseFilter->GetOutput());
    modifiedWavelets.back()->DisconnectPipeline();
    // TODO remove this visualize
#ifdef ITK_VISUALIZE_TESTS
    typedef itk::InverseFFTImageFilter<ComplexImageType> FFTInverseFilterType;
    typename FFTInverseFilterType::Pointer               fftInv = FFTInverseFilterType::New();
    fftInv->SetInput(analysisWavelets[nout]);
    fftInv->Update();
    // Testing::ViewImage(fftInv->GetOutput(),  "Wavelet coef " + n2s(nout) + "Original" );
    itk::NumberToString<unsigned int> n2s;
    Testing::ViewImage(phaseAnalyzer->GetOutput(0), "Wavelet coef " + n2s(nout) + " PhaseAnalyzed");
#endif
  }

  typedef itk::WaveletFrequencyInverse<ComplexImageType, ComplexImageType, WaveletFilterBankType> InverseWaveletType;
  typename InverseWaveletType::Pointer inverseWavelet = InverseWaveletType::New();
  inverseWavelet->SetHighPassSubBands(high_sub_bands);
  inverseWavelet->SetLevels(levels);
  inverseWavelet->SetInputs(modifiedWavelets);
  inverseWavelet->Update();

  bool apply_gaussian = false;
  // Apply a gaussian window of the size of the input.
  if (apply_gaussian == true)
  {
    typedef itk::GaussianSpatialFunction<double, dimension> FunctionType;
    typename FunctionType::Pointer                          gaussian = FunctionType::New();
    typedef FixedArray<double, dimension>                   ArrayType;
    ArrayType                                               m_Mean;
    ArrayType                                               m_Sigma;
    double                                                  m_Scale = 1.0;
    bool                                                    m_Normalized = true;
    for (unsigned int i = 0; i < dimension; ++i)
    {
      m_Mean[i] = inverseWavelet->GetOutput()->GetLargestPossibleRegion().GetIndex()[i];
      // 6.0 is equivalent to 3sigmas (border values are close to zero)
      // m_Sigma[i] = outputSize[i]/(2*3.0);
      m_Sigma[i] = inverseWavelet->GetOutput()->GetLargestPossibleRegion().GetSize()[i] / (2.0 * 2.35);
    }
    gaussian->SetSigma(m_Sigma);
    gaussian->SetMean(m_Mean);
    gaussian->SetScale(m_Scale);
    gaussian->SetNormalized(m_Normalized);

    // Create an iterator that will walk the output region
    typedef itk::FrequencyImageRegionIteratorWithIndex<ComplexImageType> OutputIterator;
    OutputIterator                                                       outIt =
      OutputIterator(inverseWavelet->GetOutput(), inverseWavelet->GetOutput()->GetRequestedRegion());
    outIt.GoToBegin();
    while (!outIt.IsAtEnd())
    {
      typename ComplexImageType::IndexType ind = outIt.GetFrequencyBin();
      typename FunctionType::InputType     f;
      for (unsigned int i = 0; i < dimension; ++i)
      {
        f[i] = static_cast<typename FunctionType::InputType::ValueType>(ind[i]);
      }
      const double value = gaussian->Evaluate(f);
      // Set the pixel value to the function value
      outIt.Set(outIt.Get() * static_cast<typename ComplexImageType::PixelType::value_type>(value));
      ++outIt;
    }
  }
  typedef itk::InverseFFTImageFilter<ComplexImageType, ImageType> InverseFFTFilterType;
  typename InverseFFTFilterType::Pointer                          inverseFFT = InverseFFTFilterType::New();
  inverseFFT->SetInput(inverseWavelet->GetOutput());
  inverseFFT->Update();
#ifdef ITK_VISUALIZE_TESTS
  Testing::ViewImage(reader->GetOutput(), "Input Image");
  Testing::ViewImage(inverseFFT->GetOutput(), "Inverse Wavelet");
#endif

  typedef itk::ImageFileWriter<typename InverseFFTFilterType::OutputImageType> WriterType;
  typename WriterType::Pointer                                                 writer = WriterType::New();
  writer->SetFileName(outputImage);
  writer->SetInput(inverseFFT->GetOutput());

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
itkRieszWaveletPhaseAnalysisTest(int argc, char * argv[])
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
      return runRieszWaveletPhaseAnalysisTest<2, HeldWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else if (waveletFunction == "Vow")
      return runRieszWaveletPhaseAnalysisTest<2, VowWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else if (waveletFunction == "Simoncelli")
      return runRieszWaveletPhaseAnalysisTest<2, SimoncelliWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else if (waveletFunction == "Shannon")
      return runRieszWaveletPhaseAnalysisTest<2, ShannonWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else
    {
      std::cerr << argv[5] << " is an unknown wavelet type " << std::endl;
      return EXIT_FAILURE;
    }
  }
  else if (dimension == 3)
  {
    if (waveletFunction == "Held")
      return runRieszWaveletPhaseAnalysisTest<3, HeldWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else if (waveletFunction == "Vow")
      return runRieszWaveletPhaseAnalysisTest<3, VowWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else if (waveletFunction == "Simoncelli")
      return runRieszWaveletPhaseAnalysisTest<3, SimoncelliWavelet>(inputImage, outputImage, inputLevels, inputBands);
    else if (waveletFunction == "Shannon")
      return runRieszWaveletPhaseAnalysisTest<3, ShannonWavelet>(inputImage, outputImage, inputLevels, inputBands);
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

/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkPhaseSymmetryImageFilter_hxx
#define itkPhaseSymmetryImageFilter_hxx

#include <string>
#include <sstream>

namespace itk
{

template <typename TInputImage, typename TOutputImage>
PhaseSymmetryImageFilter<TInputImage, TOutputImage>::PhaseSymmetryImageFilter()
{
  m_MultiplyImageFilter = MultiplyImageFilterType::New();
  m_DivideImageFilter = DivideImageFilterType::New();
  m_AddImageFilter = AddImageFilterType::New();
  m_AddImageFilter2 = AddImageFilterType::New();
  m_MaxImageFilter = MaxImageFilterType::New();
  m_AtanImageFilter = Atan2ImageFilterType::New();
  m_ShiftScaleFilter = ShiftScaleImageFilterType::New();
  m_NegateFilter = ShiftScaleImageFilterType::New();
  m_NegateFilter2 = ShiftScaleImageFilterType::New();
  m_AcosImageFilter = AcosImageFilterType::New();
  m_C2RFilter = ComplexToRealFilterType::New();
  m_C2IFilter = ComplexToImaginaryFilterType::New();
  m_C2MFilter = ComplexToModulusFilterType::New();
  m_C2AFilter = ComplexToPhaseFilterType::New();
  m_AbsImageFilter = AbsImageFilterType::New();
  m_AbsImageFilter2 = AbsImageFilterType::New();
  m_MP2CFilter = MagnitudeAndPhaseToComplexFilterType::New();

  m_FFTFilter = FFTFilterType::New();
  m_IFFTFilter = IFFTFilterType::New();

  m_NegateFilter->SetScale(-1.0);
  m_NegateFilter->SetShift(0.0);

  m_NegateFilter2->SetScale(-1.0);
  m_NegateFilter2->SetShift(0.0);

  // Create 2 initialze wavelengths
  m_Wavelengths.SetSize(2, InputImageDimension);
  for (unsigned int i = 0; i < InputImageDimension; i++)
  {
    m_Wavelengths(0, i) = 10.0;
    m_Wavelengths(1, i) = 20.0;
  }

  // Set basic orientations
  m_Orientations.SetSize(InputImageDimension, InputImageDimension);
  for (unsigned int i = 0; i < InputImageDimension; i++)
  {
    for (unsigned int j = 0; j < InputImageDimension; j++)
    {
      if (i == j)
      {
        m_Orientations(i, j) = 1.0;
      }
      else
      {
        m_Orientations(i, j) = 0.0;
      }
    }
  }

  // Defaults
  m_AngleBandwidth = 3.14159265;
  m_Sigma = 0.55;
  m_NoiseThreshold = 10.0;
  m_Polarity = 0;

  // Avoid using too much memory by default.
  this->ReleaseDataFlagOn();
}


template <typename TInputImage, typename TOutputImage>
void
PhaseSymmetryImageFilter<TInputImage, TOutputImage>::Initialize()
{
  typename TInputImage::SizeType          inputSize;
  typename TInputImage::IndexType         inputIndex;
  typename Superclass::OutputImagePointer output = this->GetOutput();
  typename Superclass::InputImagePointer  input = const_cast<TInputImage *>(this->GetInput());

  inputIndex = input->GetLargestPossibleRegion().GetIndex();
  inputSize = input->GetLargestPossibleRegion().GetSize();
  constexpr unsigned int ndims = TInputImage::ImageDimension;

  m_ShiftScaleFilter->SetInput(input);
  m_ShiftScaleFilter->SetScale(0.0);
  m_ShiftScaleFilter->SetShift(0.0);

  typename LogGaborFreqImageSourceType::Pointer         LogGaborKernel = LogGaborFreqImageSourceType::New();
  typename SteerableFiltersFreqImageSourceType::Pointer SteerableFilterKernel =
    SteerableFiltersFreqImageSourceType::New();
  typename ButterworthKernelFreqImageSourceType::Pointer ButterworthFilterKernel =
    ButterworthKernelFreqImageSourceType::New();

  // Initialize log gabor kernels
  LogGaborKernel->SetOrigin(this->GetInput()->GetOrigin());
  LogGaborKernel->SetSpacing(this->GetInput()->GetSpacing());
  LogGaborKernel->SetDirection(this->GetInput()->GetDirection());
  LogGaborKernel->SetSize(inputSize);

  // Initialize directionality kernels
  SteerableFilterKernel->SetOrigin(this->GetInput()->GetOrigin());
  SteerableFilterKernel->SetSpacing(this->GetInput()->GetSpacing());
  SteerableFilterKernel->SetDirection(this->GetInput()->GetDirection());
  SteerableFilterKernel->SetSize(inputSize);

  // Initialize low pass filter kernel
  ButterworthFilterKernel->SetOrigin(this->GetInput()->GetOrigin());
  ButterworthFilterKernel->SetSpacing(this->GetInput()->GetSpacing());
  ButterworthFilterKernel->SetDirection(this->GetInput()->GetDirection());
  ButterworthFilterKernel->SetSize(inputSize);
  ButterworthFilterKernel->SetCutoff(0.4);
  ButterworthFilterKernel->SetOrder(10.0);

  ArrayType wv;
  ArrayType orientation;

  FloatImageStack tempStack;
  FloatImageStack lgStack;
  FloatImageStack sfStack;

  // Create log gabor kernels
  for (unsigned int w = 0; w < m_Wavelengths.rows(); w++)
  {
    for (unsigned int i = 0; i < ndims; ++i)
    {
      wv[i] = m_Wavelengths.get(w, i);
    }
    LogGaborKernel->SetWavelengths(wv);
    LogGaborKernel->SetSigma(m_Sigma);
    m_MultiplyImageFilter->SetInput1(LogGaborKernel->GetOutput());
    m_MultiplyImageFilter->SetInput2(ButterworthFilterKernel->GetOutput());
    m_MultiplyImageFilter->Update();
    lgStack.push_back(m_MultiplyImageFilter->GetOutput());
    lgStack[w]->DisconnectPipeline();
  }

  // Create directionality kernels
  for (unsigned int o = 0; o < m_Orientations.rows(); o++)
  {
    for (unsigned int d = 0; d < ndims; ++d)
    {
      orientation[d] = m_Orientations.get(o, d);
    }
    SteerableFilterKernel->SetOrientation(orientation);
    SteerableFilterKernel->SetAngularBandwidth(m_AngleBandwidth);
    SteerableFilterKernel->Update();
    sfStack.push_back(SteerableFilterKernel->GetOutput());
    sfStack[o]->DisconnectPipeline();
  }

  // Create filter bank by multiplying log gabor filters with directional filters
  typename DoubleFFTShiftImageFilterType::Pointer fftShiftFilter = DoubleFFTShiftImageFilterType::New();

  for (unsigned int w = 0; w < m_Wavelengths.rows(); w++)
  {
    tempStack.clear();
    for (unsigned int o = 0; o < m_Orientations.rows(); o++)
    {
      m_MultiplyImageFilter->SetInput1(lgStack[w]);
      m_MultiplyImageFilter->SetInput2(sfStack[o]);
      fftShiftFilter->SetInput(m_MultiplyImageFilter->GetOutput());
      fftShiftFilter->Update();
      tempStack.push_back(fftShiftFilter->GetOutput());
      tempStack[o]->DisconnectPipeline();
      typename FloatImageType::RegionType region = tempStack[o]->GetBufferedRegion();
      region.SetIndex(inputIndex);
      tempStack[o]->SetRegions(region);
    }
    m_FilterBank.push_back(tempStack);
  }

  const bool releaseData = this->GetReleaseDataFlag();
  m_ShiftScaleFilter->SetReleaseDataFlag(releaseData);
  m_C2MFilter->SetReleaseDataFlag(releaseData);
  m_C2AFilter->SetReleaseDataFlag(releaseData);
  m_MultiplyImageFilter->SetReleaseDataFlag(releaseData);
  m_DivideImageFilter->SetReleaseDataFlag(releaseData);
  m_MP2CFilter->SetReleaseDataFlag(releaseData);
  m_FFTFilter->SetReleaseDataFlag(releaseData);
  m_IFFTFilter->SetReleaseDataFlag(releaseData);
  m_C2RFilter->SetReleaseDataFlag(releaseData);
  m_C2IFilter->SetReleaseDataFlag(releaseData);
  m_MaxImageFilter->SetReleaseDataFlag(releaseData);
  m_NegateFilter->SetReleaseDataFlag(releaseData);
  m_NegateFilter2->SetReleaseDataFlag(releaseData);
  m_AbsImageFilter->SetReleaseDataFlag(releaseData);
  m_AbsImageFilter2->SetReleaseDataFlag(releaseData);
  m_MaxImageFilter->SetReleaseDataFlag(releaseData);
  m_AcosImageFilter->SetReleaseDataFlag(releaseData);
}


template <typename TInputImage, typename TOutputImage>
void
PhaseSymmetryImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  typename TInputImage::SizeType          inputSize;
  typename TInputImage::IndexType         inputIndex;
  typename Superclass::OutputImagePointer output = this->GetOutput();
  typename Superclass::InputImagePointer  input = const_cast<TInputImage *>(this->GetInput());

  inputIndex = input->GetLargestPossibleRegion().GetIndex();
  inputSize = input->GetLargestPossibleRegion().GetSize();
  constexpr unsigned int ndims = TInputImage::ImageDimension;

  typename ComplexImageType::Pointer finput = ComplexImageType::New();
  typename ComplexImageType::Pointer bpinput = ComplexImageType::New();


  m_FFTFilter->SetInput(input);
  m_FFTFilter->Update();
  finput = m_FFTFilter->GetOutput();
  finput->DisconnectPipeline();


  // Get the pixel count.  We need to divide the IFFT output by this because using the inverse FFT for
  // complex to complex doesn't seem to work.   So instead, we use the forward transform and divide by pixelNum
  double pxlCount = 1.0;
  for (unsigned int i = 0; i < ndims; ++i)
  {
    pxlCount = pxlCount * double(inputSize[i]);
  }


  typename FloatImageType::Pointer EnergyThisOrient = FloatImageType::New();
  typename FloatImageType::Pointer totalAmplitude = FloatImageType::New();
  typename FloatImageType::Pointer totalEnergy = FloatImageType::New();

  // Matlab style initalization, because these images accumulate over each loop
  // Therefore, they initially all zeros
  m_ShiftScaleFilter->SetScale(0.0);
  m_ShiftScaleFilter->SetShift(0.0);

  m_ShiftScaleFilter->Update();
  totalAmplitude = m_ShiftScaleFilter->GetOutput();
  totalAmplitude->DisconnectPipeline();

  m_ShiftScaleFilter->Update();
  totalEnergy = m_ShiftScaleFilter->GetOutput();
  totalEnergy->DisconnectPipeline();

  for (unsigned int o = 0; o < m_Orientations.rows(); ++o)
  {
    // Reset the energy value
    m_ShiftScaleFilter->SetScale(0.0);
    m_ShiftScaleFilter->SetShift(0.0);
    m_ShiftScaleFilter->Update();
    EnergyThisOrient = m_ShiftScaleFilter->GetOutput();
    EnergyThisOrient->DisconnectPipeline();

    for (unsigned int w = 0; w < m_Wavelengths.rows(); ++w)
    {
      // Multiply filters by the input image in fourier domain
      m_C2MFilter->SetInput(finput);
      m_C2AFilter->SetInput(finput);
      m_ShiftScaleFilter->SetScale(1 / pxlCount);
      m_ShiftScaleFilter->SetShift(0.0);
      // Normalize the magnitude by the number of pixels
      m_ShiftScaleFilter->SetInput(m_C2MFilter->GetOutput());

      m_MultiplyImageFilter->SetInput1(m_ShiftScaleFilter->GetOutput());
      m_MultiplyImageFilter->SetInput2(m_FilterBank[w][o]);

      m_MultiplyImageFilter->Update();

      m_MP2CFilter->SetInput1(m_MultiplyImageFilter->GetOutput());
      m_MP2CFilter->SetInput2(m_C2AFilter->GetOutput());

      m_IFFTFilter->SetInput(m_MP2CFilter->GetOutput());
      m_IFFTFilter->Update();
      bpinput = m_IFFTFilter->GetOutput();
      bpinput->DisconnectPipeline();

      // Get mag, real and imag of the band passed images
      m_C2MFilter->SetInput(bpinput);
      m_C2RFilter->SetInput(bpinput);
      m_C2IFilter->SetInput(bpinput);


      m_AddImageFilter->SetInput1(m_C2MFilter->GetOutput());
      m_AddImageFilter->SetInput2(totalAmplitude);
      m_AddImageFilter->Update();
      totalAmplitude = m_AddImageFilter->GetOutput();
      totalAmplitude->DisconnectPipeline();

      // Use appropraite equation depending on polarity
      if (m_Polarity == 0)
      {
        m_AbsImageFilter->SetInput(m_C2RFilter->GetOutput());
        m_AbsImageFilter2->SetInput(m_C2IFilter->GetOutput());

        m_NegateFilter->SetInput(m_AbsImageFilter2->GetOutput());

        m_AddImageFilter->SetInput1(m_AbsImageFilter->GetOutput());
        m_AddImageFilter->SetInput2(m_NegateFilter->GetOutput());

        m_AddImageFilter2->SetInput1(m_AddImageFilter->GetOutput());
        m_AddImageFilter2->SetInput2(EnergyThisOrient);

        m_AddImageFilter2->Update();
        EnergyThisOrient = m_AddImageFilter2->GetOutput();
        EnergyThisOrient->DisconnectPipeline();
      }
      else if (m_Polarity == 1)
      {
        m_AbsImageFilter->SetInput(m_C2IFilter->GetOutput());
        m_NegateFilter->SetInput(m_AbsImageFilter->GetOutput());

        m_AddImageFilter->SetInput1(m_C2RFilter->GetOutput());
        m_AddImageFilter->SetInput2(m_NegateFilter->GetOutput());

        m_AddImageFilter2->SetInput1(m_AddImageFilter->GetOutput());
        m_AddImageFilter2->SetInput2(EnergyThisOrient);

        m_AddImageFilter2->Update();
        EnergyThisOrient = m_AddImageFilter2->GetOutput();
        EnergyThisOrient->DisconnectPipeline();
      }
      else if (m_Polarity == -1)
      {
        m_AbsImageFilter->SetInput(m_C2IFilter->GetOutput());
        m_NegateFilter->SetInput(m_C2RFilter->GetOutput());
        m_NegateFilter2->SetInput(m_AbsImageFilter->GetOutput());

        m_AddImageFilter->SetInput1(m_NegateFilter->GetOutput());
        m_AddImageFilter->SetInput2(m_NegateFilter2->GetOutput());

        m_AddImageFilter2->SetInput1(m_AddImageFilter->GetOutput());
        m_AddImageFilter2->SetInput2(EnergyThisOrient);

        m_AddImageFilter2->Update();
        EnergyThisOrient = m_AddImageFilter2->GetOutput();
        EnergyThisOrient->DisconnectPipeline();
      }
    }

    // Subtract the values below the noise threshold
    m_ShiftScaleFilter->SetInput(EnergyThisOrient);
    m_ShiftScaleFilter->SetScale(1.0);
    m_ShiftScaleFilter->SetShift(-m_NoiseThreshold);

    m_AddImageFilter->SetInput1(m_ShiftScaleFilter->GetOutput());
    m_AddImageFilter->SetInput2(totalEnergy);
    m_AddImageFilter->Update();

    totalEnergy = m_AddImageFilter->GetOutput();
    totalEnergy->DisconnectPipeline();
  }

  // Set negative values to zero
  m_ShiftScaleFilter->SetScale(0.0);
  m_ShiftScaleFilter->SetShift(0.0);
  m_MaxImageFilter->SetInput1(totalEnergy);
  m_MaxImageFilter->SetInput2(m_ShiftScaleFilter->GetOutput());

  // Divide total energy by total amplitude over all scales and orientations
  m_DivideImageFilter->SetInput1(m_MaxImageFilter->GetOutput());
  m_DivideImageFilter->SetInput2(totalAmplitude);

  m_DivideImageFilter->GraftOutput(this->GetOutput());
  m_DivideImageFilter->Update();
  m_PhaseSymmetry = m_DivideImageFilter->GetOutput();
  this->GraftOutput(m_PhaseSymmetry);
}


template <typename TInputImage, typename TOutputImage>
void
PhaseSymmetryImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  itkDebugMacro("GenerateInputRequestedRegion Start");
  Superclass::GenerateInputRequestedRegion();

  if (this->GetInput())
  {
    typename TInputImage::RegionType RequestedRegion;
    typename TInputImage::SizeType   inputSize;
    typename TInputImage::IndexType  inputIndex;
    typename TInputImage::SizeType   inputLargSize;
    typename TInputImage::IndexType  inputLargIndex;
    typename TOutputImage::SizeType  outputSize;
    typename TOutputImage::IndexType outputIndex;

    outputIndex = this->GetOutput()->GetRequestedRegion().GetIndex();
    outputSize = this->GetOutput()->GetRequestedRegion().GetSize();
    inputLargSize = this->GetInput()->GetLargestPossibleRegion().GetSize();
    inputLargIndex = this->GetInput()->GetLargestPossibleRegion().GetIndex();

    for (unsigned int i = 0; i < TInputImage::ImageDimension; i++)
    {
      inputSize[i] = outputSize[i];
      inputIndex[i] = outputIndex[i];
    }

    RequestedRegion.SetSize(inputSize);
    RequestedRegion.SetIndex(inputIndex);
    InputImagePointer input = const_cast<TInputImage *>(this->GetInput());
    input->SetRequestedRegion(RequestedRegion);
  }
}


template <typename TInputImage, typename TOutputImage>
void
PhaseSymmetryImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  typename TOutputImage::RegionType  outputRegion;
  typename TInputImage::IndexType    inputIndex;
  typename TInputImage::SizeType     inputSize;
  typename TOutputImage::SizeType    outputSize;
  typename TOutputImage::IndexType   outputIndex;
  typename TInputImage::SpacingType  inSpacing;
  typename TInputImage::PointType    inOrigin;
  typename TOutputImage::SpacingType outSpacing;
  typename TOutputImage::PointType   outOrigin;

  // Get pointers to the input and output
  typename Superclass::OutputImagePointer output = this->GetOutput();
  typename Superclass::InputImagePointer  input = const_cast<TInputImage *>(this->GetInput());

  // Return if input and output are both null
  if (!input || !output)
  {
    return;
  }

  inputIndex = input->GetLargestPossibleRegion().GetIndex();
  inputSize = input->GetLargestPossibleRegion().GetSize();
  inSpacing = input->GetSpacing();
  inOrigin = input->GetOrigin();

  // Set the LargestPossibleRegion of the output.

  for (unsigned int i = 0; i < InputImageDimension; i++)
  {
    outputSize[i] = inputSize[i];
    outputIndex[i] = inputIndex[i];
    outSpacing[i] = inSpacing[i];
    outOrigin[i] = inOrigin[i];
  }

  // Set the size of the output region
  outputRegion.SetSize(outputSize);
  // Set the index of the output region
  outputRegion.SetIndex(outputIndex);
  // Set the origin and spacing
  output->SetOrigin(outOrigin);
  output->SetSpacing(outSpacing);
  // Set the largest po
  output->SetLargestPossibleRegion(outputRegion);
}


template <typename TInputImage, typename TOutputImage>
void
PhaseSymmetryImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  //  os << indent << " Integral Filter Normalize By: " << m_Cutoff << std::endl;
}

} // end namespace itk

#endif

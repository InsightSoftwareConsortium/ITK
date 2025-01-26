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
#ifndef itkPhaseAnalysisSoftThresholdImageFilter_h
#define itkPhaseAnalysisSoftThresholdImageFilter_h

#include <itkPhaseAnalysisImageFilter.h>
namespace itk
{
/** \class PhaseAnalysisSoftThresholdImageFilter
 * This filter operates on an input MonogenicSignal in the Spatial Domain.
 * Represented as a VectorImage of ImageDimension + 1.
 * f_m ={ f, R_x*f R_y*f ... }
 * where f is a real image. R_x, R_y, ... are directional Riesz transforms in the spatial domain.
 *
 * Dev note: The best way to get the spatial Riesz, is doing an inverse FFT of the output of
 * \sa MonogenicSignalFrequencyImageFilter
 * All filters in the module IsotropicWavelets avoid doing any FFT,
 * even though they work on the frequency domain. This is a dev decision to decouple algorithms from
 * the specific frequency layout of the FFT of choice.
 * User just have to modify the GetFrequency in a new FrequencyIterator if other FFT library is chosen.
 *
 * The output should be a new real image f', so it can be integrated to an inverse Wavelet pyramid.
 * \sa itkWaveletFrequencyInverse
 * \ingroup IsotropicWavelets
 */
template <typename TInputImage,
          typename TOutputImage = Image<typename TInputImage::PixelType::ComponentType, TInputImage::ImageDimension>>
class PhaseAnalysisSoftThresholdImageFilter : public PhaseAnalysisImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PhaseAnalysisSoftThresholdImageFilter);

  /** Standard class type alias. */
  using Self = PhaseAnalysisSoftThresholdImageFilter;
  using Superclass = PhaseAnalysisImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkOverrideGetNameOfClassMacro(PhaseAnalysisSoftThresholdImageFilter);

  /** Some convenient type alias. */
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;

  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using SpacingType = typename InputImageType::SpacingType;
  using SizeType = typename InputImageRegionType::SizeType;

  using DirectionType = SpacingType;
  using FloatType = typename InputImageType::SpacingValueType;

  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  using OutputImageRegionIterator = typename Superclass::OutputImageRegionIterator;

  itkSetMacro(ApplySoftThreshold, bool);
  itkGetConstMacro(ApplySoftThreshold, bool);
  itkBooleanMacro(ApplySoftThreshold);

  itkSetMacro(NumOfSigmas, OutputImagePixelType);
  itkGetConstMacro(NumOfSigmas, OutputImagePixelType);
  itkGetConstMacro(MeanAmp, OutputImagePixelType);
  itkGetConstMacro(SigmaAmp, OutputImagePixelType);
  itkGetConstMacro(Threshold, OutputImagePixelType);

  OutputImageType *
  GetOutputCosPhase()
  {
    return itkDynamicCastInDebugMode<OutputImageType *>(this->GetOutput(2));
  }

protected:
  PhaseAnalysisSoftThresholdImageFilter();
  ~PhaseAnalysisSoftThresholdImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;
  void
  ThreadedComputeCosineOfPhase(const OutputImageRegionType & outputRegionForThread);

private:
  bool                 m_ApplySoftThreshold{ true };
  OutputImagePixelType m_NumOfSigmas;
  OutputImagePixelType m_MeanAmp;
  OutputImagePixelType m_SigmaAmp;
  OutputImagePixelType m_Threshold;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPhaseAnalysisSoftThresholdImageFilter.hxx"
#endif

#endif

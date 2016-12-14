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
#ifndef itkPhaseAnalysisSoftThresholdImageFilter_h
#define itkPhaseAnalysisSoftThresholdImageFilter_h

#include <itkPhaseAnalysisImageFilter.h>
#include "itkBarrier.h"
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
 * even though they work on the frequency domain. This is a dev decission to decouple algorithms from
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
  /** Standard class typedefs. */
  typedef PhaseAnalysisSoftThresholdImageFilter               Self;
  typedef PhaseAnalysisImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(PhaseAnalysisSoftThresholdImageFilter, PhaseAnalysisImageFilter);

  /** Some convenient typedefs. */
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;

  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;
  typedef typename InputImageType::RegionType     InputImageRegionType;
  typedef typename InputImageType::PixelType      InputImagePixelType;
  typedef typename InputImageType::SpacingType    SpacingType;
  typedef typename InputImageRegionType::SizeType SizeType;

  typedef SpacingType                               DirectionType;
  typedef typename InputImageType::SpacingValueType FloatType;

  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;

  itkSetMacro(ApplySoftThreshold, bool);
  itkGetConstMacro(ApplySoftThreshold, bool);
  itkSetMacro(NumOfSigmas, OutputImagePixelType);
  itkGetConstMacro(NumOfSigmas, OutputImagePixelType);
  itkGetConstMacro(MeanAmp, OutputImagePixelType);
  itkGetConstMacro(SigmaAmp, OutputImagePixelType);
  itkGetConstMacro(Threshold, OutputImagePixelType);

  const OutputImageType *
  GetOutputCosPhase() const
  {
    return this->GetOutput(2);
  }
  OutputImageType *
  GetOutputCosPhase()
  {
    return this->GetOutput(2);
  }

protected:
  PhaseAnalysisSoftThresholdImageFilter();
  ~PhaseAnalysisSoftThresholdImageFilter() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void
  BeforeThreadedGenerateData() ITK_OVERRIDE;
  virtual void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PhaseAnalysisSoftThresholdImageFilter);
  bool                 m_ApplySoftThreshold;
  OutputImagePixelType m_NumOfSigmas;
  OutputImagePixelType m_MeanAmp;
  OutputImagePixelType m_SigmaAmp;
  OutputImagePixelType m_Threshold;
  Barrier::Pointer     m_Barrier1;
  Barrier::Pointer     m_Barrier2;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPhaseAnalysisSoftThresholdImageFilter.hxx"
#endif

#endif

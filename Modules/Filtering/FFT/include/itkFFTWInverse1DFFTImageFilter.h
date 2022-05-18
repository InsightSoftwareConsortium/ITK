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
#ifndef itkFFTWInverse1DFFTImageFilter_h
#define itkFFTWInverse1DFFTImageFilter_h

#include "itkInverse1DFFTImageFilter.h"
#include "itkFFTWCommonExtended.h"
#include "itkImageRegionSplitterDirection.h"

#include "itkFFTImageFilterFactory.h"

#include <vector>

namespace itk
{
/** \class FFTWInverse1DFFTImageFilter
 * \brief only do FFT along one dimension using FFTW as a backend.
 *
 * \ingroup ITKFFT
 * \ingroup FourierTransform
 */

template <typename TInputImage,
          typename TOutputImage =
            Image<typename NumericTraits<typename TInputImage::PixelType>::ValueType, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT FFTWInverse1DFFTImageFilter : public Inverse1DFFTImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FFTWInverse1DFFTImageFilter);

  using Self = FFTWInverse1DFFTImageFilter;
  using Superclass = Inverse1DFFTImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard class type alias.*/
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /**
   * the proxy type is a wrapper for the fftw API
   * since the proxy is only defined over double and float,
   * trying to use any other pixel type is inoperative, as
   * is trying to use double if only the float FFTW1D version is
   * configured in, or float if only double is configured.
   */
  using FFTW1DProxyType = typename fftw::ComplexToComplexProxy<typename TOutputImage::PixelType>;
  using PlanArrayType = typename std::vector<typename FFTW1DProxyType::PlanType>;
  using PlanBufferPointerType = typename std::vector<typename FFTW1DProxyType::ComplexType *>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWInverse1DFFTImageFilter, Inverse1DFFTImageFilter);


protected:
  FFTWInverse1DFFTImageFilter();
  ~FFTWInverse1DFFTImageFilter() override;

  void
  BeforeThreadedGenerateData() override;
  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadID) override;

  /** Override to return a splitter that does not split along the direction we
   *  are performing the transform. */
  const ImageRegionSplitterBase *
  GetImageRegionSplitter() const override;

private:
  ImageRegionSplitterDirection::Pointer m_ImageRegionSplitter;

  /** Destroy FFTW Plans and associated buffers. */
  void
  DestroyPlans();

  bool                  m_PlanComputed{ false };
  PlanArrayType         m_PlanArray;
  unsigned int          m_LastImageSize{ 0 };
  PlanBufferPointerType m_InputBufferArray;
  PlanBufferPointerType m_OutputBufferArray;
};


// Describe whether input/output are real- or complex-valued
// for factory registration
template <>
struct FFTImageFilterTraits<FFTWInverse1DFFTImageFilter>
{
  template <typename TUnderlying>
  using InputPixelType = std::complex<TUnderlying>;
  template <typename TUnderlying>
  using OutputPixelType = TUnderlying;
  using FilterDimensions = std::integer_sequence<unsigned int, 4, 3, 2, 1>;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFFTWInverse1DFFTImageFilter.hxx"
#endif

#endif // itkFFTWInverse1DFFTImageFilter_h

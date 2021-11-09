/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkFFTWForward1DFFTImageFilter_h
#define itkFFTWForward1DFFTImageFilter_h

#include "itkForward1DFFTImageFilter.h"
#include "itkFFTWCommonExtended.h"
#include "itkImageRegionSplitterDirection.h"
#include "itkVersion.h"

#include <vector>


namespace itk
{

/** \class FFTWForward1DFFTImageFilter
 * \brief only do FFT along one dimension using FFTW as a backend.
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 */
template <typename TInputImage,
          typename TOutputImage = Image<std::complex<typename TInputImage::PixelType>, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT FFTWForward1DFFTImageFilter : public Forward1DFFTImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FFTWForward1DFFTImageFilter);

  using Self = FFTWForward1DFFTImageFilter;
  using Superclass = Forward1DFFTImageFilter<TInputImage, TOutputImage>;
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
  using FFTW1DProxyType = typename fftw::ComplexToComplexProxy<typename TInputImage::PixelType>;
  using PlanArrayType = typename std::vector<typename FFTW1DProxyType::PlanType>;
  using PlanBufferPointerType = typename std::vector<typename FFTW1DProxyType::ComplexType *>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWForward1DFFTImageFilter, Forward1DFFTImageFilter);


protected:
  FFTWForward1DFFTImageFilter();
  ~FFTWForward1DFFTImageFilter() override;

  void
  BeforeThreadedGenerateData() override;
  void
  ThreadedGenerateData(const OutputImageRegionType &, ThreadIdType threadID) override;

  /** Override to return a splitter that does not split along the direction we
   *  are performing the transform. */
  const ImageRegionSplitterBase *
  GetImageRegionSplitter() const override;


private:
  ImageRegionSplitterDirection::Pointer m_ImageRegionSplitter;

  /** Destroy FFTW Plans and associated buffers. */
  void
  DestroyPlans();

  bool                  m_PlanComputed;
  PlanArrayType         m_PlanArray;
  unsigned int          m_LastImageSize;
  PlanBufferPointerType m_InputBufferArray;
  PlanBufferPointerType m_OutputBufferArray;
};


/** \class FFTWForward1DFFTImageFilterFactory
 *
 * \brief Object Factory implementation for FFTWForward1DFFTImageFilter
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 */
class FFTWForward1DFFTImageFilterFactory : public itk::ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FFTWForward1DFFTImageFilterFactory);

  using Self = FFTWForward1DFFTImageFilterFactory;
  using Superclass = ObjectFactoryBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Class methods used to interface with the registered factories. */
  const char *
  GetITKSourceVersion() const override
  {
    return ITK_SOURCE_VERSION;
  }
  const char *
  GetDescription() const override
  {
    return "A Factory for FFTWForward1DFFTImageFilterFactory";
  }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWForward1DFFTImageFilterFactory, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    FFTWForward1DFFTImageFilterFactory::Pointer factory = FFTWForward1DFFTImageFilterFactory::New();

    ObjectFactoryBase::RegisterFactory(factory);
  }

private:
  template <typename InputPixelType, typename OutputPixelType, size_t ImageDimension>
  void
  OverrideFFTWForward1DFFTImageFilterTypeMacro()
  {
    using InputImageType = Image<InputPixelType, ImageDimension>;
    using OutputImageType = Image<std::complex<OutputPixelType>, ImageDimension>;
    this->RegisterOverride(typeid(Forward1DFFTImageFilter<InputImageType, OutputImageType>).name(),
                           typeid(FFTWForward1DFFTImageFilter<InputImageType, OutputImageType>).name(),
                           "FFTW Forward 1D FFT Image Filter Override",
                           true,
                           CreateObjectFunction<FFTWForward1DFFTImageFilter<InputImageType, OutputImageType>>::New());
  }

  FFTWForward1DFFTImageFilterFactory()
  {
#ifdef ITK_USE_FFTWF
    OverrideFFTWForward1DFFTImageFilterTypeMacro<float, float, 1>();
    OverrideFFTWForward1DFFTImageFilterTypeMacro<float, float, 2>();
    OverrideFFTWForward1DFFTImageFilterTypeMacro<float, float, 3>();
#endif // ITK_USE_FFTWF

#ifdef ITK_USE_FFTWD
    OverrideFFTWForward1DFFTImageFilterTypeMacro<double, double, 1>();
    OverrideFFTWForward1DFFTImageFilterTypeMacro<double, double, 2>();
    OverrideFFTWForward1DFFTImageFilterTypeMacro<double, double, 3>();
#endif // ITK_USE_FFTWD
  }
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFFTWForward1DFFTImageFilter.hxx"
#endif

#endif // itkFFTWForward1DFFTImageFilter_h

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

#ifndef itkFFTImageFilterFactory_h
#define itkFFTImageFilterFactory_h

#include "itkImage.h"
#include "itkObjectFactoryBase.h"
#include "itkVersion.h"

namespace itk
{

/** \class FFTImageFilterTraits
 *
 * \brief Helper defining pixel traits for templated FFT image filters
 *
 * FFT factory registration must account for whether the input/output
 * images for an FFT class are real- or complex-valued. Each FFT
 * image filter must be accompanied by a specialized FFTImageFilterTraits
 * class to define these mappings for factory registration.
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 */
template <template <typename, typename> class TFFTImageFilter>
struct FFTImageFilterTraits
{};


/** \class FFTImageFilterFactory
 *
 * \brief Object factory implementation for FFT filters
 *
 * ITK FFT filters are designed for overridable implementation
 * through the global ITK object factory singleton. In order to
 * instantiate a base FFT image filter object, at least one
 * valid implementation must be registered in the object factory.
 *
 * FFTImageFilterFactory exists as an implementation detail
 * designed to reduce overhead for FFT factory operations.
 * A factory object templated over an FFT image filter
 * implementation class will make constructor overrides
 * for that class available through the global object factory
 * singleton.
 *
 * FFTImageFilterFactory can be used for making an FFT implementation
 * class available through object factory initialization if the class
 * meets certain criteria:
 * - the FFT implementation class must be able to be templated over
 *   the pattern
 *   <TInput<PixelType, Dimension>, TOutput<PixelType, Dimension>
 * - the FFT implementation class must have an associated
 *   `FFTImageFilterTraits` specialization defining pixel types
 *   and image dimensions for specialization
 *
 * `TInput` and `TOutput` must be classes implementing the `itk::Image`
 * interface but need not inherit from `itk::Image`. For example,
 * an FFT filter could define operations over an
 * `itk::SpecialCoordinatesImage`.
 *
 * FFT filters not meeting these criteria may define an independent
 * factory tailored to their particular instantiation requirements.
 *
 * \sa FFTImageFilterTraits
 * \sa VnlFFTImageFilterInitFactory
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 */
template <template <typename, typename> class TFFTImageFilter,
          template <typename, unsigned int> class TInput = Image,
          template <typename, unsigned int> class TOutput = Image>
class FFTImageFilterFactory : public itk::ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FFTImageFilterFactory);

  using Self = FFTImageFilterFactory;
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
    return "An FFTImageFilter factory";
  }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTImageFilterFactory, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    FFTImageFilterFactory::Pointer factory = FFTImageFilterFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(factory);
  }

protected:
  /** Override base FFT image filter constructor with the given TFFTImageFilter template
   * with distinct specialization.
   * Requires TFFTImageFilter to have the template signature <InputImageType, OutputImageType>.
   */
  template <typename InputPixelType, typename OutputPixelType, unsigned int D, unsigned int... ImageDimensions>
  void
  OverrideFFTImageFilterType(const std::integer_sequence<unsigned int, D, ImageDimensions...> &)
  {
    using InputImageType = TInput<InputPixelType, D>;
    using OutputImageType = TOutput<OutputPixelType, D>;
    this->RegisterOverride(typeid(typename TFFTImageFilter<InputImageType, OutputImageType>::Superclass).name(),
                           typeid(TFFTImageFilter<InputImageType, OutputImageType>).name(),
                           "FFT Image Filter Override",
                           true,
                           CreateObjectFunction<TFFTImageFilter<InputImageType, OutputImageType>>::New());
    OverrideFFTImageFilterType<InputPixelType, OutputPixelType>(
      std::integer_sequence<unsigned int, ImageDimensions...>{});
  }
  template <typename InputPixelType, typename OutputPixelType>
  void
  OverrideFFTImageFilterType(const std::integer_sequence<unsigned int> &)
  {}

  FFTImageFilterFactory()
  {
    OverrideFFTImageFilterType<typename FFTImageFilterTraits<TFFTImageFilter>::template InputPixelType<float>,
                               typename FFTImageFilterTraits<TFFTImageFilter>::template OutputPixelType<float>>(
      typename FFTImageFilterTraits<TFFTImageFilter>::FilterDimensions{});

    OverrideFFTImageFilterType<typename FFTImageFilterTraits<TFFTImageFilter>::template InputPixelType<double>,
                               typename FFTImageFilterTraits<TFFTImageFilter>::template OutputPixelType<double>>(
      typename FFTImageFilterTraits<TFFTImageFilter>::FilterDimensions{});
  }
};

} // namespace itk

#endif // itkFFTImageFilterFactory_h

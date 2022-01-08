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
 * \brief Object Factory implementation for FFT filters
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 */
template <template <typename, typename> class TFFTImageFilter>
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
    using InputImageType = Image<InputPixelType, D>;
    using OutputImageType = Image<OutputPixelType, D>;
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
      std::integer_sequence<unsigned int, 4, 3, 2, 1>{});

    OverrideFFTImageFilterType<typename FFTImageFilterTraits<TFFTImageFilter>::template InputPixelType<double>,
                               typename FFTImageFilterTraits<TFFTImageFilter>::template OutputPixelType<double>>(
      std::integer_sequence<unsigned int, 4, 3, 2, 1>{});
  }
};

} // namespace itk

#endif // itkFFTImageFilterFactory_h

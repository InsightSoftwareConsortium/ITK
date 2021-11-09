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
#ifndef itkVnlForward1DFFTImageFilter_h
#define itkVnlForward1DFFTImageFilter_h

#include "itkForward1DFFTImageFilter.h"
#include <complex>
#include "itkVersion.h"

namespace itk
{

/** \class VnlForward1DFFTImageFilter
 *
 * \brief Perform the FFT along one dimension of an image using Vnl as a
 * backend.
 *
 * \ingroup Ultrasound
 */
template <typename TInputImage,
          typename TOutputImage = Image<std::complex<typename TInputImage::PixelType>, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT VnlForward1DFFTImageFilter : public Forward1DFFTImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VnlForward1DFFTImageFilter);

  /** Standard class type alias. */
  using Self = VnlForward1DFFTImageFilter;
  using Superclass = Forward1DFFTImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VnlForward1DFFTImageFilter, Forward1DFFTImageFilter);

protected:
  void
  GenerateData() override;

  VnlForward1DFFTImageFilter() = default;
  ~VnlForward1DFFTImageFilter() override = default;

private:
};


/** \class VnlForward1DFFTImageFilterFactory
 *
 * \brief Object Factory implementation for VnlForward1DFFTImageFilter
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 */
class VnlForward1DFFTImageFilterFactory : public itk::ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VnlForward1DFFTImageFilterFactory);

  using Self = VnlForward1DFFTImageFilterFactory;
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
    return "A Factory for VnlForward1DFFTImageFilterFactory";
  }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VnlForward1DFFTImageFilterFactory, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    VnlForward1DFFTImageFilterFactory::Pointer factory = VnlForward1DFFTImageFilterFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(factory);
  }

private:
  template <typename InputPixelType, typename OutputPixelType, size_t ImageDimension>
  void
  OverrideVnlForward1DFFTImageFilterTypeMacro()
  {
    using InputImageType = Image<InputPixelType, ImageDimension>;
    using OutputImageType = Image<std::complex<OutputPixelType>, ImageDimension>;
    this->RegisterOverride(typeid(Forward1DFFTImageFilter<InputImageType, OutputImageType>).name(),
                           typeid(VnlForward1DFFTImageFilter<InputImageType, OutputImageType>).name(),
                           "Vnl Forward 1D FFT Image Filter Override",
                           true,
                           CreateObjectFunction<VnlForward1DFFTImageFilter<InputImageType, OutputImageType>>::New());
  }

  VnlForward1DFFTImageFilterFactory()
  {
    OverrideVnlForward1DFFTImageFilterTypeMacro<float, float, 1>();
    OverrideVnlForward1DFFTImageFilterTypeMacro<float, float, 2>();
    OverrideVnlForward1DFFTImageFilterTypeMacro<float, float, 3>();

    OverrideVnlForward1DFFTImageFilterTypeMacro<double, double, 1>();
    OverrideVnlForward1DFFTImageFilterTypeMacro<double, double, 2>();
    OverrideVnlForward1DFFTImageFilterTypeMacro<double, double, 3>();
  }
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVnlForward1DFFTImageFilter.hxx"
#endif

#endif

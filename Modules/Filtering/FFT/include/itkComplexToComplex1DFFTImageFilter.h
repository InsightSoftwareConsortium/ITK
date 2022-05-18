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
#ifndef itkComplexToComplex1DFFTImageFilter_h
#define itkComplexToComplex1DFFTImageFilter_h

#include <complex>

#include "itkImage.h"
#include "itkImageToImageFilter.h"
#include "itkMacro.h"

namespace itk
{
/** \class ComplexToComplex1DFFTImageFilter
 * \brief Perform the Fast Fourier Transform, complex input to complex output,
 * but only along one dimension.
 *
 * The direction of the transform, 'Forward' or 'Inverse', can be set with
 * SetTransformDirection() and GetTransformDirection().
 *
 * The dimension along which to apply to filter can be specified with
 * SetDirection() and GetDirection().
 *
 * \ingroup ITKFFT
 * \ingroup FourierTransform
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT ComplexToComplex1DFFTImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ComplexToComplex1DFFTImageFilter);

  /** Standard class type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  using Self = ComplexToComplex1DFFTImageFilter;
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Dimension of the underlying image. */
  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  itkTypeMacro(ComplexToComplex1DFFTImageFilter, ImageToImageFilter);

  /** Customized object creation methods that support configuration-based
   * selection of FFT implementation.
   *
   * Default implementation is VnlFFT1D.
   */
  itkFactoryOnlyNewMacro(Self);

  /** Transform direction. */
  enum TransformDirectionType
  {
    DIRECT = 1,
    INVERSE
  };

  /** Set/Get the direction in which the transform will be applied.
   * By selecting DIRECT, this filter will perform a direct (forward) Fourier
   * Transform.
   * By selecting INVERSE, this filter will perform an inverse Fourier
   * Transform. */
  itkSetMacro(TransformDirection, TransformDirectionType);
  itkGetConstMacro(TransformDirection, TransformDirectionType);

  /** Get the direction in which the filter is to be applied. */
  itkGetConstMacro(Direction, unsigned int);

  /** Set the direction in which the filter is to be applied. */
  itkSetClampMacro(Direction, unsigned int, 0, InputImageType::ImageDimension - 1);

  /** Get the greatest supported prime factor. */
  virtual SizeValueType
  GetSizeGreatestPrimeFactor() const
  {
    return 2;
  }

protected:
  ComplexToComplex1DFFTImageFilter();
  ~ComplexToComplex1DFFTImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateInputRequestedRegion() override;
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  /** Direction in which the filter is to be applied
   * this should be in the range [0,ImageDimension-1]. */
  unsigned int m_Direction{ 0 };

  /** Direction to apply the transform (forward/inverse). */
  TransformDirectionType m_TransformDirection{ DIRECT };

private:
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkComplexToComplex1DFFTImageFilter.hxx"
#endif

#ifdef ITK_FFTIMAGEFILTERINIT_FACTORY_REGISTER_MANAGER
#  include "itkFFTImageFilterInitFactoryRegisterManager.h"
#endif

#endif // itkComplexToComplex1DFFTImageFilter_h

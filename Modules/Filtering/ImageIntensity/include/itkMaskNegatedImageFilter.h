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
#ifndef itkMaskNegatedImageFilter_h
#define itkMaskNegatedImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"
#include "itkNumericTraits.h"
#include "itkMath.h"

namespace itk
{
namespace Functor
{
/**
 * \class MaskNegatedInput
 * \brief
 * \ingroup ITKImageIntensity
 */
template <typename TInput, typename TMask, typename TOutput = TInput>
class MaskNegatedInput
{
public:
  using AccumulatorType = typename NumericTraits<TInput>::AccumulateType;

  MaskNegatedInput()
    : m_OutsideValue(NumericTraits<TOutput>::ZeroValue())
    , m_MaskingValue(NumericTraits<TMask>::ZeroValue())
  {}
  ~MaskNegatedInput() = default;
  bool
  operator!=(const MaskNegatedInput &) const
  {
    return false;
  }

  bool
  operator==(const MaskNegatedInput & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & A, const TMask & B) const
  {
    if (B != m_MaskingValue)
    {
      return m_OutsideValue;
    }
    else
    {
      return static_cast<TOutput>(A);
    }
  }

  /** Method to explicitly set the outside value of the mask */
  void
  SetOutsideValue(const TOutput & outsideValue)
  {
    m_OutsideValue = outsideValue;
  }

  /** Method to get the outside value of the mask */
  const TOutput &
  GetOutsideValue() const
  {
    return m_OutsideValue;
  }

  /** Method to explicitly set the masking value of the mask */
  void
  SetMaskingValue(const TMask & maskingValue)
  {
    m_MaskingValue = maskingValue;
  }

  /** Method to get the outside value of the mask */
  const TMask &
  GetMaskingValue() const
  {
    return m_MaskingValue;
  }

private:
  TOutput m_OutsideValue;
  TMask   m_MaskingValue;
};
} // namespace Functor

/** \class MaskNegatedImageFilter
 * \brief Mask an image with the negation (or logical compliment) of a mask.
 *
 * This class is templated over the types of the
 * input image type, the mask image type and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The pixel type of the input 2 image must have a valid definition of the
 * operator!=. This condition is required because internally this
 * filter will perform the operation
 *
   \code
          if pixel_from_mask_image != mask_value
               pixel_output_image = output_value
          else
               pixel_output_image = pixel_input_image
   \endcode
 *
 * The pixel from the input 1 is cast to the pixel type of the output image.
 *
 * Note that the input and the mask images must be of the same size.
 *
 * \warning Only pixel value with mask_value ( defaults to 0 ) will
 * be preserved.
 *
 * \sa MaskImageFilter
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/InverseOfMaskToImage,Inverse Of Mask To Image}
 * \endsphinx
 */
template <typename TInputImage, typename TMaskImage, typename TOutputImage = TInputImage>
class MaskNegatedImageFilter : public BinaryGeneratorImageFilter<TInputImage, TMaskImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MaskNegatedImageFilter);

  /** Standard class type aliases. */
  using Self = MaskNegatedImageFilter;
  using Superclass = BinaryGeneratorImageFilter<TInputImage, TMaskImage, TOutputImage>;

  using FunctorType = Functor::
    MaskNegatedInput<typename TInputImage::PixelType, typename TMaskImage::PixelType, typename TOutputImage::PixelType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MaskNegatedImageFilter, BinaryGeneratorImageFilter);

  /** Typedefs **/
  using MaskImageType = TMaskImage;

  /** Method to explicitly set the outside value of the mask. Defaults to 0 */
  void
  SetOutsideValue(const typename TOutputImage::PixelType & outsideValue)
  {
    if (Math::NotExactlyEquals(this->GetOutsideValue(), outsideValue))
    {
      this->Modified();
      this->GetFunctor().SetOutsideValue(outsideValue);
    }
  }

  const typename TOutputImage::PixelType &
  GetOutsideValue() const
  {
    return this->GetFunctor().GetOutsideValue();
  }

  /** Method to explicitly set the masking value of the mask. Defaults to 0 */
  void
  SetMaskingValue(const typename TMaskImage::PixelType & maskingValue)
  {
    if (this->GetMaskingValue() != maskingValue)
    {
      this->GetFunctor().SetMaskingValue(maskingValue);
      this->Modified();
    }
  }

  /** Method to get the masking value of the mask. */
  const typename TMaskImage::PixelType &
  GetMaskingValue() const
  {
    return this->GetFunctor().GetMaskingValue();
  }

  /** Set/Get the mask image. Pixels set to zero in the mask image will retain
   *  the original value of the input image while non-zero pixels in
   *  the mask will be set to the "OutsideValue".
   */
  void
  SetMaskImage(const MaskImageType * maskImage)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput(1, const_cast<MaskImageType *>(maskImage));
  }

  const MaskImageType *
  GetMaskImage()
  {
    return static_cast<const MaskImageType *>(this->ProcessObject::GetInput(1));
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(MaskEqualityComparableCheck, (Concept::EqualityComparable<typename TMaskImage::PixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage::PixelType, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  MaskNegatedImageFilter() = default;
  ~MaskNegatedImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "OutsideValue: " << this->GetOutsideValue() << std::endl;
  }

  void
  BeforeThreadedGenerateData() override
  {
    this->SetFunctor(this->GetFunctor());
  }

private:
  itkGetConstReferenceMacro(Functor, FunctorType);
  FunctorType &
  GetFunctor()
  {
    return m_Functor;
  }

  FunctorType m_Functor;
};
} // end namespace itk

#endif

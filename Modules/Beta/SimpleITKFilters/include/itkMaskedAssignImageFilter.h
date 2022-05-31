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
#ifndef itkMaskedAssignImageFilter_h
#define itkMaskedAssignImageFilter_h

#include "itkTernaryGeneratorImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{

/**
 *\class MaskedAssignImageFilter
 * \brief Assign values to a masked set of pixels.
 *
 * This class is templated over the types of the
 * input image, the mask image and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The first input is the "input image", the second is the "mask image",
 * the third is the "assign image". Any of the images can be set as constants,
 * with third input can be set as constant with the `SetAssignConstant` method.
 *
 * For the "masked input" non-zero values are considered the mask, and assigned values
 * from the "assign input".
 *
 * The following is the logic applied per pixel (pixel value or constant value) :
 *
   \code
          if pixel_from_mask_image != 0
               pixel_output_image = pixel_assign_image
          else
               pixel_output_image = pixel_input_image
   \endcode
 *
 * The pixel from the "input image" is cast to the pixel type of the output image.
 *
 * Note all images must be geometrically congruent.
 *
 * \sa MaskImageFilter
 * \ingroup SimpleITKFilters
 *
 */
template <typename TInputImage, typename TMaskImage, typename TOutputImage = TInputImage>
class MaskedAssignImageFilter : public TernaryGeneratorImageFilter<TInputImage, TMaskImage, TOutputImage, TOutputImage>

{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MaskedAssignImageFilter);

  /** Standard class type aliases. */
  using Self = MaskedAssignImageFilter;
  using Superclass = TernaryGeneratorImageFilter<TInputImage, TMaskImage, TOutputImage, TOutputImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MaskedAssignImageFilter, TernaryGeneratorImageFilter);

  /** Typedefs **/
  using InputImageType = TInputImage;
  using MaskImageType = TMaskImage;
  using AssignImageType = TOutputImage;
  using OutputImageType = TOutputImage;

  using OutputPixelType = typename OutputImageType::PixelType;

  itkSetInputMacro(MaskImage, MaskImageType);
  itkGetInputMacro(MaskImage, MaskImageType);

  itkSetInputMacro(AssignImage, AssignImageType);
  itkGetInputMacro(AssignImage, AssignImageType);

  virtual void
  SetAssignConstant(const OutputPixelType & v)
  {
    this->SetConstant3(v);
  }
  virtual OutputPixelType
  GetAssignConstant() const
  {
    return this->GetConstant3();
  }

protected:
  MaskedAssignImageFilter();
  ~MaskedAssignImageFilter() override = default;
};
} // end namespace itk

#endif

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMaskedAssignImageFilter.hxx"
#endif

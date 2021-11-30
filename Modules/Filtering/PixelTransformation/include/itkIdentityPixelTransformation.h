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
#ifndef itkIdentityPixelTransformation_h
#define itkIdentityPixelTransformation_h

#include "itkPixelTransformation.h"

namespace itk
{
/**
 * \class IdentityPixelTransformation
 *
 * This class is part of the pixel transformation hierarchy, whose base class is PixelTransformation.
 * It defines the identity transformation of the pixel values after an image transformation.
 * This is the default pixel transformation used in the resampleImageFilter. And it is the typical
 * pixel transformation (no transformation) for geometrically scalar images, for which the
 * pixel values are intensities without directional or density information/dependency.
 * This is true also for non-geometrical (computational) vectors such as color RGB images.
 *
 * \par
 * These pixel transformations do not replace the ImageTransform but complement it.
 * They have been introduced to be used by itkResampleImageFilter
 * when the pixel type requires more complex transform than simply relocating it.
 *
 * \ingroup Filtering
 * \ingroup PixelTransformation
 */
template <typename TPixelType,
          typename TTransformType,
          typename TOutputPointType = typename TTransformType::OutputPointType>
class ITK_TEMPLATE_EXPORT IdentityPixelTransformation
  : public PixelTransformation<TPixelType, TTransformType, TOutputPointType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(IdentityPixelTransformation);

  /** Standard class type aliases. */
  using Self = IdentityPixelTransformation;
  using Superclass = PixelTransformation<TPixelType, TTransformType, TOutputPointType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(IdentityPixelTransformation, PixelTransformation);

  using TransformType = typename Superclass::TransformType;
  using PixelType = typename Superclass::PixelType;
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;

  PixelType &
  Transform(const PixelType & value, const InputPointType & inputPoint, const OutputPointType & outputPoint) override;


protected:
  IdentityPixelTransformation();
  ~IdentityPixelTransformation() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkIdentityPixelTransformation.hxx"
#endif

#endif

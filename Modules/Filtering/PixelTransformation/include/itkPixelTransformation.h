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
#ifndef itkPixelTransformation_h
#define itkPixelTransformation_h

#include "itkObject.h"

namespace itk
{
/**
 * \class PixelTransformation
 *
 * This class is the base for the pixel transformation hierarchy.
 * It defines a generic interface for a transformation of the pixel values after
 * an image transformation. Typical non-trivial cases are the transformation of
 * images of geometric (contravariant) vectors, covariant vectors, or any more
 * complex tensor. These pixel transformations do not replace the ImageTransform
 * but complement it. They have been introduced to be used by itkResampleImageFilter
 * when the pixel type requires more complex transform than simply relocating it.
 *
 * \par
 * Each subclass of PixelTransformation defines the transformation for a particular
 * type of object (pixel).
 *
 * \ingroup Filtering
 * \ingroup PixelTransformation
 */
template <typename TPixelType,
          typename TTransformType,
          typename TOutputPointType = typename TTransformType::OutputPointType>
class ITK_TEMPLATE_EXPORT PixelTransformation : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PixelTransformation);

  /** Standard class type aliases. */
  using Self = PixelTransformation;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(PixelTransformation, Object);

  using TransformType = TTransformType;
  using PixelType = TPixelType;
  using InputPointType = typename TransformType::InputPointType;
  using OutputPointType = TOutputPointType;
  using CoordValueType = typename InputPointType::CoordRepType;

  itkSetConstObjectMacro(ImageTransform, TransformType);
  itkGetConstObjectMacro(ImageTransform, TransformType);

  /**
   * The default values for the points are meant to be used for linear transformations,
   * for which these points are irrelevant.
   */
  virtual PixelType
  Transform(const PixelType &       value,
            const InputPointType &  inputPoint = InputPointType(),
            const OutputPointType & outputPoint = OutputPointType()) = 0;


protected:
  PixelTransformation();
  ~PixelTransformation() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename TransformType::ConstPointer m_ImageTransform;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPixelTransformation.hxx"
#endif

#endif

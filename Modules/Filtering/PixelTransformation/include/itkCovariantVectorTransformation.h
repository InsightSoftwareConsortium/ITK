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
#ifndef itkCovariantVectorTransformation_h
#define itkCovariantVectorTransformation_h

#include "itkPixelTransformation.h"

namespace itk
{
/**
 * \class CovariantVectorTransformation
 *
 * This class is part of the pixel transformation hierarchy, whose base class is PixelTransformation.
 * It defines the transformation for pixel values representing covariant vectors due to the spatial
 * transformation of the image (covariant vector field).
 * The natural appearance of covariant vectors is as differential of scalar fields, loosely identified with
 * gradient vectors and with the normal vectors of the (hyper)-surfaces.
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
class ITK_TEMPLATE_EXPORT CovariantVectorTransformation
  : public PixelTransformation<TPixelType, TTransformType, TOutputPointType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CovariantVectorTransformation);

  /** Standard class type aliases. */
  using Self = CovariantVectorTransformation;
  using Superclass = PixelTransformation<TPixelType, TTransformType, TOutputPointType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(CovariantVectorTransformation, PixelTransformation);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  using TransformType = typename Superclass::TransformType;
  using PixelType = typename Superclass::PixelType;
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;

  static constexpr unsigned int Dimension = TPixelType::Dimension;
  static_assert(Dimension == TransformType::InputPointType::Dimension &&
                  Dimension == TransformType::OutputPointType::Dimension,
                "ContravariantVectorTransformation requires that PixelType and input and output images have all the "
                "same dimension");

  PixelType
  Transform(const PixelType & value, const InputPointType & inputPoint, const OutputPointType & outputPoint) override;


protected:
  CovariantVectorTransformation();
  ~CovariantVectorTransformation() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCovariantVectorTransformation.hxx"
#endif

#endif

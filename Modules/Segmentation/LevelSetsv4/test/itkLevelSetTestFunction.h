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
#ifndef itkLevelSetTestFunction_h
#define itkLevelSetTestFunction_h

#include "itkLightObject.h"
#include "itkImage.h"

namespace itk
{

/**
 * \class LevelSetTestFunction
 *
 * \brief A simple function to compare the numerical methods of the level set classes
 * to the analytical values.
 *
 * \f$ f(x,y) = \sqrt{ (x-5)(x-5) + (y-4)(y-4) } - 3 \f$
 */
template <typename TPixel>
class ITK_TEMPLATE_EXPORT LevelSetTestFunction : public LightObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetTestFunction);

  using Self = LevelSetTestFunction;
  using Superclass = LightObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(LevelSetTestFunction, LightObject);

  itkNewMacro(Self);

  static constexpr unsigned int Dimension = 2;

  using PixelType = TPixel;

  using ImageType = Image<PixelType, Dimension>;
  using IndexType = typename ImageType::IndexType;
  using PointType = typename ImageType::PointType;

  using OutputRealType = typename NumericTraits<PixelType>::RealType;
  using GradientType = CovariantVector<OutputRealType, Dimension>;
  using HessianType = Matrix<OutputRealType, Dimension>;

  OutputRealType
  Evaluate(const PointType & point) const;

  GradientType
  EvaluateGradient(const PointType & point) const;

protected:
  LevelSetTestFunction() = default;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetTestFunction.hxx"
#endif

#endif

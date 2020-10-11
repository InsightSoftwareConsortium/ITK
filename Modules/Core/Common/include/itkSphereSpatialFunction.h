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
#ifndef itkSphereSpatialFunction_h
#define itkSphereSpatialFunction_h

#include "itkInteriorExteriorSpatialFunction.h"

namespace itk
{
/** \class SphereSpatialFunction
 * \brief Spatial function implementation of a sphere
 *
 * Implements a function that returns 0 for points inside or on the surface
 * of a sphere, 1 for points outside the sphere
 *
 * \ingroup SpatialFunctions
 * \ingroup ITKCommon
 */
template <unsigned int VImageDimension = 3, typename TInput = Point<double, VImageDimension>>
class ITK_TEMPLATE_EXPORT SphereSpatialFunction : public InteriorExteriorSpatialFunction<VImageDimension, TInput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SphereSpatialFunction);

  /** Standard class type aliases. */
  using Self = SphereSpatialFunction<VImageDimension, TInput>;
  using Superclass = InteriorExteriorSpatialFunction<VImageDimension, TInput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SphereSpatialFunction, InteriorExteriorSpatialFunction);

  /** Input type for the function. */
  using InputType = typename Superclass::InputType;

  /** Output type for the function. */
  using OutputType = typename Superclass::OutputType;

  /** Evaluates the function at a given position */
  OutputType
  Evaluate(const InputType & position) const override;

  /** Get and set the center of the sphere. */
  itkGetConstMacro(Center, InputType);
  itkSetMacro(Center, InputType);

  /** Get and set the radius of the sphere */
  itkGetConstMacro(Radius, double);
  itkSetMacro(Radius, double);

protected:
  SphereSpatialFunction();
  ~SphereSpatialFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** The center of the sphere (of the same type as Input). */
  InputType m_Center;

  /** The radius of the sphere. */
  double m_Radius;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSphereSpatialFunction.hxx"
#endif

#endif

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
#ifndef itkSymmetricEllipsoidInteriorExteriorSpatialFunction_h
#define itkSymmetricEllipsoidInteriorExteriorSpatialFunction_h

#include "itkInteriorExteriorSpatialFunction.h"

namespace itk
{
/** \class SymmetricEllipsoidInteriorExteriorSpatialFunction
 * \brief Function implementation of an ellipsoid
 *
 * Similar to EllipsoidInteriorExteriorSpatialFunction in that it
 * implements a function that returns 1 for points inside or on the surface
 * of a ellipsoid and 0 for points outside the ellipsoid. However, this
 * ellipsoid is defined by a single orientation vector and deals
 * only with symmetric ellipsoids. An n-dimensional symmetric ellipsoid
 * is one which has m axes of equal length and (n - m) unique axes lengths.
 * Specifically, this class deals with the case where (n - m) = 1 and
 * the ellipsoid's major axis is oriented along a singles orientation vector.
 * \ingroup ITKCommon
 */
template <unsigned int VDimension = 3, typename TInput = Point<double, VDimension>>
class ITK_TEMPLATE_EXPORT SymmetricEllipsoidInteriorExteriorSpatialFunction
  : public InteriorExteriorSpatialFunction<VDimension, TInput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SymmetricEllipsoidInteriorExteriorSpatialFunction);

  /** Standard class type aliases. */
  using Self = SymmetricEllipsoidInteriorExteriorSpatialFunction;
  using Superclass = InteriorExteriorSpatialFunction<VDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using VectorType = Vector<double, VDimension>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time information. */
  itkTypeMacro(SymmetricEllipsoidInteriorExteriorSpatialFunction, InteriorExteriorSpatialFunction);

  /** Input type for the function. */
  using InputType = typename Superclass::InputType;

  /** Output type for the function. */
  using OutputType = typename Superclass::OutputType;

  /** Evaluates the function at a given position. */
  OutputType
  Evaluate(const InputType & position) const override;

  /** Get and set the center of the ellipsoid. */
  itkGetConstMacro(Center, InputType);
  itkSetMacro(Center, InputType);

  /** Set the orientation vector of the ellipsoid's unique axis and axes lengths.
   * Must be normalized!!!!! */
  void
  SetOrientation(VectorType orientation, double uniqueAxis, double symmetricAxes);

protected:
  SymmetricEllipsoidInteriorExteriorSpatialFunction();
  ~SymmetricEllipsoidInteriorExteriorSpatialFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** The center of the ellipsoid. */
  InputType m_Center;

  /** The unique axis length of the ellipsoid. */
  double m_UniqueAxis{ 10 };

  /** The symmetric axes lengths of the ellipsoid. */
  double m_SymmetricAxes{ 5 };

  /** The orientation vector of the ellipsoid's unique axis. */
  Vector<double, VDimension> m_Orientation;

  /** The vector ratio. */
  double m_VectorRatio{ 0.0 };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSymmetricEllipsoidInteriorExteriorSpatialFunction.hxx"
#endif

#endif

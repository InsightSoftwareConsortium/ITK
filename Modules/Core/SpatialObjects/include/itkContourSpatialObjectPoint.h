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
#ifndef itkContourSpatialObjectPoint_h
#define itkContourSpatialObjectPoint_h

#include "itkSpatialObjectPoint.h"
#include "itkCovariantVector.h"

namespace itk
{
/**
 *\class ContourSpatialObjectPoint
 * \brief Point used for a Contour definition
 *
 * This class contains all the functions necessary to define a point
 * that can be used to build surfaces.
 * A surface point has a position and only one normal.
 *
 * \sa SpatialObjectPoint
 * \ingroup ITKSpatialObjects
 */
template <unsigned int TPointDimension = 3>
class ITK_TEMPLATE_EXPORT ContourSpatialObjectPoint : public SpatialObjectPoint<TPointDimension>
{
public:
  using Self = ContourSpatialObjectPoint;
  using Superclass = SpatialObjectPoint<TPointDimension>;
  using PointType = Point<double, TPointDimension>;
  using CovariantVectorType = CovariantVector<double, TPointDimension>;

  /** Constructor. This one defines the number of dimensions
   *  in the ContourSpatialObjectPoint */
  ContourSpatialObjectPoint();

  /** Default destructor. */
  ~ContourSpatialObjectPoint() override = default;

  /** Get the picked point. */
  const PointType &
  GetPickedPointInObjectSpace() const;

  /** Set the picked point : N-D case. */
  void
  SetPickedPointInObjectSpace(const PointType & point);

  /** Get the normal. */
  const CovariantVectorType &
  GetNormalInObjectSpace() const;

  /** Set the normal : N-D case. */
  void
  SetNormalInObjectSpace(const CovariantVectorType & normal);

  /** Copy a surface point to another. */
  Self &
  operator=(const ContourSpatialObjectPoint & rhs);

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  CovariantVectorType m_NormalInObjectSpace;
  PointType           m_PickedPointInObjectSpace;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkContourSpatialObjectPoint.hxx"
#endif

#endif // itkContourSpatialObjectPoint_h

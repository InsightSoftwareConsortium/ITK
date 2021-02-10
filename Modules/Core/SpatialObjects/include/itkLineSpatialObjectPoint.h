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
#ifndef itkLineSpatialObjectPoint_h
#define itkLineSpatialObjectPoint_h

#include "itkSpatialObjectPoint.h"
#include "itkCovariantVector.h"
#include "itkFixedArray.h"

namespace itk
{
/**
 *\class LineSpatialObjectPoint
 * \brief Point used for a line definition
 *
 * This class contains all the functions necessary to define a point
 * that can be used to build lines.
 * This Class derives from SpatialObjectPoint.
 * A LineSpatialObjectPoint has NDimension-1 normals.
 * \ingroup ITKSpatialObjects
 *
 * \sphinx
 * \sphinxexample{Core/SpatialObjects/LineSpatialObject,Line Spatial Object}
 * \endsphinx
 */

template <unsigned int TPointDimension = 3>
class ITK_TEMPLATE_EXPORT LineSpatialObjectPoint : public SpatialObjectPoint<TPointDimension>
{
public:
  using Self = LineSpatialObjectPoint;
  using Superclass = SpatialObjectPoint<TPointDimension>;
  using PointType = Point<double, TPointDimension>;
  using CovariantVectorType = CovariantVector<double, TPointDimension>;
  using NormalArrayType = FixedArray<CovariantVectorType, TPointDimension - 1>;

  /** Constructor */
  LineSpatialObjectPoint();

  /** Copy Constructor */
  LineSpatialObjectPoint(const LineSpatialObjectPoint & other);

  /** Destructor */
  ~LineSpatialObjectPoint() override = default;

  /** Get Normal */
  const CovariantVectorType &
  GetNormalInObjectSpace(unsigned int index) const;

  /** Set Normal */
  void
  SetNormalInObjectSpace(CovariantVectorType & normal, unsigned int index);

  /** Copy one LineSpatialObjectPoint to another */
  Self &
  operator=(const LineSpatialObjectPoint & rhs);

protected:
  NormalArrayType m_NormalArrayInObjectSpace;

  /** Method to print the object. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLineSpatialObjectPoint.hxx"
#endif

#endif // itkLineSpatialObjectPoint_h

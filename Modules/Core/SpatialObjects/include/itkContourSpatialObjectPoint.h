/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
/** \class ContourSpatialObjectPoint
 * \brief Point used for a Contour definition
 *
 * This class contains all the functions necessary to define a point
 * that can be used to build surfaces.
 * A surface point has a position and only one normal.
 *
 * \sa SpatialObjectPoint
 * \ingroup ITKSpatialObjects
 */
template< unsigned int TPointDimension = 3 >
class ITK_TEMPLATE_EXPORT ContourSpatialObjectPoint:
  public SpatialObjectPoint< TPointDimension >
{
public:

  typedef ContourSpatialObjectPoint                  Self;
  typedef SpatialObjectPoint< TPointDimension >      Superclass;
  typedef Point< double, TPointDimension >           PointType;
  typedef CovariantVector< double, TPointDimension > VectorType;

  /** Constructor. This one defines the number of dimensions
   *  in the ContourSpatialObjectPoint */
  ContourSpatialObjectPoint();

  /** Default destructor. */
  virtual ~ContourSpatialObjectPoint() ITK_OVERRIDE;

  /** Get the picked point. */
  const PointType & GetPickedPoint() const;

  /** Set the picked point : N-D case. */
  void SetPickedPoint(const PointType & point);

  /** Set the picked point : 2D case. */
  void SetPickedPoint(const double pointx, const double pointy);

  /** Set the picked point : 3D case. */
  void SetPickedPoint(const double pointx,
                      const double pointy, const double pointz);

  /** Get the normal. */
  const VectorType & GetNormal() const;

  /** Set the normal : N-D case. */
  void SetNormal(const VectorType & normal);

  /** Set the normal : 2D case. */
  void SetNormal(const double normalx, const double normaly);

  /** Set the normal : 3D case. */
  void SetNormal(const double normalx,
                 const double normaly, const double normalz);

  /** Copy a surface point to another. */
  Self & operator=(const ContourSpatialObjectPoint & rhs);

protected:

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  VectorType m_Normal;
  PointType  m_PickedPoint;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkContourSpatialObjectPoint.hxx"
#endif

#endif // itkContourSpatialObjectPoint_h

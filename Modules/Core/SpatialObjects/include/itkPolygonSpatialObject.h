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
#ifndef itkPolygonSpatialObject_h
#define itkPolygonSpatialObject_h

#include "itkPointBasedSpatialObject.h"

namespace itk
{
/** \class PolygonSpatialObject
 *
 * \brief TODO
 * \ingroup ITKSpatialObjects
 */
template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT PolygonSpatialObject:
  public PointBasedSpatialObject< TDimension, SpatialObjectPoint< TDimension > >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolygonSpatialObject);

  using Self = PolygonSpatialObject< TDimension >;
  using Superclass = PointBasedSpatialObject< TDimension >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  using PolygonPointType = SpatialObjectPoint< TDimension >;
  using PolygonPointListType = std::vector< PolygonPointType >;

  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using SpatialObjectPointType = typename Superclass::SpatialObjectPointType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(PolygonSpatialObject, PointBasedSpatialObject);

  /** Method returning plane alignment of strand */
  int GetOrientationInObjectSpace() const;

  /** Method sets the thickness of the current strand */
  itkSetMacro(Thickness, double);

  /** Method gets the thickness of the current strand */
  itkGetMacro(Thickness, double);

  /** Returns if the polygon is closed */
  bool IsClosed() const;

  /** Method returns area of polygon described by points */
  double MeasureArea() const;

  /** Method returns the volume of the strand */
  double MeasureVolume() const;

  /** Method returns the length of the perimeter */
  double MeasurePerimeter() const;

  // For ComputeMyBounds - use the implementation in PointBasedSpatialObject

  /** Test whether a point is inside or outside the object. */
  bool IsInside(const PointType & point, unsigned int depth,
    const std::string & name) const override;

protected:
  PolygonSpatialObject();
  ~PolygonSpatialObject() override;

  void PrintSelf(std::ostream & os, Indent indent) const override;

private:
  mutable int               m_OrientationInObjectSpace;
  mutable ModifiedTimeType  m_OrientationInObjectSpaceMTime;
  mutable bool              m_IsClosed;
  mutable ModifiedTimeType  m_IsClosedMTime;
  double                    m_Thickness;
};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolygonSpatialObject.hxx"
#endif

#endif  // itkPolygonSpatialObject_h

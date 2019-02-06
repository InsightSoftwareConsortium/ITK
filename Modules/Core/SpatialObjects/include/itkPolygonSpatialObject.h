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

#include "itkBlobSpatialObject.h"

namespace itk
{
/** \class PolygonSpatialObject
 *
 * \brief TODO
 * \ingroup ITKSpatialObjects
 */
template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT PolygonSpatialObject:
  public BlobSpatialObject< TDimension >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolygonSpatialObject);

  using Self = PolygonSpatialObject< TDimension >;
  using Superclass = BlobSpatialObject< TDimension >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using PointListType = typename Superclass::PointListType;
  using BlobPointType = typename Superclass::BlobPointType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(PolygonSpatialObject, BlobSpatialObject);

  /** Method returning plane alignment of strand */
  int Plane() const;

  /** Method sets the thickness of the current strand */
  itkSetMacro(Thickness, double);

  /** Method gets the thickness of the current strand */
  itkGetMacro(Thickness, double);

  /** Returns if the polygon is closed */
  bool IsClosed() const;

  /** Returns the number of points of the polygon */
  unsigned int NumberOfPoints() const;

  /** Method returns area of polygon described by points */
  double MeasureArea() const;

  /** Method returns the volume of the strand */
  double MeasureVolume() const;

  /** Method returns the length of the perimeter */
  double MeasurePerimeter() const;

  // For ComputeObjectBounds - use the implementation in BlobSpatialObject

  /** Test whether a point is inside or outside the object. */
  bool IsInside(const PointType & point, unsigned int depth,
    const std::string & name) const override;

protected:
  PolygonSpatialObject();
  ~PolygonSpatialObject() override;

  void PrintSelf(std::ostream & os, Indent indent) const override;

private:
  int                     m_Orientation;
  ModifiedTimeType        m_OrientationMTime;
  bool                    m_IsClosed;
  ModifiedTimeType        m_IsClosedMTime;
  double                  m_Thickness;
};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolygonSpatialObject.hxx"
#endif

#endif  // itkPolygonSpatialObject_h

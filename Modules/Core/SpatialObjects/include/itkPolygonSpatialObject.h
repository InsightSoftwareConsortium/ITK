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
  typedef PolygonSpatialObject< TDimension > Self;
  typedef BlobSpatialObject< TDimension >    Superclass;
  typedef SmartPointer< Self >               Pointer;
  typedef SmartPointer< const Self >         ConstPointer;
  typedef typename Superclass::PointType     PointType;
  typedef typename Superclass::TransformType TransformType;
  typedef typename Superclass::PointListType PointListType;
  typedef typename Superclass::BlobPointType BlobPointType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(PolygonSpatialObject, BlobSpatialObject);

  /**
   * \enum PolygonGroupOrientation
   * \brief enumerates the possible spatial orientations
   */
  typedef enum {
        Axial = 0,
        Coronal = 1,
        Sagittal = 2,
        UserPlane = 3,
        Unknown = 4
        } PolygonGroupOrientation;

  /** Method returning plane alignment of strand */
  PolygonGroupOrientation Plane() const;

  /** Method sets the thickness of the current strand */
  itkSetMacro(Thickness, double);

  /** Method gets the thickness of the current strand */
  itkGetMacro(Thickness, double);

  /** Returns if the polygon is closed */
  bool IsClosed() const;

  /** Returns the number of points of the polygon */
  unsigned int NumberOfPoints() const;

  /** Method returns the Point closest to the given point */
  PointType ClosestPoint(const PointType & curPoint) const;

  /** Method returns area of polygon described by points */
  double MeasureArea() const;

  /** Method returns the volume of the strand */
  double MeasureVolume() const;

  /** Method returns the length of the perimeter */
  double MeasurePerimeter() const;

  /** Method deletes a point from the strand */
  bool DeletePoint(const PointType & pointToDelete);

  /** Method adds a point to the end of the strand */
  bool AddPoint(const PointType & pointToAdd);

  /** Method inserts point after point1 */
  bool InsertPoint(const PointType & point1, const PointType & pointToAdd);

  /** Method replaces a point */
  bool ReplacePoint(const PointType & oldpoint, const PointType & newPoint);

  /** Method removes the series of points between startpoint and endpoint */
  bool RemoveSegment(const PointType & startpoint, const PointType & endPoint);

  /** Test whether a point is inside or outside the object. */
  virtual bool IsInside(const PointType & point,
                        unsigned int depth,
                        char *name) const ITK_OVERRIDE;

  /** Test whether a point is inside or outside the object For
   * computational speed purposes, it is faster if the method does not
   * check the name of the class and the current depth. */
  virtual bool IsInside(const PointType & point) const;

protected:
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolygonSpatialObject);

  PolygonGroupOrientation m_Orientation;
  double                  m_Thickness;
  PolygonSpatialObject()
  {
    m_Orientation = Unknown;
    m_Thickness = 0.0;
  }
};
}
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolygonSpatialObject.hxx"
#endif

#endif  // itkPolygonSpatialObject_h

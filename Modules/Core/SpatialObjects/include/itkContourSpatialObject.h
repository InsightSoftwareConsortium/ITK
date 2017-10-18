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
#ifndef itkContourSpatialObject_h
#define itkContourSpatialObject_h

#include <list>

#include "itkPointBasedSpatialObject.h"
#include "itkContourSpatialObjectPoint.h"

namespace itk
{
/**
 * \class ContourSpatialObject
 * \brief Representation of a Contour based on the spatial object classes.
 *
 * The Contour is basically defined by a set of points which are inside this blob
 *
 * \sa SpatialObjectPoint
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT ContourSpatialObject:
  public PointBasedSpatialObject<  TDimension >
{
public:

  typedef ContourSpatialObject                    Self;
  typedef PointBasedSpatialObject< TDimension >   Superclass;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;
  typedef double                                  ScalarType;
  typedef ContourSpatialObjectPoint< TDimension > ControlPointType;
  typedef SpatialObjectPoint< TDimension >        InterpolatedPointType;
  typedef std::vector< ControlPointType >         ControlPointListType;
  typedef std::vector< InterpolatedPointType >    InterpolatedPointListType;

  typedef typename Superclass::SpatialObjectPointType  SpatialObjectPointType;
  typedef typename Superclass::PointType               PointType;
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::BoundingBoxType         BoundingBoxType;
  typedef VectorContainer< IdentifierType, PointType > PointContainerType;
  typedef SmartPointer< PointContainerType >           PointContainerPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(ContourSpatialObject, PointBasedSpatialObject);

  /** Returns a reference to the list of the control points. */
  ControlPointListType & GetControlPoints();

  /** Returns a reference to the list of the control points. */
  const ControlPointListType & GetControlPoints() const;

  /** Set the list of control points. */
  void SetControlPoints(ControlPointListType & newPoints);

  /** Return a control point in the list given the index */
  const ControlPointType * GetControlPoint(IdentifierType id) const
  { return &( m_ControlPoints[id] ); }

  /** Return a control point in the list given the index */
  ControlPointType * GetControlPoint(IdentifierType id)
  { return &( m_ControlPoints[id] ); }

  /** Return the number of control points in the list */
  SizeValueType GetNumberOfControlPoints(void) const
  { return static_cast<SizeValueType>( m_ControlPoints.size() ); }

  /** Returns a reference to the list of the interpolated points. */
  InterpolatedPointListType & GetInterpolatedPoints();

  /** Returns a reference to the list of the interpolated points. */
  const InterpolatedPointListType & GetInterpolatedPoints() const;

  /** Set the list of interpolated points. */
  void SetInterpolatedPoints(InterpolatedPointListType & newPoints);

  /** Return a interpolated point in the list given the index */
  const InterpolatedPointType * GetInterpolatedPoint(IdentifierType id) const
  { return &( m_InterpolatedPoints[id] ); }

  /** Return a interpolated point in the list given the index */
  InterpolatedPointType * GetInterpolatedPoint(IdentifierType id)
  { return &( m_InterpolatedPoints[id] ); }

  /** Return the number of interpolated points in the list */
  SizeValueType GetNumberOfInterpolatedPoints(void) const
  { return static_cast<SizeValueType>( m_InterpolatedPoints.size() ); }

  enum InterpolationType { NO_INTERPOLATION = 0,
                           EXPLICIT_INTERPOLATION, BEZIER_INTERPOLATION,
                           LINEAR_INTERPOLATION };

  /** Set/Get the interpolation type */
  InterpolationType GetInterpolationType() const
  { return m_InterpolationType; }
  void SetInterpolationType(InterpolationType interpolation)
  { m_InterpolationType = interpolation; }

  /** Set/Get if the contour is closed */
  itkSetMacro(Closed, bool);
  itkGetConstMacro(Closed, bool);

  /** Set/Get the display orientation of the contour */
  itkSetMacro(DisplayOrientation, int);
  itkGetConstMacro(DisplayOrientation, int);

  /** Set/Get the slice attached to the contour if any
   *  -1 is returned if no contour attached. */
  itkSetMacro(AttachedToSlice, int);
  itkGetConstMacro(AttachedToSlice, int);

  /** Returns true if the Contour is evaluable at the requested point,
   *  false otherwise.
   *
   * Note: For this class, this will always return false. -GH
   */
  bool IsEvaluableAt(const PointType & point,
                     unsigned int depth = 0, char *name = ITK_NULLPTR) const ITK_OVERRIDE;

  /** Returns the value of the Contour at that point.
   *  Currently this function returns a binary value,
   *  but it might want to return a degree of membership
   *  in case of fuzzy Contours. */
  bool ValueAt(const PointType & point, double & value,
               unsigned int depth = 0, char *name = ITK_NULLPTR) const ITK_OVERRIDE;

  /** Returns true if the point is inside the Contour, false otherwise. */
  bool IsInside(const PointType & point,
                unsigned int depth, char *name) const ITK_OVERRIDE;

  /** Test whether a point is inside or outside the object
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */
  virtual bool IsInside(const PointType & point) const;

  /** Compute the boundaries of the Contour. */
  bool ComputeLocalBoundingBox(void) const ITK_OVERRIDE;

protected:
  ITK_DISALLOW_COPY_AND_ASSIGN(ContourSpatialObject);

  ControlPointListType      m_ControlPoints;
  InterpolatedPointListType m_InterpolatedPoints;
  InterpolationType         m_InterpolationType;
  bool                      m_Closed;
  int                       m_DisplayOrientation;
  int                       m_AttachedToSlice;

  ContourSpatialObject();
  virtual ~ContourSpatialObject() ITK_OVERRIDE;

  /** Method to print the object. */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkContourSpatialObject.hxx"
#endif

#endif // itkContourSpatialObject_h

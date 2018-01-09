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
#ifndef itkLineSpatialObject_h
#define itkLineSpatialObject_h

#include <list>

#include "itkPointBasedSpatialObject.h"
#include "itkLineSpatialObjectPoint.h"

namespace itk
{
/**
 * \class LineSpatialObject
 * \brief Representation of a Line based on the spatial object classes.
 *
 * The Line is basically defined by a set of points.
 *
 * \sa LineSpatialObjectPoint
 * \ingroup ITKSpatialObjects
 *
 * \wiki
 * \wikiexample{SpatialObjects/LineSpatialObject,Line spatial object}
 * \endwiki
 */

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT LineSpatialObject:
  public PointBasedSpatialObject<  TDimension >
{
public:

  typedef LineSpatialObject                            Self;
  typedef PointBasedSpatialObject< TDimension >        Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;
  typedef double                                       ScalarType;
  typedef LineSpatialObjectPoint< TDimension >         LinePointType;
  typedef std::vector< LinePointType >                 PointListType;
  typedef const PointListType                          ConstPointListType;
  typedef typename Superclass::SpatialObjectPointType  SpatialObjectPointType;
  typedef typename Superclass::PointType               PointType;
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::BoundingBoxType         BoundingBoxType;
  typedef VectorContainer< IdentifierType, PointType > PointContainerType;
  typedef SmartPointer< PointContainerType >           PointContainerPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(LineSpatialObject, PointBasedSpatialObject);

  /** Returns a reference to the list of the Line points. */
  PointListType & GetPoints(void) { return m_Points; }
  ConstPointListType & GetPoints(void) const { return m_Points; }

  /** Set the list of line points. */
  void SetPoints(PointListType & newPoints);

  /** Return a point in the list given the index */
  const SpatialObjectPointType * GetPoint(IdentifierType id) const override
  {
    return &( m_Points[id] );
  }

  /** Return a point in the list given the index */
  SpatialObjectPointType * GetPoint(IdentifierType id) override { return &( m_Points[id] ); }

  /** Return the number of points in the list */
  SizeValueType GetNumberOfPoints(void) const override
  {
    return static_cast<SizeValueType>( m_Points.size() );
  }

  /** Returns true if the line is evaluable at the requested point,
   *  false otherwise. */
  bool IsEvaluableAt(const PointType & point,
                     unsigned int depth = 0, char *name = nullptr) const override;

  /** Returns the value of the line at that point.
   * Currently this function returns a binary value,
   * but it might want to return a degree of membership
   * in case of fuzzy Lines. */
  bool ValueAt(const PointType & point, double & value,
               unsigned int depth = 0, char *name = nullptr) const override;

  /** Returns true if the point is inside the line, false otherwise. */
  bool IsInside(const PointType & point,
                unsigned int depth, char *name) const override;

  /** Test whether a point is inside or outside the object
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */
  virtual bool IsInside(const PointType & point) const;

  /** Compute the boundaries of the line. */
  bool ComputeLocalBoundingBox() const override;

protected:
  ITK_DISALLOW_COPY_AND_ASSIGN(LineSpatialObject);

  PointListType m_Points;

  LineSpatialObject();
  ~LineSpatialObject() override;

  /** Method to print the object. */
  void PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLineSpatialObject.hxx"
#endif

#endif // itkLineSpatialObject_h

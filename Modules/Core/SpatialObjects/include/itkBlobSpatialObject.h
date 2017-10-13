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
#ifndef itkBlobSpatialObject_h
#define itkBlobSpatialObject_h

#include <list>

#include "itkPointBasedSpatialObject.h"

namespace itk
{
/**
 * \class BlobSpatialObject
 * \brief Spatial object representing a potentially amorphous object.
 *
 * The BlobSpatialObject is a discretized representation of a "blob",
 * which can be taken to be an arbitrary, possibly amorphous shape.
 * The representation is a list of the points (voxel centers) contained
 * in the object.  This can be thought of as an alternate way to
 * represent a binary image.
 *
 * \sa SpatialObjectPoint
 * \ingroup ITKSpatialObjects
 *
 * \wiki
 * \wikiexample{SpatialObjects/BlobSpatialObject,Blob}
 * \endwiki
 */

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT BlobSpatialObject:
  public PointBasedSpatialObject<  TDimension >
{
public:

  typedef BlobSpatialObject                            Self;
  typedef PointBasedSpatialObject< TDimension >        Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;
  typedef double                                       ScalarType;
  typedef SpatialObjectPoint< TDimension >             BlobPointType;
  typedef std::vector< BlobPointType >                 PointListType;
  typedef typename Superclass::PointType               PointType;
  typedef typename Superclass::SpatialObjectPointType  SpatialObjectPointType;
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::BoundingBoxType         BoundingBoxType;
  typedef VectorContainer< IdentifierType, PointType > PointContainerType;
  typedef SmartPointer< PointContainerType >           PointContainerPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(BlobSpatialObject, SpatialObject);

  /** Returns a reference to the list of the Blob points. */
  PointListType & GetPoints();

  /** Returns a reference to the list of the Blob points. */
  const PointListType & GetPoints() const;

  /** Set the list of Blob points. */
  void SetPoints(PointListType & newPoints);

  /** Return a point in the list given the index */
  const SpatialObjectPointType * GetPoint(IdentifierType id) const ITK_OVERRIDE
  {
    return &( m_Points[id] );
  }

  /** Return a point in the list given the index */
  SpatialObjectPointType * GetPoint(IdentifierType id) ITK_OVERRIDE { return &( m_Points[id] ); }

  /** Return the number of points in the list */
  SizeValueType GetNumberOfPoints(void) const ITK_OVERRIDE
  {
    return static_cast<SizeValueType>( m_Points.size() );
  }

  /** Returns true if the Blob is evaluable at the requested point,
   *  false otherwise. */
  bool IsEvaluableAt(const PointType & point,
                     unsigned int depth = 0, char *name = ITK_NULLPTR) const ITK_OVERRIDE;

  /** Returns the value of the Blob at that point.
   *  Currently this function returns a binary value,
   *  but it might want to return a degree of membership
   *  in case of fuzzy Blobs. */
  bool ValueAt(const PointType & point, double & value,
               unsigned int depth = 0, char *name = ITK_NULLPTR) const ITK_OVERRIDE;

  /** Returns true if the point is inside the Blob, false otherwise. */
  bool IsInside(const PointType & point,
                unsigned int depth, char *name) const ITK_OVERRIDE;

  /** Test whether a point is inside or outside the object
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */
  bool IsInside(const PointType & point) const;

  /** Compute the boundaries of the Blob. */
  bool ComputeLocalBoundingBox() const ITK_OVERRIDE;

protected:
  ITK_DISALLOW_COPY_AND_ASSIGN(BlobSpatialObject);

  PointListType m_Points;

  BlobSpatialObject();
  virtual ~BlobSpatialObject() ITK_OVERRIDE;

  /** Method to print the object. */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBlobSpatialObject.hxx"
#endif

#endif // itkBlobSpatialObject_h

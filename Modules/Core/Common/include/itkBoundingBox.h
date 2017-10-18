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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkBoundingBox_h
#define itkBoundingBox_h

#include "itkPoint.h"
#include "itkVectorContainer.h"
#include "itkIntTypes.h"

namespace itk
{
/** \class BoundingBox
 * \brief Represent and compute information about bounding boxes.
 *
 * BoundingBox is a supporting class that represents, computes, and
 * caches information about bounding boxes. The bounding box can
 * be computed from several sources, including manual specification
 * and computation from an input points container.
 *
 * This is a templated, n-dimensional version of the bounding box.
 * Bounding boxes are represented by n pairs of (min,max) pairs,
 * where min is the minimum coordinate value and max is the
 * maximum coordinate value for coordinate axis i.
 *
 * Template parameters for BoundingBox:
 *
 * TPointIdentifier =
 *     The type used to access a particular point (i.e., a point's id)
 *
 * TCoordRep =
 *     Numerical type with which to represent each coordinate value.
 *
 * VPointDimension =
 *    Geometric dimension of space.
 *
 * \ingroup DataRepresentation
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Utilities/BoundingBox,Bounding box}
 * \endwiki
 */

template<
  typename TPointIdentifier = IdentifierType,
  int VPointDimension = 3,
  typename TCoordRep = float,
  typename TPointsContainer =
    VectorContainer< TPointIdentifier, Point< TCoordRep, VPointDimension > >
  >
class ITK_TEMPLATE_EXPORT BoundingBox:public Object
{
public:
  /** Standard class typedefs. */
  typedef BoundingBox                Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(BoundingBox, Object);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Hold on to the type information specified by the template parameters. */
  typedef TPointIdentifier                       PointIdentifier;
  typedef TCoordRep                              CoordRepType;
  typedef TPointsContainer                       PointsContainer;
  typedef typename PointsContainer::Pointer      PointsContainerPointer;
  typedef typename PointsContainer::ConstPointer PointsContainerConstPointer;

  typedef Point< CoordRepType, VPointDimension >         PointType;
  typedef FixedArray< CoordRepType, VPointDimension *2 > BoundsArrayType;

  /** Hold on to the dimensions specified by the template parameters. */
  itkStaticConstMacro(PointDimension, unsigned int,  VPointDimension);

  /** Convenient typedefs. */
  typedef typename PointsContainer::ConstIterator PointsContainerConstIterator;
  typedef typename PointsContainer::Iterator      PointsContainerIterator;

  /** Set/Get the points from which the bounding box should be computed. The
   * bounding box is cached and is not recomputed if the points are not
   * changed. */
  void SetPoints(const PointsContainer *);

  const PointsContainer * GetPoints() const;

  /** Compute and return the corners of the bounding box */
  const PointsContainer * GetCorners();

  /** Method that actually computes bounding box. */
  bool ComputeBoundingBox() const;

  /** Get the bounding box.  This method should only be invoked after
 * ComputeBoundingBox(), otherwise the Bounds values will not be up to date.
 * Note that many methods in this class invoke ComputeBoundingBox() internally,
 * for example GetMinimum(), GetMaximum(), GetCenter(), GetDiagonalLength2().
 * Therefore it is safe to invoke GetBounds() after any of those methods. */
  itkGetConstReferenceMacro(Bounds, BoundsArrayType);

  /** Get the center of the bounding box. Returns ITK_NULLPTR if bounding box
   * cannot be computed. */
  PointType GetCenter() const;

  /** Get the minimum point of the bounding box. Returns ITK_NULLPTR if bounding box
   * cannot be computed. */
  PointType GetMinimum() const;

  /** Set the minimum point of the bounding box. May not be valid for the given
   * set of points.   Will be preserved until this filter's (i.e., the point
   * set's) modified time changes. */
  void      SetMinimum(const PointType &);

  /** Get the maximum point of the bounding box. Returns ITK_NULLPTR if bounding box
   * cannot be computed. */
  PointType GetMaximum() const;

  /** Set the maximum point of the bounding box. May not be valid for the given
   * set of points.   Will be preserved until this filter's (i.e., the point
   * set's) modified time changes. */
  void      SetMaximum(const PointType &);

  /** Adjust bounds (if necessary) as if the given point was in the set
   * of points being considered.   Does not add the given point to the set.
   * Therefore, this point not considered in future computeboundingbox/gets
   * once the point set is changed. */
  void ConsiderPoint(const PointType &);

  /** Get the length squared of the diagonal of the bounding box.
   * Returns zero if bounding box cannot be computed. Note that the
   * Accumulate type is used to represent the length. */
  typedef typename NumericTraits< CoordRepType >::AccumulateType AccumulateType;
  AccumulateType GetDiagonalLength2() const;

  /** Method that checks if a point is inside the bounding box. */
  bool IsInside(const PointType &) const;

  /** Method Compute the Modified Time based on changed to the components. */
  virtual ModifiedTimeType GetMTime(void) const ITK_OVERRIDE;

  /** Duplicates this bounding box */
  Pointer DeepCopy() const;

protected:
  BoundingBox();
  virtual ~BoundingBox() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  typedef typename PointsContainer::ConstIterator ConstIterator;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BoundingBox);

  PointsContainerConstPointer m_PointsContainer;
  PointsContainerPointer      m_CornersContainer;
  mutable BoundsArrayType     m_Bounds;
  mutable TimeStamp           m_BoundsMTime; // The last time the bounds
                                             // were computed.
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBoundingBox.hxx"
#endif

#endif

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

#include <array>

namespace itk
{

/**
 *\class BoundingBox
 *
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
 * \tparam TPointIdentifier The type used to access a particular point (i.e., a point's id)
 * \tparam TCoordRep Numerical type with which to represent each coordinate value.
 * \tparam VPointDimension Geometric dimension of space.
 *
 * \ingroup DataRepresentation
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 *
 */
template <typename TPointIdentifier = IdentifierType,
          unsigned int VPointDimension = 3,
          typename TCoordRep = float,
          typename TPointsContainer = VectorContainer<TPointIdentifier, Point<TCoordRep, VPointDimension>>>
class ITK_TEMPLATE_EXPORT BoundingBox : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BoundingBox);

  /** Standard class type aliases. */
  using Self = BoundingBox;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(BoundingBox, Object);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /* Number of corners of this bounding box. Equals `pow(2, VPointDimension)` */
  static constexpr SizeValueType NumberOfCorners = SizeValueType{ 1 } << VPointDimension;

  /** Hold on to the type information specified by the template parameters. */
  using PointIdentifier = TPointIdentifier;
  using CoordRepType = TCoordRep;
  using PointsContainer = TPointsContainer;
  using PointsContainerPointer = typename PointsContainer::Pointer;
  using PointsContainerConstPointer = typename PointsContainer::ConstPointer;

  using PointType = Point<CoordRepType, VPointDimension>;
  using BoundsArrayType = FixedArray<CoordRepType, VPointDimension * 2>;

  /** Hold on to the dimensions specified by the template parameters. */
  static constexpr unsigned int PointDimension = VPointDimension;

  /** Convenient type alias. */
  using PointsContainerConstIterator = typename PointsContainer::ConstIterator;
  using PointsContainerIterator = typename PointsContainer::Iterator;

  /** Set/Get the points from which the bounding box should be computed. The
   * bounding box is cached and is not recomputed if the points are not
   * changed. */
  void
  SetPoints(const PointsContainer *);

  const PointsContainer *
  GetPoints() const;

  /** Compute and return the corners of the bounding box.
   *\note This function returns the same points as the legacy member function
   * `GetCorners()`, but it is `const`, and it avoids dynamic memory allocation
   * by using `std::array`.
   */
  std::array<PointType, NumberOfCorners>
  ComputeCorners() const;

  /** Compute and return the corners of the bounding box */
  itkLegacyMacro(const PointsContainer * GetCorners());

  /** Method that actually computes bounding box. */
  bool
  ComputeBoundingBox() const;

  /** Get the bounding box.  This method should only be invoked after
   * ComputeBoundingBox(), otherwise the Bounds values will not be up to date.
   * Note that many methods in this class invoke ComputeBoundingBox() internally,
   * for example GetMinimum(), GetMaximum(), GetCenter(), GetDiagonalLength2().
   * Therefore it is safe to invoke GetBounds() after any of those methods. */
  itkGetConstReferenceMacro(Bounds, BoundsArrayType);

  /** Get the center of the bounding box. Returns a point at the origin
   *  when the bounding box object is just default-initialized. */
  PointType
  GetCenter() const;

  /** Get the minimum point of the bounding box. Returns a point at the origin
   *  when the bounding box object is just default-initialized. */
  PointType
  GetMinimum() const;

  /** Set the minimum point of the bounding box. May not be valid for the given
   * set of points.   Will be preserved until this filter's (i.e., the point
   * set's) modified time changes. */
  void
  SetMinimum(const PointType &);

  /** Get the maximum point of the bounding box. Returns a point at the origin
   *  when the bounding box object is just default-initialized. */
  PointType
  GetMaximum() const;

  /** Set the maximum point of the bounding box. May not be valid for the given
   * set of points.   Will be preserved until this filter's (i.e., the point
   * set's) modified time changes. */
  void
  SetMaximum(const PointType &);

  /** Adjust bounds (if necessary) as if the given point was in the set
   * of points being considered.   Does not add the given point to the set.
   * Therefore, this point not considered in future computeboundingbox/gets
   * once the point set is changed. */
  void
  ConsiderPoint(const PointType &);

  /** Get the length squared of the diagonal of the bounding box.
   * Returns zero if bounding box cannot be computed. Note that the
   * Accumulate type is used to represent the length. */
  using AccumulateType = typename NumericTraits<CoordRepType>::AccumulateType;
  AccumulateType
  GetDiagonalLength2() const;

  /** Method that checks if a point is inside the bounding box. */
  bool
  IsInside(const PointType &) const;

  /** Method Compute the Modified Time based on changed to the components. */
  ModifiedTimeType
  GetMTime() const override;

  /** Duplicates this bounding box */
  Pointer
  DeepCopy() const;

protected:
  BoundingBox();
  ~BoundingBox() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  using ConstIterator = typename PointsContainer::ConstIterator;

private:
  PointsContainerConstPointer m_PointsContainer;
#if !defined(ITK_LEGACY_REMOVE)
  PointsContainerPointer m_CornersContainer{ PointsContainer::New() };
#endif
  mutable BoundsArrayType m_Bounds;
  mutable TimeStamp       m_BoundsMTime; // The last time the bounds
                                         // were computed.
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBoundingBox.hxx"
#endif

#endif

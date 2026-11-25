/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#ifndef itkPointSetBase_h
#define itkPointSetBase_h

#include "itkDataObject.h"
#include "itkVectorContainer.h"
#include <vector>

namespace itk
{

/** \class PointSetBase
 * \brief A superclass of PointSet
 * supports point (geometric coordinate and attribute) definition.
 *
 * PointSetBase is a superclass of PointSet.
 * It provides the portion of the point set definition for geometric coordinates
 * (and region information). The defined API provides operations on points but
 * does not tie down the underlying implementation and storage.
 *
 * \tparam TPointsContainer Type of the container of points. Typically either an `itk::VectorContainer<PointType>` or an
 * `itk::MapContainer<PointType>`.).
 *
 * \ingroup MeshObjects
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */

template <typename TPointsContainer>
class ITK_TEMPLATE_EXPORT PointSetBase : public DataObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PointSetBase);

  /** Standard class type aliases. */
  using Self = PointSetBase;
  using Superclass = DataObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(PointSetBase);

  itkCloneMacro(Self);

  /** Convenient type alias obtained from TPointsContainer template parameter. */
  using PointType = typename TPointsContainer::Element;
  using CoordinateType = typename PointType::CoordinateType;
#ifndef ITK_FUTURE_LEGACY_REMOVE
  using CoordRepType ITK_FUTURE_DEPRECATED(
    "ITK 6 discourages using `CoordRepType`. Please use `CoordinateType` instead!") = CoordinateType;
#endif
  using PointIdentifier = typename TPointsContainer::ElementIdentifier;
  using PointsContainer = TPointsContainer;

  /** For improving Python support for PointSetBase and Meshes **/
  using PointsVectorContainer = typename itk::VectorContainer<PointIdentifier, CoordinateType>;
  using PointsVectorContainerPointer = typename PointsVectorContainer::Pointer;

  /** Convenient constant, indirectly obtained from TPointsContainer template parameter. */
  static constexpr unsigned int PointDimension = PointType::PointDimension;

  /** Create types that are pointers to each of the container types. */
  using PointsContainerPointer = typename PointsContainer::Pointer;
  using PointsContainerConstPointer = typename PointsContainer::ConstPointer;

  /** Create types that are iterators for each of the container types. */
  using PointsContainerConstIterator = typename PointsContainer::ConstIterator;
  using PointsContainerIterator = typename PointsContainer::Iterator;

  /** Type used to define Regions */
  using RegionType = long;

  /** Get the maximum number of regions that this data can be
   * separated into. */
  itkGetConstMacro(MaximumNumberOfRegions, RegionType);

protected:
  /** An object containing points used by the mesh.  Individual points are
   * accessed through point identifiers. */
  PointsContainerPointer m_PointsContainer{};

public:
  /** Copy the geometric and topological structure of the given input pointSet.
   * The copying is done via reference counting.
   */
  void
  PassStructure(Self * inputPointSet);

  /** Restore the PointSetBase to its initial state. Useful for data pipeline updates
   * without memory re-allocation.
   */
  void
  Initialize() override;

  /** Point dimension. The dimension of a point is fixed at compile-time. */
  static constexpr unsigned int
  GetPointDimension()
  {
    return PointDimension;
  }

  /** Get the number of points in the points container. */
  PointIdentifier
  GetNumberOfPoints() const;

  /** Set the points container. */
  void
  SetPoints(PointsContainer *);

  /** Set the points container using a 1D vector.
  \warning This member function is unsafe. It may just work, but it may also lead to undefined behavior. */
  void
  SetPoints(PointsVectorContainer *);

  /** Sets the points by specifying its coordinates. */
  void
  SetPointsByCoordinates(const std::vector<CoordinateType> & coordinates);

  /** Get the points container. */
  PointsContainer *
  GetPoints();

  /** Get the points container. */
  const PointsContainer *
  GetPoints() const;

  /** Assign a point to a point identifier.  If a spot for the point identifier
   * does not exist, it will be created automatically.
   */
  void SetPoint(PointIdentifier, PointType);

  /** Check if a point exists for a given point identifier.  If a spot for
   * the point identifier exists, the point is set, and true is returned.
   * Otherwise, false is returned, and the point is not modified.
   * If the point is nullptr, then it is never set, but the existence of the
   * point is still returned.
   */
  bool
  GetPoint(PointIdentifier, PointType *) const;

  /** Get the point for the given point identifier. */
  PointType GetPoint(PointIdentifier) const;

  /** Methods to manage streaming. */
  void
  UpdateOutputInformation() override;

  void
  SetRequestedRegionToLargestPossibleRegion() override;

  void
  CopyInformation(const DataObject * data) override;

  bool
  RequestedRegionIsOutsideOfTheBufferedRegion() override;

  bool
  VerifyRequestedRegion() override;

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method
   * implements the API from DataObject. The data object parameter must be
   * castable to a PointSetBase. */
  void
  SetRequestedRegion(const DataObject * data) override;

  /** Set/Get the Requested region */
  virtual void
  SetRequestedRegion(const RegionType & region);

  itkGetConstMacro(RequestedRegion, RegionType);

  /** Set/Get the Buffered region */
  virtual void
  SetBufferedRegion(const RegionType & region);

  itkGetConstMacro(BufferedRegion, RegionType);

protected:
  /** Default-constructor, to be used by derived classes. */
  PointSetBase() = default;

  /** Pure virtual destructor, to be used by derived classes. */
  ~PointSetBase() override = 0;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  LightObject::Pointer
  InternalClone() const override;

  // If the RegionType is ITK_UNSTRUCTURED_REGION, then the following
  // variables represent the maximum number of region that the data
  // object can be broken into, which region out of how many is
  // currently in the buffered region, and the number of regions and
  // the specific region requested for the update. Data objects that
  // do not support any division of the data can simply leave the
  // MaximumNumberOfRegions as 1. The RequestedNumberOfRegions and
  // RequestedRegion are used to define the currently requested
  // region. The LargestPossibleRegion is always requested region = 0
  // and number of regions = 1;
  RegionType m_MaximumNumberOfRegions{ 1 };
  RegionType m_NumberOfRegions{ 1 };
  RegionType m_RequestedNumberOfRegions{};
  RegionType m_BufferedRegion{ -1 };
  RegionType m_RequestedRegion{ -1 };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPointSetBase.hxx"
#endif

#endif

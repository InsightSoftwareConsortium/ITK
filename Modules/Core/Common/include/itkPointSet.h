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
#ifndef itkPointSet_h
#define itkPointSet_h

#include "itkDataObject.h"
#include "itkDefaultStaticMeshTraits.h"
#include <vector>
#include <set>

namespace itk
{

/** \class PointSet
 * \brief A superclass of the N-dimensional mesh structure;
 * supports point (geometric coordinate and attribute) definition.
 *
 * PointSet is a superclass of the N-dimensional mesh structure (itk::Mesh).
 * It provides the portion of the mesh definition for geometric coordinates
 * (and associated attribute or pixel information). The defined API provides
 * operations on points but does not tie down the underlying implementation
 * and storage.  A "MeshTraits" structure is used to define the container
 * and identifier to access the points.  See DefaultStaticMeshTraits
 * for the set of type definitions needed.  All types that are defined
 * in the "MeshTraits" structure will have duplicate type alias in the resulting
 * mesh itself.
 *
 * PointSet has two template parameters.  The first is the pixel type, or the
 * type of data stored (optionally) with the points.
 * The second is the "MeshTraits" structure controlling type information
 * characterizing the point set.  Most users will be happy with the
 * defaults, and will not have to worry about this second argument.
 *
 * Template parameters for PointSet:
 *
 * TPixelType =
 *     The type stored as data for the point.
 *
 * TMeshTraits =
 *     Type information structure for the point set.
 *
 * \ingroup MeshObjects
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/CreateAPointSet,Create a PointSet}
 * \sphinxexample{Core/Common/ReadAPointSet,Read a PointSet}
 * \sphinxexample{Core/Common/WriteAPointSet,Write a PointSet}
 * \sphinxexample{Core/Common/BoundingBoxOfAPointSet,Bounding Box Of A Point Set}
 * \endsphinx
 */

template <typename TPixelType,
          unsigned int VDimension = 3,
          typename TMeshTraits = DefaultStaticMeshTraits<TPixelType, VDimension, VDimension>>
class ITK_TEMPLATE_EXPORT PointSet : public DataObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PointSet);

  /** Standard class type aliases. */
  using Self = PointSet;
  using Superclass = DataObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard part of every itk Object. */
  itkTypeMacro(PointSet, Object);

  /** Hold on to the type information specified by the template parameters. */
  using MeshTraits = TMeshTraits;
  using PixelType = typename MeshTraits::PixelType;

  /** Convenient type alias obtained from TMeshTraits template parameter. */
  using CoordRepType = typename MeshTraits::CoordRepType;
  using PointIdentifier = typename MeshTraits::PointIdentifier;
  using PointType = typename MeshTraits::PointType;
  using PointsContainer = typename MeshTraits::PointsContainer;
  using PointDataContainer = typename MeshTraits::PointDataContainer;

  /** Convenient type alias obtained from TMeshTraits template parameter. */
  static constexpr unsigned int PointDimension = TMeshTraits::PointDimension;

  /** Create types that are pointers to each of the container types. */
  using PointsContainerPointer = typename PointsContainer::Pointer;
  using PointsContainerConstPointer = typename PointsContainer::ConstPointer;
  using PointDataContainerPointer = typename PointDataContainer::Pointer;
  using PointDataContainerConstPointer = typename PointDataContainer::ConstPointer;

  /** Create types that are iterators for each of the container types. */
  using PointsContainerConstIterator = typename PointsContainer::ConstIterator;
  using PointsContainerIterator = typename PointsContainer::Iterator;
  using PointDataContainerIterator = typename PointDataContainer::ConstIterator;

  /** Type used to define Regions */
  using RegionType = long;

  /** Get the maximum number of regions that this data can be
   * separated into. */
  itkGetConstMacro(MaximumNumberOfRegions, RegionType);

protected:
  /** An object containing points used by the mesh.  Individual points are
   * accessed through point identifiers. */
  PointsContainerPointer m_PointsContainer;

  /** An object containing data associated with the mesh's points.
   * Optionally, this can be nullptr, indicating that no data are associated with
   * the points.  The data for a point can be accessed through its point
   * identifier. */
  PointDataContainerPointer m_PointDataContainer;

public:
  /** PointSet-level operation interface. */
  void
  PassStructure(Self * inputPointSet);

  void
  Initialize() override;

  PointIdentifier
  GetNumberOfPoints() const;

  /** Define Set/Get access routines for each internal container.
   * Methods also exist to add points, cells, etc. one at a time
   * rather than through an entire container. */
  void
  SetPoints(PointsContainer *);

  PointsContainer *
  GetPoints();

  const PointsContainer *
  GetPoints() const;

  void
  SetPointData(PointDataContainer *);

  PointDataContainer *
  GetPointData();

  const PointDataContainer *
  GetPointData() const;

  /** Access routines to fill the Points container, and get information
   * from it. */
  void SetPoint(PointIdentifier, PointType);
  bool
            GetPoint(PointIdentifier, PointType *) const;
  PointType GetPoint(PointIdentifier) const;

  /** Access routines to fill the PointData container, and get information
   * from it. */
  void SetPointData(PointIdentifier, PixelType);
  bool
  GetPointData(PointIdentifier, PixelType *) const;

  /** Methods to manage streaming. */
  void
  UpdateOutputInformation() override;

  void
  SetRequestedRegionToLargestPossibleRegion() override;

  void
  CopyInformation(const DataObject * data) override;

  void
  Graft(const DataObject * data) override;

  bool
  RequestedRegionIsOutsideOfTheBufferedRegion() override;

  bool
  VerifyRequestedRegion() override;

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method
   * implements the API from DataObject. The data object parameter must be
   * castable to a PointSet. */
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
  /** Constructor for use by New() method. */
  PointSet();
  ~PointSet() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

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
  RegionType m_MaximumNumberOfRegions;
  RegionType m_NumberOfRegions;
  RegionType m_RequestedNumberOfRegions;
  RegionType m_BufferedRegion;
  RegionType m_RequestedRegion;
}; // End Class: PointSet
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPointSet.hxx"
#endif

/*
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSet.hxx"
#endif
*/

#endif

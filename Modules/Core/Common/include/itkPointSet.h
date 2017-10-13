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
 * in the "MeshTraits" structure will have duplicate typedefs in the resulting
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
 * \wiki
 * \wikiexample{PointSet/CreatePointSet,Create a PointSet}
 * \wikiexample{PointSet/ReadPointSet,Read a PointSet}
 * \wikiexample{PointSet/WritePointSet,Write a PointSet}
 * \wikiexample{PointSet/BoundingBox,Get the bounding box of a PointSet}
 * \endwiki
 */

template<
  typename TPixelType,
  unsigned int VDimension = 3,
  typename TMeshTraits = DefaultStaticMeshTraits< TPixelType, VDimension, VDimension >
  >
class ITK_TEMPLATE_EXPORT PointSet:public DataObject
{
public:
  /** Standard class typedefs. */
  typedef PointSet                   Self;
  typedef DataObject                 Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard part of every itk Object. */
  itkTypeMacro(PointSet, Object);

  /** Hold on to the type information specified by the template parameters. */
  typedef TMeshTraits                    MeshTraits;
  typedef typename MeshTraits::PixelType PixelType;

  /** Convenient typedefs obtained from TMeshTraits template parameter. */
  typedef typename MeshTraits::CoordRepType       CoordRepType;
  typedef typename MeshTraits::PointIdentifier    PointIdentifier;
  typedef typename MeshTraits::PointType          PointType;
  typedef typename MeshTraits::PointsContainer    PointsContainer;
  typedef typename MeshTraits::PointDataContainer PointDataContainer;

  /** Convenient typedefs obtained from TMeshTraits template parameter. */
  itkStaticConstMacro(PointDimension, unsigned int,
                      TMeshTraits::PointDimension);

  /** Create types that are pointers to each of the container types. */
  typedef typename PointsContainer::Pointer         PointsContainerPointer;
  typedef typename PointsContainer::ConstPointer    PointsContainerConstPointer;
  typedef typename PointDataContainer::Pointer      PointDataContainerPointer;
  typedef typename PointDataContainer::ConstPointer PointDataContainerConstPointer;

  /** Create types that are iterators for each of the container types. */
  typedef typename PointsContainer::ConstIterator    PointsContainerConstIterator;
  typedef typename PointsContainer::Iterator         PointsContainerIterator;
  typedef typename PointDataContainer::ConstIterator PointDataContainerIterator;

  /** Type used to define Regions */
  typedef long RegionType;

  /** Get the maximum number of regions that this data can be
   * separated into. */
  itkGetConstMacro(MaximumNumberOfRegions, RegionType);

protected:
  /** An object containing points used by the mesh.  Individual points are
   * accessed through point identifiers. */
  PointsContainerPointer m_PointsContainer;

  /** An object containing data associated with the mesh's points.
   * Optionally, this can be ITK_NULLPTR, indicating that no data are associated with
   * the points.  The data for a point can be accessed through its point
   * identifier. */
  PointDataContainerPointer m_PointDataContainer;

public:
  /** PointSet-level operation interface. */
  void PassStructure(Self *inputPointSet);

  virtual void Initialize(void) ITK_OVERRIDE;

  PointIdentifier GetNumberOfPoints() const;

  /** Define Set/Get access routines for each internal container.
   * Methods also exist to add points, cells, etc. one at a time
   * rather than through an entire container. */
  void SetPoints(PointsContainer *);

  PointsContainer * GetPoints();

  const PointsContainer * GetPoints() const;

  void SetPointData(PointDataContainer *);

  PointDataContainer * GetPointData();

  const PointDataContainer * GetPointData() const;

  /** Access routines to fill the Points container, and get information
   * from it. */
  void SetPoint(PointIdentifier, PointType);
  bool GetPoint(PointIdentifier, PointType *) const;
  PointType GetPoint(PointIdentifier) const;

  /** Access routines to fill the PointData container, and get information
   * from it. */
  void SetPointData(PointIdentifier, PixelType);
  bool GetPointData(PointIdentifier, PixelType *) const;

  /** Methods to manage streaming. */
  virtual void UpdateOutputInformation() ITK_OVERRIDE;

  virtual void SetRequestedRegionToLargestPossibleRegion() ITK_OVERRIDE;

  virtual void CopyInformation(const DataObject *data) ITK_OVERRIDE;

  virtual void Graft(const DataObject *data) ITK_OVERRIDE;

  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() ITK_OVERRIDE;

  virtual bool VerifyRequestedRegion() ITK_OVERRIDE;

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method
   * implements the API from DataObject. The data object parameter must be
   * castable to a PointSet. */
  virtual void SetRequestedRegion(const DataObject *data) ITK_OVERRIDE;

  /** Set/Get the Requested region */
  virtual void SetRequestedRegion(const RegionType & region);

  itkGetConstMacro(RequestedRegion, RegionType);

  /** Set/Get the Buffered region */
  virtual void SetBufferedRegion(const RegionType & region);

  itkGetConstMacro(BufferedRegion, RegionType);

protected:
  /** Constructor for use by New() method. */
  PointSet();
  ~PointSet() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

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

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PointSet);
};                              // End Class: PointSet
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSet.hxx"
#endif

/*
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSet.hxx"
#endif
*/

#endif

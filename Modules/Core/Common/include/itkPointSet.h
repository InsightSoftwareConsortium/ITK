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
#ifndef itkPointSet_h
#define itkPointSet_h

#include "itkPointSetBase.h"
#include "itkDefaultStaticMeshTraits.h"
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
class ITK_TEMPLATE_EXPORT PointSet : public PointSetBase<typename TMeshTraits::PointsContainer>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PointSet);

  /** Standard class type aliases. */
  using Self = PointSet;
  using Superclass = PointSetBase<typename TMeshTraits::PointsContainer>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(PointSet);

  /** Hold on to the type information specified by the template parameters. */
  using MeshTraits = TMeshTraits;
  using PixelType = typename MeshTraits::PixelType;

  /** Convenient type alias obtained from TMeshTraits template parameter. */
  using PointIdentifier = typename MeshTraits::PointIdentifier;
  using PointType = typename MeshTraits::PointType;
  using PointDataContainer = typename MeshTraits::PointDataContainer;

  /** Create types that are pointers to each of the container types. */
  using PointDataContainerPointer = typename PointDataContainer::Pointer;
  using PointDataContainerConstPointer = typename PointDataContainer::ConstPointer;

  /** Create types that are iterators for each of the container types. */
  using PointDataContainerIterator = typename PointDataContainer::ConstIterator;

protected:
  /** An object containing data associated with the mesh's points.
   * Optionally, this can be nullptr, indicating that no data are associated with
   * the points.  The data for a point can be accessed through its point
   * identifier. */
  PointDataContainerPointer m_PointDataContainer{};

public:
  /** Restore the PointSet to its initial state. Useful for data pipeline updates
   * without memory re-allocation.
   */
  void
  Initialize() override;

  /** Set the point data container. */
  void
  SetPointData(PointDataContainer *);

  /** Get the point data container. */
  PointDataContainer *
  GetPointData();

  /** Get the point data container. */
  const PointDataContainer *
  GetPointData() const;

  /** Assign data to a point identifier.  If a spot for the point identifier
   * does not exist, it will be created automatically.  There is no check if
   * a point with the same identifier exists.
   */
  void SetPointData(PointIdentifier, PixelType);

  /** Check if point data exists for a given point identifier.  If a spot for
   * the point identifier exists, "data" is set, and true is returned.
   * Otherwise, false is returned, and "data" is not modified.
   * If "data" is nullptr, then it is never set, but the existence of the point
   * data is still returned.
   */
  bool
  GetPointData(PointIdentifier, PixelType *) const;

  void
  Graft(const DataObject * data) override;

protected:
  /** Constructor for use by New() method. */
  PointSet() = default;
  ~PointSet() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  LightObject::Pointer
  InternalClone() const override;

}; // End Class: PointSet
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPointSet.hxx"
#endif

#endif

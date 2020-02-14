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
#ifndef itkMesh_h
#define itkMesh_h


#include "itkPointSet.h"

#include "itkBoundingBox.h"
#include "itkCellInterface.h"
#include "itkMapContainer.h"
#include "itkCommonEnums.h"
#include "ITKMeshExport.h"
#include <vector>
#include <set>

namespace itk
{

/** \class Mesh
 * \brief Implements the N-dimensional mesh structure.
 *
 * \par Overview
 * Mesh implements the N-dimensional mesh structure for ITK.  It provides
 * an API to perform operations on points, cells, boundaries, etc., but
 * does not tie down the underlying implementation and storage.  A
 * "MeshTraits" structure is used to define the container and identifier
 * types that will be used to access the mesh.  See DefaultStaticMeshTraits
 * for the set of type definitions needed.  All types that are defined
 * in the "MeshTraits" structure will have duplicate type alias in the resulting
 * mesh itself.
 *
 * Mesh is an adaptive, evolving structure. Typically points and cells
 * are created, with the cells referring to their defining points. If
 * additional topological information is required, then BuildCellLinks() is
 * called and links from the points back to the cells that use them are
 * created. This allows implicit topological information about the faces
 * and edges of the cells to be determined. (For example, a "face" neighbor
 * to a cell can be determined by intersection the sets of cells that use
 * the points defining the face. This is an inherent assumption on the
 * manifold relationship of the cells in the mesh.) In some cases, either
 * because the mesh is non-manifold, because we wish to explicitly store
 * information with the faces and edges of the mesh, or because performance
 * requirements demand that boundaries are explicitly represented (the set
 * intersection does not need to be performed); then Mesh can be further
 * extended by adding explicit boundary assignments.
 *
 * \par Usage
 * Mesh has three template parameters.  The first is the pixel type, or the
 * type of data stored (optionally) with points, cells, and/or boundaries.
 * The second is the geometric dimension of the points defining the mesh. This
 * also limits the maximum topological dimension of the cells that can be
 * inserted. The third template parameter is the "MeshTraits" structure
 * controlling type information for the mesh.  Most users will be happy
 * with the defaults, and will not have to worry about this third argument.
 *
 * One of the most important parts of using this mesh is how to create
 * cells to insert into it.  The cells for the mesh take two template
 * parameters.  The first is the pixel type, and should correspond
 * exactly to that type given to the mesh.  The second is a
 * "CellTraits" which holds a sub-set of the "MeshTraits" structure
 * definitions, and is also a member of them.  Any cell which is to be
 * inserted to a mesh should have MeshTraits::CellTraits as its second
 * template parameter.
 *
 * Template parameters for Mesh:
 *
 * TPixelType =
 *     The type stored as data for an entity (cell, point, or boundary).
 *
 * TMeshTraits =
 *     Type information structure for the mesh.
 *
 * \par References
 * No reference information is available.
 *
 * \ingroup MeshObjects
 * \ingroup ITKMesh
 *
 * \sphinx
 * \sphinxexample{Core/Mesh/AddPointsAndEdges,Add Points And Edges}
 * \sphinxexample{Core/Mesh/ConvertMeshToUnstructeredGrid,Convert Mesh To Unstructered Grid}
 * \sphinxexample{Core/Mesh/WorkingWithPointAndCellData,Working With Point And Cell Data}
 * \endsphinx
 */
template <typename TPixelType,
          unsigned int VDimension = 3,
          typename TMeshTraits = DefaultStaticMeshTraits<TPixelType, VDimension, VDimension>>
class ITK_TEMPLATE_EXPORT Mesh : public PointSet<TPixelType, VDimension, TMeshTraits>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(Mesh);

  /** Standard type alias. */
  using Self = Mesh;
  using Superclass = PointSet<TPixelType, VDimension, TMeshTraits>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using RegionType = typename Superclass::RegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard part of every itk Object. */
  itkTypeMacro(Mesh, PointSet);

  /** Hold on to the type information specified by the template parameters. */
  using MeshTraits = TMeshTraits;
  using PixelType = typename MeshTraits::PixelType;
  using CellPixelType = typename MeshTraits::CellPixelType;
  using MeshClassCellsAllocationMethodEnum = MeshEnums::MeshClassCellsAllocationMethod;

  /** Convenient constants obtained from TMeshTraits template parameter. */
  static constexpr unsigned int PointDimension = TMeshTraits::PointDimension;
  static constexpr unsigned int MaxTopologicalDimension = TMeshTraits::MaxTopologicalDimension;

#if !defined(ITK_LEGACY_REMOVE)
  using CellsAllocationMethodType = MeshClassCellsAllocationMethodEnum;
  /** Enables backwards compatibility for enum values */
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr CellsAllocationMethodType CellsAllocationMethodUndefined =
    MeshClassCellsAllocationMethodEnum::CellsAllocationMethodUndefined;
  static constexpr CellsAllocationMethodType CellsAllocatedAsStaticArray =
    MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedAsStaticArray;
  static constexpr CellsAllocationMethodType CellsAllocatedAsADynamicArray =
    MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedAsADynamicArray;
  static constexpr CellsAllocationMethodType CellsAllocatedDynamicallyCellByCell =
    MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedDynamicallyCellByCell;
#endif

  /** Convenient type alias obtained from TMeshTraits template parameter. */
  using CoordRepType = typename MeshTraits::CoordRepType;
  using InterpolationWeightType = typename MeshTraits::InterpolationWeightType;
  using PointIdentifier = typename MeshTraits::PointIdentifier;
  using CellIdentifier = typename MeshTraits::CellIdentifier;
  using CellFeatureIdentifier = typename MeshTraits::CellFeatureIdentifier;
  using PointHashType = typename MeshTraits::PointHashType;
  using PointType = typename MeshTraits::PointType;
  using PointsContainer = typename MeshTraits::PointsContainer;
  using CellTraits = typename MeshTraits::CellTraits;
  using CellsContainer = typename MeshTraits::CellsContainer;
  using PointCellLinksContainer = typename MeshTraits::PointCellLinksContainer;
  using CellLinksContainer = typename MeshTraits::CellLinksContainer;
  using PointDataContainer = typename MeshTraits::PointDataContainer;
  using CellDataContainer = typename MeshTraits::CellDataContainer;

  /** Used to support geometric operations on the toolkit. */
  using BoundingBoxType = BoundingBox<PointIdentifier, Self::PointDimension, CoordRepType, PointsContainer>;

  /** Create types that are pointers to each of the container types. */
  using PointsContainerPointer = typename PointsContainer::Pointer;
  using CellsContainerPointer = typename CellsContainer::Pointer;
  using CellsContainerConstPointer = typename CellsContainer::ConstPointer;
  using CellLinksContainerPointer = typename CellLinksContainer::Pointer;
  using PointDataContainerPointer = typename PointDataContainer::Pointer;
  using CellDataContainerPointer = typename CellDataContainer::Pointer;
  using CellDataContainerConstPointer = typename CellDataContainer::ConstPointer;
  using BoundingBoxPointer = typename BoundingBoxType::Pointer;
  using CellLinksContainerConstPointer = typename CellLinksContainer::ConstPointer;

  /** Create types that are iterators for each of the container types. */
  using PointsContainerConstIterator = typename PointsContainer::ConstIterator;
  using PointsContainerIterator = typename PointsContainer::Iterator;
  using CellsContainerConstIterator = typename CellsContainer::ConstIterator;
  using CellsContainerIterator = typename CellsContainer::Iterator;
  using CellLinksContainerIterator = typename CellLinksContainer::ConstIterator;
  using PointDataContainerIterator = typename PointDataContainer::ConstIterator;
  using CellDataContainerIterator = typename CellDataContainer::ConstIterator;
  using PointCellLinksContainerIterator = typename PointCellLinksContainer::const_iterator;

  /** A useful rename. */
  using CellFeatureCount = CellFeatureIdentifier;

  /** The base cell type for cells in this mesh. */
  using CellType = CellInterface<CellPixelType, CellTraits>;
  using CellAutoPointer = typename CellType::CellAutoPointer;

  /** Visiting cells. */
  using CellMultiVisitorType = typename CellType::MultiVisitor;

  /** \class BoundaryAssignmentIdentifier
   *  An explicit cell boundary assignment can be accessed through the cell
   *  identifier to which the assignment is made, and the feature Id of the
   *  boundary feature within the cell that is being assigned.
   *
   *  This class provides a pair of these identifiers with appropriate
   *  comparison operators available for use of the Ids in sorted container
   *  classes.
   * \ingroup ITKMesh
   */
  class BoundaryAssignmentIdentifier
  {
  public:
    /** Create an alias to BoundaryAssignmentIdentifier. */
    using Self = BoundaryAssignmentIdentifier;

    /** Constructor just takes the cell and feature identifiers, or defaults
     *  to their individual default values.  */
    BoundaryAssignmentIdentifier() = default;
    BoundaryAssignmentIdentifier(CellIdentifier cellId, CellFeatureIdentifier featureId)
      : m_CellId(cellId)
      , m_FeatureId(featureId)
    {}

    /** The Cell's identification. */
    CellIdentifier m_CellId;

    /** The identification of the feature within the cell. */
    CellFeatureIdentifier m_FeatureId;

    /** Most containers require a "<" operator to be defined for their key
     *  types.  */
    bool
    operator<(const Self & r) const
    {
      return ((m_CellId < r.m_CellId) || ((m_CellId == r.m_CellId) && (m_FeatureId < r.m_FeatureId)));
    }

    /** Most containers require a "==" operator to be defined for their key
     *  types.  */
    bool
    operator==(const Self & r) const
    {
      return ((m_CellId == r.m_CellId) && (m_FeatureId == r.m_FeatureId));
    }
  }; // End Class: Mesh::BoundaryAssignmentIdentifier

  /** Used for manipulating boundaries and boundary attributes.  A
   * BoundaryAssignmentsContainerVector is indexed by dimension.  For
   * each dimension, it points to a MapContainer indexed by a
   * BoundaryAssignmentIdentifier, which encapsulates a cell
   * identifier and a boundary feature identifier.  The boundary
   * feature identifier distinguishes different boundary features for
   * a given cell at a given dimension.  */
  using BoundaryAssignmentsContainer = MapContainer<BoundaryAssignmentIdentifier, CellIdentifier>;
  using BoundaryAssignmentsContainerPointer = typename BoundaryAssignmentsContainer::Pointer;
  using BoundaryAssignmentsContainerVector = std::vector<BoundaryAssignmentsContainerPointer>;

protected:
  /** Holds cells used by the mesh.  Individual cells are accessed
   *  through cell identifiers.  */
  CellsContainerPointer m_CellsContainer;

  /** An object containing data associated with the mesh's cells.
   *  Optionally, this can be nullptr, indicating that no data are associated
   *  with the cells.  The data for a cell can be accessed through its cell
   *  identifier.  */
  CellDataContainerPointer m_CellDataContainer;

  /** An object containing parent cell links for each point.  Since a point
   *  can be used by multiple cells, each point identifier accesses another
   *  container which holds the cell identifiers */
  mutable CellLinksContainerPointer m_CellLinksContainer;

  /** A vector of objects containing explicit cell boundary assignments.
   *  The vector is indexed by the topological dimension of the cell
   *  boundary.  The container for each topological dimension holds
   *  boundary identifiers of the assigned boundaries.  The containers are
   *  keyed by BoundaryAssignmentIdentifier objects (see above).  The
   *  boundary identifiers can be used to access the boundaries themselves
   *  in the containers stored in the Boundaries vector.  They can also be
   *  used to access the data stored by a particular boundary through the
   *  containers in the BoundaryData vector.  */
  BoundaryAssignmentsContainerVector m_BoundaryAssignmentsContainers;

public:
  /** Mesh-level operation interface. */
  CellIdentifier
  GetNumberOfCells() const;

  void
  PassStructure(Self * inputMesh);

  void
  Initialize() override;

  /** Methods for managing Mesh filters that have internal mini-pipelines */
  void
  CopyInformation(const DataObject * data) override;

  void
  Graft(const DataObject * data) override;

  /** Get the bounding box of the mesh. The methods return a pointer to
   * the user-supplied bounding box as a convenience. */
  const BoundingBoxType *
  GetBoundingBox() const;

  /** Access m_CellsLinksContainer, which contains parent cell links
   * for each point.  Since a point can be used by multiple cells,
   * each point identifier accesses another container which holds the
   * cell identifiers */
  void
  SetCellLinks(CellLinksContainer *);

  CellLinksContainer *
  GetCellLinks();

  const CellLinksContainer *
  GetCellLinks() const;

  /** Access m_CellsContainer, which holds cells used by the mesh.
   *  Individual cells are accessed through cell identifiers.  */
  void
  SetCells(CellsContainer *);

  CellsContainer *
  GetCells();

  const CellsContainer *
  GetCells() const;

  /** Access m_CellDataContainer, which contains data associated with
   *  the mesh's cells.  Optionally, this can be nullptr, indicating that
   *  no data are associated with the cells.  The data for a cell can
   *  be accessed through its cell identifier.  */
  void
  SetCellData(CellDataContainer *);

  CellDataContainer *
  GetCellData();

  const CellDataContainer *
  GetCellData() const;

  /** Delete entries in m_CellDataContainer which do not have a corresponding
   * entry in m_CellsContainer.
   */
  void
  DeleteUnusedCellData();

#if !defined(ITK_WRAPPING_PARSER)
  /**
   * Set/get the BoundaryAssignmentsContainer for a given dimension.
   * The BoundaryAssignmentsContainer is a MapContainer indexed by a
   * BoundaryAssignmentIdentifier, which encapsulates a cell
   * identifier and a boundary feature identifier.  The boundary
   * feature identifier distinguishes different boundary features for
   * a given cell at a given dimension.
   */
  void
  SetBoundaryAssignments(int dimension, BoundaryAssignmentsContainer *);


  BoundaryAssignmentsContainerPointer
  GetBoundaryAssignments(int dimension);

  const BoundaryAssignmentsContainerPointer
  GetBoundaryAssignments(int dimension) const;
#endif

  /** Access routines to fill the Cells container (m_CellsContainer),
   *  and get information from it.  If SetCell is used to overwrite a
   *  cell currently in the mesh, it is the caller's responsibility to
   *  release the memory for the cell currently at the CellIdentifier
   *  position prior to calling SetCell. */
  void
  SetCell(CellIdentifier, CellAutoPointer &);
  bool
  GetCell(CellIdentifier, CellAutoPointer &) const;
  /** Access routines to fill the CellData container, and get information
   *  from it.  */
  void SetCellData(CellIdentifier, CellPixelType);
  bool
  GetCellData(CellIdentifier, CellPixelType *) const;

  /**
   * Explicitly assign \a boundaryId as a part of the boundary of \a
   * cellId.  The identifiers \a boundaryId and \a cellId must
   * identify cell objects already in the mesh.  The dimension of \a
   * boundaryId must be specified by \a dimension, and a unique
   * CellFeatureIdentifier \a featureId must be assigned for each
   * distinct boundary feature of a given dimension.
   * CellFeatureIdentifier is equivalent to <tt>IdentifierType</tt> by
   * default, and this type alias will not typically need to be changed.
   * The UsingCells list of \a boundaryId is automatically updated to
   * include \a cellId.
   */
  void
  SetBoundaryAssignment(int                   dimension,
                        CellIdentifier        cellId,
                        CellFeatureIdentifier featureId,
                        CellIdentifier        boundaryId);

  /** For the given cellId, get the identifier of a particular
   * boundary feature of the given dimension.  The featureId
   * determines which boundary feature of the specified dimension is
   * returned.  For instance, if dimension is 1 and featureId is 0,
   * then GetBoundaryAssignment finds the 0th edge of the given cell.
   * The return value indicates whether a feature of the appropriate
   * dimension and featureId exists.  If it does not, the
   * BoundaryIdentifier pointer is left unchanged. */
  bool
  GetBoundaryAssignment(int                   dimension,
                        CellIdentifier        cellId,
                        CellFeatureIdentifier featureId,
                        CellIdentifier *      boundaryId) const;

  bool
  RemoveBoundaryAssignment(int dimension, CellIdentifier cellId, CellFeatureIdentifier featureId);

  /** Interface to cells. */
  CellFeatureCount
  GetNumberOfCellBoundaryFeatures(int dimension, CellIdentifier) const;

  /** Get the boundary feature of the given dimension of the given cell
   * corresponding to the given feature identifier. */
  bool
  GetCellBoundaryFeature(int dimension, CellIdentifier, CellFeatureIdentifier, CellAutoPointer &) const;
  /** Get the set of cells neighboring the given cell across the given boundary
   * feature.  Returns the number of neighbors found.  If cellSet is not nullptr,
   * the set of cell pointers is filled in with identifiers of the neighboring
   * cells. */
  CellIdentifier
  GetCellBoundaryFeatureNeighbors(int dimension,
                                  CellIdentifier,
                                  CellFeatureIdentifier,
                                  std::set<CellIdentifier> * cellSet);

  /** Get the set of cells having the given cell as part of their
   *  boundary.  Returns the number of neighbors found.  If cellSet is
   *  not nullptr, the set of cell pointers is filled in with identifiers
   *  of the neighboring cells. */
  CellIdentifier
  GetCellNeighbors(CellIdentifier cellId, std::set<CellIdentifier> * cellSet);

  /**
   * Check if there is an explicitly assigned boundary feature for the
   * given dimension and cell- and cell-feature-identifiers.  If there
   * is, a pointer to it is given back through \a boundary (if \a
   * boundary != nullptr) and \c true is returned.  Otherwise, \c false is
   * returned.
   */
  bool
  GetAssignedCellBoundaryIfOneExists(int dimension, CellIdentifier, CellFeatureIdentifier, CellAutoPointer &) const;
  /** Dynamically build the links from points back to their using cells.  This
   * information is stored in the cell links container, not in the points. */
  void
  BuildCellLinks() const;

  /** This method iterates over all the cells in the mesh and has
   *  each cell Accept the MultiVisitor. See MultiVisitor for more
   *  information.  (Note, this follows the Visitor Design Pattern.) */
  virtual void
  Accept(CellMultiVisitorType * mv) const;

  /** Set/Get the identification of the method used to allocate cells
      \warning Failure to call this method correctly will lead to memory leaks
      and/or segmentation faults because the cell memory will not be erased or
      will be erased with an improper method.  */
  itkSetMacro(CellsAllocationMethod, MeshClassCellsAllocationMethodEnum);
  itkGetConstReferenceMacro(CellsAllocationMethod, MeshClassCellsAllocationMethodEnum);

protected:
  /** Constructor for use by New() method. */
  Mesh();
  ~Mesh() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Release the memory allocated for the cells pointers. This is done
      based on information provided by the user through the method
      SetCellsAllocationMethod()   */
  void
  ReleaseCellsMemory();

  /** The bounding box (xmin,xmax, ymin,ymax, ...) of the mesh. The
   * bounding box is used for searching, picking, display, etc. */
  BoundingBoxPointer m_BoundingBox;

private:
  MeshClassCellsAllocationMethodEnum m_CellsAllocationMethod;
}; // End Class: Mesh

/** Define how to print enumeration */
extern ITKMesh_EXPORT std::ostream &
                      operator<<(std::ostream & out, const MeshEnums::MeshClassCellsAllocationMethod value);
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  ifndef ITK_WRAPPING_PARSER
#    include "itkMesh.hxx"
#  endif
#endif

#endif

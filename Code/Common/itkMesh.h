/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMesh.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMesh_h
#define __itkMesh_h

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkPointSet.h"
#include "itkCellInterface.h"
#include "itkMapContainer.h"
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
 * in the "MeshTraits" structure will have duplicate typedefs in the resulting
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
 */
template <
  typename TPixelType,
  unsigned int VDimension = 3,
  typename TMeshTraits = DefaultStaticMeshTraits< TPixelType , VDimension, VDimension >
  >
class ITK_EXPORT Mesh : public PointSet<TPixelType, VDimension, TMeshTraits>
{
public:
  /** Standard typedefs. */
  typedef Mesh                Self;
  typedef PointSet<TPixelType, VDimension, TMeshTraits>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard part of every itk Object. */
  itkTypeMacro(Mesh, PointSet);

  /** Hold on to the type information specified by the template parameters. */
  typedef TMeshTraits   MeshTraits;
  typedef typename MeshTraits::PixelType                PixelType;  
  typedef typename MeshTraits::CellPixelType            CellPixelType;  

  /** Convenient constants obtained from TMeshTraits template parameter. */
  itkStaticConstMacro(PointDimension, unsigned int,
                      TMeshTraits::PointDimension);
  itkStaticConstMacro(MaxTopologicalDimension, unsigned int,
                      TMeshTraits::MaxTopologicalDimension);

  /** Enum defining the possible methods used to allocate memory for
   * the Cells */
  typedef  enum {     CellsAllocationMethodUndefined,
                      CellsAllocatedAsStaticArray, 
                      CellsAllocatedAsADynamicArray,
                      CellsAllocatedDynamicallyCellByCell
                                                } CellsAllocationMethodType;

  /** Convenient typedefs obtained from TMeshTraits template parameter. */
  typedef typename MeshTraits::CoordRepType            CoordRepType;  
  typedef typename MeshTraits::InterpolationWeightType InterpolationWeightType;
  typedef typename MeshTraits::PointIdentifier         PointIdentifier;
  typedef typename MeshTraits::CellIdentifier          CellIdentifier;
  typedef typename MeshTraits::CellFeatureIdentifier   CellFeatureIdentifier;
  typedef typename MeshTraits::PointType               PointType;
  typedef typename MeshTraits::PointsContainer         PointsContainer;
  typedef typename MeshTraits::CellTraits              CellTraits;
  typedef typename MeshTraits::CellsContainer          CellsContainer;
  typedef typename MeshTraits::PointCellLinksContainer PointCellLinksContainer;
  typedef typename MeshTraits::CellLinksContainer      CellLinksContainer;
  typedef typename MeshTraits::PointDataContainer      PointDataContainer;
  typedef typename MeshTraits::CellDataContainer       CellDataContainer;  

  /** Used to support geometric operations on the toolkit. */
  typedef PointLocator<PointIdentifier,itkGetStaticConstMacro(PointDimension),
                       CoordRepType,PointsContainer>  PointLocatorType;
  typedef BoundingBox<PointIdentifier,itkGetStaticConstMacro(PointDimension),
                      CoordRepType,PointsContainer>   BoundingBoxType;

  /** Create types that are pointers to each of the container types. */
  typedef typename PointsContainer::Pointer       PointsContainerPointer;
  typedef typename CellsContainer::Pointer        CellsContainerPointer;
  typedef typename CellLinksContainer::Pointer    CellLinksContainerPointer;
  typedef typename PointDataContainer::Pointer    PointDataContainerPointer;
  typedef typename CellDataContainer::Pointer     CellDataContainerPointer;
  typedef typename PointLocatorType::Pointer      PointLocatorPointer;
  typedef typename BoundingBoxType::Pointer       BoundingBoxPointer;

  /** Create types that are iterators for each of the container types. */
  typedef typename
          PointsContainer::ConstIterator        PointsContainerConstIterator;
  typedef typename
          PointsContainer::Iterator             PointsContainerIterator;
  typedef typename
          CellsContainer::ConstIterator         CellsContainerConstIterator;
  typedef typename
          CellsContainer::Iterator              CellsContainerIterator;
  typedef typename
          CellLinksContainer::ConstIterator     CellLinksContainerIterator;
  typedef typename
          PointDataContainer::ConstIterator     PointDataContainerIterator;
  typedef typename
          CellDataContainer::ConstIterator      CellDataContainerIterator;
  typedef typename
     PointCellLinksContainer::const_iterator  PointCellLinksContainerIterator;

  /** A useful rename. */
  typedef CellFeatureIdentifier  CellFeatureCount;

  /** The base cell type for cells in this mesh. */
  typedef CellInterface<CellPixelType,CellTraits>  CellType;
  typedef typename CellType::CellAutoPointer       CellAutoPointer;

  /** Visiting cells. */
  typedef typename CellType::MultiVisitor CellMultiVisitorType;

protected:


  /** An explicit cell boundary assignment can be accessed through the cell
   *  identifier to which the assignment is made, and the feature Id of the
   *  boundary feature within the cell that is being assigned.
   *
   *  This class provides a pair of these identifiers with appropriate
   *  comparison operators available for use of the Ids in sorted container
   *  classes.  */
  class BoundaryAssignmentIdentifier
  {
  public:
    /** Create an alias to BoundaryAssignmentIdentifier. */
    typedef BoundaryAssignmentIdentifier Self;

    /** Constructor just takes the cell and feature identifiers, or defaults
     *  to their individual default values.  */
    BoundaryAssignmentIdentifier() {}
    BoundaryAssignmentIdentifier(CellIdentifier cellId,
                                 CellFeatureIdentifier featureId):
      m_CellId(cellId), m_FeatureId(featureId) {}    

    /** The Cell's identification. */
    CellIdentifier m_CellId;

    /** The identification of the feature within the cell. */
    CellFeatureIdentifier  m_FeatureId;

    /** Most containers require a "<" operator to be defined for their key
     *  types.  */
    bool operator < (const Self& r) const
      {
      return ((m_CellId < r.m_CellId) ||
              ((m_CellId == r.m_CellId) && (m_FeatureId < r.m_FeatureId)));
      }

    /** Most containers require a "==" operator to be defined for their key
     *  types.  */
    bool operator == (const Self& r) const
      {
      return ((m_CellId == r.m_CellId) && (m_FeatureId == r.m_FeatureId));
      }
  }; // End Class: Mesh::BoundaryAssignmentIdentifier


  /** Holds cells used by the mesh.  Individual cells are accessed
   *  through cell identifiers.  */
  CellsContainerPointer  m_CellsContainer;

  /** An object containing data associated with the mesh's cells.
   *  Optionally, this can be NULL, indicating that no data are associated
   *  with the cells.  The data for a cell can be accessed through its cell
   *  identifier.  */
  CellDataContainerPointer  m_CellDataContainer;

  /** An object containing parent cell links for each point.  Since a point
   *  can be used by multiple cells, each point identifier accesses another
   *  container which holds the cell identifiers */
  CellLinksContainerPointer  m_CellLinksContainer;

  /** Used for manipulating boundaries and boundary attributes.  A
   * BoundaryAssignmentsContainerVector is indexed by dimension.  For
   * each dimension, it points to a MapContainer indexed by a
   * BoundaryAssignmentIdentifier, which encapsulates a cell
   * identifier and a boundary feature identifier.  The boundary
   * feature identifier distinguishes different boundary features for
   * a given cell at a given dimension.  */
  typedef MapContainer< BoundaryAssignmentIdentifier , CellIdentifier >
        BoundaryAssignmentsContainer;
  typedef typename BoundaryAssignmentsContainer::Pointer
        BoundaryAssignmentsContainerPointer;
  typedef std::vector< BoundaryAssignmentsContainerPointer >
        BoundaryAssignmentsContainerVector;

  /** A vector of objects containing explicit cell boundary assignments.
   *  The vector is indexed by the topological dimension of the cell
   *  boundary.  The container for each topological dimension holds
   *  boundary identifiers of the assigned boundaries.  The containers are
   *  keyed by BoundaryAssignmentIdentifier objects (see above).  The
   *  boundary identifiers can be used to access the boundaries themselves
   *  in the containers stored in the Boundaries vector.  They can also be
   *  used to access the data stored by a particular boundary through the
   *  containers in the BoundaryData vector.  */
  BoundaryAssignmentsContainerVector  m_BoundaryAssignmentsContainers;

public:
  /** Mesh-level operation interface. */
  unsigned long GetNumberOfCells() const;
  void PassStructure(Self* inputMesh);
  virtual void Initialize();

  /** Access m_CellsLinksContainer, which contains parent cell links
   * for each point.  Since a point can be used by multiple cells,
   * each point identifier accesses another container which holds the
   * cell identifiers */
  void SetCellLinks(CellLinksContainer*);
  CellLinksContainerPointer GetCellLinks();
  const CellLinksContainerPointer GetCellLinks() const;

  /** Access m_CellsContainer, which holds cells used by the mesh.
   *  Individual cells are accessed through cell identifiers.  */
  void SetCells(CellsContainer*);
  CellsContainerPointer GetCells();
  const CellsContainerPointer GetCells() const;

  /** Access m_CellDataContainer, which contains data associated with
   *  the mesh's cells.  Optionally, this can be NULL, indicating that
   *  no data are associated with the cells.  The data for a cell can
   *  be accessed through its cell identifier.  */
  void SetCellData(CellDataContainer*);
  CellDataContainerPointer GetCellData();
  const CellDataContainerPointer GetCellData() const;

  /** 
   * Set/get the BoundaryAssignmentsContainer for a given dimension.
   * The BoundaryAssignmentsContainer is a MapContainer indexed by a
   * BoundaryAssignmentIdentifier, which encapsulates a cell
   * identifier and a boundary feature identifier.  The boundary
   * feature identifier distinguishes different boundary features for
   * a given cell at a given dimension.
   */
  void SetBoundaryAssignments(int dimension,
                              BoundaryAssignmentsContainer*);
  BoundaryAssignmentsContainerPointer GetBoundaryAssignments(int dimension);
  const BoundaryAssignmentsContainerPointer GetBoundaryAssignments(
    int dimension) const;

  /** Access routines to fill the Cells container (m_CellsContainer),
   *  and get information from it.  If SetCell is used to overwrite a
   *  cell currently in the mesh, it is the caller's responsibility to
   *  release the memory for the cell currently at the CellIdentifier
   *  position prior to calling SetCell. */
  void SetCell(CellIdentifier, CellAutoPointer & );
  bool GetCell(CellIdentifier, CellAutoPointer & ) const;

  /** Access routines to fill the CellData container, and get information
   *  from it.  */
  void SetCellData(CellIdentifier, CellPixelType);
  bool GetCellData(CellIdentifier, CellPixelType*) const;

  /** 
   * Explicitly assign \a boundaryId as a part of the boundary of \a
   * cellId.  The identifiers \a boundaryId and \a cellId must
   * identify cell objects already in the mesh.  The dimension of \a
   * boundaryId must be specified by \a dimension, and a unique
   * CellFeatureIdentifier \a featureId must be assigned for each
   * distinct boundary feature of a given dimension.
   * CellFeatureIdentifier is equivalent to <tt>unsigned long</tt> by
   * default, and will not typically need to be changed.  The
   * UsingCells list of \a boundaryId is automatically updated to
   * include \a cellId.
   */
  void SetBoundaryAssignment(int dimension, CellIdentifier cellId,
                             CellFeatureIdentifier featureId,
                             CellIdentifier boundaryId);

  /** For the given cellId, get the identifier of a particular
   * boundary feature of the given dimension.  The featureId
   * determines which boundary feature of the specified dimension is
   * returned.  For instance, if dimension is 1 and featureId is 0,
   * then GetBoundaryAssignment finds the 0th edge of the given cell.
   * The return value indicates whether a feature of the appropriate
   * dimension and featureId exists.  If it does not, the
   * BoundaryIdentifier pointer is left unchanged. */
  bool GetBoundaryAssignment(int dimension, CellIdentifier cellId,
                             CellFeatureIdentifier featureId,
                             CellIdentifier* boundaryId) const;
  bool RemoveBoundaryAssignment(int dimension, CellIdentifier cellId,
                                CellFeatureIdentifier featureId);

  /** Interface to cells. */
  CellFeatureCount GetNumberOfCellBoundaryFeatures(int dimension,
                                                   CellIdentifier) const;

  /** Get the boundary feature of the given dimension of the given cell
   * corresponding to the given feature identifier. */
  bool GetCellBoundaryFeature(int dimension, CellIdentifier,
                              CellFeatureIdentifier, CellAutoPointer& ) const;

  /** Get the set of cells neighboring the given cell across the given boundary
   * feature.  Returns the number of neighbors found.  If cellSet is not NULL,
   * the set of cell pointers is filled in with identifiers of the neighboring
   * cells. */
  unsigned long GetCellBoundaryFeatureNeighbors(
    int dimension, CellIdentifier, CellFeatureIdentifier,
    std::set<CellIdentifier>* cellSet);

  /** Get the set of cells having the given cell as part of their
   *  boundary.  Returns the number of neighbors found.  If cellSet is
   *  not NULL, the set of cell pointers is filled in with identifiers
   *  of the neighboring cells. */
  unsigned long GetCellNeighbors( CellIdentifier cellId,
                                  std::set<CellIdentifier>* cellSet );

  /**
   * Check if there is an explicitly assigned boundary feature for the
   * given dimension and cell- and cell-feature-identifiers.  If there
   * is, a pointer to it is given back through \a boundary (if \a
   * boundary != NULL) and \c true is returned.  Otherwise, \c false is
   * returned.
   */
  bool GetAssignedCellBoundaryIfOneExists(int dimension, CellIdentifier,
                                          CellFeatureIdentifier,
                                          CellAutoPointer& ) const;

  /** Dynamically build the links from points back to their using cells.  This
   * information is stored in the cell links container, not in the points. */
  void BuildCellLinks();

  /** Get the bounding box of a cell in the mesh. The user
   *  must supply the bounding box. The methods return a pointer to
   *  the user-supplied bounding box as a convenience. */
  BoundingBoxPointer GetCellBoundingBox(CellIdentifier cellId, 
                                        BoundingBoxPointer bbox);

  /** This method iterates over all the cells in the mesh and has
   *  each cell Accept the MultiVisitor. See MultiVisitor for more 
   *  information.  (Note, this follows the Visitor Design Pattern.) */
  virtual void Accept(CellMultiVisitorType* mv);

  /** Methods that support streaming. */
  virtual void UpdateOutputInformation();
  virtual void SetRequestedRegionToLargestPossibleRegion();
  virtual void CopyInformation(const DataObject *data);
  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion();
  virtual bool VerifyRequestedRegion();

  /** Get the maximum number of regions that this data can be
   *  separated into. */
  int GetMaximumNumberOfRegions() const
    {return m_MaximumNumberOfRegions;}

  /** Set the requested region from this data object to match the requested
   *  region of the data object passed in as a parameter.  This method 
   *  implements the API from DataObject. The data object parameter must be
   *  castable to a Mesh. */
  virtual void SetRequestedRegion(DataObject *data);

  /** Set/Get the Requested region */
  virtual void SetRequestedRegion( const RegionType & region );
  itkGetMacro( RequestedRegion, RegionType );

  /** Set/Get the identification of the method used to allocate cells
      \warning Failure to call this method correctly will lead to memory leaks
      and/or segmentation faults because the cell memory will not be erased or
      will be erased with an improper method.  */
  itkSetMacro( CellsAllocationMethod, CellsAllocationMethodType );  
  itkGetConstMacro( CellsAllocationMethod, CellsAllocationMethodType );  

protected:
  /** Constructor for use by New() method. */
  Mesh();
  ~Mesh();
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Release the memory allocated for the cells pointers. This is done
      based on information provided by the user through the method
      SetCellsAllocationMethod()   */
  void ReleaseCellsMemory();

private:
  Mesh(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  CellsAllocationMethodType             m_CellsAllocationMethod;

}; // End Class: Mesh

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMesh.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMesh.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkMesh_h
#define __itkMesh_h

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
 * Mesh implements the N-dimensional mesh structure for ITK.  It provides
 * an API to perform operations on points, cells, boundaries, etc., but
 * does not tie down the underlying implementation and storage.  A
 * "MeshTraits" structure is used to define the container and identifier
 * types that will be used to access the mesh.  See DefaultStaticMeshTraits
 * for the set of type definitions needed.  All types that are defined
 * in the "MeshTraits" structure will have duplicate typedefs in the resulting
 * mesh itself.
 *
 * Mesh has two template parameters.  The first is the pixel type, or the
 * type of data stored (optionally) with points, cells, and/or boundaries.
 * The second is the "MeshTraits" structure controlling type information for
 * the mesh.  Most users will be happy with the defaults, and will not have
 * to worry about this second argument.
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
 */
  
template <
  typename TPixelType,
  unsigned int VDimension = 3,
  typename TMeshTraits = DefaultStaticMeshTraits< TPixelType , VDimension >
  >
class Mesh : public PointSet<TPixelType, VDimension, TMeshTraits>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Mesh                Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef PointSet<TPixelType, VDimension, TMeshTraits>  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(Mesh, PointSet);

  /** 
   * Hold on to the type information specified by the template parameters.
   */
  typedef TMeshTraits   MeshTraits;
  typedef typename MeshTraits::PixelType                PixelType;  

  /** 
   * Convenient typedefs obtained from TMeshTraits template parameter.
   */
  enum {PointDimension = MeshTraits::PointDimension};
  enum {MaxTopologicalDimension = MeshTraits::MaxTopologicalDimension};
  typedef typename MeshTraits::CoordRepType             CoordRepType;  
  typedef typename MeshTraits::InterpolationWeightType  InterpolationWeightType;
  typedef typename MeshTraits::PointIdentifier          PointIdentifier;
  typedef typename MeshTraits::CellIdentifier           CellIdentifier;
  typedef typename MeshTraits::BoundaryIdentifier       BoundaryIdentifier;
  typedef typename MeshTraits::CellFeatureIdentifier    CellFeatureIdentifier;
  typedef typename MeshTraits::PointType                PointType;
  typedef typename MeshTraits::PointsContainer          PointsContainer;
  typedef typename MeshTraits::CellTraits               CellTraits;
  typedef typename MeshTraits::CellsContainer           CellsContainer;
  typedef typename MeshTraits::PointCellLinksContainer  PointCellLinksContainer;
  typedef typename MeshTraits::CellLinksContainer       CellLinksContainer;
  typedef typename MeshTraits::PointDataContainer       PointDataContainer;
  typedef typename MeshTraits::CellDataContainer        CellDataContainer;  
  typedef typename MeshTraits::BoundariesContainer      BoundariesContainer;
  typedef typename MeshTraits::BoundaryDataContainer    BoundaryDataContainer;

  /**
   * Used to support geometric operations on the toolkit.
   */
  typedef PointLocator<PointIdentifier,PointDimension,
                       CoordRepType,PointsContainer>  PointLocatorType;
  typedef BoundingBox<PointIdentifier,PointDimension,
                      CoordRepType,PointsContainer>   BoundingBoxType;

  /**
   * Create types that are pointers to each of the container types.
   */
  typedef typename PointsContainer::Pointer        PointsContainerPointer;
  typedef typename CellsContainer::Pointer         CellsContainerPointer;
  typedef typename CellLinksContainer::Pointer     CellLinksContainerPointer;
  typedef typename PointDataContainer::Pointer     PointDataContainerPointer;
  typedef typename CellDataContainer::Pointer      CellDataContainerPointer;
  typedef typename BoundariesContainer::Pointer    BoundariesContainerPointer;
  typedef typename BoundaryDataContainer::Pointer  BoundaryDataContainerPointer;  
  typedef typename PointLocatorType::Pointer       PointLocatorPointer;
  typedef typename BoundingBoxType::Pointer        BoundingBoxPointer;

  /**
   * Create types that are iterators for each of the container types.
   */
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
          BoundariesContainer::ConstIterator    BoundariesContainerIterator;
  typedef typename
          BoundaryDataContainer::ConstIterator  BoundaryDataContainerIterator;
  typedef typename
     PointCellLinksContainer::const_iterator  PointCellLinksContainerIterator;
  
  /**
   * A useful rename.
   */
  typedef CellFeatureIdentifier  CellFeatureCount;
  
  /**
   * The base cell type for cells in this mesh.
   */
  typedef CellInterface<PixelType,CellTraits>  Cell;
  typedef typename CellInterface<PixelType,CellTraits>::Pointer  CellPointer;

  /**
   * It happens that boundaries are also cells.
   */
  typedef Cell BoundaryType;
  typedef CellPointer BoundaryPointer;
  
  /**
   * Visiting cells.
   */
  typedef typename Cell::MultiVisitor CellMultiVisitorType;

protected:
  /**
   * An explicit cell boundary assignment can be accessed through the cell
   * identifier to which the assignment is made, and the feature Id of the
   * boundary feature within the cell that is being assigned.
   *
   * This class provides a pair of these identifiers with appropriate
   * comparison operators available for use of the Ids in sorted container
   * classes.
   */
  class BoundaryAssignmentIdentifier
  {
  public:
    /**
     * Create an alias to BoundaryAssignmentIdentifier.
     */
    typedef BoundaryAssignmentIdentifier Self;

    /**
     * Constructor just takes the cell and feature identifiers, or
     * defaults to their individual default values.
     */
    BoundaryAssignmentIdentifier() {}
    BoundaryAssignmentIdentifier(CellIdentifier cellId,
                                 CellFeatureIdentifier featureId):
      m_CellId(cellId), m_FeatureId(featureId) {}    
    
    /**
     * The Cell's identification.
     */
    CellIdentifier m_CellId;
  
    /**
     * The identification of the feature within the cell.
     */
    CellFeatureIdentifier  m_FeatureId;
  
    /**
     * Most containers require a "<" operator to be defined for their
     * key types.
     */
    bool operator < (const Self& r) const
      {
      return ((m_CellId < r.m_CellId) ||
              ((m_CellId == r.m_CellId) && (m_FeatureId < r.m_FeatureId)));
      }
  
    /**
     * Most containers require a "==" operator to be defined for their
     * key types.
     */
    bool operator == (const Self& r) const
      {
      return ((m_CellId == r.m_CellId) && (m_FeatureId == r.m_FeatureId));
      }
  }; // End Class: Mesh::BoundaryAssignmentIdentifier

  /**
   * An object containing cells used by the mesh.  Individual cells are
   * accessed through cell identifiers.
   */
  CellsContainerPointer  m_CellsContainer;
  
  /**
   * An object containing data associated with the mesh's cells.
   * Optionally, this can be NULL, indicating that no data are associated with
   * the cells.  The data for a cell can be accessed through its cell
   * identifier.
   */
  CellDataContainerPointer  m_CellDataContainer;

  /**
   * An object containing parent cell links for each point.  Since a point
   * can be used by multiple cells, each point identifier accesses another
   * container which holds the cell identifiers
   */
  CellLinksContainerPointer  m_CellLinksContainer;
  
  /**
   * Since multiple cells can be assigned the same boundary (when they are
   * neighbors, for example), the boundaries themselves are stored by
   * containers in the Boundaries vector, which is indexed by the
   * topological dimension of a boundary.  Individual cells are assigned
   * boundary identifiers through the BoundaryAssignments vector.  These
   * identifiers are used to access the container in this vector corresponding
   * to the topological dimension of the boundary.
   */
  typedef std::vector< BoundariesContainerPointer >
        BoundariesContainerVector;
  BoundariesContainerVector  m_BoundariesContainers;

  /**
   * If any information is to be stored with boundaries, it is placed in
   * a container in this vector.  The vector is indexed by the topological
   * dimension of the boundary.  A boundary identifier is used to access
   * the actual data corresponding to a boundary in the container
   * corresponding to its dimension.
   */
  typedef std::vector< BoundaryDataContainerPointer >
        BoundaryDataContainerVector;
  BoundaryDataContainerVector  m_BoundaryDataContainers;

  typedef MapContainer< BoundaryAssignmentIdentifier , BoundaryIdentifier >
        BoundaryAssignmentsContainer;
  typedef typename BoundaryAssignmentsContainer::Pointer
        BoundaryAssignmentsContainerPointer;
  /**
   * A vector of objects containing explicit cell boundary assignments.
   * The vector is indexed by the topological dimension of the cell boundary.
   * The container for each topological dimension holds boundary identifiers
   * of the assigned boundaries.  The containers are keyed by
   * BoundaryAssignmentIdentifier objects (see above).  The boundary
   * identifiers can be used to access the boundaries themselves in the
   * containers stored in the Boundaries vector.  They can also be used
   * to access the data stored by a particular boundary through the
   * containers in the BoundaryData vector.
   */
  typedef std::vector< BoundaryAssignmentsContainerPointer >
        BoundaryAssignmentsContainerVector;
  BoundaryAssignmentsContainerVector  m_BoundaryAssignmentsContainers;
  
public:
  /**
   * Mesh-level operation interface.
   */
  void PassStructure(Self* inputMesh);
  void ReInitialize(void);

  unsigned long GetNumberOfCells(void);

  /**
   * Define Set/Get access routines for each internal container.
   * Methods also exist to add points, cells, etc. one at a time
   * rather than through an entire container.
   */
  void SetCellLinks(CellLinksContainer*);
  CellLinksContainerPointer GetCellLinks(void);

  void SetCells(CellsContainer*);
  CellsContainerPointer GetCells(void);

  void SetCellData(CellDataContainer*);
  CellDataContainerPointer GetCellData(void);

  void SetBoundaries(int dimension, BoundariesContainer*);
  BoundariesContainerPointer GetBoundaries(int dimension);

  void SetBoundaryData(int dimension, BoundaryDataContainer*);
  BoundaryDataContainerPointer GetBoundaryData(int dimension);
  
  void SetBoundaryAssignments(int dimension,
                              BoundaryAssignmentsContainer*);
  BoundaryAssignmentsContainerPointer
  GetBoundaryAssignments(int dimension);
  
  /**
   * Access routines to fill the Cells container, and get information
   * from it.
   */
  void SetCell(CellIdentifier, Cell*);
  bool GetCell(CellIdentifier, CellPointer*) const;

  /**
   * Access routines to fill the CellData container, and get information
   * from it.
   */
  void SetCellData(CellIdentifier, PixelType);
  bool GetCellData(CellIdentifier, PixelType*) const;
  
  /**
   * Access routines to fill the Boundaries container, and get information
   * from it.
   */
  void SetBoundary(int dimension, BoundaryIdentifier, BoundaryType*);
  bool GetBoundary(int dimension, BoundaryIdentifier, BoundaryPointer*)
    const;
  
  /**
   * Access routines to fill the BoundaryData container, and get information
   * from it.
   */
  void SetBoundaryData(int dimension, BoundaryIdentifier, PixelType);
  bool GetBoundaryData(int dimension, BoundaryIdentifier, PixelType*) const;

  /**
   * Access routines to fill the BoundaryAssignments container, and get
   * information from it.
   */
  void SetBoundaryAssignment(int dimension, CellIdentifier cellId,
                             CellFeatureIdentifier featureId,
                             BoundaryIdentifier);
  bool GetBoundaryAssignment(int dimension, CellIdentifier cellId,
                             CellFeatureIdentifier featureId,
                             BoundaryIdentifier*) const;
  bool RemoveBoundaryAssignment(int dimension, CellIdentifier cellId,
                                CellFeatureIdentifier featureId);

  /**
   * Interface to cells.
   */
  CellFeatureCount GetNumberOfCellBoundaryFeatures(int dimension,
                                                   CellIdentifier) const;
  BoundaryPointer GetCellBoundaryFeature(int dimension, CellIdentifier,
                                           CellFeatureIdentifier) const;
  unsigned long GetCellBoundaryFeatureNeighbors(
    int dimension, CellIdentifier, CellFeatureIdentifier,
    std::set<CellIdentifier>* cellSet);
  
  bool GetAssignedCellBoundaryIfOneExists(int dimension, CellIdentifier,
                                          CellFeatureIdentifier,
                                          BoundaryPointer*) const;
  void BuildCellLinks(void);
  
  /**
   * Get the bounding box of a cell in the mesh. The user
   * must supply the bounding box. The methods return a pointer to
   * the user-supplied bounding box as a convenience.
   */
  BoundingBoxPointer GetCellBoundingBox(CellIdentifier cellId, 
                                        BoundingBoxPointer bbox);
  
  /**
   *  This method iterates over all the cells in the mesh and has
   *  each cell Accept the MultiVisitor. See MultiVisitor for more 
   *  information.  (Note, this follows the Visitor Design Pattern.)
   */
  void Accept(CellMultiVisitorType* mv);

  virtual void UpdateOutputInformation();
  virtual void SetRequestedRegionToLargestPossibleRegion();
  virtual void CopyInformation(DataObject *data);
  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion();
  virtual bool VerifyRequestedRegion();

  /**
   * Get the maximum number of regions that this data can be
   * separated into.
   */
  int GetMaximumNumberOfRegions() const
    {return m_MaximumNumberOfRegions;}
      
  /**
   * Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method 
   * implements the API from DataObject. The data object parameter must be
   * castable to a Mesh.
   */
  virtual void SetRequestedRegion(DataObject *data);

protected:
  /**
   * Constructor for use by New() method.
   */
  Mesh();
  ~Mesh() {}
  Mesh(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);
  
}; // End Class: Mesh

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMesh.txx"
#endif
  
#endif

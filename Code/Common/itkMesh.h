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

#include "itkObject.h"
#include "itkDataObject.h"
#include "itkObjectFactory.h"
#include "itkSmartPointer.h"
#include "itkPoint.h"
#include "itkCellInterface.h"
#include "itkMeshTypeDefault.h"
#include "itkMapContainer.h"
#include "itkPointLocator.h"
#include "itkBoundingBox.h"
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
 * "MeshType" structure is used to define the container and identifier
 * types that will be used to access the mesh.  See MeshTypeDefault
 * for the set of type definitions needed.  All types that are defined
 * in the "MeshType" structure will have duplicate typedefs in the resulting
 * mesh itself.
 *
 * Mesh has two template parameters.  The first is the pixel type, or the
 * type of data stored (optionally) with points, cells, and/or boundaries.
 * The second is the "MeshType" structure controlling type information for
 * the mesh.  Most users will be happy with the defaults, and will not have
 * to worry about this second argument.
 *
 * One of the most important parts of using this mesh is how to create cells
 * to insert into it.  The cells for the mesh take two template parameters.
 * The first is the pixel type, and should correspond exactly to that type
 * given to the mesh.  The second is a "CellType" which holds a sub-set of the
 * "MeshType" structure definitions, and is also a member of them.  Any
 * cell which is to be inserted to a mesh should have MeshType::CellType
 * as its second template parameter.
 *
 * Template parameters for Mesh:
 *
 * TPixelType =
 *     The type stored as data for an entity (cell, point, or boundary).
 *
 * TMeshType =
 *     Type information structure for the mesh.
 */
  
template <
  typename TPixelType,
  typename TMeshType = MeshTypeDefault< TPixelType >
  >
class Mesh: public DataObject
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Mesh                Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef DataObject  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(Mesh, Object);

  /** 
   * Return the region type.
   */
  virtual int GetRegionType()
    {return DataObject::ITK_UNSTRUCTURED_REGION;}

  /**
   * Get the maximum number of regions that this data can be
   * separated into.
   */
  int GetMaximumNumberOfRegions() const
    {return m_MaximumNumberOfRegions;}
      
  /** \typedef PixelType
   * Hold on to the type information specified by the template parameters.
   */
  typedef TMeshType   MeshType;
  typedef typename MeshType::PixelType                PixelType;  
  enum {PointDimension = MeshType::PointDimension};
  enum {MaxTopologicalDimension = MeshType::MaxTopologicalDimension};
  typedef typename MeshType::CoordRep                 CoordRep;  
  typedef typename MeshType::InterpolationWeight      InterpolationWeight;
  typedef typename MeshType::PointIdentifier          PointIdentifier;
  typedef typename MeshType::CellIdentifier           CellIdentifier;
  typedef typename MeshType::BoundaryIdentifier       BoundaryIdentifier;
  typedef typename MeshType::CellFeatureIdentifier    CellFeatureIdentifier;
  typedef typename MeshType::Point                    Point;
  typedef typename MeshType::PointsContainer          PointsContainer;
  typedef typename MeshType::CellType                 CellType;
  typedef typename MeshType::CellsContainer           CellsContainer;
  typedef typename MeshType::PointCellLinksContainer  PointCellLinksContainer;
  typedef typename MeshType::CellLinksContainer       CellLinksContainer;
  typedef typename MeshType::PointDataContainer       PointDataContainer;
  typedef typename MeshType::CellDataContainer        CellDataContainer;  
  typedef typename MeshType::BoundariesContainer      BoundariesContainer;
  typedef typename MeshType::BoundaryDataContainer    BoundaryDataContainer;
  typedef PointLocator<PointIdentifier,PointDimension,
                       CoordRep,PointsContainer> PointLocatorType;
  typedef BoundingBox<PointIdentifier,PointDimension,
                      CoordRep,PointsContainer> BoundingBoxType;

  /** \typedef
   * Create types that are pointers to each of the container types.
   */
  typedef typename PointsContainer::Pointer        PointsContainerPointer;
  typedef typename CellsContainer::Pointer         CellsContainerPointer;
  typedef typename CellLinksContainer::Pointer     CellLinksContainerPointer;
  typedef typename PointDataContainer::Pointer     PointDataContainerPointer;
  typedef typename CellDataContainer::Pointer      CellDataContainerPointer;
  typedef typename BoundariesContainer::Pointer    BoundariesContainerPointer;
  typedef typename
          BoundaryDataContainer::Pointer  BoundaryDataContainerPointer;  
  typedef typename PointLocatorType::Pointer  PointLocatorPointer;
  typedef typename BoundingBoxType::Pointer   BoundingBoxPointer;

  /** \typedef
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
  
  /** \typedef
   * A useful rename.
   */
  typedef CellFeatureIdentifier  CellFeatureCount;
  
  /**
   * The base cell type for cells in this mesh.
   */
  typedef CellInterface<PixelType,CellType>  Cell;
  typedef typename CellInterface<PixelType,CellType>::Pointer  CellPointer;

  /**
   * It happens that boundaries are also cells.
   */
  typedef Cell Boundary;
  typedef CellPointer BoundaryPointer;
  
  /**
   * Visiting cells
   */
  typedef typename Cell::MultiVisitor CellMultiVisitor;

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
   * An object containing points used by the mesh.  Individual points are
   * accessed through point identifiers.
   */
  PointsContainerPointer  m_PointsContainer;

  /**
   * An object containing data associated with the mesh's points.
   * Optionally, this can be NULL, indicating that no data are associated with
   * the points.  The data for a point can be accessed through its point
   * identifier.
   */
  PointDataContainerPointer  m_PointDataContainer;

  /**
   * An object containing parent cell links for each point.  Since a point
   * can be used by multiple cells, each point identifier accesses another
   * container which holds the cell identifiers
   */
  CellLinksContainerPointer  m_CellLinksContainer;
  
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
  
  /**
   * PointLocator is used to accelerate the search for points. This
   * supports the FindClosestPoint() method. 
   */
  PointLocatorPointer m_PointLocator;
  
  /**
   * The bounding box (xmin,xmax, ymin,ymax, ...) of the mesh. The 
   * bounding box is used for searching, picking, display, etc.
   */
  BoundingBoxPointer m_BoundingBox;

public:
  /**
   * Mesh-level operation interface.
   */
  void PassStructure(Self* in_mesh);
  void ReInitialize(void);

  unsigned long GetNumberOfPoints(void);
  unsigned long GetNumberOfCells(void);

  /**
   * Define Set/Get access routines for each internal container.
   * Methods also exist to add points, cells, etc. one at a time
   * rather than through an entire container.
   */
  void SetPoints(PointsContainer*);
  PointsContainerPointer GetPoints(void);

  void SetPointData(PointDataContainer*);
  PointDataContainerPointer GetPointData(void);

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
   * Access routines to fill the Points container, and get information
   * from it.
   */
  void SetPoint(PointIdentifier, Point);
  bool GetPoint(PointIdentifier, Point*) const;

  /**
   * Access routines to fill the PointData container, and get information
   * from it.
   */
  void SetPointData(PointIdentifier, PixelType);
  bool GetPointData(PointIdentifier, PixelType*) const;

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
  void SetBoundary(int dimension, BoundaryIdentifier, Boundary*);
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
   * Get the bounding box of the mesh. The methods return a pointer to
   * the user-supplied bounding box as a convenience.
   */
  BoundingBoxPointer GetBoundingBox();

  /**
   * Get the bounding box of a cell in the mesh. The user
   * must supply the bounding box. The methods return a pointer to
   * the user-supplied bounding box as a convenience.
   */
  BoundingBoxPointer GetCellBoundingBox(CellIdentifier cellId, BoundingBoxPointer bbox);
  
  /**
   * Geometric operations convert between coordinate systems, perform 
   * interpolation, and locate points and cells.
   */
  bool FindClosestPoint(CoordRep coords[PointDimension],
                        PointIdentifier* pointId);
  // FindCell(.........)

  /**
   *  This method iterates over all the cells in the mesh and has
   *  each cell Accept the MultiVisitor. See MultiVisitor for more 
   *  information.  (Note, this follows the Visitor Design Pattern.)
   */
  void Accept(CellMultiVisitor* mv);

  virtual void UpdateOutputInformation();
  virtual void SetRequestedRegionToLargestPossibleRegion();
  virtual void CopyInformation(DataObject *data);
  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion();
  virtual bool VerifyRequestedRegion();

protected:
  /**
   * Constructor for use by New() method.
   */
  Mesh();
  ~Mesh() {}
  Mesh(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);
  
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
  int m_MaximumNumberOfRegions;
  int m_NumberOfRegions;
  int m_BufferedRegion;
  int m_RequestedNumberOfRegions;
  int m_RequestedRegion;

}; // End Class: Mesh

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMesh.txx"
#endif
  
#endif

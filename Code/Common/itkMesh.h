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

/**
 * itkMesh implements the N-dimensional mesh structure for ITK.  It provides
 * an API to perform operations on points, cells, boundaries, etc., but
 * does not tie down the underlying implementation and storage.  A
 * "MeshType" structure is used to define the container and identifier
 * types that will be used to access the mesh.  See itkMeshTypeDefault.h
 * for the set of type definitions needed.  All types that are defined
 * in the "MeshType" structure will have duplicate typedefs in the resulting
 * mesh itself.
 *
 * itkMesh has two template parameters.  The first is the pixel type, or the
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
 */

#ifndef __itkMesh_h
#define __itkMesh_h

#include <vector>
#include <list>

#include "itkObject.h"
#include "itkSmartPointer.h"
#include "itkPoint.h"
#include "itkCell.h"
#include "itkMeshTypeDefault.h"
#include "itkMapContainer.h"

template <
  /**
   * The type stored as data for an entity (cell, point, or boundary).
   */
  typename TPixelType,

  /**
   * Type information structure for the mesh.
   */
  typename TMeshType = itkMeshTypeDefault< TPixelType >
  >
class itkMesh: public itkObject
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef itkMesh                Self;
  typedef itkSmartPointer<Self>  Pointer;
  
  /**
   * Hold on to the type information specified by the template parameters.
   */
  typedef TMeshType   MeshType;
  typedef typename MeshType::PixelType                PixelType;  
  enum { PointDimension = MeshType::PointDimension };
  enum { MaxTopologicalDimension = MeshType::MaxTopologicalDimension };
  typedef typename MeshType::CoordRep                 CoordRep;  
  typedef typename MeshType::PointIdentifier          PointIdentifier;
  typedef typename MeshType::CellIdentifier           CellIdentifier;
  typedef typename MeshType::BoundaryIdentifier       BoundaryIdentifier;
  typedef typename MeshType::CellFeatureIdentifier    CellFeatureIdentifier;
  typedef typename MeshType::PointsContainer          PointsContainer;
  typedef typename MeshType::CellType                 CellType;
  typedef typename MeshType::CellsContainer           CellsContainer;
  typedef typename MeshType::PointCellLinksContainer  PointCellLinksContainer;
  typedef typename MeshType::CellLinksContainer       CellLinksContainer;
  typedef typename MeshType::PointDataContainer       PointDataContainer;
  typedef typename MeshType::CellDataContainer        CellDataContainer;  
  typedef typename MeshType::BoundariesContainer      BoundariesContainer;
  typedef typename MeshType::BoundaryDataContainer    BoundaryDataContainer;

  /**
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

  /**
   * Create types that are iterators for each of the container types.
   */
  typedef typename
          PointsContainer::ConstIterator        PointsContainerIterator;
  typedef typename
          CellsContainer::ConstIterator         CellsContainerIterator;
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
   * Define the type of point stored in this mesh.
   */
  typedef itkPoint< PointDimension , CoordRep >  Point;
  
  /**
   * Define the base cell type for cells in this mesh.
   * It also happens that boundaries are also cells.
   */
  typedef itkCell< PixelType , CellType >  Cell;
  typedef Cell                             Boundary;
  
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
  }; // End Class: itkMesh::BoundaryAssignmentIdentifier

protected:
  /**
   * An object containing points used by the mesh.  Individual points are
   * accessed through point identifiers.
   */
  PointsContainerPointer  m_Points;

  /**
   * An object containing data associated with the mesh's points.
   * Optionally, this can be NULL, indicating that no data are associated with
   * the points.  The data for a point can be accessed through its point
   * identifier.
   */
  PointDataContainerPointer  m_PointData;

  /**
   * An object containing parent cell links for each point.  Since a point
   * can be used by multiple cells, each point identifier accesses another
   * container which holds the cell identifiers
   */
  CellLinksContainerPointer  m_CellLinks;
  
  /**
   * An object containing cells used by the mesh.  Individual cellss are
   * accessed through cell identifiers.
   */
  CellsContainerPointer  m_Cells;
  
  /**
   * An object containing data associated with the mesh's cells.
   * Optionally, this can be NULL, indicating that no data are associated with
   * the cells.  The data for a cell can be accessed through its cell
   * identifier.
   */
  CellDataContainerPointer  m_CellData;
  
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
  BoundariesContainerVector  m_Boundaries;

  /**
   * If any information is to be stored with boundaries, it is placed in
   * a container in this vector.  The vector is indexed by the topological
   * dimension of the boundary.  A boundary identifier is used to access
   * the actual data corresponding to a boundary in the container
   * corresponding to its dimension.
   */
  typedef std::vector< BoundaryDataContainerPointer >
        BoundaryDataContainerVector;
  BoundaryDataContainerVector  m_BoundaryData;

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
  typedef itkMapContainer< BoundaryAssignmentIdentifier , BoundaryIdentifier >
        BoundaryAssignmentsContainer;
  typedef BoundaryAssignmentsContainer::Pointer
        BoundaryAssignmentsContainerPointer;
  typedef std::vector< BoundaryAssignmentsContainerPointer >
        BoundaryAssignmentsContainerVector;
  BoundaryAssignmentsContainerVector  m_BoundaryAssignments;
  
  /**
   * Define the mesh's public interface.  This includes access routines along
   * with specific mesh operations.
   */
public:
  static Pointer New(void);  
  
  /**
   * Define Set/Get access routines for each internal container.
   */
  void SetPointsContainer(PointsContainer*);
  PointsContainerPointer GetPointsContainer(void);

  void SetPointDataContainer(PointDataContainer*);
  PointDataContainerPointer GetPointDataContainer(void);

  void SetCellLinksContainer(CellLinksContainer*);
  CellLinksContainerPointer GetCellLinksContainer(void);

  void SetCellsContainer(CellsContainer*);
  CellsContainerPointer GetCellsContainer(void);

  void SetCellDataContainer(CellDataContainer*);
  CellDataContainerPointer GetCellDataContainer(void);

  void SetBoundariesContainer(int dimension, BoundariesContainer*);
  BoundariesContainerPointer GetBoundariesContainer(int);

  void SetBoundaryDataContainer(int dimension, BoundaryDataContainer*);
  BoundaryDataContainerPointer GetBoundaryDataContainer(int);
  
  void SetBoundaryAssignmentsContainer(int dimension,
				       BoundaryAssignmentsContainer*);
  BoundaryAssignmentsContainerPointer GetBoundaryAssignmentsContainer(int);

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
  bool GetCell(CellIdentifier, Cell::Pointer*) const;

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
  bool GetBoundary(int dimension, BoundaryIdentifier, Boundary::Pointer*)
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
  Boundary::Pointer GetCellBoundaryFeature(int dimension, CellIdentifier,
					   CellFeatureIdentifier) const;
  

  /**
   * Mesh-level operation interface.
   */

  unsigned long GetBoundaryFeatureNeighbors(
    int dimension, CellIdentifier, CellFeatureIdentifier,
    std::list<CellIdentifier>* cellList);

  bool GetAssignedBoundaryIfOneExists(int dimension, CellIdentifier,
				      CellFeatureIdentifier,
				      Boundary::Pointer*) const;

  void BuildCellLinks(void);
  
  /**
   * Standard part of itkObject class.  Used for debugging output.
   */
  itkTypeMacro(itkMesh, itkObject);

  /**
   * Define some internal utility methods.
   */
protected:
  /**
   * Constructor for use by New() method.
   */
  itkMesh();
}; // End Class: itkMesh

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMesh.cxx"
#endif

#endif

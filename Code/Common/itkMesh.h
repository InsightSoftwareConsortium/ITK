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

#include <vector>

#include "itkObject.h"
#include "itkSmartPointer.h"
#include "itkSetGet.h"
#include "itkContainerInterfaces.h"
#include "itkPoint.h"
#include "itkCell.h"
#include "itkMeshType.h"
#include "itkIdentifierTraits.h"
#include "itkAutoVectorContainer.h"
#include "itkMapContainer.h"

/**
 * itkMesh ....
 */

template <
  /**
   * The type associated with a point, cell, or boundary for use in storing
   * its data.
   */
  typename TPixelType,

  /**
   * Type information structure.
   */
  typename TMeshType = itkMeshTypeDefault
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
  typedef TPixelType  PixelType;
  typedef TMeshType   MeshType;
  enum { PointDimension = MeshType::PointDimension };
  enum { MaxTopologicalDimension = MeshType::MaxTopologicalDimension };
  typedef typename MeshType::CoordRep                 CoordRep;
  typedef typename MeshType::CellIdentifier           CellIdentifier;
  typedef typename MeshType::PointIdentifier          PointIdentifier;
  typedef typename MeshType::BoundaryIdentifier       BoundaryIdentifier;
  typedef typename MeshType::CellFeatureId            CellFeatureId;
  typedef typename MeshType::PointCellLinksContainer  PointCellLinksContainer;

  /**
   * A useful rename.
   */
  typedef CellFeatureId  CellFeatureCount;
  
  /**
   * Define the type of point stored in this mesh.
   */
  typedef itkPoint< PointDimension , CoordRep >  Point;
  
  /**
   * Define the base cell type for cells in this mesh.
   * It also happens that boundaries are also cells.
   */
  typedef itkCell< PixelType , MeshType >  Cell;
  typedef Cell                             Boundary;
  
  /**
   * Define a type that points to the container of cell links stored for
   * each point in the cell links container.
   */
  typedef typename PointCellLinksContainer::Pointer
        PointCellLinksContainerPointer;
  
  /**
   * An explicit cell boundary assignment can be accessed through the cell
   * identifier to which the assignment is made, and the feature Id of the
   * boundary feature within the cell that is being assigned.
   *
   * This class provides a pair of these identifiers with appropriate
   * comparison operators available for use of the Ids in sorted container
   * classes.
   */
  class BoundaryAssignmentId
  {
  public:
    typedef BoundaryAssignmentId Self;

    /**
     * Constructor just takes the cell and feature identifiers, or
     * defaults to their individual default values.
     */
    BoundaryAssignmentId() {}
    BoundaryAssignmentId(CellIdentifier cellId, CellFeatureId featureId):
      m_CellId(cellId), m_FeatureId(featureId) {}    
    
    /**
     * The Cell's identification.
     */
    CellIdentifier m_CellId;
  
    /**
     * The identification of the feature within the cell.
     */
    CellFeatureId  m_FeatureId;
  
    /**
     * Most containers require a "<" operator to be defined for their
     * key types.
     */
    bool operator < (const Self& r)
      {
	return ((m_CellId < r.m_CellId) ||
		((m_CellId == r.m_CellId) && (m_FeatureId < r.m_FeatureId)));
      }
  
    /**
     * Most containers require a "==" operator to be defined for their
     * key types.
     */
    bool operator == (const Self& r)
      {
	return ((m_CellId == r.m_CellId) && (m_FeatureId == r.m_FeatureId));
      }
  }; // End Class: itkMesh::BoundaryAssignmentId

protected:
  /**
   * An object containing points used by the mesh.  Individual points are
   * accessed through point identifiers.
   */
  typedef itkIndexedContainer< PointIdentifier , Point >
        PointsContainer;
  PointsContainer::Pointer  m_Points;

  /**
   * An object containing data associated with the mesh's points.
   * Optionally, this can be NULL, indicating that no data are associated with
   * the points.  The data for a point can be accessed through its point
   * identifier.
   */
  typedef itkIndexedContainer< PointIdentifier , PixelType >
        PointDataContainer;
  PointDataContainer::Pointer  m_PointData;

  /**
   * An object containing parent cell links for each point.  Since a point
   * can be used by multiple cells, each point identifier accesses another
   * container which holds the cell identifiers
   */
  typedef itkIndexedContainer< PointIdentifier , PointCellLinksContainerPointer >
        CellLinksContainer;
  CellLinksContainer::Pointer  m_CellLinks;
  
  /**
   * An object containing cells used by the mesh.  Individual cellss are
   * accessed through cell identifiers.
   */
  typedef itkIndexedContainer< CellIdentifier , Cell::Pointer >
        CellsContainer;
  CellsContainer::Pointer  m_Cells;
  
  /**
   * An object containing data associated with the mesh's cells.
   * Optionally, this can be NULL, indicating that no data are associated with
   * the cells.  The data for a cell can be accessed through its cell
   * identifier.
   */
  typedef itkIndexedContainer< CellIdentifier , PixelType >
        CellDataContainer;
  CellDataContainer::Pointer  m_CellData;
  
  /**
   * Since multiple cells can be assigned the same boundary (when they are
   * neighbors, for example), the boundaries themselves are stored by
   * containers in the Boundaries vector, which is indexed by the
   * topological dimension of a boundary.  Individual cells are assigned
   * boundary identifiers through the BoundaryAssignments vector.  These
   * identifiers are used to access the container in this vector corresponding
   * to the topological dimension of the boundary.
   */
  typedef itkIndexedContainer< BoundaryIdentifier , Boundary::Pointer >
        BoundariesContainer;
  typedef std::vector< BoundariesContainer::Pointer >
        BoundariesContainerVector;
  BoundariesContainerVector  m_Boundaries;

  /**
   * If any information is to be stored with boundaries, it is placed in
   * a container in this vector.  The vector is indexed by the topological
   * dimension of the boundary.  A boundary identifier is used to access
   * the actual data corresponding to a boundary in the container
   * corresponding to its dimension.
   */
  typedef itkIndexedContainer< BoundaryIdentifier , PixelType >
        BoundaryDataContainer;  
  typedef std::vector< BoundaryDataContainer::Pointer >
        BoundaryDataContainerVector;
  BoundaryDataContainerVector  m_BoundaryData;

  /**
   * A vector of objects containing explicit cell boundary assignments.
   * The vector is indexed by the topological dimension of the cell boundary.
   * The container for each topological dimension holds boundary identifiers
   * of the assigned boundaries.  The containers are keyed by
   * BoundaryAssignmentId objects (see above).  The boundary identifiers
   * can be used to access the boundaries themselves in the containers
   * stored in the Boundaries vector.  They can also be used to access
   * the data stored by a particular boundary through the containers in
   * the BoundaryData vector.
   */
  typedef itkIndexedContainer< BoundaryAssignmentId , BoundaryIdentifier >
        BoundaryAssignmentsContainer;
  typedef std::vector< BoundaryAssignmentsContainer::Pointer >
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
  virtual void SetPointsContainer(PointsContainer*);
  virtual PointsContainer::Pointer GetPointsContainer(void);

  virtual void SetPointDataContainer(PointDataContainer*);
  virtual PointDataContainer::Pointer GetPointDataContainer(void);

  virtual void SetCellLinksContainer(CellLinksContainer*);
  virtual CellLinksContainer::Pointer GetCellLinksContainer(void);

  virtual void SetCellsContainer(CellsContainer*);
  virtual CellsContainer::Pointer GetCellsContainer(void);

  virtual void SetCellDataContainer(CellDataContainer*);
  virtual CellDataContainer::Pointer GetCellDataContainer(void);

  virtual void SetBoundariesContainer(int dimension, BoundariesContainer*);
  virtual BoundariesContainer::Pointer GetBoundariesContainer(int);

  virtual void SetBoundaryDataContainer(int dimension, BoundaryDataContainer*);
  virtual BoundaryDataContainer::Pointer GetBoundaryDataContainer(int);
  
  virtual void SetBoundaryAssignmentsContainer(int dimension, BoundaryAssignmentsContainer*);
  virtual BoundaryAssignmentsContainer::Pointer GetBoundaryAssignmentsContainer(int);

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
  void SetBoundary(BoundaryIdentifier, Boundary*);
  bool GetBoundary(BoundaryIdentifier, Boundary::Pointer*) const;

  /**
   * Access routines to fill the BoundaryData container, and get information
   * from it.
   */
  void SetBoundaryData(BoundaryIdentifier, PixelType);
  bool GetBoundaryData(BoundaryIdentifier, PixelType*) const;

  /**
   * Access routines to fill the BoundaryAssignments container, and get
   * information from it.
   */
  void SetBoundaryAssignment(BoundaryAssignmentId, int dimension,
			     BoundaryIdentifier);
  bool GetBoundaryAssignment(BoundaryAssignmentId, int dimension,
			     BoundaryIdentifier*) const;
  void SetBoundaryAssignment(CellIdentifier cellId, CellFeatureId featureId,
			     int dimension, BoundaryIdentifier);
  bool GetBoundaryAssignment(CellIdentifier cellId, CellFeatureId featureId,
			     int dimension, BoundaryIdentifier*) const;
  bool RemoveBoundaryAssignment(BoundaryAssignmentId, int dimension);
  bool RemoveBoundaryAssignment(CellIdentifier cellId, CellFeatureId featureId,
				int dimension);

  /**
   * Interface to cells.
   */
  CellFeatureCount GetNumberOfCellBoundaryFeatures(CellIdentifier,
						   int dimension);
  Cell::Pointer GetCellBoundaryFeature(CellIdentifier, int dimension,
				       CellFeatureId);
  
  /**
   * Standard part of itkObject class.  Used for debugging output.
   */
  virtual const char *GetClassName() const { return "itkMesh"; }

  /**
   * Define some internal utility methods.
   */
protected:
  /**
   * Constructor for use by New() method.
   */
  itkMesh();
  
  /**
   * Routines to create default versions of each internal mesh container.
   * If the user has not provided one of the internal containers when it is
   * needed, the corresponding routine from below is called upon to construct
   * the container.
   *
   * Most of the defaults are different depending on whether their identifiers
   * are of integral type.  The choice is made based on the identifier's
   * traits.
   *
   * Boundary assignment containers are always indexed by BoundaryAssignmentId.
   * This is not an integral type.  Therefore, there is only one default
   * construction routine.
   */ 
  PointsContainer::Pointer ConstructDefaultPoints(void);
  PointDataContainer::Pointer ConstructDefaultPointData(void);
  CellLinksContainer::Pointer ConstructDefaultCellLinks(void);
  CellsContainer::Pointer ConstructDefaultCells(void);
  CellDataContainer::Pointer ConstructDefaultCellData(void);
  BoundariesContainer::Pointer ConstructDefaultBoundaries(void);
  BoundaryDataContainer::Pointer ConstructDefaultBoundaryData(void);
  BoundaryAssignmentsContainer::Pointer ConstructDefaultBoundaryAssignments(void);
  
  friend class itkCell< PixelType , MeshType >;
}; // End Class: itkMesh

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMesh.cxx"
#endif

#endif

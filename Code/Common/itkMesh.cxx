/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMesh.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkMesh.h"

/**
 * Object factory.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::Pointer
itkMesh< TPixelType , TMeshType >
::New(void)
{
  return new Self;
}


/**
 * Access routine to set the points container.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetPointsContainer(PointsContainer* points)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): setting Points container to " << points);
  if(m_Points != points)
    {
    m_Points = points;
    this->Modified();
    }
}


/**
 * Access routine to get the points container.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::PointsContainerPointer
itkMesh< TPixelType , TMeshType >
::GetPointsContainer(void)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): returning Points container of " << m_Points );
  return m_Points;
}


/**
 * Access routine to set the point data container.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetPointDataContainer(PointDataContainer* pointData)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): setting PointData container to " << pointData);
  if(m_PointData != pointData)
    {
    m_PointData = pointData;
    this->Modified();
    }
}


/**
 * Access routine to get the point data container.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::PointDataContainerPointer
itkMesh< TPixelType , TMeshType >
::GetPointDataContainer(void)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): returning PointData container of " << m_PointData );
  return m_PointData;
}


/**
 * Access routine to set the cell links container.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetCellLinksContainer(CellLinksContainer* cellLinks)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): setting CellLinks container to " << cellLinks);
  if(m_CellLinks != cellLinks)
    {
    m_CellLinks = cellLinks;
    this->Modified();
    }
}


/**
 * Access routine to get the cell links container.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::CellLinksContainerPointer
itkMesh< TPixelType , TMeshType >
::GetCellLinksContainer(void)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): returning CellLinks container of " << m_CellLinks );
  return m_CellLinks;
}


/**
 * Access routine to set the cells container.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetCellsContainer(CellsContainer* cells)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): setting Cells container to " << cells);
  if(m_Cells != cells)
    {
    m_Cells = cells;
    this->Modified();
    }
}


/**
 * Access routine to get the cells container.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::CellsContainerPointer
itkMesh< TPixelType , TMeshType >
::GetCellsContainer(void)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): returning Cells container of " << m_Cells );
  return m_Cells;
}


/**
 * Access routine to set the cell data container.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetCellDataContainer(CellDataContainer* cellData)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): setting CellData container to " << cellData);
  if(m_CellData != cellData)
    {
    m_CellData = cellData;
    this->Modified();
    }
}


/**
 * Access routine to get the cell data container.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::CellDataContainerPointer
itkMesh< TPixelType , TMeshType >
::GetCellDataContainer(void)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): returning CellData container of " << m_CellData );
  return m_CellData;
}


/**
 * Access routine to set the boundaries container for a given dimension.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetBoundariesContainer(int dimension, BoundariesContainer* boundaries)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): setting Boundaries[" << dimension
                << "] container to " << boundaries);
  if(m_Boundaries[dimension] != boundaries)
    {
    m_Boundaries[dimension] = boundaries;
    this->Modified();
    }
}



/**
 * Access routine to get the boundaries container for a given dimension.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::BoundariesContainerPointer
itkMesh< TPixelType , TMeshType >
::GetBoundariesContainer(int dimension)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): returning Boundaries[" << dimension
                << "] container of " << m_Boundaries[dimension]);
  return m_Boundaries[dimension];
}


/**
 * Access routine to set the boundary data container for a given dimension.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetBoundaryDataContainer(int dimension, BoundaryDataContainer* boundaryData)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): setting BoundaryData[" << dimension
                << "] container to " << boundaryData);
  if(m_BoundaryData[dimension] != boundaryData)
    {
    m_BoundaryData[dimension] = boundaryData;
    this->Modified();
    }
}


/**
 * Access routine to get the boundary data container for a given dimension.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::BoundaryDataContainerPointer
itkMesh< TPixelType , TMeshType >
::GetBoundaryDataContainer(int dimension)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): returning BoundaryData[" << dimension
                << "] container of " << m_BoundaryData[dimension]);
  return m_BoundaryData[dimension];
}


/**
 * Access routine to set the boundary assignment container for a given
 * dimension.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetBoundaryAssignmentsContainer(
  int dimension,
  BoundaryAssignmentsContainer* boundaryAssignments)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): setting BoundaryAssignments[" << dimension
                << "] container to " << boundaryAssignments);
  if(m_BoundaryAssignments[dimension] != boundaryAssignments)
    {
    m_BoundaryAssignments[dimension] = boundaryAssignments;
    this->Modified();
    }
}


/**
 * Access routine to get the boundary assignment container for a given
 * dimension.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::BoundaryAssignmentsContainerPointer
itkMesh< TPixelType , TMeshType >
::GetBoundaryAssignmentsContainer(int dimension)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): returning BoundaryAssignments[" << dimension
                << "] container of " << m_BoundaryAssignments[dimension]);
  return m_BoundaryAssignments[dimension];
}


/**
 * Assign a point to a point identifier.  If a spot for the point identifier
 * does not exist, it will be created automatically.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetPoint(PointIdentifier ptId, Point point)
{
  /**
   * Make sure a points container exists.
   */
  if(m_Points == NULL)
    {
    this->SetPointsContainer(PointsContainer::New());
    }

  /**
   * Insert the point into the container with the given identifier.
   */
  m_Points->SetElement(ptId, point);
}


/**
 * Check if a point exists for a given point identifier.  If a spot for
 * the point identifier exists, "point" is set, and true is returned.
 * Otherwise, false is returned, and "point" is not modified.
 * If "point" is NULL, then it is never set, but the existence of the point
 * is still returned.
 */
template <typename TPixelType, typename TMeshType>
bool
itkMesh< TPixelType , TMeshType >
::GetPoint(PointIdentifier ptId, Point* point) const
{
  /**
   * If the points container doesn't exist, then the point doesn't either.
   */
  if(m_Points == NULL)
    return false;
  
  /**
   * Ask the container if the point identifier exists.
   */
  return m_Points->GetElementIfIndexExists(ptId, point);
}


/**
 * Assign data to a point identifier.  If a spot for the point identifier
 * does not exist, it will be created automatically.  There is no check if
 * a point with the same identifier exists.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetPointData(PointIdentifier ptId, PixelType data)
{
  /**
   * Make sure a point data container exists.
   */
  if(m_PointData == NULL)
    {
    this->SetPointDataContainer(PointDataContainer::New());
    }

  /**
   * Insert the point data into the container with the given identifier.
   */
  m_PointData->SetElement(ptId, data);
}


/**
 * Check if point data exists for a given point identifier.  If a spot for
 * the point identifier exists, "data" is set, and true is returned.
 * Otherwise, false is returned, and "data" is not modified.
 * If "data" is NULL, then it is never set, but the existence of the point
 * data is still returned.
 */
template <typename TPixelType, typename TMeshType>
bool
itkMesh< TPixelType , TMeshType >
::GetPointData(PointIdentifier ptId, PixelType* data) const
{
  /**
   * If the point data container doesn't exist, then the point data doesn't
   * either.
   */
  if(m_PointData == NULL)
    return false;
  
  /**
   * Ask the container if the point identifier exists.
   */
  return m_PointData->GetElementIfIndexExists(ptId, data);
}


/**
 * Assign a cell to a cell identifier.  If a spot for the cell identifier
 * does not exist, it will be created automatically.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetCell(CellIdentifier cellId, Cell* cell)
{
  /**
   * Make sure a cells container exists.
   */
  if(m_Cells == NULL)
    {
    this->SetCellsContainer(CellsContainer::New());
    }
  
  /**
   * Insert the cell into the container with the given identifier.
   */
  m_Cells->SetElement(cellId, cell);
}


/**
 * Check if a cell exists for a given cell identifier.  If a spot for
 * the cell identifier exists, "cell" is set, and true is returned.
 * Otherwise, false is returned, and "cell" is not modified.
 * If "cell" is NULL, then it is never set, but the existence of the cell
 * is still returned.
 */
template <typename TPixelType, typename TMeshType>
bool
itkMesh< TPixelType , TMeshType >
::GetCell(CellIdentifier cellId, Cell::Pointer* cell) const
{
  /**
   * If the cells container doesn't exist, then the cell doesn't exist.
   */
  if(m_Cells == NULL)
    return false;
  
  /**
   * Ask the container if the cell identifier exists.
   */
  return m_Cells->GetElementIfIndexExists(cellId, cell);
}


/**
 * Assign data to a cell identifier.  If a spot for the cell identifier
 * does not exist, it will be created automatically.  There is no check if
 * a cell with the same identifier exists.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetCellData(CellIdentifier cellId, PixelType data)
{
  /**
   * Make sure a cell data container exists.
   */
  if(m_CellData == NULL)
    {
    this->SetCellDataContainer(CellDataContainer::New());
    }

  /**
   * Insert the cell data into the container with the given identifier.
   */
  m_CellData->SetElement(cellId, data);
}


/**
 * Check if cell data exists for a given cell identifier.  If a spot for
 * the cell identifier exists, "data" is set, and true is returned.
 * Otherwise, false is returned, and "data" is not modified.
 * If "data" is NULL, then it is never set, but the existence of the cell
 * data is still returned.
 */
template <typename TPixelType, typename TMeshType>
bool
itkMesh< TPixelType , TMeshType >
::GetCellData(CellIdentifier cellId, PixelType* data) const
{
  /**
   * If the cell data container doesn't exist, then the cell data doesn't
   * either.
   */
  if(m_CellData == NULL)
    return false;
  
  /**
   * Ask the container if the cell identifier exists.
   */
  return m_CellData->GetElementIfIndexExists(cellId, data);
}


/**
 * Assign a boundary to a boundary identifier.  If a spot for the boundary
 * identifier does not exist, it will be created automatically.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetBoundary(int dimension, BoundaryIdentifier boundaryId, Boundary* boundary)
{
  /**
   * Make sure a boundaries container exists.
   */
  if(m_Boundaries[dimension] == NULL)
    {
    this->SetBoundariesContainer(dimension, BoundariesContainer::New());
    }
  
  /**
   * Insert the boundary into the container with the given identifier.
   */
  m_Boundaries[dimension]->SetElement(boundaryId, boundary);
}


/**
 * Check if a boundary exists for a given boundary identifier.  If a spot for
 * the boundary identifier exists, "boundary" is set, and true is returned.
 * Otherwise, false is returned, and "boundary" is not modified.
 * If "boundary" is NULL, then it is never set, but the existence of the
 * boundary is still returned.
 */
template <typename TPixelType, typename TMeshType>
bool
itkMesh< TPixelType , TMeshType >
::GetBoundary(int dimension, BoundaryIdentifier boundaryId,
	      Boundary::Pointer* boundary) const
{
  /**
   * If the boundaries container doesn't exist, then the boundary
   * doesn't exist.
   */
  if(m_Boundaries[dimension] == NULL)
    return false;
  
  /**
   * Ask the container if the boundary identifier exists.
   */
  return m_Boundaries[dimension]->GetElementIfIndexExists(boundaryId, boundary);
}


/**
 * Assign data to a boundary identifier.  If a spot for the boundary identifier
 * does not exist, it will be created automatically.  There is no check if
 * a boundary with the same identifier exists.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetBoundaryData(int dimension, BoundaryIdentifier boundaryId, PixelType data)
{
  /**
   * Make sure a boundary data container exists.
   */
  if(m_BoundaryData[dimension] == NULL)
    {
    this->SetBoundaryDataContainer(dimension, BoundaryDataContainer::New());
    }

  /**
   * Insert the boundary data into the container with the given identifier.
   */
  m_BoundaryData[dimension]->SetElement(boundaryId, data);
}


/**
 * Check if boundary data exists for a given boundary identifier.  If a spot
 * for the boundary identifier exists, "data" is set, and true is returned.
 * Otherwise, false is returned, and "data" is not modified.
 * If "data" is NULL, then it is never set, but the existence of the boundary
 * data is still returned.
 */
template <typename TPixelType, typename TMeshType>
bool
itkMesh< TPixelType , TMeshType >
::GetBoundaryData(int dimension, BoundaryIdentifier boundaryId,
		  PixelType* data) const
{
  /**
   * If the boundary data container doesn't exist, then the boundary data doesn't
   * either.
   */
  if(m_BoundaryData[dimension] == NULL)
    return false;
  
  /**
   * Ask the container if the boundary identifier exists.
   */
  return m_BoundaryData[dimension]->GetElementIfIndexExists(boundaryId, data);
}


/**
 * Create an explicit boundary assignment.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetBoundaryAssignment(int dimension, CellIdentifier cellId,
			CellFeatureIdentifier featureId,
			BoundaryIdentifier boundaryId)
{
  BoundaryAssignmentIdentifier assignId(cellId, featureId);
    
  /**
   * Make sure a boundary assignment container exists for the given dimension.
   */
  if(m_BoundaryAssignments[dimension] == NULL)
    {
    this->SetBoundaryAssignmentsContainer(
      dimension, BoundaryAssignmentsContainer::New());
    }

  /**
   * Insert the boundary assignment into the container with the given
   * assignment identifier in the given dimension.
   */
  m_BoundaryAssignments[dimension]->SetElement(assignId, boundaryId);
}


/**
 * Check if an explicit boundary assignment exists.
 */
template <typename TPixelType, typename TMeshType>
bool
itkMesh< TPixelType , TMeshType >
::GetBoundaryAssignment(int dimension, CellIdentifier cellId,
			CellFeatureIdentifier featureId,
			BoundaryIdentifier* boundaryId) const
{
  BoundaryAssignmentIdentifier assignId(cellId, featureId);  

  /**
   * If the boundary assignments container for the given dimension doesn't
   * exist, then the boundary assignment doesn't either.
   */
  if(m_BoundaryAssignments[dimension] == NULL)
    return false;
  
  /**
   * Ask the container if the boundary assignment exists.
   */
  return m_BoundaryAssignments[dimension]->
    GetElementIfIndexExists(assignId, boundaryId);
}


/**
 * Remove an explicit boundary assignment if it exists.
 * Returns whether the assignment was found at all.
 */
template <typename TPixelType, typename TMeshType>
bool
itkMesh< TPixelType , TMeshType >
::RemoveBoundaryAssignment(int dimension, CellIdentifier cellId,
			   CellFeatureIdentifier featureId)
{
  BoundaryAssignmentIdentifier assignId(cellId, featureId);

  /**
   * If the boundary assignments container for the given dimension doesn't
   * exist, then the boundary assignment doesn't either.
   */
  if(m_BoundaryAssignments[dimension] == NULL)
    return false;
  
  /**
   * Ask the container if the boundary assignment exists, and delete it if
   * so.
   */
  if(m_BoundaryAssignments[dimension]->IndexExists(assignId))
    {
    m_BoundaryAssignments[dimension]->DeleteIndex(assignId);
    return true;
    }
  else return false;  
}


/**
 * Get the number of cell boundary features of the given topological dimension
 * on the cell with the given identifier.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::CellFeatureCount
itkMesh< TPixelType , TMeshType >
::GetNumberOfCellBoundaryFeatures(int dimension, CellIdentifier cellId) const
{
  /**
   * Make sure the cell container exists and contains the given cell Id.
   */
  if(m_Cells == NULL) return 0;
  if(!m_Cells->IndexExists(cellId)) return 0;
  
  /**
   * Ask the cell for its boundary count of the given dimension.
   */
  return m_Cells->GetElement(cellId)->GetNumberOfBoundaryFeatures(dimension);
}


/**
 * Get the boundary feature of the given dimension of the given cell
 * corresponding to the given feature identifier.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::Boundary::Pointer
itkMesh< TPixelType , TMeshType >
::GetCellBoundaryFeature(int dimension, CellIdentifier cellId,
			 CellFeatureIdentifier featureId) const
{
  /**
   * First check if the boundary has been explicitly assigned.
   */
  Boundary::Pointer boundary;
  if(GetAssignedBoundaryIfOneExists(dimension, cellId, featureId, &boundary))
    {
    return boundary;
    }
  
  /**
   * It was not explicitly assigned, so ask the cell to construct it.
   */
  if((m_Cells != NULL) && m_Cells->IndexExists(cellId))
    {
    return m_Cells->GetElement(cellId)->
      GetBoundaryFeature(dimension, featureId);
    }
  
  /**
   * The cell did not exist, so just give up.
   */
  return Cell::Pointer(NULL);
}


/**
 * Get the set of cells neighboring the given cell across the given boundary
 * feature.  Returns the number of neighbors found.  If cellList is not
 * NULL, the list of cell pointers is filled in with identifiers of the
 * neighboring cells.
 */
template <typename TPixelType, typename TMeshType>
unsigned long
itkMesh< TPixelType , TMeshType >
::GetBoundaryFeatureNeighbors(int dimension, CellIdentifier cellId,
			      CellFeatureIdentifier featureId,
			      std::list<CellIdentifier>* cellList)
{
  /**
   * Sanity check on mesh status.
   */
  if((m_Points == NULL) || (m_Cells == NULL))
    {
    /**
     * TODO: Throw EXCEPTION here.
     */
    return 0;
    }
  
  /**
   * First check if the boundary has been explicitly assigned.
   */
  Boundary::Pointer boundary;
  if(this->GetAssignedBoundaryIfOneExists(
    dimension, cellId, featureId, &boundary))
    {
    /**
     * Explicitly assigned boundary found.  Loop through its "UsingCells"
     * and put every one but the requested cell itself in the output
     * cell list.
     */
    if(cellList != NULL)
      {
      for(Boundary::UsingCellsContainerIterator usingCell = 
	    boundary->UsingCellsBegin() ;
	  usingCell != boundary->UsingCellsEnd() ; ++usingCell)
	{
	if((*usingCell) != cellId)
	  {
	  cellList->push_back(*usingCell);
	  }
	}
      }
    /**
     * The number of neighboring cells is the number of cells using the
     * boundary less one for the cell through which the request was made.
     */
    return (boundary->GetNumUsingCells()-1);
    }
  
  /**
   * An explicit assignment for the boundary was not found.  We use set
   * operations through point neighboring information to get the neighbors.
   * This requires that the CellLinks be built.
   */
  if(m_CellLinks == NULL)
    {
    this->BuildCellLinks();
    }
  else if((m_Points->GetMTime() > m_CellLinks->GetMTime()) ||
	  (m_Cells->GetMTime()  > m_CellLinks->GetMTime()))
    {
    this->BuildCellLinks();
    }
  /**
   * Cell links are up to date....
   */
}


/**
 * Check if there is an explicitly assigned boundary feature for the
 * given dimension and cell- and cell-feature-identifiers.  If there is,
 * a pointer to it is given back through "boundary" (if it isn't NULL) and
 * true is returned.  Otherwise, false is returned.
 */
template <typename TPixelType, typename TMeshType>
bool
itkMesh< TPixelType , TMeshType >
::GetAssignedBoundaryIfOneExists(int dimension, CellIdentifier cellId,
				 CellFeatureIdentifier featureId,
				 Boundary::Pointer* boundary) const
{
  if((m_BoundaryAssignments[dimension] != NULL) &&
     (m_Boundaries[dimension] != NULL))
    {
    BoundaryAssignmentIdentifier assignId(cellId, featureId);
    BoundaryIdentifier boundaryId;
    
    if(m_BoundaryAssignments[dimension]->
       GetElementIfIndexExists(assignId, &boundaryId))
      {
      return m_Boundaries[dimension]->
	GetElementIfIndexExists(boundaryId, boundary);
      }
    }
  
  /**
   * An explicitly assigned boundary was not found.
   */
  return false;
}


/**
 * Dynamically build the links from points back to their using cells.  This
 * information is stored in the cell links container, not in the points.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::BuildCellLinks(void)
{
  /**
   * Make sure the cell links container exists.
   */
  if(m_CellLinks == NULL)
    {
    this->SetCellLinksContainer(CellLinksContainer::New());
    }
  
}


/******************************************************************************
 * PROTECTED METHOD DEFINITIONS
 *****************************************************************************/

/**
 * A protected default constructor allows the New() routine to create an
 * instance of itkMesh.  All the containers are initialized to non-existent.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >
::itkMesh():
  m_Points(NULL),
  m_PointData(NULL),
  m_Cells(NULL),
  m_CellData(NULL),
  m_CellLinks(NULL),
  m_Boundaries(BoundariesContainerVector(MaxTopologicalDimension)),
  m_BoundaryData(BoundaryDataContainerVector(MaxTopologicalDimension)),
  m_BoundaryAssignments(BoundaryAssignmentsContainerVector(MaxTopologicalDimension))
{}


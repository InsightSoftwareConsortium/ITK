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
 *
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
itkMesh< TPixelType , TMeshType >::PointsContainer::Pointer
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
itkMesh< TPixelType , TMeshType >::PointDataContainer::Pointer
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
itkMesh< TPixelType , TMeshType >::CellLinksContainer::Pointer
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
itkMesh< TPixelType , TMeshType >::CellsContainer::Pointer
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
itkMesh< TPixelType , TMeshType >::CellDataContainer::Pointer
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
itkMesh< TPixelType , TMeshType >::BoundariesContainer::Pointer
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
itkMesh< TPixelType , TMeshType >::BoundaryDataContainer::Pointer
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
itkMesh< TPixelType , TMeshType >::BoundaryAssignmentsContainer::Pointer
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
    this->SetPointsContainer(this->ConstructDefaultPoints());
    }

  /**
   * Insert the point into the container with the given identifier.
   */
  (*m_Points)[ptId] = point;
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
  if(m_Points->IndexExists(ptId))
    {
    if(point != NULL)
      {
      *point = (*m_Points)[ptId];
      }
    
    return true;
    }
  else return false;
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
    this->SetPointDataContainer(this->ConstructDefaultPointData());
    }

  /**
   * Insert the point data into the container with the given identifier.
   */
  (*m_PointData)[ptId] = data;
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
  if(m_PointData->IndexExists(ptId))
    {
    if(data != NULL)
      {
      *data = (*m_PointData)[ptId];
      }
    return true;
    }
  else return false;
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
    this->SetCellsContainer(this->ConstructDefaultCells());
    }
  
  /**
   * Insert the cell into the container with the given identifier.
   */
  (*m_Cells)[cellId] = cell;
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
::GetCell(CellIdentifier cellId, CellPointer* cell) const
{
  /**
   * If the cells container doesn't exist, then the cell doesn't exist.
   */
  if(m_Cells == NULL)
    return false;
  
  /**
   * Ask the container if the cell identifier exists.
   */
  if(m_Cells->IndexExists(cellId))
    {
    if(cell != NULL)
      {
      *cell = (*m_Cells)[cellId];
      }
    return true;
    }
  else return false;
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
    this->SetCellDataContainer(this->ConstructDefaultCellData());
    }

  /**
   * Insert the cell data into the container with the given identifier.
   */
  (*m_CellData)[cellId] = data;
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
  if(m_CellData->IndexExists(cellId))
    {
    if(data != NULL)
      {
      *data = (*m_CellData)[cellId];
      }
    return true;
    }
  else return false;
}


/**
 * Assign a boundary to a boundary identifier.  If a spot for the boundary
 * identifier does not exist, it will be created automatically.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetBoundary(BoundaryIdentifier boundaryId, Boundary* boundary)
{
  /**
   * Make sure a boundaries container exists.
   */
  if(m_Boundaries == NULL)
    {
    this->SetBoundariesContainer(this->ConstructDefaultBoundaries());
    }
  
  /**
   * Insert the boundary into the container with the given identifier.
   */
  (*m_Boundaries)[boundaryId] = boundary;
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
::GetBoundary(BoundaryIdentifier boundaryId, BoundaryPointer* boundary) const
{
  /**
   * If the boundaries container doesn't exist, then the boundary
   * doesn't exist.
   */
  if(m_Boundaries == NULL)
    return false;
  
  /**
   * Ask the container if the boundary identifier exists.
   */
  if(m_Boundaries->IndexExists(boundaryId))
    {
    if(boundary != NULL)
      {
      *boundary = (*m_Boundaries)[boundaryId];
      }
    return true;
    }
  else return false;
}


/**
 * Assign data to a boundary identifier.  If a spot for the boundary identifier
 * does not exist, it will be created automatically.  There is no check if
 * a boundary with the same identifier exists.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetBoundaryData(BoundaryIdentifier boundaryId, PixelType data)
{
  /**
   * Make sure a boundary data container exists.
   */
  if(m_BoundaryData == NULL)
    {
    this->SetBoundaryDataContainer(this->ConstructDefaultBoundaryData());
    }

  /**
   * Insert the boundary data into the container with the given identifier.
   */
  (*m_BoundaryData)[boundaryId] = data;
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
::GetBoundaryData(BoundaryIdentifier boundaryId, PixelType* data) const
{
  /**
   * If the boundary data container doesn't exist, then the boundary data doesn't
   * either.
   */
  if(m_BoundaryData == NULL)
    return false;
  
  /**
   * Ask the container if the boundary identifier exists.
   */
  if(m_BoundaryData->IndexExists(boundaryId))
    {
    if(data != NULL)
      {
      *data = (*m_BoundaryData)[boundaryId];
      }
    return true;
    }
  else return false;
}


/**
 * Create an explicit boundary assignment.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetBoundaryAssignment(BoundaryAssignmentId assignId, int dimension,
			BoundaryIdentifier boundaryId)
{
  /**
   * Make sure a boundary assignment container exists for the given dimension.
   */
  if(m_BoundaryAssignemnts[dimension] == NULL)
    {
    this->SetBoundaryAssignmentsContainer(
      dimension, this->ConstructDefaultBoundaryAssignments());
    }

  /**
   * Insert the boundary assignment into the container with the given
   * assignment identifier in the given dimension.
   */
  (*(m_BoundaryAssignments[dimension]))[assignId] = boundaryId;
}


/**
 * Check if an explicit boundary assignment exists.
 */
template <typename TPixelType, typename TMeshType>
bool
itkMesh< TPixelType , TMeshType >
::GetBoundaryAssignment(BoundaryAssignmentId assignId, int dimension,
			BoundaryIdentifier* boundaryId) const
{
  /**
   * If the boundary assignments container for the given dimension doesn't
   * exist, then the boundary assignment doesn't either.
   */
  if(m_BoundaryAssignments[dimension] == NULL)
    return false;
  
  /**
   * Ask the container if the boundary assignment exists.
   */
  if(m_BoundaryAssignment->IndexExists(assignId))
    {
    if(boundaryId != NULL)
      {
      *boundaryId = (*(m_BoundaryAssignments[dimension]))[assignId];
      }
    return true;
    }
  else return false;
}


/**
 * Define a front-end to the SetBoundaryAssignment() function that does not
 * require a BoundaryAssignmentId, but instead takes its indivudal components
 * as parameters.
 */
template <typename TPixelType, typename TMeshType>
void
itkMesh< TPixelType , TMeshType >
::SetBoundaryAssignment(CellIdentifier cellId, CellFeatureId featureId,
			int dimension, BoundaryIdentifier boundaryId)
{
  this->SetBoundaryAssignment(BoundaryAssignmentId(cellId, featureId),
			      dimension, boundaryId);
}


/**
 * Define a front-end to the GetBoundaryAssignment() function that does not
 * require a BoundaryAssignmentId, but instead takes its indivudal components
 * as parameters.
 */
template <typename TPixelType, typename TMeshType>
bool
itkMesh< TPixelType , TMeshType >
::GetBoundaryAssignment(CellIdentifier cellId, CellFeatureId featureId,
			int dimension, BoundaryIdentifier* boundaryId) const
{
  return this->GetBoundaryAssignment(BoundaryAssignmentId(cellId, featureId),
				     dimension, boundaryId);
}


/**
 * Remove an explicit boundary assignment if it exists.
 * Returns whether the assignment was found at all.
 */
template <typename TPixelType, typename TMeshType>
bool
itkMesh< TPixelType , TMeshType >
::RemoveBoundaryAssignment(BoundaryAssignmentId assignId, int dimension)
{
  /**
   * If the boundary assignments container for the given dimension doesn't
   * exist, then the boundary assignment doesn't either.
   */
  if(m_BoundaryAssignments[dimension] == NULL)
    return false;
  
  /**
   * Ask the container if the boundary assignment exists.
   */
  if(m_BoundaryAssignment->IndexExists(assignId))
    {
    m_BoundaryAssignments[dimension]->DeleteIndex(assignId);
    return true;
    }
  else return false;  
}


/**
 * Define a front-end to the RemoveBoundaryAssignment() function that does not
 * require a BoundaryAssignmentId, but instead takes its indivudal components
 * as parameters.
 */
template <typename TPixelType, typename TMeshType>
bool
itkMesh< TPixelType , TMeshType >
::RemoveBoundaryAssignment(CellIdentifier cellId, CellFeatureId featureId,
			   int dimension)
{
  return this->RemoveBoundaryAssignments(
    BoundaryAssignmentId(cellId, featureId), dimension);
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


/**
 * Define the default containers for each internal container.
 */
#define DefaultPointsContainerForIntegralIdentifiers          itkAutoVectorContainer< PointIdentifier , Point >
#define DefaultPointsContainerForNonIntegralIdentifiers       itkMapContainer< PointIdentifier , Point >
#define DefaultPointDataContainerForIntegralIdentifiers       itkAutoVectorContainer< PointIdentifier , PixelType >
#define DefaultPointDataContainerForNonIntegralIdentifiers    itkMapContainer< PointIdentifier , PixelType >
#define DefaultCellLinksContainerForIntegralIdentifiers       itkAutoVectorContainer< PointIdentifier , PointCellLinksContainerPointer >
#define DefaultCellLinksContainerForNonIntegralIdentifiers    itkMapContainer< PointIdentifier , PointCellLinksContainerPointer >
#define DefaultCellsContainerForIntegralIdentifiers           itkAutoVectorContainer< CellIdentifier , CellPointer >
#define DefaultCellsContainerForNonIntegralIdentifiers        itkMapContainer< CellIdentifier , CellPointer >
#define DefaultCellDataContainerForIntegralIdentifiers        itkAutoVectorContainer< CellIdentifier , PixelType >
#define DefaultCellDataContainerForNonIntegralIdentifiers     itkMapContainer< CellIdentifier , PixelType >
#define DefaultBoundariesContainerForIntegralIdentifiers      itkAutoVectorContainer< BoundaryIdentifier , BoundaryPointer >
#define DefaultBoundariesContainerForNonIntegralIdentifiers   itkMapContainer< BoundaryIdentifier , BoundaryPointer >
#define DefaultBoundaryDataContainerForIntegralIdentifiers    itkAutoVectorContainer< BoundaryIdentifier , PixelType >
#define DefaultBoundaryDataContainerForNonIntegralIdentifiers itkMapContainer< BoundaryIdentifier , PixelType >
#define DefaultBoundaryAssignmentsContainer                   itkMapContainer< BoundaryAssignmentId , BoundaryIdentifier >


/**
 * Points container default construction.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::PointsContainer::Pointer
itkMesh< TPixelType , TMeshType >
::ConstructDefaultPoints(void)
{
  if(itkIdentifierTraits< PointIdentifier >::IsIntegralType)
    return PointsContainer::Pointer(DefaultPointsContainerForIntegralIdentifiers::New());
  else
    return PointsContainer::Pointer(DefaultPointsContainerForNonIntegralIdentifiers::New());
}


/**
 * PointData container default construction.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::PointDataContainer::Pointer
itkMesh< TPixelType , TMeshType >
::ConstructDefaultPointData(void)
{
  if(itkIdentifierTraits< PointIdentifier >::IsIntegralType)
    return PointDataContainer::Pointer(DefaultPointDataContainerForIntegralIdentifiers::New());
  else
    return PointDataContainer::Pointer(DefaultPointDataContainerForNonIntegralIdentifiers::New());
}


/**
 * CellLinks container default construction.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::CellLinksContainer::Pointer
itkMesh< TPixelType , TMeshType >
::ConstructDefaultCellLinks(void)
{
  if(itkIdentifierTraits< PointIdentifier >::IsIntegralType)
    return CellLinksContainer::Pointer(DefaultCellLinksContainerForIntegralIdentifiers::New());
  else
    return CellLinksContainer::Pointer(DefaultCellLinksContainerForNonIntegralIdentifiers::New());
}


/**
 * Cells container default construction.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::CellsContainer::Pointer
itkMesh< TPixelType , TMeshType >
::ConstructDefaultCells(void)
{
  if(itkIdentifierTraits< CellIdentifier >::IsIntegralType)
    return CellsContainer::Pointer(DefaultCellsContainerForIntegralIdentifiers::New());
  else
    return CellsContainer::Pointer(DefaultCellsContainerForNonIntegralIdentifiers::New());
}


/**
 * CellData container default construction.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::CellDataContainer::Pointer
itkMesh< TPixelType , TMeshType >
::ConstructDefaultCellData(void)
{
  if(itkIdentifierTraits< CellIdentifier >::IsIntegralType)
    return CellDataContainer::Pointer(DefaultCellDataContainerForIntegralIdentifiers::New());
  else
    return CellDataContainer::Pointer(DefaultCellDataContainerForNonIntegralIdentifiers::New());
}


/**
 * Boundaries container default construction.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::BoundariesContainer::Pointer
itkMesh< TPixelType , TMeshType >
::ConstructDefaultBoundaries(void)
{
  if(itkIdentifierTraits< BoundaryIdentifier >::IsIntegralType)
    return BoundariesContainer::Pointer(DefaultBoundariesContainerForIntegralIdentifiers::New());
  else
    return BoundariesContainer::Pointer(DefaultBoundariesContainerForNonIntegralIdentifiers::New());
}


/**
 * BoundaryData container default construction.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::BoundaryDataContainer::Pointer
itkMesh< TPixelType , TMeshType >
::ConstructDefaultBoundaryData(void)
{
  if(itkIdentifierTraits< BoundaryIdentifier >::IsIntegralType)
    return BoundaryDataContainer::Pointer(DefaultBoundaryDataContainerForIntegralIdentifiers::New());
  else
    return BoundaryDataContainer::Pointer(DefaultBoundaryDataContainerForNonIntegralIdentifiers::New());
}


/**
 * BoundaryAssignments container default construction.
 */
template <typename TPixelType, typename TMeshType>
itkMesh< TPixelType , TMeshType >::BoundaryAssignmentsContainer::Pointer
itkMesh< TPixelType , TMeshType >
::ConstructDefaultBoundaryAssignments(void)
{
  return BoundaryAssignmentsContainer::Pointer(DefaultBoundaryAssignmentsContainer::New());
}


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
    m_Points = ConstructDefaultPoints();
    itkDebugMacro(<< this->GetClassName() << " (" << this
                  << "): setting Points container to " << m_Points);
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
    *point = (*m_Points)[ptId];
    return true;
    }
  else return false;
}


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
    m_Cells = ConstructDefaultCells();
    itkDebugMacro(<< this->GetClassName() << " (" << this
                  << "): setting Cellss container to " << m_Cells);
    }
  
  /**
   * Insert the cell into the container with the given identifier.
   */
  (*m_Cells)[cellId] = cell;
}


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
    *cell = (*m_Cells)[cellId];
    return true;
    }
  else return false;
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
#define DefaultBoundaryAssignmentsContainer                   itkMapContainer< BoundaryAssignmentID , BoundaryIdentifier >


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


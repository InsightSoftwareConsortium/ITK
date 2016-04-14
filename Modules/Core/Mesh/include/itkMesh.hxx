/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkMesh_hxx
#define itkMesh_hxx

#include "itkMesh.h"
#include "itkProcessObject.h"
#include <algorithm>
#include <iterator>

namespace itk
{
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number Of Points: "
     << ( ( this->m_PointsContainer.GetPointer() ) ?  this->m_PointsContainer->Size() : 0 ) << std::endl;
  os << indent << "Number Of Cell Links: "
     << ( ( m_CellLinksContainer ) ?  m_CellLinksContainer->Size() : 0 ) << std::endl;
  os << indent << "Number Of Cells: "
     << ( ( m_CellsContainer ) ?  m_CellsContainer->Size() : 0 ) << std::endl;
  os << indent << "Cell Data Container pointer: "
     << ( ( m_CellDataContainer ) ?  m_CellDataContainer.GetPointer() : ITK_NULLPTR ) << std::endl;
  os << indent << "Size of Cell Data Container: "
     << ( ( m_CellDataContainer ) ?  m_CellDataContainer->Size() : 0 ) << std::endl;
  os << indent << "Number of explicit cell boundary assignments: "
     << static_cast< CellIdentifier >( m_BoundaryAssignmentsContainers.size() ) << std::endl;
  os << indent << "CellsAllocationMethod: "
     << m_CellsAllocationMethod << std::endl;
}

/**
 * Access routine to set the cell links container.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::SetCellLinks(CellLinksContainer *cellLinks)
{
  itkDebugMacro("setting CellLinks container to " << cellLinks);
  if ( m_CellLinksContainer != cellLinks )
    {
    m_CellLinksContainer = cellLinks;
    this->Modified();
    }
}

/**
 * Access routines to get the cell links container.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename Mesh< TPixelType, VDimension, TMeshTraits >::CellLinksContainer *
Mesh< TPixelType, VDimension, TMeshTraits >
::GetCellLinks()
{
  itkDebugMacro("returning CellLinks container of "
                << m_CellLinksContainer);
  return m_CellLinksContainer;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
const typename Mesh< TPixelType, VDimension, TMeshTraits >::CellLinksContainer *
Mesh< TPixelType, VDimension, TMeshTraits >
::GetCellLinks() const
{
  itkDebugMacro("returning CellLinks container of "
                << m_CellLinksContainer);
  return m_CellLinksContainer;
}

/**
 * Access routine to set the cells container.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::SetCells(CellsContainer *cells)
{
  itkDebugMacro("setting Cells container to " << cells);
  if ( m_CellsContainer != cells )
    {
    this->ReleaseCellsMemory();
    m_CellsContainer = cells;
    this->Modified();
    }
}

/**
 * Access routines to get the cells container.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename Mesh< TPixelType, VDimension, TMeshTraits >::CellsContainer *
Mesh< TPixelType, VDimension, TMeshTraits >
::GetCells()
{
  itkDebugMacro("returning Cells container of " << m_CellsContainer);
  return m_CellsContainer;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
const typename Mesh< TPixelType, VDimension, TMeshTraits >::CellsContainer *
Mesh< TPixelType, VDimension, TMeshTraits >
::GetCells() const
{
  itkDebugMacro("returning Cells container of " << m_CellsContainer);
  return m_CellsContainer;
}

/**
 * Access routine to set the cell data container.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::SetCellData(CellDataContainer *cellData)
{
  itkDebugMacro("setting CellData container to " << cellData);
  if ( m_CellDataContainer != cellData )
    {
    m_CellDataContainer = cellData;
    this->Modified();
    }
}

/**
 * Access routines to get the cell data container.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename Mesh< TPixelType, VDimension, TMeshTraits >::CellDataContainer *
Mesh< TPixelType, VDimension, TMeshTraits >
::GetCellData()
{
  itkDebugMacro("returning CellData container of "
                << m_CellDataContainer);
  return m_CellDataContainer;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
const typename Mesh< TPixelType, VDimension, TMeshTraits >::CellDataContainer *
Mesh< TPixelType, VDimension, TMeshTraits >
::GetCellData() const
{
  itkDebugMacro("returning CellData container of "
                << m_CellDataContainer);
  return m_CellDataContainer;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
const typename Mesh< TPixelType, VDimension, TMeshTraits >::BoundingBoxType *
Mesh< TPixelType, VDimension, TMeshTraits >
::GetBoundingBox(void) const
{
  m_BoundingBox->SetPoints( this->m_PointsContainer.GetPointer() );
  if ( m_BoundingBox->GetMTime() > this->GetMTime() )
    {
    m_BoundingBox->ComputeBoundingBox();
    }
  return m_BoundingBox;
}

/**
 * Access routine to set the boundary assignment container for a given
 * dimension.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::SetBoundaryAssignments(
  int dimension,
  BoundaryAssignmentsContainer *boundaryAssignments)
{
  itkDebugMacro("setting BoundaryAssignments[" << dimension
                                               << "] container to " << boundaryAssignments);
  if ( m_BoundaryAssignmentsContainers[dimension] != boundaryAssignments )
    {
    m_BoundaryAssignmentsContainers[dimension] = boundaryAssignments;
    this->Modified();
    }
}

/**
 * Access routines to get the boundary assignment container for a given
 * dimension.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename Mesh< TPixelType, VDimension, TMeshTraits >::BoundaryAssignmentsContainerPointer
Mesh< TPixelType, VDimension, TMeshTraits >
::GetBoundaryAssignments(int dimension)
{
  itkDebugMacro("returning BoundaryAssignments[" << dimension
                                                 << "] container of "
                                                 << m_BoundaryAssignmentsContainers[dimension]);
  return m_BoundaryAssignmentsContainers[dimension];
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
const typename Mesh< TPixelType, VDimension, TMeshTraits >
::BoundaryAssignmentsContainerPointer
Mesh< TPixelType, VDimension, TMeshTraits >
::GetBoundaryAssignments(int dimension) const
{
  itkDebugMacro("returning BoundaryAssignments[" << dimension
                                                 << "] container of "
                                                 << m_BoundaryAssignmentsContainers[dimension]);
  return m_BoundaryAssignmentsContainers[dimension];
}

/**
 * Assign a cell to a cell identifier.  If a spot for the cell identifier
 * does not exist, it will be created automatically.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::SetCell(CellIdentifier cellId, CellAutoPointer & cellPointer)
{
  /**
   * Make sure a cells container exists.
   */
  if ( !m_CellsContainer )
    {
    this->SetCells( CellsContainer::New() );
    }

  /**
   * Insert the cell into the container with the given identifier.
   */
  m_CellsContainer->InsertElement( cellId, cellPointer.ReleaseOwnership() );
}

/**
 * Check if a cell exists for a given cell identifier.  If a spot for
 * the cell identifier exists, "cell" is set, and true is returned.
 * Otherwise, false is returned, and "cell" is not modified.
 * If "cell" is ITK_NULLPTR, then it is never set, but the existence of the cell
 * is still returned.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
bool
Mesh< TPixelType, VDimension, TMeshTraits >
::GetCell(CellIdentifier cellId, CellAutoPointer & cellPointer) const
{
  /**
   * If the cells container doesn't exist, then the cell doesn't exist.
   */
  if ( m_CellsContainer.IsNull() )
    {
    cellPointer.Reset();
    return false;
    }

  /**
   * Ask the container if the cell identifier exists.
   */
  CellType * cellptr = ITK_NULLPTR;
  const bool found = m_CellsContainer->GetElementIfIndexExists(cellId, &cellptr);
  if ( found )
    {
    cellPointer.TakeNoOwnership(cellptr);
    }
  else
    {
    cellPointer.Reset();
    }

  return found;
}

/**
 * Assign data to a cell identifier.  If a spot for the cell identifier
 * does not exist, it will be created automatically.  There is no check if
 * a cell with the same identifier exists.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::SetCellData(CellIdentifier cellId, CellPixelType data)
{
  /**
   * Make sure a cell data container exists.
   */
  if ( !m_CellDataContainer )
    {
    this->SetCellData( CellDataContainer::New() );
    }

  /**
   * Insert the cell data into the container with the given identifier.
   */
  m_CellDataContainer->InsertElement(cellId, data);
}

/**
 * Check if cell data exists for a given cell identifier.  If a spot for
 * the cell identifier exists, "data" is set, and true is returned.
 * Otherwise, false is returned, and "data" is not modified.
 * If "data" is ITK_NULLPTR, then it is never set, but the existence of the cell
 * data is still returned.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
bool
Mesh< TPixelType, VDimension, TMeshTraits >
::GetCellData(CellIdentifier cellId, CellPixelType *data) const
{
  /**
   * If the cell data container doesn't exist, then the cell data doesn't
   * either.
   */
  if ( !m_CellDataContainer )
    {
    return false;
    }

  /**
   * Ask the container if the cell identifier exists.
   */
  return m_CellDataContainer->GetElementIfIndexExists(cellId, data);
}

/**
 * Explicitly assign boundaryId as a part of the boundary of cellId.
 * The identifiers boundaryId and cellId must identify cell objects
 * already in the mesh.  The dimension of boundaryId must be specified
 * by 'dimension', and a unique CellFeatureIdentifier featureId must be
 * assigned for each distinct boundary feature of a given dimension.
 * CellFeatureIdentifier is equivalent to IdentifierType by default,
 * and will not typically need to be changed.  The UsingCells list of
 * boundaryId is automatically updated to include cellId.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::SetBoundaryAssignment(int dimension, CellIdentifier cellId,
                        CellFeatureIdentifier featureId,
                        CellIdentifier boundaryId)
{
  BoundaryAssignmentIdentifier assignId(cellId, featureId);

  /**
   * Make sure a boundary assignment container exists for the given dimension.
   */
  if ( !m_BoundaryAssignmentsContainers[dimension] )
    {
    this->SetBoundaryAssignments(
      dimension, BoundaryAssignmentsContainer::New() );
    }

  /**
   * Insert the boundary assignment into the container with the given
   * assignment identifier in the given dimension.
   */
  m_BoundaryAssignmentsContainers[dimension]->InsertElement(assignId, boundaryId);

  /**
   * Add cellId to the UsingCells list of boundaryId.
   */
  CellAutoPointer boundaryCell;
  this->GetCell(boundaryId, boundaryCell);
  boundaryCell->AddUsingCell(cellId);
}

/**
 * Check if an explicit boundary assignment exists.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
bool
Mesh< TPixelType, VDimension, TMeshTraits >
::GetBoundaryAssignment(int dimension, CellIdentifier cellId,
                        CellFeatureIdentifier featureId,
                        CellIdentifier *boundaryId) const
{
  BoundaryAssignmentIdentifier assignId(cellId, featureId);

  /**
   * If the boundary assignments container for the given dimension doesn't
   * exist, then the boundary assignment doesn't either.
   */
  if ( !m_BoundaryAssignmentsContainers[dimension] )
    {
    return false;
    }

  /**
   * Ask the container if the boundary assignment exists.
   */
  return m_BoundaryAssignmentsContainers[dimension]->
         GetElementIfIndexExists(assignId, boundaryId);
}

/**
 * Remove an explicit boundary assignment if it exists.
 * Returns whether the assignment was found at all.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
bool
Mesh< TPixelType, VDimension, TMeshTraits >
::RemoveBoundaryAssignment(int dimension, CellIdentifier cellId,
                           CellFeatureIdentifier featureId)
{
  BoundaryAssignmentIdentifier assignId(cellId, featureId);

  /**
   * If the boundary assignments container for the given dimension doesn't
   * exist, then the boundary assignment doesn't either.
   */
  if ( !m_BoundaryAssignmentsContainers[dimension] )
    {
    return false;
    }

  /**
   * Ask the container if the boundary assignment exists, and delete it if
   * so.
   */
  if ( m_BoundaryAssignmentsContainers[dimension]->IndexExists(assignId) )
    {
    m_BoundaryAssignmentsContainers[dimension]->DeleteIndex(assignId);
    return true;
    }
  else { return false; }
}

/**
 * Get the number of cell boundary features of the given topological dimension
 * on the cell with the given identifier.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename Mesh< TPixelType, VDimension, TMeshTraits >::CellFeatureCount
Mesh< TPixelType, VDimension, TMeshTraits >
::GetNumberOfCellBoundaryFeatures(int dimension, CellIdentifier cellId) const
{
  /**
   * Make sure the cell container exists and contains the given cell Id.
   */
  if ( !m_CellsContainer ) { return 0; }
  if ( !m_CellsContainer->IndexExists(cellId) ) { return 0; }

  /**
   * Ask the cell for its boundary count of the given dimension.
   */
  return m_CellsContainer->GetElement(cellId)->
         GetNumberOfBoundaryFeatures(dimension);
}

/**
 * Copy the geometric and topological structure of the given input mesh.
 * The copying is done via reference counting.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::PassStructure(Self *)
{
  // IMPLEMENT ME
}

/**
 * Get the number of cells in the CellsContainer.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename Mesh< TPixelType, VDimension, TMeshTraits >::CellIdentifier
Mesh< TPixelType, VDimension, TMeshTraits >
::GetNumberOfCells() const
{
  if ( !m_CellsContainer )
    {
    return 0;
    }
  else
    {
    return m_CellsContainer->Size();
    }
}

/**
 * Restore the Mesh to its initial state.  Useful for data pipeline updates
 * without memory re-allocation.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::Initialize()
{
  itkDebugMacro("Mesh Initialize method ");
  Superclass::Initialize();

  this->ReleaseCellsMemory();

  m_CellsContainer = ITK_NULLPTR;
  m_CellDataContainer = ITK_NULLPTR;
  m_CellLinksContainer = ITK_NULLPTR;
}

/**
 * Get the boundary feature of the given dimension of the given cell
 * corresponding to the given feature identifier.  If the boundary
 * feature has been explicitly assigned, then \a boundary will be left
 * pointing to the appropriate cell in the mesh.  If the boundary has
 * not been explicitly assigned, then a boundary cell will be
 * constructed and placed in \a boundary.  The constructed cell will
 * not be added to the mesh or somehow cached.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
bool
Mesh< TPixelType, VDimension, TMeshTraits >
::GetCellBoundaryFeature(int dimension, CellIdentifier cellId,
                         CellFeatureIdentifier featureId,
                         CellAutoPointer & boundary) const
{
  /**
   * First check if the boundary has been explicitly assigned.
   */
  if ( GetAssignedCellBoundaryIfOneExists(dimension, cellId, featureId, boundary) )
    {
    return true;
    }

  /**
   * It was not explicitly assigned, so ask the cell to construct it.
   * This will be a geometric copy of the actual boundary feature, not
   * a pointer to an actual cell in the mesh.
   */
  if ( ( !m_CellsContainer.IsNull() ) && m_CellsContainer->IndexExists(cellId) )
    {
    // Don't take ownership
    CellType *thecell = m_CellsContainer->GetElement(cellId);
    if ( thecell->GetBoundaryFeature(dimension, featureId, boundary) )
      {
      return true;
      }
    else
      {
      boundary.Reset();
      return false;
      }
    }

  /**
   * The cell did not exist, so just give up.
   */
  boundary.Reset();

  return false;
}

/**
 * Get the set of cells neighboring the given cell across the given boundary
 * feature.  Returns the number of neighbors found.  If cellSet is not
 * ITK_NULLPTR, the set of cell pointers is filled in with identifiers of the
 * neighboring cells.
 *
 * NOTE: We would like to change this to use an "output iterator"
 * (in STL fashion) instead of an actual container to return the neighbor
 * identifiers.  This requires templated member support by the compiler,
 * though, and we are not sure how wide-spread this support is.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename Mesh< TPixelType, VDimension, TMeshTraits >::CellIdentifier
Mesh< TPixelType, VDimension, TMeshTraits >
::GetCellBoundaryFeatureNeighbors(int dimension, CellIdentifier cellId,
                                  CellFeatureIdentifier featureId,
                                  std::set< CellIdentifier > *cellSet)
{
  /**
   * Sanity check on mesh status.
   */
  if ( !this->m_PointsContainer || !m_CellsContainer
       || ( !m_CellsContainer->IndexExists(cellId) ) )
    {
    /**
     * TODO: Throw EXCEPTION here?
     */
    return 0;
    }

  /**
   * First check if the boundary has been explicitly assigned.
   */
  CellAutoPointer boundary;
  if ( this->GetAssignedCellBoundaryIfOneExists(
         dimension, cellId, featureId, boundary) )
    {
    /**
     * Explicitly assigned boundary found.  Loop through its UsingCells,
     * and put them in the output set except for the cell through which the
     * request was made.  First we empty the output set.
     */
    if ( cellSet != ITK_NULLPTR )
      {
      cellSet->erase( cellSet->begin(), cellSet->end() );

      typename CellType::UsingCellsContainerIterator usingCell;
      for ( usingCell = boundary->UsingCellsBegin();
            usingCell != boundary->UsingCellsEnd(); ++usingCell )
        {
        if ( ( *usingCell ) != cellId )
          {
          cellSet->insert(*usingCell);
          }
        }
      }
    /**
     * The number of neighboring cells is the number of cells using the
     * boundary less one for the cell through which the request was made.
     */
    return ( boundary->GetNumberOfUsingCells() - 1 );
    }

  /**
   * An explicit assignment for the boundary was not found.  We use set
   * operations through point neighboring information to get the neighbors.
   * This requires that the CellLinks be built.
   */
  if ( !m_CellLinksContainer )
    {
    this->BuildCellLinks();
    }
  else if ( ( this->m_PointsContainer->GetMTime() > m_CellLinksContainer->GetMTime() )
            || ( m_CellsContainer->GetMTime()  > m_CellLinksContainer->GetMTime() ) )
    {
    this->BuildCellLinks();
    }

  /**
   * Cell links are up to date. We can proceed with the set operations.
   * We need to intersect the CellLinks sets for each point on the boundary
   * feature.
   */

  /**
   * First, ask the cell to construct the boundary feature so we can look
   * at its points.
   */
  m_CellsContainer->GetElement(cellId)
  ->GetBoundaryFeature(dimension, featureId, boundary);

  /**
   * Now get the cell links for the first point.  Also allocate a second set
   * for temporary storage during set intersections below.
   */
  typename CellType::PointIdConstIterator pointId = boundary->PointIdsBegin();
  PointCellLinksContainer *currentCells =
    new PointCellLinksContainer( m_CellLinksContainer->GetElement(*pointId++) );
  PointCellLinksContainer *tempCells = new PointCellLinksContainer();

  /**
   * Next, loop over the other points, and intersect their cell links with
   * the current result.  We maintain "currentCells" as a pointer to the
   * current cell set instead of a set itself to prevent an extra copy of
   * the temporary set after each intersection.
   */
  while ( pointId != boundary->PointIdsEnd() )
    {
    /**
     * Clean out temporary cell set from previous iteration.
     */
    tempCells->erase( tempCells->begin(), tempCells->end() );

    /**
     * Perform the intersection.
     */
    std::set_intersection( m_CellLinksContainer->CreateElementAt(*pointId).begin(),
                           m_CellLinksContainer->CreateElementAt(*pointId).end(),
                           currentCells->begin(),
                           currentCells->end(),
                           std::inserter( *tempCells, tempCells->begin() ) );

    /**
     * Switch the cell set pointers to make the intersection result the
     * current set.
     */
    std::swap(currentCells, tempCells);

    /**
     * Move on to the next point.
     */
    ++pointId;
    }

  /**
   * Don't need the second set anymore.
   */
  delete tempCells;

  /** delete the boundary feature added as a temporary auxiliar object,
      being an AutoPointer it will release memory when going out of scope */

  /**
   * Now we have a set of all the cells which share all the points on the
   * boundary feature.  We simply need to copy this set to the output cell
   * set, less the cell through which the request was made.
   */
  currentCells->erase(cellId);
  CellIdentifier numberOfNeighboringCells = static_cast<CellIdentifier>( currentCells->size() );
  if ( cellSet != ITK_NULLPTR )
    {
    *cellSet = *currentCells;
    }

  /**
   * Don't need the cell set anymore.
   */
  delete currentCells;

  /**
   * Return the number of neighboring cells that were put into the set.
   */
  return numberOfNeighboringCells;
}

/**
 * Get the set of cells having the given cell as part of their
 * boundary.  Returns the number of neighbors found.  If cellSet is not
 * ITK_NULLPTR, the set of cell pointers is filled in with identifiers of the
 * neighboring cells.
 *
 * NOTE: We would like to change this to use an "output iterator"
 * (in STL fashion) instead of an actual container to return the neighbor
 * identifiers.  This requires templated member support by the compiler,
 * though, and we are not sure how wide-spread this support is.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename Mesh< TPixelType, VDimension, TMeshTraits >::CellIdentifier
Mesh< TPixelType, VDimension, TMeshTraits >
::GetCellNeighbors(CellIdentifier cellId, std::set< CellIdentifier > *cellSet)
{
  /**
   * Sanity check on mesh status.
   */
  if ( !this->m_PointsContainer || !m_CellsContainer
       || ( !m_CellsContainer->IndexExists(cellId) ) )
    {
    /**
     * TODO: Throw EXCEPTION here?
     */
    return 0;
    }

  /**
   * Get the cell itself.  IndexExists call above should ensure that
   * GetCell() returns true, but be safe anyway.
   */
  CellAutoPointer cell;
  if ( !this->GetCell(cellId, cell) )
    {
    return 0;
    }

  /**
   * If the cell's UsingCells list is nonempty, then use it.
   */
  if ( cell->GetNumberOfUsingCells() != 0 )
    {
    /**
     * Loop through UsingCells and put them in the output set.  First
     * we empty the output set.
     */
    if ( cellSet != ITK_NULLPTR )
      {
      cellSet->erase( cellSet->begin(), cellSet->end() );

      typename CellType::UsingCellsContainerIterator usingCell;
      for ( usingCell = cell->UsingCellsBegin();
            usingCell != cell->UsingCellsEnd(); ++usingCell )
        {
        cellSet->insert(*usingCell);
        }
      }
    return cell->GetNumberOfUsingCells();
    }

  /**
   * The cell's UsingCells list was empy.  We use set operations
   * through point neighboring information to get the neighbors.  This
   * requires that the CellLinks be built.
   */
  if ( !m_CellLinksContainer
       || ( this->m_PointsContainer->GetMTime() > m_CellLinksContainer->GetMTime() )
       || ( m_CellsContainer->GetMTime()  > m_CellLinksContainer->GetMTime() ) )
    {
    this->BuildCellLinks();
    }

  /**
   * Cell links are up to date. We can proceed with the set operations.
   * We need to intersect the CellLinks sets for each point on the
   * given cell.
   */

  /**
   * Now get the cell links for the first point.  Also allocate a second set
   * for temporary storage during set intersections below.
   */
  typename CellType::PointIdConstIterator pointId = cell->PointIdsBegin();
  PointCellLinksContainer *currentCells =
    new PointCellLinksContainer( m_CellLinksContainer->GetElement(*pointId++) );
  PointCellLinksContainer *tempCells = new PointCellLinksContainer();

  /**
   * Next, loop over the other points, and intersect their cell links with
   * the current result.  We maintain "currentCells" as a pointer to the
   * current cell set instead of a set itself to prevent an extra copy of
   * the temporary set after each intersection.
   */
  while ( pointId != cell->PointIdsEnd() )
    {
    /**
     * Clean out temporary cell set from previous iteration.
     */
    tempCells->erase( tempCells->begin(), tempCells->end() );

    /**
     * Perform the intersection.
     */
    std::set_intersection( m_CellLinksContainer->CreateElementAt(*pointId).begin(),
                           m_CellLinksContainer->CreateElementAt(*pointId).end(),
                           currentCells->begin(),
                           currentCells->end(),
                           std::inserter( *tempCells, tempCells->begin() ) );

    /**
     * Switch the cell set pointers to make the intersection result the
     * current set.
     */
    std::swap(currentCells, tempCells);

    /**
     * Move on to the next point.
     */
    ++pointId;
    }

  /**
   * Don't need the second set anymore.
   */
  delete tempCells;

  /**
   * Now we have a set of all the cells which share all the points on
   * the original cell determined by cellId.  We simply need to copy
   * this set to the output cell set.
   */
  CellIdentifier numberOfNeighboringCells = static_cast<CellIdentifier>( currentCells->size() );
  if ( cellSet != ITK_NULLPTR )
    {
    *cellSet = *currentCells;
    }

  /**
   * Don't need the cell set anymore.
   */
  delete currentCells;

  /**
   * Return the number of neighboring cells that were put into the set.
   */
  return numberOfNeighboringCells;
}

/**
 * Check if there is an explicitly assigned boundary feature for the
 * given dimension and cell- and cell-feature-identifiers.  If there is,
 * a pointer to it is given back through "boundary" (if it isn't 0) and
 * true is returned.  Otherwise, false is returned.
 *
 * This version is new.  It does not treat boundaries as a separate
 * type.  A boundary (boundary component, really) is just a cell that
 * is part of the boundary of another cell.  As this conversion is
 * completed, the parts that use the boundary types will be removed.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
bool
Mesh< TPixelType, VDimension, TMeshTraits >
::GetAssignedCellBoundaryIfOneExists(int dimension, CellIdentifier cellId,
                                     CellFeatureIdentifier featureId,
                                     CellAutoPointer & boundary) const
{
  if ( m_BoundaryAssignmentsContainers[dimension].IsNotNull() )
    {
    BoundaryAssignmentIdentifier assignId(cellId, featureId);
    CellIdentifier               boundaryId;

    if ( m_BoundaryAssignmentsContainers[dimension]->
         GetElementIfIndexExists(assignId, &boundaryId) )
      {
      CellType * boundaryptr = ITK_NULLPTR;
      const bool found = m_CellsContainer->
                         GetElementIfIndexExists(boundaryId, &boundaryptr);
      if ( found )
        {
        boundary.TakeNoOwnership(boundaryptr);
        }
      return found;
      }
    }

  /** An explicitly assigned boundary was not found. */
  boundary.Reset();
  return false;
}

/**
 * Dynamically build the links from points back to their using cells.  This
 * information is stored in the cell links container, not in the points.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::Accept(CellMultiVisitorType *mv) const
{
  if ( !this->m_CellsContainer )
    {
    return;
    }

  CellsContainerConstIterator itr =  this->m_CellsContainer->Begin();

  while ( itr !=  this->m_CellsContainer->End() )
    {
    if ( itr->Value() )
      {
      itr->Value()->Accept(itr->Index(), mv);
      }
    else
      {
      itkDebugMacro( "Null cell at " << itr->Index() );
      }
    ++itr;
    }
}

/**
 * Dynamically build the links from points back to their using cells.  This
 * information is stored in the cell links container, not in the points.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::BuildCellLinks() const
{
  /**
   * Make sure we have a cells and a points container.
   */
  if ( !this->m_PointsContainer || !m_CellsContainer )
    {
    /**
     * TODO: Throw EXCEPTION here?
     */
    return;
    }

  /**
   * Make sure the cell links container exists.
   */
  if ( !m_CellLinksContainer )
    {
    this->m_CellLinksContainer = CellLinksContainer::New();
    }

  /**
   * Loop through each cell, and add its identifier to the CellLinks of each
   * of its points.
   */
  for ( CellsContainerIterator cellItr = m_CellsContainer->Begin();
        cellItr != m_CellsContainer->End(); ++cellItr )
    {
    CellIdentifier cellId  = cellItr->Index();
    CellType *     cellptr = cellItr->Value();

    /**
     * For each point, make sure the cell links container has its index,
     * and then insert the cell ID into the point's set.
     */
    for ( typename CellType::PointIdConstIterator pointId = cellptr->PointIdsBegin();
          pointId != cellptr->PointIdsEnd(); ++pointId )
      {
      ( m_CellLinksContainer->CreateElementAt(*pointId) ).insert(cellId);
      }
    }
}

/******************************************************************************
 * PROTECTED METHOD DEFINITIONS
 *****************************************************************************/

/**
 * A protected default constructor allows the New() routine to create an
 * instance of Mesh.  All the containers are initialized to empty
 * containers.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
Mesh< TPixelType, VDimension, TMeshTraits >
::Mesh()
{
  m_CellsContainer = CellsContainer::New();
  m_CellDataContainer =  CellDataContainer::New();
  m_CellLinksContainer = CellLinksContainer::New();
  m_BoundingBox = BoundingBoxType::New();
  m_BoundaryAssignmentsContainers = BoundaryAssignmentsContainerVector(MaxTopologicalDimension);
  m_CellsAllocationMethod = CellsAllocatedDynamicallyCellByCell;
}

/**
 * Mesh Destructor takes care of releasing the memory of Cells
 * and CellBoundaries objects for which normal pointers are
 * stored.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
Mesh< TPixelType, VDimension, TMeshTraits >
::~Mesh()
{
  itkDebugMacro("Mesh Destructor ");
  this->ReleaseCellsMemory();
}

/**
 * Releasing the memory of Cells aobjects for which normal pointers
 * are stored. The method used for memory release is based on information
 * provided by the user who is the only who know how the memory was allocated.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::ReleaseCellsMemory()
{
  itkDebugMacro("Mesh  ReleaseCellsMemory method ");
  // Cells are stored as normal pointers in the CellContainer.
  //
  // The following cases are assumed here:
  //
  // 0) The user forgot to tell the mesh how he allocated  the memory.
  //    In this case an exception is thrown. There is now way the mesh
  //    can guess how to correctly release the memory.
  // 1) The user allocated the cells as an static array and then
  //    passed pointers to the mesh. The mesh doesn't have to release
  //    any memory in this case. The user however has to be careful
  //    in making sure that the mesh is not used out of the scope in
  //    which the static array of cells is valid.(e.g. the pointer
  //    of the mesh should not be passed as a return parameter...)
  // 2) the user allocated the Cells as a big array so the
  //    memory has to be released by getting the pointer to
  //    the first cell in the array and calling "delete[] cells"
  // 3) the user allocated the Cells on a cell-by-cell basis
  //    so every cell has to be deleted using   "delete cell"
  //
  if ( !m_CellsContainer )
    {
    itkDebugMacro("m_CellsContainer is null");
    return;
    }

  itkDebugMacro( "m_CellsContainer->GetReferenceCount()= " << m_CellsContainer->GetReferenceCount() );

  if ( m_CellsContainer->GetReferenceCount() == 1 )
    {
    switch ( m_CellsAllocationMethod )
      {
      case CellsAllocationMethodUndefined:
        {
        // The user forgot to tell the mesh about how he allocated
        // the cells. No responsible guess can be made here. Call for help.
        itkGenericExceptionMacro(<< "Cells Allocation Method was not specified. See SetCellsAllocationMethod()");
        break;
        }
      case CellsAllocatedAsStaticArray:
        {
        // The cells will be naturally destroyed when
        // the original array goes out of scope.
        itkDebugMacro("CellsAllocatedAsStaticArray ");
        break;
        }
      case CellsAllocatedAsADynamicArray:
        {
        // the pointer to the first Cell is assumed to be the
        // base pointer of the array
        CellsContainerIterator first = m_CellsContainer->Begin();
        CellType *             baseOfCellsArray = first->Value();
        delete[] baseOfCellsArray;
        m_CellsContainer->Initialize();
        itkDebugMacro("CellsAllocatedAsADynamicArray");
        break;
        }
      case CellsAllocatedDynamicallyCellByCell:
        {
        itkDebugMacro("CellsAllocatedDynamicallyCellByCell start");
        // It is assumed that every cell was allocated independently.
        // A Cell iterator is created for going through the cells
        // deleting one by one.
        CellsContainerIterator cell  = m_CellsContainer->Begin();
        CellsContainerIterator end   = m_CellsContainer->End();
        while ( cell != end )
          {
          const CellType *cellToBeDeleted = cell->Value();
          itkDebugMacro(<< "Mesh destructor deleting cell = " << cellToBeDeleted);
          delete cellToBeDeleted;
          ++cell;
          }
        m_CellsContainer->Initialize();
        itkDebugMacro("CellsAllocatedDynamicallyCellByCell end");
        break;
        }
      }
    }
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::CopyInformation(const DataObject *data)
{
  this->Superclass::CopyInformation(data);

  const Self *mesh = dynamic_cast< const Self * >( data );

  if ( !mesh )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::Mesh::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  // Copy here specific elements of the Mesh
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
Mesh< TPixelType, VDimension, TMeshTraits >
::Graft(const DataObject *data)
{
  this->Superclass::Graft(data);

  const Self *mesh = dynamic_cast< const Self * >( data );

  if ( !mesh )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::Mesh::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  this->ReleaseCellsMemory();
  this->m_CellsContainer     = mesh->m_CellsContainer;
  this->m_CellDataContainer  = mesh->m_CellDataContainer;
  this->m_CellLinksContainer = mesh->m_CellLinksContainer;
  this->m_BoundaryAssignmentsContainers = mesh->m_BoundaryAssignmentsContainers;

  // The cell allocation method must be maintained. The reference count
  // test on the container will prevent premature deletion of cells.
  this->m_CellsAllocationMethod = mesh->m_CellsAllocationMethod;
}
} // end namespace itk

#endif

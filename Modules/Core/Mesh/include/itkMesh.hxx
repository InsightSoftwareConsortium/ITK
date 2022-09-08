/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

#include "itkProcessObject.h"
#include <algorithm>
#include <iterator>

namespace itk
{
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number Of Points: " << ((this->m_PointsContainer.GetPointer()) ? this->m_PointsContainer->Size() : 0)
     << std::endl;
  os << indent << "Number Of Cell Links: " << ((m_CellLinksContainer) ? m_CellLinksContainer->Size() : 0) << std::endl;
  os << indent << "Number Of Cells: " << ((m_CellsContainer) ? m_CellsContainer->Size() : 0) << std::endl;
  os << indent
     << "Cell Data Container pointer: " << ((m_CellDataContainer) ? m_CellDataContainer.GetPointer() : nullptr)
     << std::endl;
  os << indent << "Size of Cell Data Container: " << ((m_CellDataContainer) ? m_CellDataContainer->Size() : 0)
     << std::endl;
  os << indent << "Number of explicit cell boundary assignments: "
     << static_cast<CellIdentifier>(m_BoundaryAssignmentsContainers.size()) << std::endl;
  os << indent << "CellsAllocationMethod: " << m_CellsAllocationMethod << std::endl;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::SetCellLinks(CellLinksContainer * cellLinks)
{
  itkDebugMacro("setting CellLinks container to " << cellLinks);
  if (m_CellLinksContainer != cellLinks)
  {
    m_CellLinksContainer = cellLinks;
    this->Modified();
  }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
Mesh<TPixelType, VDimension, TMeshTraits>::GetCellLinks() -> CellLinksContainer *
{
  itkDebugMacro("returning CellLinks container of " << m_CellLinksContainer);
  return m_CellLinksContainer;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
Mesh<TPixelType, VDimension, TMeshTraits>::GetCellLinks() const -> const CellLinksContainer *
{
  itkDebugMacro("returning CellLinks container of " << m_CellLinksContainer);
  return m_CellLinksContainer;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::SetCells(CellsContainer * cells)
{
  itkDebugMacro("setting Cells container to " << cells);
  if (m_CellsContainer != cells)
  {
    this->ReleaseCellsMemory();
    m_CellsContainer = cells;
    this->Modified();
  }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
Mesh<TPixelType, VDimension, TMeshTraits>::GetCellsArray() -> CellsVectorContainer *
{
  itkDebugMacro("Getting Cells as Vector Container ");

  if (cellOutputVectorContainer == nullptr)
  {
    cellOutputVectorContainer = CellsVectorContainer::New();
  }
  else
  {
    cellOutputVectorContainer->Initialize();
  }

  IdentifierType index = 0;
  for (auto cellItr = m_CellsContainer->Begin(); cellItr != m_CellsContainer->End(); ++cellItr)
  {
    auto         cellPointer = cellItr->Value();
    unsigned int numOfPoints = cellPointer->GetNumberOfPoints();

    // Insert the cell type
    cellOutputVectorContainer->InsertElement(index++, static_cast<IdentifierType>(cellPointer->GetType()));
    // Insert the number of points in the cell
    cellOutputVectorContainer->InsertElement(index++, numOfPoints);

    auto pointIds = cellPointer->GetPointIds();

    // Insert the points in the cell
    for (unsigned int i = 0; i < numOfPoints; ++i)
    {
      cellOutputVectorContainer->InsertElement(index++, pointIds[i]);
    }
  }

  return cellOutputVectorContainer;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::CreateCell(int cellType, CellAutoPointer & cellPointer)
{
  auto cellTypeEnum = static_cast<CellGeometryEnum>(cellType);

  switch (cellTypeEnum)
  {
    case CellGeometryEnum::VERTEX_CELL:
      cellPointer.TakeOwnership(new OutputVertexCellType);
      break;
    case CellGeometryEnum::LINE_CELL:
      cellPointer.TakeOwnership(new OutputLineCellType);
      break;
    case CellGeometryEnum::POLYLINE_CELL:
      cellPointer.TakeOwnership(new OutputPolyLineCellType);
      break;
    case CellGeometryEnum::TRIANGLE_CELL:
      cellPointer.TakeOwnership(new OutputTriangleCellType);
      break;
    case CellGeometryEnum::QUADRILATERAL_CELL:
      cellPointer.TakeOwnership(new OutputQuadrilateralCellType);
      break;
    case CellGeometryEnum::POLYGON_CELL:
      cellPointer.TakeOwnership(new OutputPolygonCellType);
      break;
    case CellGeometryEnum::TETRAHEDRON_CELL:
      cellPointer.TakeOwnership(new OutputTetrahedronCellType);
      break;
    case CellGeometryEnum::HEXAHEDRON_CELL:
      cellPointer.TakeOwnership(new OutputHexahedronCellType);
      break;
    case CellGeometryEnum::QUADRATIC_EDGE_CELL:
      cellPointer.TakeOwnership(new OutputQuadraticEdgeCellType);
      break;
    case CellGeometryEnum::QUADRATIC_TRIANGLE_CELL:
      cellPointer.TakeOwnership(new OutputQuadraticTriangleCellType);
      break;
    default:
      itkExceptionMacro(<< "Unknown mesh cell");
  }

  return;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::SetCellsArray(CellsVectorContainer * cells)
{
  itkDebugMacro("setting Cells container to " << cells);

  this->ReleaseCellsMemory();
  IdentifierType index = 0;
  IdentifierType cellId = 0;

  while (index < cells->Size())
  {
    int currentCellType = static_cast<int>(cells->GetElement(index++));
    int numOfPoints = static_cast<int>(cells->GetElement(index++));

    CellAutoPointer cellPointer;

    CreateCell(currentCellType, cellPointer);

    // Insert the number of points as given in the input 1D array
    for (int i = 0; i < numOfPoints; ++i)
    {
      cellPointer->SetPointId(i, cells->GetElement(index++));
    }
    this->m_CellsContainer->InsertElement(cellId++, cellPointer.ReleaseOwnership());
  }

  this->Modified();
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::SetCellsArray(CellsVectorContainer * cells, int cellType)
{
  itkDebugMacro("setting Cells container to " << cells);

  this->ReleaseCellsMemory();
  IdentifierType index = 0;
  IdentifierType cellId = 0;

  while (index < cells->Size())
  {
    CellAutoPointer cellPointer;

    CreateCell(cellType, cellPointer);

    // Insert the number of points as per the cell type
    for (unsigned int i = 0; i < cellPointer->GetNumberOfPoints(); ++i)
    {
      cellPointer->SetPointId(i, cells->GetElement(index++));
    }
    this->m_CellsContainer->InsertElement(cellId++, cellPointer.ReleaseOwnership());
  }

  this->Modified();
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
Mesh<TPixelType, VDimension, TMeshTraits>::GetCells() -> CellsContainer *
{
  itkDebugMacro("returning Cells container of " << m_CellsContainer);
  return m_CellsContainer;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
Mesh<TPixelType, VDimension, TMeshTraits>::GetCells() const -> const CellsContainer *
{
  itkDebugMacro("returning Cells container of " << m_CellsContainer);
  return m_CellsContainer;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::SetCellData(CellDataContainer * cellData)
{
  itkDebugMacro("setting CellData container to " << cellData);
  if (m_CellDataContainer != cellData)
  {
    m_CellDataContainer = cellData;
    this->Modified();
  }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
Mesh<TPixelType, VDimension, TMeshTraits>::GetCellData() -> CellDataContainer *
{
  if (!m_CellDataContainer)
  {
    this->SetCellData(CellDataContainer::New());
  }
  itkDebugMacro("returning CellData container of " << m_CellDataContainer);
  return m_CellDataContainer;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
Mesh<TPixelType, VDimension, TMeshTraits>::GetCellData() const -> const CellDataContainer *
{
  itkDebugMacro("returning CellData container of " << m_CellDataContainer);
  return m_CellDataContainer;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
Mesh<TPixelType, VDimension, TMeshTraits>::GetBoundingBox() const -> const BoundingBoxType *
{
  m_BoundingBox->SetPoints(this->m_PointsContainer.GetPointer());
  if (m_BoundingBox->GetMTime() > this->GetMTime())
  {
    m_BoundingBox->ComputeBoundingBox();
  }
  return m_BoundingBox;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::SetBoundaryAssignments(int                            dimension,
                                                                  BoundaryAssignmentsContainer * boundaryAssignments)
{
  itkDebugMacro("setting BoundaryAssignments[" << dimension << "] container to " << boundaryAssignments);
  if (m_BoundaryAssignmentsContainers[dimension] != boundaryAssignments)
  {
    m_BoundaryAssignmentsContainers[dimension] = boundaryAssignments;
    this->Modified();
  }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
Mesh<TPixelType, VDimension, TMeshTraits>::GetBoundaryAssignments(int dimension) -> BoundaryAssignmentsContainerPointer
{
  itkDebugMacro("returning BoundaryAssignments[" << dimension << "] container of "
                                                 << m_BoundaryAssignmentsContainers[dimension]);
  return m_BoundaryAssignmentsContainers[dimension];
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
Mesh<TPixelType, VDimension, TMeshTraits>::GetBoundaryAssignments(int dimension) const
  -> const BoundaryAssignmentsContainerPointer
{
  itkDebugMacro("returning BoundaryAssignments[" << dimension << "] container of "
                                                 << m_BoundaryAssignmentsContainers[dimension]);
  return m_BoundaryAssignmentsContainers[dimension];
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::SetCell(CellIdentifier cellId, CellAutoPointer & cellPointer)
{
  /**
   * Make sure a cells container exists.
   */
  if (!m_CellsContainer)
  {
    this->SetCells(CellsContainer::New());
  }

  /**
   * Insert the cell into the container with the given identifier.
   */
  m_CellsContainer->InsertElement(cellId, cellPointer.ReleaseOwnership());
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool
Mesh<TPixelType, VDimension, TMeshTraits>::GetCell(CellIdentifier cellId, CellAutoPointer & cellPointer) const
{
  /**
   * If the cells container doesn't exist, then the cell doesn't exist.
   */
  if (m_CellsContainer.IsNull())
  {
    cellPointer.Reset();
    return false;
  }

  /**
   * Ask the container if the cell identifier exists.
   */
  CellType * cellptr = nullptr;
  const bool found = m_CellsContainer->GetElementIfIndexExists(cellId, &cellptr);
  if (found)
  {
    cellPointer.TakeNoOwnership(cellptr);
  }
  else
  {
    cellPointer.Reset();
  }

  return found;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::SetCellData(CellIdentifier cellId, CellPixelType data)
{
  /**
   * Make sure a cell data container exists.
   */
  if (!m_CellDataContainer)
  {
    this->SetCellData(CellDataContainer::New());
  }

  /**
   * Insert the cell data into the container with the given identifier.
   */
  m_CellDataContainer->InsertElement(cellId, data);
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool
Mesh<TPixelType, VDimension, TMeshTraits>::GetCellData(CellIdentifier cellId, CellPixelType * data) const
{
  /**
   * If the cell data container doesn't exist, then the cell data doesn't
   * either.
   */
  if (!m_CellDataContainer)
  {
    return false;
  }

  /**
   * Ask the container if the cell identifier exists.
   */
  return m_CellDataContainer->GetElementIfIndexExists(cellId, data);
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::SetBoundaryAssignment(int                   dimension,
                                                                 CellIdentifier        cellId,
                                                                 CellFeatureIdentifier featureId,
                                                                 CellIdentifier        boundaryId)
{
  BoundaryAssignmentIdentifier assignId(cellId, featureId);

  /**
   * Make sure a boundary assignment container exists for the given dimension.
   */
  if (!m_BoundaryAssignmentsContainers[dimension])
  {
    this->SetBoundaryAssignments(dimension, BoundaryAssignmentsContainer::New());
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

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool
Mesh<TPixelType, VDimension, TMeshTraits>::GetBoundaryAssignment(int                   dimension,
                                                                 CellIdentifier        cellId,
                                                                 CellFeatureIdentifier featureId,
                                                                 CellIdentifier *      boundaryId) const
{
  BoundaryAssignmentIdentifier assignId(cellId, featureId);

  /**
   * If the boundary assignments container for the given dimension doesn't
   * exist, then the boundary assignment doesn't either.
   */
  if (!m_BoundaryAssignmentsContainers[dimension])
  {
    return false;
  }

  /**
   * Ask the container if the boundary assignment exists.
   */
  return m_BoundaryAssignmentsContainers[dimension]->GetElementIfIndexExists(assignId, boundaryId);
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool
Mesh<TPixelType, VDimension, TMeshTraits>::RemoveBoundaryAssignment(int                   dimension,
                                                                    CellIdentifier        cellId,
                                                                    CellFeatureIdentifier featureId)
{
  BoundaryAssignmentIdentifier assignId(cellId, featureId);

  /**
   * If the boundary assignments container for the given dimension doesn't
   * exist, then the boundary assignment doesn't either.
   */
  if (!m_BoundaryAssignmentsContainers[dimension])
  {
    return false;
  }

  /**
   * Ask the container if the boundary assignment exists, and delete it if
   * so.
   */
  if (m_BoundaryAssignmentsContainers[dimension]->IndexExists(assignId))
  {
    m_BoundaryAssignmentsContainers[dimension]->DeleteIndex(assignId);
    return true;
  }
  else
  {
    return false;
  }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
Mesh<TPixelType, VDimension, TMeshTraits>::GetNumberOfCellBoundaryFeatures(int dimension, CellIdentifier cellId) const
  -> CellFeatureCount
{
  /**
   * Make sure the cell container exists and contains the given cell Id.
   */
  if (!m_CellsContainer)
  {
    return 0;
  }
  if (!m_CellsContainer->IndexExists(cellId))
  {
    return 0;
  }

  /**
   * Ask the cell for its boundary count of the given dimension.
   */
  return m_CellsContainer->GetElement(cellId)->GetNumberOfBoundaryFeatures(dimension);
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::PassStructure(Self *)
{
  // IMPLEMENT ME
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
Mesh<TPixelType, VDimension, TMeshTraits>::GetNumberOfCells() const -> CellIdentifier
{
  if (!m_CellsContainer)
  {
    return 0;
  }
  else
  {
    return m_CellsContainer->Size();
  }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::Initialize()
{
  itkDebugMacro("Mesh Initialize method ");
  Superclass::Initialize();

  this->ReleaseCellsMemory();

  m_CellsContainer = nullptr;
  m_CellDataContainer = nullptr;
  m_CellLinksContainer = nullptr;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool
Mesh<TPixelType, VDimension, TMeshTraits>::GetCellBoundaryFeature(int                   dimension,
                                                                  CellIdentifier        cellId,
                                                                  CellFeatureIdentifier featureId,
                                                                  CellAutoPointer &     boundary) const
{
  /**
   * First check if the boundary has been explicitly assigned.
   */
  if (GetAssignedCellBoundaryIfOneExists(dimension, cellId, featureId, boundary))
  {
    return true;
  }

  /**
   * It was not explicitly assigned, so ask the cell to construct it.
   * This will be a geometric copy of the actual boundary feature, not
   * a pointer to an actual cell in the mesh.
   */
  if ((!m_CellsContainer.IsNull()) && m_CellsContainer->IndexExists(cellId))
  {
    // Don't take ownership
    CellType * thecell = m_CellsContainer->GetElement(cellId);
    if (thecell->GetBoundaryFeature(dimension, featureId, boundary))
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

/** NOTE: We would like to change this to use an "output iterator"
 * (in STL fashion) instead of an actual container to return the neighbor
 * identifiers.  This requires templated member support by the compiler,
 * though, and we are not sure how wide-spread this support is.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
typename Mesh<TPixelType, VDimension, TMeshTraits>::CellIdentifier
Mesh<TPixelType, VDimension, TMeshTraits>::GetCellBoundaryFeatureNeighbors(int                        dimension,
                                                                           CellIdentifier             cellId,
                                                                           CellFeatureIdentifier      featureId,
                                                                           std::set<CellIdentifier> * cellSet)
{
  /**
   * Sanity check on mesh status.
   */
  if (!this->m_PointsContainer || !m_CellsContainer || (!m_CellsContainer->IndexExists(cellId)))
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
  if (this->GetAssignedCellBoundaryIfOneExists(dimension, cellId, featureId, boundary))
  {
    /**
     * Explicitly assigned boundary found.  Loop through its UsingCells,
     * and put them in the output set except for the cell through which the
     * request was made.  First we empty the output set.
     */
    if (cellSet != nullptr)
    {
      cellSet->erase(cellSet->begin(), cellSet->end());

      typename CellType::UsingCellsContainerIterator usingCell;
      for (usingCell = boundary->UsingCellsBegin(); usingCell != boundary->UsingCellsEnd(); ++usingCell)
      {
        if ((*usingCell) != cellId)
        {
          cellSet->insert(*usingCell);
        }
      }
    }
    /**
     * The number of neighboring cells is the number of cells using the
     * boundary less one for the cell through which the request was made.
     */
    return (boundary->GetNumberOfUsingCells() - 1);
  }

  /**
   * An explicit assignment for the boundary was not found.  We use set
   * operations through point neighboring information to get the neighbors.
   * This requires that the CellLinks be built.
   */
  if (!m_CellLinksContainer)
  {
    this->BuildCellLinks();
  }
  else if ((this->m_PointsContainer->GetMTime() > m_CellLinksContainer->GetMTime()) ||
           (m_CellsContainer->GetMTime() > m_CellLinksContainer->GetMTime()))
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
  m_CellsContainer->GetElement(cellId)->GetBoundaryFeature(dimension, featureId, boundary);

  /**
   * Now get the cell links for the first point.
   */
  typename CellType::PointIdConstIterator pointId = boundary->PointIdsBegin();
  PointCellLinksContainer                 currentCells(m_CellLinksContainer->GetElement(*pointId++));

  /**
   * Next, loop over the other points, and intersect their cell links with
   * the current result.
   */
  while (pointId != boundary->PointIdsEnd())
  {
    PointCellLinksContainer tempCells{};

    /**
     * Perform the intersection.
     */
    std::set_intersection(m_CellLinksContainer->CreateElementAt(*pointId).begin(),
                          m_CellLinksContainer->CreateElementAt(*pointId).end(),
                          currentCells.begin(),
                          currentCells.end(),
                          std::inserter(tempCells, tempCells.begin()));

    /**
     * Move the intersection result into the current set.
     */
    currentCells = std::move(tempCells);

    /**
     * Move on to the next point.
     */
    ++pointId;
  }

  /** delete the boundary feature added as a temporary auxiliary object,
      being an AutoPointer it will release memory when going out of scope */

  /**
   * Now we have a set of all the cells which share all the points on the
   * boundary feature.  We simply need to copy this set to the output cell
   * set, less the cell through which the request was made.
   */
  currentCells.erase(cellId);
  auto numberOfNeighboringCells = static_cast<CellIdentifier>(currentCells.size());
  if (cellSet != nullptr)
  {
    *cellSet = std::move(currentCells);
  }

  /**
   * Return the number of neighboring cells that were put into the set.
   */
  return numberOfNeighboringCells;
}

/** NOTE: We would like to change this to use an "output iterator"
 * (in STL fashion) instead of an actual container to return the neighbor
 * identifiers.  This requires templated member support by the compiler,
 * though, and we are not sure how wide-spread this support is.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
Mesh<TPixelType, VDimension, TMeshTraits>::GetCellNeighbors(CellIdentifier cellId, std::set<CellIdentifier> * cellSet)
  -> CellIdentifier
{
  /**
   * Sanity check on mesh status.
   */
  if (!this->m_PointsContainer || !m_CellsContainer || (!m_CellsContainer->IndexExists(cellId)))
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
  if (!this->GetCell(cellId, cell))
  {
    return 0;
  }

  /**
   * If the cell's UsingCells list is nonempty, then use it.
   */
  if (cell->GetNumberOfUsingCells() != 0)
  {
    /**
     * Loop through UsingCells and put them in the output set.  First
     * we empty the output set.
     */
    if (cellSet != nullptr)
    {
      cellSet->erase(cellSet->begin(), cellSet->end());

      typename CellType::UsingCellsContainerIterator usingCell;
      for (usingCell = cell->UsingCellsBegin(); usingCell != cell->UsingCellsEnd(); ++usingCell)
      {
        cellSet->insert(*usingCell);
      }
    }
    return cell->GetNumberOfUsingCells();
  }

  /**
   * The cell's UsingCells list was empty.  We use set operations
   * through point neighboring information to get the neighbors.  This
   * requires that the CellLinks be built.
   */
  if (!m_CellLinksContainer || (this->m_PointsContainer->GetMTime() > m_CellLinksContainer->GetMTime()) ||
      (m_CellsContainer->GetMTime() > m_CellLinksContainer->GetMTime()))
  {
    this->BuildCellLinks();
  }

  /**
   * Cell links are up to date. We can proceed with the set operations.
   * We need to intersect the CellLinks sets for each point on the
   * given cell.
   */

  /**
   * Now get the cell links for the first point.
   */
  typename CellType::PointIdConstIterator pointId = cell->PointIdsBegin();
  PointCellLinksContainer                 currentCells(m_CellLinksContainer->GetElement(*pointId++));

  /**
   * Next, loop over the other points, and intersect their cell links with
   * the current result.
   */
  while (pointId != cell->PointIdsEnd())
  {
    PointCellLinksContainer tempCells{};

    /**
     * Perform the intersection.
     */
    std::set_intersection(m_CellLinksContainer->CreateElementAt(*pointId).begin(),
                          m_CellLinksContainer->CreateElementAt(*pointId).end(),
                          currentCells.begin(),
                          currentCells.end(),
                          std::inserter(tempCells, tempCells.begin()));

    /**
     * Move the intersection result into the current set.
     */
    currentCells = std::move(tempCells);

    /**
     * Move on to the next point.
     */
    ++pointId;
  }

  /**
   * Now we have a set of all the cells which share all the points on
   * the original cell determined by cellId.  We simply need to copy
   * this set to the output cell set.
   */
  auto numberOfNeighboringCells = static_cast<CellIdentifier>(currentCells.size());
  if (cellSet != nullptr)
  {
    *cellSet = std::move(currentCells);
  }

  /**
   * Return the number of neighboring cells that were put into the set.
   */
  return numberOfNeighboringCells;
}

/** This version is new.  It does not treat boundaries as a separate
 * type.  A boundary (boundary component, really) is just a cell that
 * is part of the boundary of another cell.  As this conversion is
 * completed, the parts that use the boundary types will be removed.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool
Mesh<TPixelType, VDimension, TMeshTraits>::GetAssignedCellBoundaryIfOneExists(int                   dimension,
                                                                              CellIdentifier        cellId,
                                                                              CellFeatureIdentifier featureId,
                                                                              CellAutoPointer &     boundary) const
{
  if (m_BoundaryAssignmentsContainers[dimension].IsNotNull())
  {
    BoundaryAssignmentIdentifier assignId(cellId, featureId);
    CellIdentifier               boundaryId;

    if (m_BoundaryAssignmentsContainers[dimension]->GetElementIfIndexExists(assignId, &boundaryId))
    {
      CellType * boundaryptr = nullptr;
      const bool found = m_CellsContainer->GetElementIfIndexExists(boundaryId, &boundaryptr);
      if (found)
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

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::Accept(CellMultiVisitorType * mv) const
{
  if (!this->m_CellsContainer)
  {
    return;
  }

  CellsContainerConstIterator itr = this->m_CellsContainer->Begin();

  while (itr != this->m_CellsContainer->End())
  {
    if (itr->Value())
    {
      itr->Value()->Accept(itr->Index(), mv);
    }
    else
    {
      itkDebugMacro("Null cell at " << itr->Index());
    }
    ++itr;
  }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::BuildCellLinks() const
{
  /**
   * Make sure we have a cells and a points container.
   */
  if (!this->m_PointsContainer || !m_CellsContainer)
  {
    /**
     * TODO: Throw EXCEPTION here?
     */
    return;
  }

  /**
   * Make sure the cell links container exists.
   */
  if (!m_CellLinksContainer)
  {
    this->m_CellLinksContainer = CellLinksContainer::New();
  }

  /**
   * Loop through each cell, and add its identifier to the CellLinks of each
   * of its points.
   */
  for (CellsContainerIterator cellItr = m_CellsContainer->Begin(); cellItr != m_CellsContainer->End(); ++cellItr)
  {
    CellIdentifier cellId = cellItr->Index();
    CellType *     cellptr = cellItr->Value();

    /**
     * For each point, make sure the cell links container has its index,
     * and then insert the cell ID into the point's set.
     */
    for (typename CellType::PointIdConstIterator pointId = cellptr->PointIdsBegin(); pointId != cellptr->PointIdsEnd();
         ++pointId)
    {
      (m_CellLinksContainer->CreateElementAt(*pointId)).insert(cellId);
    }
  }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
Mesh<TPixelType, VDimension, TMeshTraits>::Mesh()
{
  m_CellsContainer = CellsContainer::New();
  m_CellDataContainer = CellDataContainer::New();
  m_CellLinksContainer = CellLinksContainer::New();
  m_BoundingBox = BoundingBoxType::New();
  m_BoundaryAssignmentsContainers = BoundaryAssignmentsContainerVector(MaxTopologicalDimension);
  m_CellsAllocationMethod = MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedDynamicallyCellByCell;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
Mesh<TPixelType, VDimension, TMeshTraits>::~Mesh()
{
  itkDebugMacro("Mesh Destructor ");
  this->ReleaseCellsMemory();
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::ReleaseCellsMemory()
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
  if (!m_CellsContainer)
  {
    itkDebugMacro("m_CellsContainer is null");
    return;
  }

  itkDebugMacro("m_CellsContainer->GetReferenceCount()= " << m_CellsContainer->GetReferenceCount());

  if (m_CellsContainer->GetReferenceCount() == 1)
  {
    switch (m_CellsAllocationMethod)
    {
      case MeshClassCellsAllocationMethodEnum::CellsAllocationMethodUndefined:
      {
        // The user forgot to tell the mesh about how he allocated
        // the cells. No responsible guess can be made here. Call for help.
        itkGenericExceptionMacro(<< "Cells Allocation Method was not specified. See SetCellsAllocationMethod()");
        break;
      }
      case MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedAsStaticArray:
      {
        // The cells will be naturally destroyed when
        // the original array goes out of scope.
        itkDebugMacro("CellsAllocatedAsStaticArray ");
        break;
      }
      case MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedAsADynamicArray:
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
      case MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedDynamicallyCellByCell:
      {
        itkDebugMacro("CellsAllocatedDynamicallyCellByCell start");
        // It is assumed that every cell was allocated independently.
        // A Cell iterator is created for going through the cells
        // deleting one by one.
        CellsContainerIterator cell = m_CellsContainer->Begin();
        CellsContainerIterator end = m_CellsContainer->End();
        while (cell != end)
        {
          const CellType * cellToBeDeleted = cell->Value();
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

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::CopyInformation(const DataObject * data)
{
  this->Superclass::CopyInformation(data);

  const auto * mesh = dynamic_cast<const Self *>(data);

  if (!mesh)
  {
    // pointer could not be cast back down
    itkExceptionMacro(<< "itk::Mesh::CopyInformation() cannot cast " << typeid(data).name() << " to "
                      << typeid(Self *).name());
  }

  // Copy here specific elements of the Mesh
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::Graft(const DataObject * data)
{
  this->Superclass::Graft(data);

  const auto * mesh = dynamic_cast<const Self *>(data);

  if (!mesh)
  {
    // pointer could not be cast back down
    itkExceptionMacro(<< "itk::Mesh::CopyInformation() cannot cast " << typeid(data).name() << " to "
                      << typeid(Self *).name());
  }

  this->ReleaseCellsMemory();
  this->m_CellsContainer = mesh->m_CellsContainer;
  this->m_CellDataContainer = mesh->m_CellDataContainer;
  this->m_CellLinksContainer = mesh->m_CellLinksContainer;
  this->m_BoundaryAssignmentsContainers = mesh->m_BoundaryAssignmentsContainers;

  // The cell allocation method must be maintained. The reference count
  // test on the container will prevent premature deletion of cells.
  this->m_CellsAllocationMethod = mesh->m_CellsAllocationMethod;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
Mesh<TPixelType, VDimension, TMeshTraits>::DeleteUnusedCellData()
{

  if (nullptr == this->GetCellData())
  {
    return;
  }

  std::vector<typename CellDataContainer::ElementIdentifier> cell_data_to_delete;
  for (auto it = this->GetCellData()->Begin(); it != this->GetCellData()->End(); ++it)
  {
    if (!this->GetCells()->IndexExists(it.Index()))
    {
      cell_data_to_delete.push_back(it.Index());
    }
  }
  for (const auto c : cell_data_to_delete)
  {
    this->GetCellData()->DeleteIndex(c);
  }
}
} // end namespace itk

#endif

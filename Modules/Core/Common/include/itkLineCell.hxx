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
#ifndef itkLineCell_hxx
#define itkLineCell_hxx

#include <algorithm> // For copy_n.

namespace itk
{
/**
 * Standard CellInterface:
 */
template <typename TCellInterface>
void
LineCell<TCellInterface>::MakeCopy(CellAutoPointer & cellPointer) const
{
  cellPointer.TakeOwnership(new Self);
  cellPointer->SetPointIds(this->GetPointIds());
}

/**
 * Standard CellInterface:
 * Get the topological dimension of this cell.
 */
template <typename TCellInterface>
unsigned int
LineCell<TCellInterface>::GetDimension() const
{
  return Self::CellDimension;
}

/**
 * Standard CellInterface:
 * Get the number of points required to define the cell.
 */
template <typename TCellInterface>
unsigned int
LineCell<TCellInterface>::GetNumberOfPoints() const
{
  return Self::NumberOfPoints;
}

/**
 * Standard CellInterface:
 * Get the number of boundary entities of the given dimension.
 */
template <typename TCellInterface>
auto
LineCell<TCellInterface>::GetNumberOfBoundaryFeatures(int dimension) const -> CellFeatureCount
{
  switch (dimension)
  {
    case 0:
      return GetNumberOfVertices();
    default:
      return 0;
  }
}

/**
 * Standard CellInterface:
 * Get the boundary feature of the given dimension specified by the given
 * cell feature Id.
 * The Id can range from 0 to GetNumberOfBoundaryFeatures(dimension)-1.
 */
template <typename TCellInterface>
bool
LineCell<TCellInterface>::GetBoundaryFeature(int                   dimension,
                                             CellFeatureIdentifier featureId,
                                             CellAutoPointer &     cellPointer)
{
  VertexAutoPointer vertexPointer;
  if ((dimension == 0) && this->GetVertex(featureId, vertexPointer))
  {
    TransferAutoPointer(cellPointer, vertexPointer);
    return true;
  }
  /* else */
  cellPointer.Reset();
  return false;
}

/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to
 * get all the point ids needed by the cell.
 */
template <typename TCellInterface>
void
LineCell<TCellInterface>::SetPointIds(PointIdConstIterator first)
{
  std::copy_n(first, Self::NumberOfPoints, m_PointIds.begin());
}

/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the range
 * of iterators [first, last) contains the correct number of points needed to
 * define the cell.  The position *last is NOT referenced, so it can safely
 * be one beyond the end of an array or other container.
 */
template <typename TCellInterface>
void
LineCell<TCellInterface>::SetPointIds(PointIdConstIterator first, PointIdConstIterator last)
{
  int                  localId = 0;
  PointIdConstIterator ii(first);

  while (ii != last)
  {
    m_PointIds[localId++] = *ii++;
  }
}

/**
 * Standard CellInterface:
 * Set an individual point identifier in the cell.
 */
template <typename TCellInterface>
void
LineCell<TCellInterface>::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}

/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
auto
LineCell<TCellInterface>::PointIdsBegin() -> PointIdIterator
{
  return &m_PointIds[0];
}

/**
 * Standard CellInterface:
 * Get a const begin iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TCellInterface>
auto
LineCell<TCellInterface>::PointIdsBegin() const -> PointIdConstIterator
{
  return &m_PointIds[0];
}

/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
auto
LineCell<TCellInterface>::PointIdsEnd() -> PointIdIterator
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TCellInterface>
auto
LineCell<TCellInterface>::PointIdsEnd() const -> PointIdConstIterator
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

/**
 * Line-specific:
 * Get the number of vertices for this line.
 */
template <typename TCellInterface>
auto
LineCell<TCellInterface>::GetNumberOfVertices() const -> CellFeatureCount
{
  return Self::NumberOfPoints;
}

/**
 * Line-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */
template <typename TCellInterface>
bool
LineCell<TCellInterface>::GetVertex(CellFeatureIdentifier vertexId, VertexAutoPointer & vertexPointer)
{
  auto * vert = new VertexType;

  vert->SetPointId(0, m_PointIds[vertexId]);
  vertexPointer.TakeOwnership(vert);
  return true;
}
} // end namespace itk

#endif

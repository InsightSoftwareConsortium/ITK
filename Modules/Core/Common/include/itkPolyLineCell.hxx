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
#ifndef itkPolyLineCell_hxx
#define itkPolyLineCell_hxx

namespace itk
{
/**
 * Standard CellInterface:
 */
template <typename TCellInterface>
void
PolyLineCell<TCellInterface>::MakeCopy(CellAutoPointer & cellPointer) const
{
  auto * newPolylineCell = new Self;

  cellPointer.TakeOwnership(newPolylineCell);
  const PointIdentifier numberOfPoints = this->GetNumberOfPoints();
  if (numberOfPoints)
  {
    newPolylineCell->SetPointIds(0, numberOfPoints, this->GetPointIds());
  }
  else
  {
    newPolylineCell->ClearPoints();
  }
}

/**
 * Standard CellInterface:
 * Get the topological dimension of this cell.
 */
template <typename TCellInterface>
unsigned int
PolyLineCell<TCellInterface>::GetDimension() const
{
  return Self::CellDimension;
}

/**
 * Standard CellInterface:
 * Get the number of points present in the cell.
 */
template <typename TCellInterface>
unsigned int
PolyLineCell<TCellInterface>::GetNumberOfPoints() const
{
  return static_cast<unsigned int>(m_PointIds.size());
}

/**
 * Standard CellInterface:
 * Get the number of boundary entities of the given dimension.
 */
template <typename TCellInterface>
auto
PolyLineCell<TCellInterface>::GetNumberOfBoundaryFeatures(int dimension) const -> CellFeatureCount
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
PolyLineCell<TCellInterface>::GetBoundaryFeature(int                   dimension,
                                                 CellFeatureIdentifier featureId,
                                                 CellAutoPointer &     cellPointer)
{
  VertexAutoPointer vertexPointer;
  if ((dimension == 0) && this->GetVertex(featureId, vertexPointer))
  {
    TransferAutoPointer(cellPointer, vertexPointer);
    return true;
  }
  // else
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
PolyLineCell<TCellInterface>::SetPointIds(PointIdConstIterator first)
{
  PointIdConstIterator ii(first);

  for (unsigned int i = 0; i < m_PointIds.size(); ++i)
  {
    m_PointIds[i] = *ii++;
  }
}

/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to
 * get all the point ids needed by the cell.
 */
template <typename TCellInterface>
void
PolyLineCell<TCellInterface>::SetPointIds(int itkNotUsed(dummy), int num, PointIdConstIterator first)
{
  PointIdConstIterator ii(first);

  m_PointIds.clear();
  for (int i = 0; i < num; ++i)
  {
    m_PointIds.push_back(*ii++);
  }
}

/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the range
 * of iterators [first, last) contains the correct number of points needed to
 * define the cell.  The position *last is NOT dereferenced, so it can safely
 * be one beyond the end of an array or other container.
 */
template <typename TCellInterface>
void
PolyLineCell<TCellInterface>::SetPointIds(PointIdConstIterator first, PointIdConstIterator last)
{
  PointIdConstIterator ii(first);

  m_PointIds.clear();
  while (ii != last)
  {
    m_PointIds.push_back(*ii++);
  }
}

/**
 * Standard CellInterface:
 * Set an individual point identifier in the cell.
 */
template <typename TCellInterface>
void
PolyLineCell<TCellInterface>::SetPointId(int localId, PointIdentifier ptId)
{
  if (m_PointIds.size() < static_cast<unsigned int>(localId + 1))
  {
    m_PointIds.resize(localId + 1);
  }
  m_PointIds[localId] = ptId;
}

/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
auto
PolyLineCell<TCellInterface>::PointIdsBegin() -> PointIdIterator
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
PolyLineCell<TCellInterface>::PointIdsBegin() const -> PointIdConstIterator
{
  return &m_PointIds[0];
}

/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
auto
PolyLineCell<TCellInterface>::PointIdsEnd() -> PointIdIterator
{
  if (!m_PointIds.empty())
  {
    return &m_PointIds.back() + 1;
  }
  else
  {
    return nullptr;
  }
}

/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TCellInterface>
auto
PolyLineCell<TCellInterface>::PointIdsEnd() const -> PointIdConstIterator
{
  if (!m_PointIds.empty())
  {
    return &m_PointIds.back() + 1;
  }
  else
  {
    return nullptr;
  }
}

/**
 * clear all the points in the cell.
 */
template <typename TCellInterface>
void
PolyLineCell<TCellInterface>::ClearPoints()
{
  InitializePoints(2);
}


/**
 * Line-specific:
 * Get the number of vertices for this polyline cell.
 */
template <typename TCellInterface>
auto
PolyLineCell<TCellInterface>::GetNumberOfVertices() const -> CellFeatureCount
{
  return static_cast<CellFeatureCount>(m_PointIds.size());
}

/**
 * Line-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */
template <typename TCellInterface>
bool
PolyLineCell<TCellInterface>::GetVertex(CellFeatureIdentifier vertexId, VertexAutoPointer & vertexPointer)
{
  auto * vert = new VertexType;

  vert->SetPointId(0, m_PointIds[vertexId]);
  vertexPointer.TakeOwnership(vert);
  return true;
}
} // end namespace itk

#endif

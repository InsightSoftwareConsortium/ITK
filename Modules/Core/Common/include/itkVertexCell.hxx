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
#ifndef itkVertexCell_hxx
#define itkVertexCell_hxx

#include <algorithm> // For copy_n.

namespace itk
{

template <typename TCellInterface>
void
VertexCell<TCellInterface>::MakeCopy(CellAutoPointer & cellPointer) const
{
  cellPointer.TakeOwnership(new Self);
  cellPointer->SetPointIds(this->GetPointIds());
}

template <typename TCellInterface>
unsigned int
VertexCell<TCellInterface>::GetDimension() const
{
  return Self::CellDimension;
}

template <typename TCellInterface>
unsigned int
VertexCell<TCellInterface>::GetNumberOfPoints() const
{
  return Self::NumberOfPoints;
}

template <typename TCellInterface>
auto
VertexCell<TCellInterface>::GetNumberOfBoundaryFeatures(int) const -> CellFeatureCount
{
  return 0;
}

template <typename TCellInterface>
bool
VertexCell<TCellInterface>::GetBoundaryFeature(int, CellFeatureIdentifier, CellAutoPointer & cellAPtr)
{
  cellAPtr.Reset();
  return false;
}

template <typename TCellInterface>
void
VertexCell<TCellInterface>::SetPointIds(PointIdConstIterator first)
{
  std::copy_n(first, Self::NumberOfPoints, m_PointIds.begin());
}

template <typename TCellInterface>
void
VertexCell<TCellInterface>::SetPointIds(PointIdConstIterator first, PointIdConstIterator last)
{
  int                  localId = 0;
  PointIdConstIterator ii(first);

  while (ii != last)
  {
    m_PointIds[localId++] = *ii++;
  }
}

template <typename TCellInterface>
void
VertexCell<TCellInterface>::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}

template <typename TCellInterface>
auto
VertexCell<TCellInterface>::PointIdsBegin() -> PointIdIterator
{
  return &m_PointIds[0];
}

template <typename TCellInterface>
auto
VertexCell<TCellInterface>::PointIdsBegin() const -> PointIdConstIterator
{
  return &m_PointIds[0];
}

template <typename TCellInterface>
auto
VertexCell<TCellInterface>::PointIdsEnd() -> PointIdIterator
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

template <typename TCellInterface>
auto
VertexCell<TCellInterface>::PointIdsEnd() const -> PointIdConstIterator
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

template <typename TCellInterface>
void
VertexCell<TCellInterface>::SetPointId(PointIdentifier ptId)
{
  m_PointIds[0] = ptId;
}

template <typename TCellInterface>
auto
VertexCell<TCellInterface>::GetPointId() -> PointIdentifier
{
  return m_PointIds[0];
}

template <typename TCellInterface>
bool
VertexCell<TCellInterface>::EvaluatePosition(CoordRepType *            x,
                                             PointsContainer *         points,
                                             CoordRepType *            closestPoint,
                                             CoordRepType              pcoord[],
                                             double *                  minDist2,
                                             InterpolationWeightType * weights)
{
  PointType X = points->GetElement(m_PointIds[0]);

  if (closestPoint)
  {
    for (unsigned int i = 0; i < PointDimension; ++i)
    {
      closestPoint[i] = X[i];
    }
  }

  double dist2 = 0;
  {
    for (unsigned int i = 0; i < PointDimension; ++i)
    {
      dist2 += (X[i] - x[i]) * (X[i] - x[i]);
    }
  }

  if (minDist2)
  {
    *minDist2 = dist2;
  }

  if (weights)
  {
    weights[0] = 1.0;
  }

  if (dist2 == 0.0)
  {
    if (pcoord)
    {
      pcoord[0] = 0.0;
    }
    return true;
  }
  else
  {
    if (pcoord)
    {
      pcoord[0] = -10.0;
    }
    return false;
  }
}
} // end namespace itk

#endif

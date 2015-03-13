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

#ifndef itkCellAreaTriangleCellSubdivisionCriterion_hxx
#define itkCellAreaTriangleCellSubdivisionCriterion_hxx

#include "itkCellAreaTriangleCellSubdivisionCriterion.h"

namespace itk
{
template <typename TTriangleCellSubdivisionFilter>
void
CellAreaTriangleCellSubdivisionCriterion<TTriangleCellSubdivisionFilter>::Compute(MeshType *                 mesh,
                                                                                  SubdivisionCellContainer & cellIds)
{
  cellIds.clear();
  const CellsContainer * cells = mesh->GetCells();
  if (!cells)
  {
    itkExceptionMacro("<<Input mesh has not cells");
  }

  CellsContainerConstIterator cter = cells->Begin();
  while (cter != cells->End())
  {
    CellType * cell = cter->Value();
    if (!cell || cell->GetType() != CellType::POLYGON_CELL || cell->GetNumberOfPoints() != 3)
    {
      continue;
    }

    PointType       pointArray[3];
    PointIdIterator pter = cell->PointIdsBegin();
    PointIdentifier nn = 0;

    while (pter != cell->PointIdsEnd())
    {
      mesh->GetPoint(*pter, &pointArray[nn]);
      ++pter;
      ++nn;
    }

    CoordRepType area = TriangleHelper<PointType>::ComputeArea(pointArray[0], pointArray[1], pointArray[2]);
    if (area > m_MaximumArea)
    {
      cellIds.push_back(static_cast<typename SubdivisionCellContainer::value_type>(cter->Index()));
    }

    ++cter;
  }
}

} // namespace itk
#endif

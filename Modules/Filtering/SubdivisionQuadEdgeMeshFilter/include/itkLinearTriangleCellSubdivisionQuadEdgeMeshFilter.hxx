/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkLinearTriangleCellSubdivisionQuadEdgeMeshFilter_hxx
#define itkLinearTriangleCellSubdivisionQuadEdgeMeshFilter_hxx

#include "itkLinearTriangleCellSubdivisionQuadEdgeMeshFilter.h"

namespace itk
{
template <typename TInputMesh, typename TOutputMesh>
void
LinearTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::AddNewCellPoints(InputCellType * cell)
{
  if (cell->GetType() != InputCellType::POLYGON_CELL || cell->GetNumberOfPoints() != 3)
  {
    itkExceptionMacro(<< " The input cell is not a triangle cell");
  }

  OutputMeshType * output = this->GetOutput();

  InputPointIdentifier pointIdArray[3];
  InputPointType       pointArray[3];

  InputPointIdIterator  it = cell->PointIdsBegin();
  OutputPointIdentifier n = 0;
  OutputPointIdentifier numberOfPoints = output->GetNumberOfPoints();

  while (it != cell->PointIdsEnd())
  {
    pointIdArray[n] = *it;
    this->GetOutput()->GetPoint(*it, &pointArray[n]);
    ++it;
    ++n;
  }

  for (unsigned int ii = 0; ii < 3; ++ii)
  {
    unsigned int jj = (ii + 1) % 3;

    InputQEType * edge = this->GetInput()->FindEdge(pointIdArray[ii], pointIdArray[jj]);

    if (!this->m_EdgesPointIdentifier->IndexExists(edge))
    {
      InputPointType  newPoint;
      OutputPointType outPoint;
      newPoint.SetToMidPoint(pointArray[ii], pointArray[jj]);
      outPoint.CastFrom(newPoint);

      this->m_EdgesPointIdentifier->InsertElement(edge, numberOfPoints);
      this->m_EdgesPointIdentifier->InsertElement(edge->GetSym(), numberOfPoints);
      output->SetPoint(numberOfPoints, outPoint);

      ++numberOfPoints;
    }
  }
}
} // namespace itk
#endif

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

#include <itkDefaultDynamicMeshTraits.h>
#include <itkMesh.h>
#include <itkTriangleCell.h>

const unsigned int Dimension = 2;
using TPixel = float;
using TMeshTraits = itk::DefaultDynamicMeshTraits<TPixel>;
using TMesh = itk::Mesh<TPixel, Dimension, TMeshTraits>;
using TPoint = TMesh::PointType;
using TCell = TMesh::CellType;
using TTriangle = itk::TriangleCell<TCell>;

int
itkMeshCellDataTest(int, char *[])
{

  // 1------3------5
  //   \    | \    |
  //      \ |    \ |
  // 0      2------4

  const auto mesh = TMesh::New();
  mesh->SetPoint(0, TPoint{ { { 0.0, 0.0 } } });
  mesh->SetPoint(1, TPoint{ { { 0.0, 1.0 } } });
  mesh->SetPoint(2, TPoint{ { { 1.0, 0.0 } } });
  mesh->SetPoint(3, TPoint{ { { 1.0, 1.0 } } });
  mesh->SetPoint(4, TPoint{ { { 4.0, 0.0 } } });
  mesh->SetPoint(5, TPoint{ { { 5.0, 1.0 } } });

  {
    TCell::CellAutoPointer cellpointer;
    cellpointer.TakeOwnership(new TTriangle);
    cellpointer->SetPointId(0, 1);
    cellpointer->SetPointId(1, 2);
    cellpointer->SetPointId(2, 3);
    mesh->SetCell(0, cellpointer);
  }
  {
    TCell::CellAutoPointer cellpointer;
    cellpointer.TakeOwnership(new TTriangle);
    cellpointer->SetPointId(0, 3);
    cellpointer->SetPointId(1, 2);
    cellpointer->SetPointId(2, 4);
    mesh->SetCell(1, cellpointer);
  }
  {
    TCell::CellAutoPointer cellpointer;
    cellpointer.TakeOwnership(new TTriangle);
    cellpointer->SetPointId(0, 3);
    cellpointer->SetPointId(1, 4);
    cellpointer->SetPointId(2, 5);
    mesh->SetCell(2, cellpointer);
  }

  itkAssertOrThrowMacro(6 == mesh->GetNumberOfPoints(), "Wrong number of points.");
  itkAssertOrThrowMacro(3 == mesh->GetNumberOfCells(), "Wrong number of cells.");

  // Add one extra entry in the cell data array
  mesh->SetCellData(0, 0);
  mesh->SetCellData(1, 1);
  mesh->SetCellData(2, 2);
  mesh->SetCellData(3, 3); // Extra entry

  itkAssertOrThrowMacro(4 == mesh->GetCellData()->Size(), "Wrong number of entries in the cell data array.");

  mesh->DeleteUnusedCellData();

  itkAssertOrThrowMacro(3 == mesh->GetCellData()->Size(), "Wrong number of entries in the cell data array.");

  return EXIT_SUCCESS;
}

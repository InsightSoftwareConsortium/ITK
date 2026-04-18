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
#include "itkPolyData.h"

#include "itkTestingMacros.h"

int
itkPolyDataTest(int, char *[])
{
  using PixelType = double;

  using PolyDataType = itk::PolyData<PixelType>;
  PolyDataType::Pointer  polyData = PolyDataType::New();
  constexpr unsigned int PointDimension = PolyDataType::PointDimension;

  polyData->Initialize();


  PolyDataType::PointsContainer::Pointer pointsContainer = PolyDataType::PointsContainer::New();

  PolyDataType::PointType point;
  point[0] = 1.0;
  point[1] = 3.0;
  point[2] = 5.0;
  pointsContainer->InsertElement(0, point);
  point[0] = 2.0;
  point[1] = 4.0;
  point[2] = 6.0;
  pointsContainer->InsertElement(1, point);
  polyData->SetPoints(pointsContainer);

  for (unsigned int dim = 0; dim < PointDimension; ++dim)
  {
    ITK_TEST_SET_GET_VALUE(point[dim], polyData->GetPoint(1)[dim]);
  }

  point[0] = 3.0;
  point[1] = 5.0;
  point[2] = 7.0;
  polyData->SetPoint(2, point);
  for (unsigned int dim = 0; dim < PointDimension; ++dim)
  {
    ITK_TEST_SET_GET_VALUE(point[dim], polyData->GetPoint(2)[dim]);
  }
  polyData->SetPoint(1, point);
  for (unsigned int dim = 0; dim < PointDimension; ++dim)
  {
    ITK_TEST_SET_GET_VALUE(point[dim], polyData->GetPoint(1)[dim]);
  }

  PolyDataType::CellsContainer::Pointer vertices = PolyDataType::CellsContainer::New();
  vertices->InsertElement(0, 1);
  vertices->InsertElement(1, 4);
  vertices->InsertElement(2, 1);
  vertices->InsertElement(3, 7);
  polyData->SetVertices(vertices);
  ITK_TEST_SET_GET_VALUE(vertices.GetPointer(), polyData->GetVertices());

  PolyDataType::CellsContainer::Pointer lines = PolyDataType::CellsContainer::New();
  lines->InsertElement(0, 2);
  lines->InsertElement(1, 4);
  lines->InsertElement(2, 5);
  lines->InsertElement(3, 2);
  lines->InsertElement(4, 7);
  lines->InsertElement(5, 8);
  polyData->SetLines(lines);
  ITK_TEST_SET_GET_VALUE(lines.GetPointer(), polyData->GetLines());

  PolyDataType::CellsContainer::Pointer polygons = PolyDataType::CellsContainer::New();
  polygons->InsertElement(0, 4);
  polygons->InsertElement(1, 4);
  polygons->InsertElement(2, 5);
  polygons->InsertElement(3, 6);
  polygons->InsertElement(4, 7);
  polygons->InsertElement(5, 4);
  polygons->InsertElement(6, 8);
  polygons->InsertElement(7, 9);
  polygons->InsertElement(8, 10);
  polygons->InsertElement(9, 11);
  polyData->SetPolygons(polygons);
  ITK_TEST_SET_GET_VALUE(polygons.GetPointer(), polyData->GetPolygons());

  PolyDataType::CellsContainer::Pointer triangleStrips = PolyDataType::CellsContainer::New();
  triangleStrips->InsertElement(0, 3);
  triangleStrips->InsertElement(1, 4);
  triangleStrips->InsertElement(2, 5);
  triangleStrips->InsertElement(3, 6);
  triangleStrips->InsertElement(4, 3);
  triangleStrips->InsertElement(5, 8);
  triangleStrips->InsertElement(6, 9);
  triangleStrips->InsertElement(7, 10);
  polyData->SetTriangleStrips(triangleStrips);
  ITK_TEST_SET_GET_VALUE(triangleStrips.GetPointer(), polyData->GetTriangleStrips());

  PolyDataType::PointDataContainer::Pointer pointDataContainer = PolyDataType::PointDataContainer::New();
  pointDataContainer->InsertElement(0, 2.0);
  pointDataContainer->InsertElement(1, 7.0);
  polyData->SetPointData(pointDataContainer);
  double pointData;
  polyData->GetPointData(1, &pointData);
  ITK_TEST_SET_GET_VALUE(7.0, pointData);
  polyData->SetPointData(2, 9.9);
  polyData->GetPointData(2, &pointData);
  ITK_TEST_SET_GET_VALUE(9.9, pointData);

  PolyDataType::CellDataContainer::Pointer cellDataContainer = PolyDataType::CellDataContainer::New();
  cellDataContainer->InsertElement(0, 2.0);
  cellDataContainer->InsertElement(1, 7.0);
  polyData->SetCellData(cellDataContainer);
  double cellData;
  polyData->GetCellData(1, &cellData);
  ITK_TEST_SET_GET_VALUE(7.0, cellData);
  polyData->SetCellData(2, 9.9);
  polyData->GetCellData(2, &cellData);
  ITK_TEST_SET_GET_VALUE(9.9, cellData);

  ITK_EXERCISE_BASIC_OBJECT_METHODS(polyData, PolyData, DataObject);

  return EXIT_SUCCESS;
}

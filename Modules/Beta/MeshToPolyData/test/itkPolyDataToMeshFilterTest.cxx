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
#include "itkMesh.h"
#include "itkPolyData.h"
#include "itkPolyDataToMeshFilter.h"

#include "itkTestingMacros.h"

template <typename TPixelType>
void
MakePolyDataSample(itk::PolyData<TPixelType> *);

int
itkPolyDataToMeshFilterTest(int, char *[])
{

  using PixelType = double;

  using PolyDataType = itk::PolyData<PixelType>;
  PolyDataType::Pointer polyData = PolyDataType::New();

  MakePolyDataSample<PixelType>(polyData);

  using FilterType = itk::PolyDataToMeshFilter<PolyDataType>;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(polyData);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  using MeshType = FilterType::OutputMeshType;
  MeshType::Pointer meshResult = filter->GetOutput();

  ITK_TEST_EXPECT_EQUAL(meshResult->GetNumberOfPoints(), polyData->GetNumberOfPoints());
  ITK_TEST_EXPECT_EQUAL(meshResult->GetPoint(0)[0], 1);
  ITK_TEST_EXPECT_EQUAL(meshResult->GetPoint(0)[1], 3);
  ITK_TEST_EXPECT_EQUAL(meshResult->GetPoint(0)[2], 5);
  ITK_TEST_EXPECT_EQUAL(meshResult->GetPoint(1)[0], 4);
  ITK_TEST_EXPECT_EQUAL(meshResult->GetPoint(1)[1], 6);
  ITK_TEST_EXPECT_EQUAL(meshResult->GetPoint(1)[2], 8);
  ITK_TEST_EXPECT_EQUAL(meshResult->GetPoint(2)[0], 3);
  ITK_TEST_EXPECT_EQUAL(meshResult->GetPoint(2)[1], 5);
  ITK_TEST_EXPECT_EQUAL(meshResult->GetPoint(2)[2], 7);

  ITK_TEST_EXPECT_EQUAL(meshResult->GetNumberOfCells(), 7);

  // Verify vertex cells
  typename MeshType::CellAutoPointer cellPtr;
  meshResult->GetCell(0, cellPtr);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetNumberOfPoints(), 1);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[0], 4);
  meshResult->GetCell(1, cellPtr);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetNumberOfPoints(), 1);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[0], 7);

  // Verify line cells
  meshResult->GetCell(2, cellPtr);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetNumberOfPoints(), 3);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[0], 3);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[1], 4);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[2], 5);
  meshResult->GetCell(3, cellPtr);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetNumberOfPoints(), 2);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[0], 7);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[1], 8);

  // Verify triangle strips (now cells
  meshResult->GetCell(4, cellPtr);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetNumberOfPoints(), 3);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[0], 0);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[1], 2);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[2], 1);
  meshResult->GetCell(5, cellPtr);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetNumberOfPoints(), 3);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[0], 2);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[1], 1);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[2], 3);


  // Verify triangle cells
  meshResult->GetCell(6, cellPtr);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetNumberOfPoints(), 3);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[0], 0);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[1], 1);
  ITK_TEST_EXPECT_EQUAL(cellPtr->GetPointIdsContainer()[2], 2);

  return EXIT_SUCCESS;
}

// From itkPolyDataTest.cxx
template <typename TPixelType>
void
MakePolyDataSample(itk::PolyData<TPixelType> * polyData)
{
  using PolyDataType = itk::PolyData<TPixelType>;

  polyData->Initialize();

  using PointsContainerType = typename PolyDataType::PointsContainer;
  typename PointsContainerType::Pointer pointsContainer = PointsContainerType::New();

  typename PolyDataType::PointType point;
  point[0] = 1.0;
  point[1] = 3.0;
  point[2] = 5.0;
  pointsContainer->InsertElement(0, point);
  point[0] = 2.0;
  point[1] = 4.0;
  point[2] = 6.0;
  pointsContainer->InsertElement(1, point);
  point[0] = 3.0;
  point[1] = 5.0;
  point[2] = 7.0;
  pointsContainer->InsertElement(2, point);
  polyData->SetPoints(pointsContainer);

  point[0] = 4.0;
  point[1] = 6.0;
  point[2] = 8.0;
  polyData->SetPoint(1, point);

  using CellContainerType = typename PolyDataType::CellsContainer;
  typename CellContainerType::Pointer vertices = CellContainerType::New();
  vertices->InsertElement(0, 1);
  vertices->InsertElement(1, 4);
  vertices->InsertElement(2, 1);
  vertices->InsertElement(3, 7);
  polyData->SetVertices(vertices);

  typename CellContainerType::Pointer lines = CellContainerType::New();
  lines->InsertElement(0, 3);
  lines->InsertElement(1, 3);
  lines->InsertElement(2, 4);
  lines->InsertElement(3, 5);

  lines->InsertElement(4, 2);
  lines->InsertElement(5, 7);
  lines->InsertElement(6, 8);
  polyData->SetLines(lines);

  typename CellContainerType::Pointer strips = CellContainerType::New();
  strips->InsertElement(0, 4);
  strips->InsertElement(1, 0);
  strips->InsertElement(2, 2);
  strips->InsertElement(3, 1);
  strips->InsertElement(4, 3);
  polyData->SetTriangleStrips(strips);

  typename CellContainerType::Pointer polygons = CellContainerType::New();
  polygons->InsertElement(0, 3);
  polygons->InsertElement(1, 0);
  polygons->InsertElement(2, 1);
  polygons->InsertElement(3, 2);
  polyData->SetPolygons(polygons);

  return;
}

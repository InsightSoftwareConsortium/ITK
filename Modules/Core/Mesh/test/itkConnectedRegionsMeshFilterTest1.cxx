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

#include "itkConnectedRegionsMeshFilter.h"
#include "itkSphereMeshSource.h"
#include "itkTestingMacros.h"

#include <iostream>

/*
 * Test the mesh connectivity class.
 */
int
itkConnectedRegionsMeshFilterTest1(int, char *[])
{

  /**
   * Some type alias to make things easier.
   */

  using MeshType = itk::Mesh<int>;
  using ConnectFilterType = itk::ConnectedRegionsMeshFilter<MeshType, MeshType>;
  using PointType = itk::Point<float, 3>;

  // Define a simple mesh of three connected pieces. The mesh consists
  // of several different cell types.

  auto inMesh = MeshType::New();

  // Pass the mesh through the filter in a variety of ways.
  //
  PointType::ValueType pInit[3] = { 1, 2, 3 };
  PointType            p = pInit;
  auto                 connect = ConnectFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(connect, ConnectedRegionsMeshFilter, MeshToMeshFilter);


  connect->SetInput(inMesh);
  connect->SetClosestPoint(p);
  connect->AddSeed(0);
  connect->InitializeSeedList();
  connect->AddSeed(1);
  connect->AddSeed(2);
  connect->DeleteSeed(1);
  connect->Update();

  // Test with each of the extraction modes
  connect->SetExtractionModeToAllRegions();
  auto extractionMode = 4;
  ITK_TEST_SET_GET_VALUE(extractionMode, connect->GetExtractionMode());
  connect->Update();

  extractionMode = 1;
  connect->SetExtractionModeToCellSeededRegions();
  ITK_TEST_SET_GET_VALUE(extractionMode, connect->GetExtractionMode());
  connect->Update();

  extractionMode = 5;
  connect->SetExtractionModeToClosestPointRegion();
  ITK_TEST_SET_GET_VALUE(extractionMode, connect->GetExtractionMode());
  connect->Update();

  extractionMode = 3;
  connect->SetExtractionModeToLargestRegion();
  ITK_TEST_SET_GET_VALUE(extractionMode, connect->GetExtractionMode());
  connect->Update();

  extractionMode = 0;
  connect->SetExtractionModeToPointSeededRegions();
  ITK_TEST_SET_GET_VALUE(extractionMode, connect->GetExtractionMode());
  connect->Update();

  extractionMode = 2;
  connect->SetExtractionModeToSpecifiedRegions();
  ITK_TEST_SET_GET_VALUE(extractionMode, connect->GetExtractionMode());
  connect->Update();

  // Create a Sphere for running the filter on real input data.
  using SphereMeshSourceType = itk::SphereMeshSource<MeshType>;

  auto meshSource = SphereMeshSourceType::New();

  PointType center;
  center.Fill(0);
  PointType::ValueType scaleInit[3] = { 1, 1, 1 };
  PointType            scale = scaleInit;

  meshSource->SetCenter(center);
  meshSource->SetResolutionX(10);
  meshSource->SetResolutionY(10);
  meshSource->SetScale(scale);
  meshSource->Modified();
  meshSource->Update();

  connect->SetInput(meshSource->GetOutput());

  try
  {
    connect->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

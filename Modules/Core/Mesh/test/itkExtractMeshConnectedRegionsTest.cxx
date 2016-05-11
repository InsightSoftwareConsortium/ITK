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

#include "itkConnectedRegionsMeshFilter.h"
#include "itkSphereMeshSource.h"

#include <iostream>

/*
 * Test the mesh connectivity class.
 */
int itkExtractMeshConnectedRegionsTest(int, char* [])
{

  /**
   * Some typedefs to make things easier.
   */

  typedef itk::Mesh< int >                                   MeshType;
  typedef itk::ConnectedRegionsMeshFilter<MeshType,MeshType> ConnectFilterType;
  typedef itk::Point<float,3>                                PointType;

  // Define a simple mesh of three connected pieces. The mesh consists
  // of several different cell types.

  MeshType::Pointer inMesh = MeshType::New();

  // Pass the mesh through the filter in a variety of ways.
  //
  PointType::ValueType pInit[3] = {1,2,3};
  PointType p = pInit;
  ConnectFilterType::Pointer connect = ConnectFilterType::New();

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
  connect->Update();

  connect->SetExtractionModeToCellSeededRegions();
  connect->Update();

  connect->SetExtractionModeToClosestPointRegion();
  connect->Update();

  connect->SetExtractionModeToLargestRegion();
  connect->Update();

  connect->SetExtractionModeToPointSeededRegions();
  connect->Update();

  connect->SetExtractionModeToSpecifiedRegions();
  connect->Update();

  // Create a Sphere for running the filter on real input data.
  typedef itk::SphereMeshSource< MeshType >  SphereMeshSourceType;

  SphereMeshSourceType::Pointer meshSource = SphereMeshSourceType::New();

  PointType center; center.Fill(0);
  PointType::ValueType scaleInit[3] = {1,1,1};
  PointType scale = scaleInit;

  meshSource->SetCenter(center);
  meshSource->SetResolutionX( 10 );
  meshSource->SetResolutionY( 10 );
  meshSource->SetScale(scale);
  meshSource->Modified();
  meshSource->Update();

  connect->SetInput( meshSource->GetOutput() );

  try
    {
    connect->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

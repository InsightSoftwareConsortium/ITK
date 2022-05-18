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

#include "itkQuadEdgeMesh.h"
#include "itkRegularSphereMeshSource.h"
#include "itkMeshFileWriter.h"

#include <iostream>
#include "itkTestingMacros.h"

int
itkRegularSphereQuadEdgeMeshSourceTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputFileName.vtk" << std::endl;
    return EXIT_FAILURE;
  }

  using MeshType = itk::QuadEdgeMesh<float, 3>;

  using SphereMeshSourceType = itk::RegularSphereMeshSource<MeshType>;

  auto mySphereMeshSource = SphereMeshSourceType::New();

  using PointType = SphereMeshSourceType::PointType;
  using VectorType = SphereMeshSourceType::VectorType;

  PointType center;
  center.Fill(0.0);

  VectorType scale;
  scale.Fill(1.0);

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(1);
  mySphereMeshSource->SetScale(scale);

  mySphereMeshSource->Modified();

  ITK_TRY_EXPECT_NO_EXCEPTION(mySphereMeshSource->Update());


  std::cout << "mySphereMeshSource: " << mySphereMeshSource;

  MeshType::Pointer myMesh = mySphereMeshSource->GetOutput();

  PointType pt;
  pt.Fill(0.);

  for (unsigned int i = 0; i < myMesh->GetNumberOfPoints(); ++i)
  {
    myMesh->GetPoint(i, &pt);
    std::cout << "Point[" << i << "]: " << pt << std::endl;
  }

  using WriterType = itk::MeshFileWriter<MeshType>;

  auto writer = WriterType::New();
  writer->SetInput(myMesh);
  writer->SetFileName(argv[1]);
  writer->Write();

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}

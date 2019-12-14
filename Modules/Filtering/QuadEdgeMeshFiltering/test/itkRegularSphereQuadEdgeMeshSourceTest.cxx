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

  SphereMeshSourceType::Pointer mySphereMeshSource = SphereMeshSourceType::New();

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

  try
  {
    mySphereMeshSource->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "mySphereMeshSource: " << mySphereMeshSource;

  MeshType::Pointer myMesh = mySphereMeshSource->GetOutput();

  PointType pt;
  pt.Fill(0.);

  std::cout << "Testing itk::RegularSphereMeshSource " << std::endl;

  for (unsigned int i = 0; i < myMesh->GetNumberOfPoints(); i++)
  {
    myMesh->GetPoint(i, &pt);
    std::cout << "Point[" << i << "]: " << pt << std::endl;
  }

  using WriterType = itk::MeshFileWriter<MeshType>;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(myMesh);
  writer->SetFileName(argv[1]);
  writer->Write();

  std::cout << "Test End " << std::endl;

  return EXIT_SUCCESS;
}

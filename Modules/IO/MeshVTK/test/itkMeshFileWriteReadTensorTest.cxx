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
#include "itkMeshFileWriter.h"
#include "itkVTKPolyDataMeshIO.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkTestingMacros.h"

int
itkMeshFileWriteReadTensorTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << "<OutputMesh2D.vtk> "
              << "<OutputMesh3D.vtk> " << std::endl;
    return EXIT_FAILURE;
  }
  const auto outputMesh2D = std::string(argv[1]);
  const auto outputMesh3D = std::string(argv[2]);

  using TensorElementType = float;
  using Tensor2dType = itk::SymmetricSecondRankTensor<TensorElementType, 2>;
  using Tensor3dType = itk::SymmetricSecondRankTensor<TensorElementType, 3>;

  using Mesh2dType = itk::Mesh<Tensor2dType, 2>;
  using Mesh3dType = itk::Mesh<Tensor3dType, 3>;

  using MeshWriter2dType = itk::MeshFileWriter<Mesh2dType>;
  using MeshWriter3dType = itk::MeshFileWriter<Mesh3dType>;

  // Test the 2D case
  std::cout << "Testing VTKPolyDataMeshIO for a mesh with 2D tensor pixels..." << std::endl;

  Mesh2dType::PointType point2d;
  point2d.Fill(1);

  Mesh2dType::PixelType pixel2d;
  pixel2d.Fill(0);

  auto mesh2d = Mesh2dType::New();
  mesh2d->SetPoint(0, point2d);
  mesh2d->SetPointData(0, pixel2d);

  auto mesh2dWriter = MeshWriter2dType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(mesh2dWriter, MeshFileWriter, ProcessObject);

  itk::VTKPolyDataMeshIO::Pointer vtkPolyDataMeshIO = itk::VTKPolyDataMeshIO::New();

  // Supported read extensions are empty by default
  ITK_TEST_EXPECT_TRUE(vtkPolyDataMeshIO->GetSupportedReadExtensions().size() == 0);

  const itk::MeshIOBase::ArrayOfExtensionsType supportedExtensions{ ".vtk" };
  ITK_TEST_EXPECT_TRUE(vtkPolyDataMeshIO->GetSupportedWriteExtensions() == supportedExtensions);

  mesh2dWriter->SetMeshIO(vtkPolyDataMeshIO);
  ITK_TEST_SET_GET_VALUE(vtkPolyDataMeshIO, mesh2dWriter->GetMeshIO());

  mesh2dWriter->SetFileName(outputMesh2D);
  ITK_TEST_SET_GET_VALUE(outputMesh2D, std::string(mesh2dWriter->GetFileName()));

  bool useCompression = false;
  ITK_TEST_SET_GET_BOOLEAN(mesh2dWriter, UseCompression, useCompression);

  mesh2dWriter->SetInput(mesh2d);

  ITK_TRY_EXPECT_NO_EXCEPTION(mesh2dWriter->Update());


  std::cout << "End of VTKPolyDataMeshIO 2D test. Completed successfully!" << std::endl << std::endl;


  // Test the 3D case
  std::cout << "Testing VTKPolyDataMeshIO for a mesh with 3D tensor pixels..." << std::endl;

  Mesh3dType::PointType point3d;
  point3d.Fill(1);

  Mesh3dType::PixelType pixel3d;
  pixel3d.Fill(0);

  auto mesh3d = Mesh3dType::New();
  mesh3d->SetPoint(0, point3d);
  mesh3d->SetPointData(0, pixel3d);

  auto mesh3dWriter = MeshWriter3dType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(mesh3dWriter, MeshFileWriter, ProcessObject);


  itk::VTKPolyDataMeshIO::Pointer vtkPolyDataMeshIO2 = itk::VTKPolyDataMeshIO::New();
  mesh3dWriter->SetMeshIO(vtkPolyDataMeshIO2);
  ITK_TEST_SET_GET_VALUE(vtkPolyDataMeshIO2, mesh3dWriter->GetMeshIO());

  mesh3dWriter->SetFileName(outputMesh3D);
  ITK_TEST_SET_GET_VALUE(outputMesh3D, std::string(mesh3dWriter->GetFileName()));

  mesh3dWriter->SetInput(mesh3d);

  ITK_TRY_EXPECT_NO_EXCEPTION(mesh3dWriter->Update());


  std::cout << "End of VTKPolyDataMeshIO 3D test. Completed successfully!" << std::endl << std::endl;

  return EXIT_SUCCESS;
}

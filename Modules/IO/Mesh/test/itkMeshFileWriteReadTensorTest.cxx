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
#include "itkMesh.h"
#include "itkMeshFileWriter.h"
#include "itkVTKPolyDataMeshIO.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkTestingMacros.h"

int itkMeshFileWriteReadTensorTest( int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: "
              << "<OutputMesh2D.vtk> "
              << "<OutputMesh3D.vtk> "
              << std::endl;
    return EXIT_FAILURE;
    }
  const char * outputMesh2D = argv[1];
  const char * outputMesh3D = argv[2];

  typedef float                                               TensorElementType;
  typedef itk::SymmetricSecondRankTensor<TensorElementType,2> Tensor2dType;
  typedef itk::SymmetricSecondRankTensor<TensorElementType,3> Tensor3dType;

  typedef itk::Mesh<Tensor2dType,2> Mesh2dType;
  typedef itk::Mesh<Tensor3dType,3> Mesh3dType;

  typedef itk::MeshFileWriter<Mesh2dType> MeshWriter2dType;
  typedef itk::MeshFileWriter<Mesh3dType> MeshWriter3dType;

  // Test the 2D case
  std::cout << "Testing VTKPolyDataMeshIO for a mesh with 2D tensor pixels..." << std::endl;

  Mesh2dType::PointType point2d;
  point2d.Fill( 1 );

  Mesh2dType::PixelType pixel2d;
  pixel2d.Fill( 0 );

  Mesh2dType::Pointer mesh2d = Mesh2dType::New();
  mesh2d->SetPoint( 0, point2d );
  mesh2d->SetPointData( 0, pixel2d );

  MeshWriter2dType::Pointer mesh2dWriter = MeshWriter2dType::New();

  EXERCISE_BASIC_OBJECT_METHODS( mesh2dWriter, MeshFileWriter, ProcessObject );

  mesh2dWriter->SetMeshIO( itk::VTKPolyDataMeshIO::New() );
  mesh2dWriter->SetInput( mesh2d );
  mesh2dWriter->SetFileName( outputMesh2D );
  try
    {
    mesh2dWriter->Update();
    }
  catch( itk::ExceptionObject &excp )
    {
    std::cerr << "Failed the VTKPolyDataMeshIO 2d test" << excp;
    return EXIT_FAILURE;
    }
  std::cout << "End of VTKPolyDataMeshIO 2D test. Completed successfully!" << std::endl << std::endl;


  // Test the 3D case
  std::cout << "Testing VTKPolyDataMeshIO for a mesh with 3D tensor pixels..." << std::endl;

  Mesh3dType::PointType point3d;
  point3d.Fill( 1 );

  Mesh3dType::PixelType pixel3d;
  pixel3d.Fill( 0 );

  Mesh3dType::Pointer mesh3d = Mesh3dType::New();
  mesh3d->SetPoint( 0, point3d );
  mesh3d->SetPointData( 0, pixel3d );

  MeshWriter3dType::Pointer mesh3dWriter = MeshWriter3dType::New();

  EXERCISE_BASIC_OBJECT_METHODS( mesh3dWriter, MeshFileWriter, ProcessObject );

  mesh3dWriter->SetMeshIO( itk::VTKPolyDataMeshIO::New() );
  mesh3dWriter->SetInput( mesh3d );
  mesh3dWriter->SetFileName( outputMesh3D );
  try
    {
    mesh3dWriter->Update();
    }
  catch( itk::ExceptionObject &excp )
    {
    std::cerr << "Failed the VTKPolyDataMeshIO 3D test" << excp;
    return EXIT_FAILURE;
    }
  std::cout << "End of VTKPolyDataMeshIO 3D test. Completed successfully!" << std::endl << std::endl;

  return EXIT_SUCCESS;
}

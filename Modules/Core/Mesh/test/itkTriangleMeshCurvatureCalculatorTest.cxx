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

#include <iostream>

#include "itkRegularSphereMeshSource.h"
#include "itkTriangleMeshCurvatureCalculator.h"
#include "itkTestingMacros.h"
#include "itkMeshFileReader.h"


/* Test the Curvature calculator for sphere source and a vtk mesh file */
int
itkTriangleMeshCurvatureCalculatorTest(int argc, char * argv[])
{
  const unsigned int Dimension = 3;
  using PixelType = double;

  // Declare the type of the input mesh.
  using TriangleMeshType = itk::Mesh<PixelType, Dimension>;

  // Create Curvature calculator and set the type.
  using CurvatureCalculatorType = itk::TriangleMeshCurvatureCalculator<TriangleMeshType>;
  auto curvCalculator = CurvatureCalculatorType::New();

  // Exercise basic object methods
  ITK_EXERCISE_BASIC_OBJECT_METHODS(curvCalculator, TriangleMeshCurvatureCalculator, Object);

  // Test for empty mesh, curvature data should be null.
  TriangleMeshType::Pointer triangleMesh = TriangleMeshType::New();

  // Test if calling Compute() before setting mesh throws error.
  ITK_TRY_EXPECT_EXCEPTION(curvCalculator->Compute());

  curvCalculator->SetTriangleMesh(triangleMesh);

  // Test the curvature type setter and getter.
  curvCalculator->SetCurvatureType(itk::TriangleMeshCurvatureCalculatorEnums::Curvatures::MeanCurvature);
  ITK_TEST_SET_GET_VALUE(itk::TriangleMeshCurvatureCalculatorEnums::Curvatures::MeanCurvature,
                         curvCalculator->GetCurvatureType());

  // Now test if it throws exception for not-supported curvature type.
  ITK_TRY_EXPECT_EXCEPTION(curvCalculator->Compute());

  curvCalculator->SetCurvatureTypeToGaussian();

  // Test if Gauss curvature is set.
  ITK_TEST_SET_GET_VALUE(itk::TriangleMeshCurvatureCalculatorEnums::Curvatures::GaussCurvature,
                         curvCalculator->GetCurvatureType());

  curvCalculator->Compute();

  // Output should be null for empty mesh.
  auto gaussCurvatureData = curvCalculator->GetGaussCurvatureData();
  ITK_TEST_EXPECT_TRUE(gaussCurvatureData == nullptr);


  // Test for Sphere mesh Source
  // Declare Triangle mesh source
  using SphereMeshSourceType = itk::RegularSphereMeshSource<TriangleMeshType>;
  using PointType = SphereMeshSourceType::PointType;
  using VectorType = SphereMeshSourceType::VectorType;

  auto      mySphereMeshSource = SphereMeshSourceType::New();
  PointType center;
  center.Fill(0);
  PointType::ValueType scaleInit_1[Dimension] = { 5, 5, 5 };
  VectorType           scale = scaleInit_1;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(1);
  mySphereMeshSource->SetScale(scale);
  mySphereMeshSource->Update();

  triangleMesh = mySphereMeshSource->GetOutput();

  curvCalculator->SetTriangleMesh(triangleMesh);
  curvCalculator->SetCurvatureTypeToGaussian();
  curvCalculator->Compute();

  gaussCurvatureData = curvCalculator->GetGaussCurvatureData();

  // Values obtained using the VTK Gaussian Curvature
  float v1 = 0.06087285;
  float v2 = 0.04463759;

  // Test if values are correct for scale 5 and resolution 1 sphere
  for (unsigned int k = 0; k < triangleMesh->GetNumberOfPoints(); ++k)
  {
    if (k < 6)
    {
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(gaussCurvatureData->GetElement(k), v1));
    }
    else
    {
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(gaussCurvatureData->GetElement(k), v2));
    }
  }


  // Test the method for different scale. The curvature should decrease for larger sphere.
  PointType::ValueType scaleInit_2[Dimension] = { 100, 100, 100 };
  scale = scaleInit_2;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(1);
  mySphereMeshSource->SetScale(scale);
  mySphereMeshSource->Update();

  triangleMesh = mySphereMeshSource->GetOutput();
  curvCalculator->SetTriangleMesh(triangleMesh);
  curvCalculator->SetCurvatureTypeToGaussian();
  curvCalculator->Compute();

  gaussCurvatureData = curvCalculator->GetGaussCurvatureData();

  float v3 = 0.00015218;
  float v4 = 0.00011159;

  // Test if values are correct for scale 100 and resolution 1 sphere.
  for (unsigned int k = 0; k < triangleMesh->GetNumberOfPoints(); ++k)
  {
    if (k < 6)
    {
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(gaussCurvatureData->GetElement(k), v3));
    }
    else
    {
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(gaussCurvatureData->GetElement(k), v4));
    }
  }

  // Test for non-triangle Mesh. It should throw Exception
  using CellType = TriangleMeshType::CellType;
  using TetrahedronType = itk::TetrahedronCell<CellType>;
  CellType::CellAutoPointer cellpointer;

  // Insert a Tetrahedron Cell in the mesh

  // First obtain the cell on that index to be deleted to avoid memory leak
  CellType::CellAutoPointer cellToDelete;
  triangleMesh->GetCell(0, cellToDelete);
  cellToDelete.TakeOwnership();

  // Next insert a Tetrahedron Cell on that index
  cellpointer.TakeOwnership(new TetrahedronType);
  cellpointer->SetPointId(0, 0);
  cellpointer->SetPointId(1, 1);
  cellpointer->SetPointId(2, 2);
  cellpointer->SetPointId(3, 3);
  triangleMesh->SetCell(0, cellpointer);

  curvCalculator->SetTriangleMesh(triangleMesh);
  curvCalculator->SetCurvatureTypeToGaussian();
  ITK_TRY_EXPECT_EXCEPTION(curvCalculator->Compute());

  // Test streaming enumeration for TriangleMeshCurvatureCalculatorEnums elements
  const std::set<itk::TriangleMeshCurvatureCalculatorEnums::Curvatures> allCurvatures{
    itk::TriangleMeshCurvatureCalculatorEnums::Curvatures::GaussCurvature,
    itk::TriangleMeshCurvatureCalculatorEnums::Curvatures::MeanCurvature,
    itk::TriangleMeshCurvatureCalculatorEnums::Curvatures::MinCurvature,
    itk::TriangleMeshCurvatureCalculatorEnums::Curvatures::MaxCurvature
  };
  for (const auto & ee : allCurvatures)
  {
    std::cout << "STREAMED ENUM VALUE itk::TriangleMeshCurvatureCalculatorEnums::Curvatures: " << ee << std::endl;
  }

  // Test for a vtk mesh obtained using MeshFileReader.
  if (argc > 1)
  {
    using ReaderType = itk::MeshFileReader<TriangleMeshType>;
    ReaderType::Pointer polyDataReader = ReaderType::New();
    polyDataReader->SetFileName(argv[1]);
    polyDataReader->Update();

    TriangleMeshType::Pointer inputMesh = polyDataReader->GetOutput();
    curvCalculator->SetTriangleMesh(inputMesh);
    curvCalculator->SetCurvatureTypeToGaussian();
    curvCalculator->Compute();

    gaussCurvatureData = curvCalculator->GetGaussCurvatureData();
    ITK_TEST_EXPECT_TRUE(gaussCurvatureData->Size() == inputMesh->GetNumberOfPoints());
  }

  return EXIT_SUCCESS;
}

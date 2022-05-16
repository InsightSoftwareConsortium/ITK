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
#include "itkRegularSphereMeshSource.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkSimplexMeshAdaptTopologyFilter.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkTestingMacros.h"

int
itkSimplexMeshAdaptTopologyFilterTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " threshold selectionMethod" << std::endl;
    return EXIT_FAILURE;
  }

  // Declare the type of the input and output mesh
  using TriangleMeshTraits = itk::DefaultDynamicMeshTraits<double, 3, 3, double, double>;
  using SimplexMeshTraits = itk::DefaultDynamicMeshTraits<double, 3, 3, double, double>;
  using TriangleMeshType = itk::Mesh<double, 3, TriangleMeshTraits>;
  using SimplexMeshType = itk::SimplexMesh<double, 3, SimplexMeshTraits>;

  // declare triangle mesh source
  using SphereMeshSourceType = itk::RegularSphereMeshSource<TriangleMeshType>;
  using PointType = SphereMeshSourceType::PointType;
  using VectorType = SphereMeshSourceType::VectorType;

  // declare the triangle to simplex mesh filter
  using SimplexFilterType = itk::TriangleMeshToSimplexMeshFilter<TriangleMeshType, SimplexMeshType>;

  auto      mySphereMeshSource = SphereMeshSourceType::New();
  PointType center;
  center.Fill(10);
  PointType::ValueType scaleInit[3] = { 3, 3, 3 };
  VectorType           scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(2);
  mySphereMeshSource->SetScale(scale);

  std::cout << "Triangle mesh created. " << std::endl;

  auto simplexFilter = SimplexFilterType::New();
  simplexFilter->SetInput(mySphereMeshSource->GetOutput());
  simplexFilter->Update();

  SimplexMeshType::Pointer simplexMesh = simplexFilter->GetOutput();
  simplexMesh->DisconnectPipeline();

  std::cout << "Simplex Mesh: " << simplexMesh << std::endl;

  using FilterType = itk::SimplexMeshAdaptTopologyFilter<SimplexMeshType, SimplexMeshType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, SimplexMeshAdaptTopologyFilter, MeshToMeshFilter);


  auto threshold = std::stod(argv[1]);
  filter->SetThreshold(threshold);
  ITK_TEST_SET_GET_VALUE(threshold, filter->GetThreshold());

  auto selectionMethod = std::stoi(argv[2]);
  filter->SetSelectionMethod(selectionMethod);
  ITK_TEST_SET_GET_VALUE(selectionMethod, filter->GetSelectionMethod());

  filter->SetInput(simplexMesh);
  filter->Update();

  std::cout << "ModifiedCount: " << filter->GetModifiedCount() << std::endl;

  std::cout << "[TEST DONE]" << std::endl;

  return EXIT_SUCCESS;
}

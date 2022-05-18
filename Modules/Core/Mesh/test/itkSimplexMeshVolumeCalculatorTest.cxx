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

#include "itkSimplexMeshVolumeCalculator.h"
#include "itkRegularSphereMeshSource.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkTestingMacros.h"

int
itkSimplexMeshVolumeCalculatorTest(int, char *[])
{
  // Declare the type of the input and output mesh
  using MeshTraits = itk::DefaultDynamicMeshTraits<double, 3, 3, double, double, double>;

  using TriangleMeshType = itk::Mesh<double, 3, MeshTraits>;
  using SimplexMeshType = itk::SimplexMesh<double, 3, MeshTraits>;


  // declare triangle mesh source
  using SphereMeshSourceType = itk::RegularSphereMeshSource<TriangleMeshType>;
  using PointType = SphereMeshSourceType::PointType;
  using VectorType = SphereMeshSourceType::VectorType;

  // Declare the type of the gradient image
  using SimplexFilterType = itk::TriangleMeshToSimplexMeshFilter<TriangleMeshType, SimplexMeshType>;

  auto      mySphereMeshSource = SphereMeshSourceType::New();
  PointType center;
  center.Fill(0);
  PointType::ValueType scaleInit[3] = { 10, 10, 10 };
  VectorType           scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetScale(scale);

  auto simplexFilter = SimplexFilterType::New();
  simplexFilter->SetInput(mySphereMeshSource->GetOutput());

  using VolumeCalculatorType = itk::SimplexMeshVolumeCalculator<SimplexMeshType>;


  auto calculator = VolumeCalculatorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(calculator, SimplexMeshVolumeCalculator, Object);


  calculator->SetSimplexMesh(simplexFilter->GetOutput());
  for (int i = 1; i <= 5; ++i)
  {
    mySphereMeshSource->SetResolution(i);
    simplexFilter->Update();

    calculator->Compute();
    std::cout << "Resolution: " << i << ", Volume: " << calculator->GetVolume() << ", Area: " << calculator->GetArea()
              << std::endl;
  }

  calculator->Print(std::cout);

  double volume = calculator->GetVolume();

  const double pi = std::atan(1.0) * 4.0;
  const double knownVolume = 4.0 / 3.0 * pi * (1000.0); // scale was 10 = radius

  std::cout << "knownVolume: " << knownVolume << " versus computedVolume: " << volume << std::endl;

  if (itk::Math::abs(volume - knownVolume) > (1e-2 * knownVolume))
  {
    std::cerr << "Error in the Volume computation " << std::endl;
    std::cerr << "We expected " << knownVolume << std::endl;
    std::cerr << "But we got  " << volume << std::endl;
    return EXIT_FAILURE;
  }


  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;
}

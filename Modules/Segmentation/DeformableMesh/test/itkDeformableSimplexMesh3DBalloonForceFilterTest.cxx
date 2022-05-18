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
#include "itkDefaultDynamicMeshTraits.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkDeformableSimplexMesh3DBalloonForceFilter.h"

#include "itkSobelEdgeDetectionImageFilter.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkTestingMacros.h"

int
itkDeformableSimplexMesh3DBalloonForceFilterTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " kappa" << std::endl;
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

  using DeformFilterType = itk::DeformableSimplexMesh3DBalloonForceFilter<SimplexMeshType, SimplexMeshType>;

  std::cout << "Creating dummy image...";

  using OriginalImageType = itk::Image<float, 3>;
  using IndexType = OriginalImageType::IndexType;
  using ImageSizeType = OriginalImageType::SizeType;

  auto originalImage = OriginalImageType::New();

  ImageSizeType imageSize;
  imageSize.Fill(20);
  originalImage->SetRegions(imageSize);
  originalImage->Allocate();


  IndexType index;
  for (int x = 0; x < 20; ++x)
  {
    for (int y = 0; y < 20; ++y)
    {
      for (int z = 0; z < 20; ++z)
      {
        index[0] = x;
        index[1] = y;
        index[2] = z;
        if (((x == 5 || x == 15) && y >= 5 && y <= 15 && z >= 5 && z <= 15) ||
            ((y == 5 || y == 15) && x >= 5 && x <= 15 && z >= 5 && z <= 15) ||
            ((z == 5 || z == 15) && y >= 5 && y <= 15 && x >= 5 && x <= 15))
        {
          originalImage->SetPixel(index, 1);
        }
        else
        {
          originalImage->SetPixel(index, 0);
        }
      }
    }
  }

  using EdgeFilterType = itk::SobelEdgeDetectionImageFilter<OriginalImageType, OriginalImageType>;

  auto edgeFilter = EdgeFilterType::New();
  edgeFilter->SetInput(originalImage);
  edgeFilter->Update();

  using GradientImageType = DeformFilterType::GradientImageType;
  using GradientFilterType = itk::GradientRecursiveGaussianImageFilter<OriginalImageType, GradientImageType>;

  auto gradientFilter = GradientFilterType::New();
  gradientFilter->SetInput(edgeFilter->GetOutput());
  gradientFilter->SetSigma(1.0);
  gradientFilter->Update();

  std::cout << "done." << std::endl;

  auto deformFilter = DeformFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    deformFilter, DeformableSimplexMesh3DBalloonForceFilter, DeformableSimplexMesh3DFilter);


  deformFilter->SetInput(simplexFilter->GetOutput());
  deformFilter->SetGradient(gradientFilter->GetOutput());
  deformFilter->SetAlpha(0.2);
  deformFilter->SetBeta(0.1);

  auto kappa = std::stod(argv[1]);
  deformFilter->SetKappa(kappa);
  ITK_TEST_SET_GET_VALUE(kappa, deformFilter->GetKappa());

  deformFilter->SetIterations(100);
  deformFilter->SetRigidity(0);
  deformFilter->Update();

  SimplexMeshType::Pointer deformResult = deformFilter->GetOutput();

  std::cout << "Deformation Result: " << deformResult << std::endl;


  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;
}

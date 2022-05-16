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
#include "itkDeformableSimplexMesh3DFilter.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkSigmoidImageFilter.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkSimplexMeshVolumeCalculator.h"
#include "itkTestingMacros.h"

int
itkDeformableSimplexMesh3DFilterTest(int, char *[])
{
  // Declare the type of the input and output mesh

  using TriangleMeshTraits = itk::DefaultDynamicMeshTraits<double, 3, 3, double, double>;
  using SimplexMeshTraits = itk::DefaultDynamicMeshTraits<double, 3, 3, double, double>;
  using TriangleMeshType = itk::Mesh<double, 3, TriangleMeshTraits>;
  using SimplexMeshType = itk::SimplexMesh<double, 3, SimplexMeshTraits>;

  // declare the image class
  using OriginalImageType = itk::Image<float, 3>;
  using IndexType = OriginalImageType::IndexType;
  using ImageSizeType = OriginalImageType::SizeType;

  // decale the deformation class
  using DeformFilterType = itk::DeformableSimplexMesh3DFilter<SimplexMeshType, SimplexMeshType>;
  using GradientImageType = DeformFilterType::GradientImageType;

  // declare all the filters for filtering the image
  using GradientAnisotropicImageType =
    itk::GradientAnisotropicDiffusionImageFilter<OriginalImageType, OriginalImageType>;
  using GradientMagnitudeType =
    itk::GradientMagnitudeRecursiveGaussianImageFilter<OriginalImageType, OriginalImageType>;
  using SigmoidImageType = itk::SigmoidImageFilter<OriginalImageType, OriginalImageType>;
  using GradientFilterType = itk::GradientRecursiveGaussianImageFilter<OriginalImageType, GradientImageType>;


  // declare triangle mesh source
  using SphereMeshSourceType = itk::RegularSphereMeshSource<TriangleMeshType>;
  using PointType = SphereMeshSourceType::PointType;
  using VectorType = SphereMeshSourceType::VectorType;

  // declare the triangle to simplex mesh filter
  using SimplexFilterType = itk::TriangleMeshToSimplexMeshFilter<TriangleMeshType, SimplexMeshType>;


  // decalre the simplex mesh volume calculator
  using SimplexVolumeType = itk::SimplexMeshVolumeCalculator<SimplexMeshType>;

  // create the actual mesh, sphere
  auto      mySphereMeshSource = SphereMeshSourceType::New();
  PointType center;
  center.Fill(10);
  PointType::ValueType scaleInit[3] = { 3, 3, 3 };
  VectorType           scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(2);
  mySphereMeshSource->SetScale(scale);
  mySphereMeshSource->Update();

  std::cout << "Triangle mesh created. " << std::endl;

  // send the sphere mesh ( triangle cells) to create a simplex mesh
  auto simplexFilter = SimplexFilterType::New();
  simplexFilter->SetInput(mySphereMeshSource->GetOutput());
  simplexFilter->Update();

  SimplexMeshType::Pointer simplexMesh = simplexFilter->GetOutput();
  simplexMesh->DisconnectPipeline();

  std::cout << "Simplex Mesh: " << simplexMesh << std::endl;

  std::cout << "Creating dummy image...";
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
  std::cout << "Creating dummy image done" << std::endl;

  std::cout << " starting to Filter Image" << std::endl;
  auto gradientanisotropicfilter = GradientAnisotropicImageType::New();
  gradientanisotropicfilter->SetInput(originalImage);
  gradientanisotropicfilter->SetNumberOfIterations(5);
  gradientanisotropicfilter->SetTimeStep(0.0625);
  gradientanisotropicfilter->SetConductanceParameter(3);
  gradientanisotropicfilter->Update();
  std::cout << "GradientAnisotropicDiffusion is DONE!" << std::endl;

  auto gradientmagnitudefilter = GradientMagnitudeType::New();
  gradientmagnitudefilter->SetInput(gradientanisotropicfilter->GetOutput());
  gradientmagnitudefilter->SetSigma(1.0);
  gradientmagnitudefilter->Update();
  std::cout << "GradientMagnitude is DONE!" << std::endl;

  auto sigmoidimagefilter = SigmoidImageType::New();
  sigmoidimagefilter->SetInput(gradientmagnitudefilter->GetOutput());
  sigmoidimagefilter->SetOutputMinimum(0);
  sigmoidimagefilter->SetOutputMaximum(1);
  sigmoidimagefilter->SetAlpha(10);
  sigmoidimagefilter->SetBeta(100);
  sigmoidimagefilter->Update();
  std::cout << "Sigmoid is DONE!" << std::endl;

  auto gradientFilter = GradientFilterType::New();
  gradientFilter->SetInput(sigmoidimagefilter->GetOutput());
  gradientFilter->SetSigma(1.0);
  gradientFilter->Update();
  std::cout << "GradientMagnitude is DONE!" << std::endl;

  auto deformFilter = DeformFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(deformFilter, DeformableSimplexMesh3DFilter, MeshToMeshFilter);


  constexpr unsigned int numberOfCycles = 100;
  double                 alpha = 0.1;
  double                 beta = -0.1;
  double                 gamma = 0.05;
  double                 damping = 0.65;
  int                    iterations = 5;
  unsigned int           rigidity = 1;

  for (unsigned int i = 0; i < numberOfCycles; ++i)
  {
    // must disconnect the pipeline
    simplexMesh->DisconnectPipeline();
    deformFilter->SetInput(simplexMesh);
    deformFilter->SetGradient(gradientFilter->GetOutput());

    deformFilter->SetAlpha(alpha);
    ITK_TEST_SET_GET_VALUE(alpha, deformFilter->GetAlpha());

    deformFilter->SetBeta(beta);
    ITK_TEST_SET_GET_VALUE(beta, deformFilter->GetBeta());

    deformFilter->SetGamma(gamma);
    ITK_TEST_SET_GET_VALUE(gamma, deformFilter->GetGamma());

    deformFilter->SetDamping(damping);
    ITK_TEST_SET_GET_VALUE(damping, deformFilter->GetDamping());

    deformFilter->SetIterations(iterations);
    ITK_TEST_SET_GET_VALUE(iterations, deformFilter->GetIterations());

    deformFilter->SetRigidity(rigidity);
    ITK_TEST_SET_GET_VALUE(rigidity, deformFilter->GetRigidity());

    deformFilter->Update();
  }

  std::cout << "Deform filter Step: " << deformFilter->GetStep() << std::endl;

  SimplexMeshType::Pointer deformResult = deformFilter->GetOutput();

  // calculate the volume of the mesh
  auto volumecalculator = SimplexVolumeType::New();
  volumecalculator->SetSimplexMesh(deformFilter->GetOutput());
  volumecalculator->Compute();

  std::cout << "whole volume is " << volumecalculator->GetVolume() << std::endl;
  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;
}

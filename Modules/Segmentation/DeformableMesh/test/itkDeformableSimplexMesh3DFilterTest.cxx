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

int itkDeformableSimplexMesh3DFilterTest(int , char * [] )
{
  // Declare the type of the input and output mesh

  typedef itk::DefaultDynamicMeshTraits<double, 3, 3,double,double>  TriangleMeshTraits;
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3, double,double> SimplexMeshTraits;
  typedef itk::Mesh<double,3, TriangleMeshTraits>                    TriangleMeshType;
  typedef itk::SimplexMesh<double,3, SimplexMeshTraits>              SimplexMeshType;

  // declare the image class
  typedef itk::Image<float,3>                       OriginalImageType;
  typedef OriginalImageType::IndexType              IndexType;
  typedef OriginalImageType::SizeType               ImageSizeType;

  // decale the deformation class
  typedef itk::DeformableSimplexMesh3DFilter<SimplexMeshType,SimplexMeshType> DeformFilterType;
  typedef DeformFilterType::GradientImageType                                 GradientImageType;

  // declare all the filters for filtering the image
  typedef itk::GradientAnisotropicDiffusionImageFilter < OriginalImageType, OriginalImageType >  GradientAnisotropicImageType;
  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter < OriginalImageType, OriginalImageType >  GradientMagnitudeType;
  typedef itk::SigmoidImageFilter< OriginalImageType, OriginalImageType > SigmoidImageType;
  typedef itk::GradientRecursiveGaussianImageFilter<OriginalImageType,GradientImageType> GradientFilterType;


  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType> SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType                PointType;
  typedef SphereMeshSourceType::VectorType               VectorType;

   // declare the triangle to simplex mesh filter
  typedef itk::TriangleMeshToSimplexMeshFilter<TriangleMeshType, SimplexMeshType> SimplexFilterType;


  // decalre the simplex mesh volume calculator
  typedef itk::SimplexMeshVolumeCalculator<SimplexMeshType> SimplexVolumeType;

  // create the actual mesh, sphere
  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();
  PointType center;
  center.Fill(10);
  PointType::ValueType scaleInit[3] = {3,3,3};
  VectorType scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(2);
  mySphereMeshSource->SetScale(scale);
  mySphereMeshSource->Update();

  std::cout << "Triangle mesh created. " << std::endl;

  // send the sphere mesh ( triangle cells) to create a simplex mesh
  SimplexFilterType::Pointer simplexFilter = SimplexFilterType::New();
  simplexFilter->SetInput( mySphereMeshSource->GetOutput() );
  simplexFilter->Update();

  SimplexMeshType::Pointer simplexMesh = simplexFilter->GetOutput();
  simplexMesh->DisconnectPipeline();

  std::cout << "Simplex Mesh: " << simplexMesh << std::endl;

  std::cout << "Creating dummy image...";
  OriginalImageType::Pointer originalImage = OriginalImageType::New();

  ImageSizeType imageSize;
  imageSize.Fill(20);
  originalImage->SetRegions( imageSize );
  originalImage->Allocate();

  IndexType index;
  for (int x = 0; x < 20; x++)
  {
    for (int y = 0; y < 20; y++)
    {
      for (int z = 0; z < 20; z++)
      {
        index[0] = x;
        index[1] = y;
        index[2] = z;
        if ( ( (x == 5 || x == 15) && y >= 5 && y <= 15 && z >= 5 && z <= 15)  ||
             ( (y == 5 || y == 15) && x >= 5 && x <= 15 && z >= 5 && z <= 15)  ||
             ( (z == 5 || z == 15) && y >= 5 && y <= 15 && x >= 5 && x <= 15)
           )
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
  GradientAnisotropicImageType::Pointer gradientanisotropicfilter = GradientAnisotropicImageType::New();
  gradientanisotropicfilter->SetInput(originalImage);
  gradientanisotropicfilter->SetNumberOfIterations(5);
  gradientanisotropicfilter->SetTimeStep(0.0625);
  gradientanisotropicfilter->SetConductanceParameter(3);
  gradientanisotropicfilter->Update();
  std::cout << "GradientAnisotropicDiffusion is DONE!" << std::endl;

  GradientMagnitudeType::Pointer gradientmagnitudefilter = GradientMagnitudeType::New();
  gradientmagnitudefilter->SetInput( gradientanisotropicfilter->GetOutput() );
  gradientmagnitudefilter->SetSigma(1.0);
  gradientmagnitudefilter->Update();
  std::cout << "GradientMagnitude is DONE!" << std::endl;

  SigmoidImageType::Pointer sigmoidimagefilter = SigmoidImageType::New();
  sigmoidimagefilter->SetInput( gradientmagnitudefilter->GetOutput());
  sigmoidimagefilter->SetOutputMinimum(0);
  sigmoidimagefilter->SetOutputMaximum(1);
  sigmoidimagefilter->SetAlpha(10);
  sigmoidimagefilter->SetBeta(100);
  sigmoidimagefilter->Update();
  std::cout << "Sigmoid is DONE!" << std::endl;

  GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
  gradientFilter->SetInput( sigmoidimagefilter->GetOutput() );
  gradientFilter->SetSigma(1.0);
  gradientFilter->Update();
  std::cout << "GradientMagnitude is DONE!" << std::endl;

  DeformFilterType::Pointer deformFilter = DeformFilterType::New();

  const unsigned int numberOfCycles = 100;

  for (unsigned int i = 0; i < numberOfCycles; i++)
    {
    // must disconnect the pipeline
    simplexMesh->DisconnectPipeline();
    deformFilter->SetInput( simplexMesh );
    deformFilter->SetGradient( gradientFilter->GetOutput() );
    deformFilter->SetAlpha(0.1);
    deformFilter->SetBeta(-0.1);
    deformFilter->SetIterations(5);
    deformFilter->SetRigidity(1);
    deformFilter->Update();
    }
  SimplexMeshType::Pointer deformResult =  deformFilter->GetOutput();

  // calculate the volume of the mesh
  SimplexVolumeType::Pointer volumecalculator = SimplexVolumeType::New();
  volumecalculator->SetSimplexMesh(deformFilter->GetOutput()  );
  volumecalculator->Compute();

  std::cout << "whole volume is " << volumecalculator->GetVolume() << std::endl;
  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;
}

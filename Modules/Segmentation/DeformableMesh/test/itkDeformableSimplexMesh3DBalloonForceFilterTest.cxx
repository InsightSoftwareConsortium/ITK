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
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkDeformableSimplexMesh3DBalloonForceFilter.h"

#include "itkSobelEdgeDetectionImageFilter.h"
#include "itkGradientRecursiveGaussianImageFilter.h"

int itkDeformableSimplexMesh3DBalloonForceFilterTest(int , char * [] )
{

  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3,double,double>
                                                        TriangleMeshTraits;
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3, double,double>
                                                        SimplexMeshTraits;
  typedef itk::Mesh<double,3, TriangleMeshTraits>       TriangleMeshType;
  typedef itk::SimplexMesh<double,3, SimplexMeshTraits> SimplexMeshType;

  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType> SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType                PointType;
  typedef SphereMeshSourceType::VectorType               VectorType;

  // declare the triangle to simplex mesh filter
  typedef itk::TriangleMeshToSimplexMeshFilter<TriangleMeshType, SimplexMeshType> SimplexFilterType;

  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();
  PointType center;
  center.Fill(10);
  PointType::ValueType scaleInit[3] = {3,3,3};
  VectorType scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution( 2 );
  mySphereMeshSource->SetScale(scale);

  std::cout << "Triangle mesh created. " << std::endl;

  SimplexFilterType::Pointer simplexFilter = SimplexFilterType::New();
  simplexFilter->SetInput( mySphereMeshSource->GetOutput() );

  typedef itk::DeformableSimplexMesh3DBalloonForceFilter<SimplexMeshType,SimplexMeshType> DeformFilterType;

  std::cout << "Creating dummy image...";

  typedef itk::Image<float,3>                       OriginalImageType;
  typedef OriginalImageType::IndexType              IndexType;
  typedef OriginalImageType::SizeType               ImageSizeType;

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

  typedef itk::SobelEdgeDetectionImageFilter<OriginalImageType,OriginalImageType>   EdgeFilterType;

  EdgeFilterType::Pointer edgeFilter = EdgeFilterType::New();
  edgeFilter->SetInput( originalImage );
  edgeFilter->Update();

  typedef DeformFilterType::GradientImageType       GradientImageType;
  typedef itk::GradientRecursiveGaussianImageFilter<OriginalImageType,GradientImageType> GradientFilterType;

  GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
  gradientFilter->SetInput( edgeFilter->GetOutput() );
  gradientFilter->SetSigma( 1.0 );
  gradientFilter->Update();

  std::cout << "done." << std::endl;

  DeformFilterType::Pointer deformFilter = DeformFilterType::New();
  deformFilter->SetInput( simplexFilter->GetOutput() );
  deformFilter->SetGradient( gradientFilter->GetOutput() );
  deformFilter->SetAlpha(0.2);
  deformFilter->SetBeta(0.1);
  deformFilter->SetKappa(0.01);
  deformFilter->SetIterations(100);
  deformFilter->SetRigidity(0);
  deformFilter->Update();

  SimplexMeshType::Pointer deformResult =  deformFilter->GetOutput();

  std::cout << "Deformation Result: " << deformResult << std::endl;


  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;
}

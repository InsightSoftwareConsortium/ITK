/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableSimplexMesh3DGradientConstraintForceFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#pragma warning ( disable : 4503 )
#endif
 
#include <math.h>
#include <iostream>
#include <time.h>

#include "itkImage.h"
#include "itkRegularSphereMeshSource.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkSimplexMeshGeometry.h"
#include "itkSimplexMesh.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkDeformableSimplexMesh3DGradientConstraintForceFilter.h"

#include "itkSobelEdgeDetectionImageFilter.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkTimeProbe.h"

int itkDeformableSimplexMesh3DGradientConstraintForceFilterTest(int , char * [] )
{
  
  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3,double,double> TriangleMeshTraits;
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3, double,double> SimplexMeshTraits;
  typedef itk::Mesh<double,3, TriangleMeshTraits> TriangleMeshType;
  typedef itk::SimplexMesh<double,3, SimplexMeshTraits> SimplexMeshType;

  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType>  SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType PointType;
  typedef SphereMeshSourceType::VectorType VectorType;

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

  typedef itk::DeformableSimplexMesh3DGradientConstraintForceFilter<SimplexMeshType,SimplexMeshType> DeformFilterType;
 
  std::cout << "Creating dummy image...";

  typedef itk::Image<float,3>                       OriginalImageType;
  typedef OriginalImageType::PixelType              PixelType;
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
  deformFilter->SetRange(1);
  deformFilter->SetIterations(100); 
  deformFilter->SetRigidity(0);
  deformFilter->Update();

  SimplexMeshType::Pointer deformResult =  deformFilter->GetOutput();

  std::cout << "Deformation Result: " << deformResult << std::endl;


  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;
}





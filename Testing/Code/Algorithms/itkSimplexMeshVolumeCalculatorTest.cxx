/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSimplexMeshVolumeCalculatorTest.cxx
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
#endif
 
#include <math.h>
#include <iostream>

#include "itkMesh.h"
#include "itkSimplexMesh.h"
#include "itkSimplexMeshGeometry.h"
#include "itkSimplexMeshVolumeCalculator.h"
#include "itkRegularSphereMeshSource.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"

int itkSimplexMeshVolumeCalculatorTest(int , char *[] )
{ 
  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3, double, double, double> MeshTraits;

  typedef itk::Mesh<double,3,MeshTraits> TriangleMeshType;
  typedef itk::SimplexMesh<double,3,MeshTraits> SimplexMeshType;


  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType>  SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType PointType;
  typedef SphereMeshSourceType::VectorType VectorType;

  // Declare the type of the gradient image
  typedef itk::TriangleMeshToSimplexMeshFilter<TriangleMeshType, SimplexMeshType>  SimplexFilterType;

  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();
  PointType center; center.Fill(0);
  PointType::ValueType scaleInit[3] = {10,10,10};
  VectorType scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(2); 
  mySphereMeshSource->SetScale(scale);

  SimplexFilterType::Pointer simplexFilter = SimplexFilterType::New();
  simplexFilter->SetInput( mySphereMeshSource->GetOutput() );
  simplexFilter->Update();
 
  typedef itk::SimplexMeshVolumeCalculator< 
                 SimplexMeshType > VolumeCalculatorType;

    
  VolumeCalculatorType::Pointer calculator = VolumeCalculatorType::New();

  calculator->SetSimplexMesh( simplexFilter->GetOutput() );
  calculator->Compute();

  double volume = calculator->GetVolume();

  std::cout << " volume = " << volume << std::endl;

  const double pi = atan(1.0) * 4.0;
  const double knownVolume = 4.0/3.0 * pi * (1000.0);  // scale was 10 = radius 
  
  if( fabs( volume - knownVolume ) > 1e-4 )
    {
    std::cerr << "Error in the Volume computation " << std::endl;
    std::cerr << "We expected " << knownVolume << std::endl;
    std::cerr << "But we got  " << volume << std::endl;
    return EXIT_FAILURE;
    }
    

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;

}





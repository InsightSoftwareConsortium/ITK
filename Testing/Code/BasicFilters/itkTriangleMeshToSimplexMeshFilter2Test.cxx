/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkTriangleMeshToSimplexMeshFilter2Test.cxx
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

#include "itkMesh.h"
#include "itkSimplexMesh.h"
#include "itkSimplexMeshGeometry.h"
#include "itkRegularSphereMeshSource.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkTimeProbe.h"

int itkTriangleMeshToSimplexMeshFilter2Test(int , char *[] )
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
  
  SimplexMeshType::Pointer simplexMesh = simplexFilter->GetOutput();
  simplexMesh->DisconnectPipeline();

  typedef  SimplexMeshType::NeighborListType              NeighborsListType;
  NeighborsListType* neighbors;
  
  for (int i=0; i < 9; i++)
  {  
    itk::TimeProbe * timeProbe = new itk::TimeProbe(); 
    
    timeProbe->Start();
    unsigned int lastIndex = simplexMesh->GetPoints()->Size();
    for (unsigned int pointIndex = 0; pointIndex < lastIndex; pointIndex++)
      {
      neighbors = simplexMesh->GetNeighbors( pointIndex, i );
      if (pointIndex != (lastIndex - 1))
        {
        delete neighbors;
        }
      }
    timeProbe->Stop();
    std::cout << "Rigidity: " << i << ", neighbor list size: " << neighbors->size() << std::endl;
 
    std::cout << ", Elapsed time (for getting neighbors): " << timeProbe->GetMeanTime() << std::endl;
    delete neighbors;
  }

    
  
//  std::cout << "Simplex Mesh: " << simplexMesh << std::endl;

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;

}





/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSimplexMeshTest.cxx
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
#include <time.h>

#include "itkMesh.h"
#include "itkSimplexMesh.h"
#include "itkSimplexMeshGeometry.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkTimeProbe.h"

int itkSimplexMeshTest(int , char *[] )
{ 
  
  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3, double, double, double> MeshTraits;

  typedef itk::Mesh<double,3,MeshTraits> TriangleMeshType;
  typedef itk::SimplexMesh<double,3,MeshTraits> SimplexMeshType;


  SimplexMeshType::Pointer simplexMesh = SimplexMeshType::New();

  typedef  SimplexMeshType::NeighborListType              NeighborsListType;
  NeighborsListType* neighbors;
  NeighborsListType::iterator nIt, nEnd;
  
  for (int i=0; i < 9; i++)
    {  
    itk::TimeProbe * timeProbe = new itk::TimeProbe(); 
    
    timeProbe->Start();
    for (int pointIndex = 0; pointIndex < simplexMesh->GetPoints()->Size(); pointIndex++)
    {
      neighbors = simplexMesh->GetNeighbors( pointIndex, i );
    }
    timeProbe->Stop();
    if (neighbors)
      {
      std::cout << "Rigidity: " << i << ", neighbor list size: " << neighbors->size() << std::endl;
      }
    std::cout << ", Elapsed time (for getting neighbors): " << timeProbe->GetMeanTime() << std::endl;
    }

    

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;

}





/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimplexMeshAdaptTopologyFilterTest.cxx
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

#include "itkMesh.h"
#include "itkSimplexMesh.h"
#include "itkRegularSphereMeshSource.h"
#include "itkSimplexMeshAdaptTopologyFilter.h"
#include "itkDefaultDynamicMeshTraits.h"

int itkSimplexMeshAdaptTopologyFilterTest( int , char * [] )
{ 

   // Declare the type of the input and output mesh
  typedef itk::DefaultStaticMeshTraits<double, 3, 3, double, double, double>   MeshTraits;
  typedef itk::SimplexMesh< double, 3, MeshTraits > MeshType;
 
  typedef itk::SimplexMeshAdaptTopologyFilter< MeshType, MeshType > FilterType;

  FilterType::Pointer filter = FilterType::New();


  std::cout << "[TEST DONE]" << std::endl;

  return EXIT_SUCCESS;

}





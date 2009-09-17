/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimplexMeshWithFloatCoordRepTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkDefaultDynamicMeshTraits.h"
#include "itkSimplexMesh.h"
#include "itkDeformableSimplexMesh3DFilter.h"

int itkSimplexMeshWithFloatCoordRepTest(int, char* []) 
{
   const unsigned int Dimension = 3;

   typedef float                                                    PixelType;
   typedef float                                                    CoordRepType;
   typedef itk::DefaultDynamicMeshTraits<
     PixelType,Dimension,Dimension,CoordRepType >                   MeshTraits;
   typedef itk::SimplexMesh< PixelType,Dimension,MeshTraits >       MeshType;
   typedef itk::DeformableSimplexMesh3DFilter < MeshType,MeshType > DeformType;

   DeformType::Pointer deform = DeformType::New();
   deform->Print(std::cout);
   return EXIT_SUCCESS;
}

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkParametricSpaceToImageSpaceMeshFilterTest.cxx
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

#include "itkParametricSpaceToImageSpaceMeshFilter.h"
#include "itkMesh.h"
#include "itkImage.h"


int itkParametricSpaceToImageSpaceMeshFilterTest(int, char* [] ) 
{
  typedef   itk::Point<float,2>                          MeshPointDataType;

  typedef   itk::Mesh< MeshPointDataType, 3 >            InputMeshType;
  typedef   itk::Mesh< InputMeshType::PointType, 2 >     ImageSpaceMeshType;

  typedef   itk::ParametricSpaceToImageSpaceMeshFilter<
                                      InputMeshType,
                                      ImageSpaceMeshType
                                     >         ParametricFilterType;

  ParametricFilterType::Pointer      
                      parametercFilter = ParametricFilterType::New();

  return EXIT_SUCCESS;
}





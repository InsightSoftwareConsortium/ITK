/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgem_NoPointTest.cxx
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

#include "itkQuadEdgeMesh.h"
#include <iostream>
#include <vcl_limits.h>

int itkQuadEdgem_NoPointTest( int , char* [] )
{
  typedef itk::QuadEdgeMesh< double, 3 >     MeshType;
  typedef MeshType::QEType                   QEType;
  typedef QEType::OriginRefType              OriginRefType;
  
  OriginRefType VCL_LIMIT = vcl_numeric_limits< OriginRefType >::max( );
  OriginRefType GQE_LIMIT = QEType::m_NoPoint;
  OriginRefType QEM_LIMIT = MeshType::m_NoPoint; 

  std::cout << "VCL limit:     " << VCL_LIMIT << std::endl;
  std::cout << "Geom QE limit: " << GQE_LIMIT << std::endl;
  std::cout << "QE mesh limit: " << QEM_LIMIT << std::endl;
 
  if( VCL_LIMIT != GQE_LIMIT ) return EXIT_FAILURE;
  if( VCL_LIMIT != QEM_LIMIT ) return EXIT_FAILURE;
  if( QEM_LIMIT != GQE_LIMIT ) return EXIT_FAILURE;

  if( VCL_LIMIT == 0 ) return EXIT_FAILURE;
  if( GQE_LIMIT == 0 ) return EXIT_FAILURE;
  if( QEM_LIMIT == 0 ) return EXIT_FAILURE;

  return EXIT_SUCCESS;
}

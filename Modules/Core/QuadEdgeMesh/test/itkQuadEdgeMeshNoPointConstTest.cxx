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

#include "itkQuadEdgeMesh.h"
#include <iostream>

int itkQuadEdgeMeshNoPointConstTest( int , char* [] )
{
  typedef itk::QuadEdgeMesh< double, 3 >     MeshType;
  typedef MeshType::QEType                   QEType;
  typedef QEType::OriginRefType              OriginRefType;

  OriginRefType VCL_LIMIT = std::numeric_limits< OriginRefType >::max( );
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

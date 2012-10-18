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

#include "itkMeshFileTestHelper.h"

int itkMeshFileReadWriteVectorAttributeTest(int argc, char * argv[])
{
  if(argc < 3)
    {
    std::cerr<<"Invalid commands, You need input and output mesh file name "<<std::endl;
    return EXIT_FAILURE;
    }

  bool IsBinary = ( argc > 3 );

  const unsigned int dimension = 3;
  typedef itk::CovariantVector< float, dimension >     PixelType;

  typedef itk::Mesh< PixelType, dimension >            MeshType;
  typedef itk::QuadEdgeMesh< PixelType, dimension >    QEMeshType;

  int result = EXIT_SUCCESS;

  if( test< MeshType >( argv[1], argv[2], IsBinary ) )
    {
    std::cerr << "Failure for itk::Mesh" << std::endl;
    result = EXIT_FAILURE;
    }

  if( test< QEMeshType >( argv[1], argv[2], IsBinary ) )
    {
    std::cerr << "Failure for itk::QuadEdgeMesh" << std::endl;
    result = EXIT_FAILURE;
    }

  return result;
}

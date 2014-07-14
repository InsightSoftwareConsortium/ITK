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

#include "itkQuadEdgeMeshEulerOperatorSplitEdgeFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int itkQuadEdgeMeshEulerOperatorSplitEdgeTest( int, char * [] )
{

  typedef itk::QuadEdgeMesh< double, 3 >                      MeshType;
  typedef MeshType::Pointer                                   MeshPointer;
  typedef MeshType::QEType                                    QEType;

  typedef itk::QuadEdgeMeshEulerOperatorSplitEdgeFunction< MeshType, QEType>
    SplitEdge;

  /////////////////////////////////////////
  //
  //          Split Edge
  //
  /////////////////////////////////////////
  std::cout << "Checking SplitEdge." << std::endl;
  MeshPointer mesh = MeshType::New();
  CreateSquareTriangularMesh<MeshType>( mesh );

  SplitEdge::Pointer splitEdge = SplitEdge::New( );
  std::cout << "     " << "Test No Mesh Input";
  if( splitEdge->Evaluate( (QEType*)1 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

  (void)splitEdge->GetNameOfClass();

  splitEdge->SetInput( mesh );
  std::cout << "     " << "Test No QE Input";
  if( splitEdge->Evaluate( (QEType*)ITK_NULLPTR ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

  std::cout << "     ";
  std::cout << "Split an internal edge (possible).";
  if( !splitEdge->Evaluate( mesh->FindEdge(  6, 12 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 26, 57, 32, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << ".OK" << std::endl;

  std::cout << "Checking SplitEdge." << "OK" << std::endl << std::endl;
  return EXIT_SUCCESS;
}

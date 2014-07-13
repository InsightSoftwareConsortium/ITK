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

#include "itkQuadEdgeMeshEulerOperatorJoinFacetFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int itkQuadEdgeMeshEulerOperatorJoinFacetTest(int argc, char* argv[] )
{
  (void)argc;
  (void)argv;

  typedef itk::QuadEdgeMesh< double, 3 >                      MeshType;
  typedef MeshType::Pointer                                   MeshPointer;
  typedef MeshType::QEType                                    QEType;

  typedef itk::QuadEdgeMeshEulerOperatorJoinFacetFunction< MeshType, QEType>
    JoinFacet;

  // EULER OPERATOR TESTS
  MeshPointer  mesh = MeshType::New();
  CreateSquareTriangularMesh<MeshType>( mesh );

  // The initial configuration and numbering of simpleSquare.vtk:
  //    Vertices: 25 , Edges: 56, Faces: 32, Boundary = 1, Chi = 1
  //
  //   20 --------- 21 --------- 22 --------- 23 --------- 24
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   15 --------- 16 --------- 17 --------- 18 --------- 19
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   10 --------- 11 --------- 12 --------- 13 --------- 14
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
  //
  /////////////////////////////////////////
  //
  //          Join Facet
  //
  /////////////////////////////////////////

  std::cout << "Checking JointFacet." << std::endl;

  JoinFacet::Pointer joinFacet = JoinFacet::New( );
#ifndef NDEBUG
  std::cout << "     " << "Test No Mesh Input";
  if( joinFacet->Evaluate( (QEType*)1 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
#endif

  (void)joinFacet->GetNameOfClass();

  joinFacet->SetInput( mesh );

#ifndef NDEBUG
  std::cout << "     " << "Test QE Input not internal";
  QEType* dummy = new QEType;
  if( joinFacet->Evaluate( dummy ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  delete dummy;
  std::cout << "OK" << std::endl;

  std::cout << "     " << "Test No QE Input";
  if( joinFacet->Evaluate( (QEType*)0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
#endif

  //
  //   20 --------- 21 --------- 22 --------- 23 --------- 24
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   15 --------- 16 --------- 17 --------- 18 --------- 19
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   10 --------- 11 --------- 12 ---<-H--- 13 --------- 14
  //    |        __/ |        __/          __/ |        __/ |
  //    |     __/    |     __/ new      __/    |     __/    |
  //    |  __/       |  __/     face __/       |  __/       |
  //    | /          | /            /          | /          |
  //    5 ---------- 6 ----G->--- 7 ---------- 8 ---------  9
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
  //

  std::cout << "     " << "Test Edge deletion (possible)";
  // Find an internal edge and collapse it
  if( !joinFacet->Evaluate( mesh->FindEdge( 12, 7 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  // Since the edge was internal we lost an edge and an face:
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 25, 55, 31, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 12 ).GetValence( ) != 5 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 12 ).GetValence( )
              << " ]." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
  std::cout << "Checking JointFacet." << "OK" << std::endl << std::endl;

  return EXIT_SUCCESS;

}

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

int itkQuadEdgeMeshDeleteEdgeTest( int , char* [] )
{
  typedef double                            PixelType;
  typedef itk::QuadEdgeMesh< PixelType, 3 > MeshType;
  std::string indent = "    ";

  MeshType::Pointer mesh = MeshType::New( );

  // Points
  MeshType::PointType p0, p1, p2, p3, p4, p5;
  p0[ 0 ] =  0.00000000000000; p0[ 1 ] =  0.00000000000000; p0[ 2 ] = 5.0;
  p1[ 0 ] =  0.00000000000000; p1[ 1 ] = 10.00000000000000; p1[ 2 ] = 0.0;
  p2[ 0 ] = -9.51056516295153; p2[ 1 ] =  3.09016994374947; p2[ 2 ] = 0.0;
  p3[ 0 ] = -5.87785252292473; p3[ 1 ] = -8.09016994374947; p3[ 2 ] = 0.0;
  p4[ 0 ] =  5.87785252292473; p4[ 1 ] = -8.09016994374948; p4[ 2 ] = 0.0;
  p5[ 0 ] =  9.51056516295154; p5[ 1 ] =  3.09016994374947; p5[ 2 ] = 0.0;

  MeshType::PointIdentifier pid0 = mesh->AddPoint( p0 );
  MeshType::PointIdentifier pid1 = mesh->AddPoint( p1 );
  MeshType::PointIdentifier pid2 = mesh->AddPoint( p2 );
  MeshType::PointIdentifier pid3 = mesh->AddPoint( p3 );
  MeshType::PointIdentifier pid4 = mesh->AddPoint( p4 );
  MeshType::PointIdentifier pid5 = mesh->AddPoint( p5 );

  // Cells in a proper way
  mesh->AddEdge( pid3, pid4 );
  mesh->AddEdge( pid4, pid0 );
  mesh->AddEdge( pid0, pid3 );
  mesh->AddFaceTriangle( pid3, pid4, pid0 );

  mesh->AddEdge( pid4, pid5 );
  mesh->AddEdge( pid5, pid0 );
  mesh->AddFaceTriangle( pid4, pid5, pid0 );

  mesh->AddEdge( pid5, pid1 );
  mesh->AddEdge( pid1, pid0 );
  mesh->AddFaceTriangle( pid5, pid1, pid0 );

  mesh->AddEdge( pid1, pid2 );
  mesh->AddEdge( pid2, pid0 );
  mesh->AddEdge( pid2, pid3 );

  int EdgesBefore = mesh->ComputeNumberOfEdges();

  // Deleting two arbitrary edges:
  mesh->DeleteEdge( pid3, pid4 );
  mesh->DeleteEdge( pid0, pid5 );

  std::cout << indent << "Trying to remove only two edges...";
  if ( EdgesBefore - mesh->ComputeNumberOfEdges() == 2 )
    {
    std::cout << "OK." << std::endl;
    return( EXIT_SUCCESS );
    }
  std::cout << "FAILED." << std::endl;
  return( EXIT_FAILURE );
}

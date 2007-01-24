/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshAddFaceTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkQuadEdgeMesh.h"
#include "itkQuadEdgeMeshBoundaryEdgesMeshFunction.h"

int itkQuadEdgeMeshAddFaceTest1( int , char *[] )
{
  typedef itk::QuadEdgeMesh< double, 3 > MeshType;
  typedef MeshType::PointType            PointType;
  typedef MeshType::PointIdentifier      PointIdentifier;
  typedef MeshType::CellIdentifier       CellIdentifier;
  typedef std::vector< PointIdentifier > PointIdList;

  MeshType::Pointer mesh = MeshType::New();

  /*
   *                    p3--------------p2
   *                    / \             / \
   *                   /   \           /   \
   *                  /     \         /     \
   *                 /       \       /       \
   *                /         \     /         \
   *               /           \   /           \
   *              /             \ /             \
   *            p4---------------p0--------------p1
   *              \             / \             /
   *               \           /   \           /
   *                \         /     \         /
   *                 \       /       \       /
   *                  \     /         \     /
   *                   \   /           \   /
   *                    \ /             \ /
   *                    p5---------------p6
   */
  const int NumPoints = 7;
  PointIdentifier pid[NumPoints];

  PointType::CoordRepType        a = sqrt( 3.0 ) / 2.0;

  typedef PointType::ValueArrayType ValueArrayType;

  ValueArrayType pointCoordinates[NumPoints]= 
  { {  0.0, 0.0, 0.0 },
    {  1.0, 0.0, 0.0 },
    {  0.5,   a, 0.0 },
    { -0.5,   a, 0.0 },
    { -1.0, 0.0, 0.0 },
    { -0.5,  -a, 0.0 },
    {  0.5,  -a, 0.0 } };

  PointType points[NumPoints];
  for(int j = 0; j < NumPoints; j++)
    {
    points[j] = pointCoordinates[j];
    }

  for(int i = 0; i < NumPoints; i++)
    {
    pid[i] = mesh->AddPoint( points[i] );
    }

  /////////////////////////////////////////////////////////////
  std::cout << "Adding a degenerate triangle should return false"
    << std::endl;

  if( !mesh->AddFaceTriangle( pid[0], pid[0], pid[2] ) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  /////////////////////////////////////////////////////////////////////
  std::cout << "Adding a triangle with three new edges" << std::endl;
  /*
   *                                    p2
   *                                    / \
   *                                   /   \
   *                                  /     \
   *                                 /       \
   *                                /         \
   *                               /   NEW     \
   *                              /             \
   *                            p0---------------p1
   */
  if( mesh->AddFaceTriangle( pid[0], pid[1], pid[2] ) &&
      mesh->FindEdge( pid[0], pid[1] )                &&
      mesh->FindEdge( pid[1], pid[0] )                &&
      mesh->FindEdge( pid[1], pid[2] )                &&
      mesh->FindEdge( pid[2], pid[1] )                &&
      mesh->FindEdge( pid[2], pid[0] )                &&
      mesh->FindEdge( pid[0], pid[2] ) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  /////////////////////////////////////////////////////////////
  std::cout << "Adding a triangle with two new edges" << std::endl;
  /*
   *                    p3--------------p2
   *                      \             / \
   *                       \           /   \
   *                        \   NEW   /     \
   *                         \       /       \
   *                          \     /         \
   *                           \   /           \
   *                            \ /             \
   *                             p0--------------p1
   */

  if( mesh->AddFaceTriangle( pid[ 0 ], pid[ 2 ], pid[ 3 ] ) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  /////////////////////////////////////////////////////////////
  std::cout << "Adding a flipped triangle" << std::endl;
  /*
   *                    p3--------------p2
   *                      \             / \
   *                       \           /   \
   *                        \         /     \
   *                         \       /       \
   *                          \     /         \
   *                           \   /           \
   *                            \ /             \
   *            p4---------------p0--------------p1
   *              \             /
   *               \           /
   *                \   NEW   /
   *                 \       /
   *                  \     /
   *                   \   /
   *                    \ /
   *                    p5
   */
  if( mesh->AddFaceTriangle( pid[0], pid[5], pid[4] ) &&
      mesh->FindEdge( pid[0], pid[5] )                 &&
      mesh->FindEdge( pid[5], pid[0] )                 &&
      mesh->FindEdge( pid[5], pid[4] )                 &&
      mesh->FindEdge( pid[4], pid[5] )                 &&
      mesh->FindEdge( pid[4], pid[0] )                 &&
      mesh->FindEdge( pid[0], pid[4] ) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  /////////////////////////////////////////////////////////////
  std::cout << "Adding a triangle with an inconsistent orientation should "
    "return false" << std::endl;
  /*
   *                    p3--------------p2
   *                    / \             / \
   *                   /   \           /   \
   *                  /     \         /     \
   *                 /       \       /       \
   *                /         \     /         \
   *               /    NEW    \   /           \
   *              /             \ /             \
   *            p4---------------p0--------------p1
   *              \             /
   *               \           /
   *                \         /
   *                 \       /
   *                  \     /
   *                   \   /
   *                    \ /
   *                    p5
   */
  if( !mesh->AddFaceTriangle( pid[0], pid[3], pid[4] ) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  ///////////////////////////////////////////////////////////////////////
  // typical cases
  std::cout << "Reset triangles" << std::endl;
  mesh = MeshType::New();

  for(int i=0; i < NumPoints; i++)
    {
    pid[i] = mesh->AddPoint( points[i] );
    }

  ///////////////////////////////////////////////////////////////////////
  std::cout << "Adding triangles [0,1,2], [0,2,3], [0,4,5]" << std::endl;

  if( mesh->AddFaceTriangle( pid[0], pid[1], pid[2] ) &&
      mesh->AddFaceTriangle( pid[0], pid[2], pid[3] ) &&
      mesh->AddFaceTriangle( pid[0], pid[4], pid[5] ) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  ///////////////////////////////////////////////////////////////////////
  std::cout << "Adding a triangle with one new edge" << std::endl;
  /*
   *                    p3--------------p2
   *                    / \             / \
   *                   /   \           /   \
   *                  /     \         /     \
   *                 /       \       /       \
   *                /         \     /         \
   *               /    NEW    \   /           \
   *              /             \ /             \
   *            p4---------------p0--------------p1
   *              \             /
   *               \           /
   *                \         /
   *                 \       /
   *                  \     /
   *                   \   /
   *                    \ /
   *                    p5
   */
  if( mesh->AddFaceTriangle( pid[0], pid[3], pid[4] ) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  ///////////////////////////////////////////////////////////////////////
  std::cout << "Adding a triangle with two new edges" << std::endl;
  /*
   *                    p3--------------p2
   *                    / \             / \
   *                   /   \           /   \
   *                  /     \         /     \
   *                 /       \       /       \
   *                /         \     /         \
   *               /           \   /           \
   *              /             \ /             \
   *            p4---------------p0--------------p1
   *              \             / \             /
   *               \           /   \           /
   *                \         /     \   NEW   /
   *                 \       /       \       /
   *                  \     /         \     /
   *                   \   /           \   /
   *                    \ /             \ /
   *                    p5               p6
   */
  if( mesh->AddFaceTriangle( pid[0], pid[6], pid[1] ) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  /////////////////////////////////////////////////////////////
  std::cout << "Adding a triangle to close the 1-ring" << std::endl;
  /*
   *                    p3--------------p2
   *                    / \             / \
   *                   /   \           /   \
   *                  /     \         /     \
   *                 /       \       /       \
   *                /         \     /         \
   *               /           \   /           \
   *              /             \ /             \
   *            p4---------------p0--------------p1
   *              \             / \             /
   *               \           /   \           /
   *                \         /     \         /
   *                 \       /       \       /
   *                  \     /         \     /
   *                   \   /   NEW     \   /
   *                    \ /             \ /
   *                    p5---------------p6
   */
  if( mesh->AddFaceTriangle( pid[5], pid[6], pid[0] ) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  /////////////////////////////////////////////////////////////////////
  /* Try to merge two triangulations with opposite orientations.
   * We build a first surface patch made of two triangles [p0, p1, p2]
   * and [p0, p6, p1] built with counter-clockwise orientation.
   * We build a second surface patch also made of two triangles
   * [p0, p5, p4] and [p0, p4, p3] which are build with clockwise
   * orientation. Hence those two patches have opposite orientations:
   *
   *                    p3              p2
   *                    / \             / \
   *   Build           /   \           /   \
   *   clockwise ===> /     \         /     \
   *                 /       \       /       \
   *                /    *    \     /    *    \   <=== Build counter-
   *               /           \   /           \             clockwise
   *              /             \ /             \
   *            p4---------------p0--------------p1
   *              \             / \             /
   *               \           /   \           /
   *                \    *    /     \    *    /
   *                 \       /       \       /
   *                  \     /         \     /
   *                   \   /           \   /
   *                    \ /             \ /
   *                    p5               p6
   *
   * Then we merge those two patches by building the triangle [p0, p2, p3]:
   *
   *                    p3--------------p2
   *                    / \             / \
   *   Build           /   \           /   \
   *   clockwise ===> /     \   NEW   /     \
   *                 /       \       /       \
   *                /    *    \     /    *    \   <=== Build counter-
   *               /           \   /           \             clockwise
   *              /             \ /             \
   *            p4---------------p0--------------p1
   *              \             / \             /
   *               \           /   \           /
   *                \    *    /     \    *    /
   *                 \       /       \       /
   *                  \     /         \     /
   *                   \   /           \   /
   *                    \ /             \ /
   *                    p5               p6
   *
   * Currently itkQE::Mesh::AddFace() is unable to correct the orientation
   * after merging two patches with opposite orientations.
   */

  std::cout << "Adding a triangle between patches of opposite orientations "
    "should return false" << std::endl;

  MeshType::Pointer inconsistentMesh = MeshType::New();

  for(int i=0; i < NumPoints; i++)
    {
    pid[i] = inconsistentMesh->AddPoint( points[i] );
    }

  if( inconsistentMesh->AddFaceTriangle( pid[0], pid[1], pid[2] )
      && inconsistentMesh->AddFaceTriangle( pid[0], pid[6], pid[1] )
      && inconsistentMesh->AddFaceTriangle( pid[0], pid[5], pid[4] )
      && inconsistentMesh->AddFaceTriangle( pid[0], pid[4], pid[3] ) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Adding another triangle between patches of opposite "
    "orientations. It should return false." << std::endl;

  if( !inconsistentMesh->AddFaceTriangle( pid[0], pid[2], pid[3] ) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  //////////////////////////////////////////////////////////////////////
  /* Adding a quadrangle.
   *
   *                    p3--------------p2
   *                    /               /
   *                   /               /
   *                  /               /
   *                 /               /
   *                /               /
   *               /               /
   *              /               /
   *            p4---------------p0
   */

  std::cout << "Adding a quadrangle where pid4 = pid3 + 1" << std::endl;
  for( int i=0; i < NumPoints; i++)
    {
    pid[i] = mesh->AddPoint( points[i] );
    }

  PointIdList quadPointIds;

  quadPointIds.push_back( pid[0] );
  quadPointIds.push_back( pid[2] );
  quadPointIds.push_back( pid[3] );
  quadPointIds.push_back( pid[4] );

  MeshType::QEPrimal* quadFace = mesh->AddFace( quadPointIds );

  if( quadFace )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Adding a quadrangle where pid4 != pid3 + 1" << std::endl;
  for(int i=0; i < NumPoints; i++)
    {
    pid[i] = mesh->AddPoint( points[i] );
    }

  PointIdList quadPointIds2;

  quadPointIds2.push_back( pid[0] );
  quadPointIds2.push_back( pid[2] );
  quadPointIds2.push_back( pid[4] );
  quadPointIds2.push_back( pid[3] );

  MeshType::QEPrimal* quadFace2 = mesh->AddFace( quadPointIds2 );

  if( quadFace2 )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Indices of the quadrangle: ["
    << quadFace2->GetOrigin() << " "
    << quadFace2->GetLnext()->GetOrigin() << " "
    << quadFace2->GetLnext()->GetLnext()->GetOrigin() << " "
    << quadFace2->GetLnext()->GetLnext()->GetLnext()->GetOrigin() << " "
    << quadFace2->GetLnext()->GetLnext()->GetLnext()->GetLnext()->GetOrigin()
    << "]" << std::endl;

  std::cout << "Correct indices should be: ["
    << pid[0] << " "
    << pid[2] << " "
    << pid[4] << " "
    << pid[3] << " "
    << pid[0] << "]" << std::endl;

  std::cout << "Testing value for pid4" << std::endl;
  if( quadFace2->GetLnext()->GetLnext()->GetLnext()->GetOrigin()
      == pid[3] )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  /* Adding a face with five edges.
   *
   *                    p3--------------p2
   *                    /                 \
   *                   /                   \
   *                  /                     \
   *                 /                       \
   *                /                         \
   *               /                           \
   *              /                             \
   *            p4---------------p0--------------p1
   */
  std::cout << "Adding a face with five edges" << std::endl;

  for(int i=0; i < NumPoints; i++)
    {
    pid[i] = mesh->AddPoint( points[i] );
    }

  PointIdList fivePointIds;

  fivePointIds.push_back( pid[0] );
  fivePointIds.push_back( pid[2] );
  fivePointIds.push_back( pid[4] );
  fivePointIds.push_back( pid[3] );

  MeshType::QEPrimal* fiveFace = mesh->AddFace( fivePointIds );

  if( fiveFace )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  /////////////////////////////////////////////////////////////////////
  /* The Moebius strip should be rejected by itkQE::Mesh::AddFace() of a
   * non-orientable surface. Only six points are defined. The geometry of
   * this Moebius strip is flat (in the z = 0 plane) but geometry is not
   * relevant for this test. Gluing happens along the edge [p0, p1].
   *
   *            p1 ---------p3----------p5-----------p0
   *             |\          |\          |\          |
   *             | \         | \         | \         |
   *             |  \        |  \        |  \        |
   *             |   \       |   \       |   \       |
   *             |    \      |    \      |    \      |
   *             |     \     |     \     |     \     |
   *             |      \    |      \    |      \    |
   *             |       \   |       \   |       \   |
   *             |        \  |        \  |        \  |
   *             |         \ |         \ |         \ |
   *             |          \|          \|          \|
   *            p0----------p2-----------p4----------p1
   *
   */

  MeshType::Pointer moebiusMesh = MeshType::New();

  const int moebNumPoints = 6;
  PointIdentifier moebPid[moebNumPoints];

  ValueArrayType moebPointCoordinates[moebNumPoints] =
  { { 0.0, 0.0, 0.0 },
    { 0.0, 1.0, 0.0 },
    { 1.0, 0.0, 0.0 },
    { 1.0, 1.0, 0.0 },
    { 2.0, 0.0, 0.0 },
    { 2.0, 1.0, 0.0 } };


  PointType moebPoints[moebNumPoints];
  for(int j = 0; j < moebNumPoints; j++)
    {
    moebPoints[j] = moebPointCoordinates[j];
    }

  for( int i=0; i < moebNumPoints; i++ )
    {
    moebPid[i] = moebiusMesh->AddPoint( moebPoints[i] );
    }

  std::cout << "Adding triangles of a non-orientable surface but one "
    "triangle" << std::endl;

  if( moebiusMesh->AddFaceTriangle(moebPid[0], moebPid[2], moebPid[1])
     && moebiusMesh->AddFaceTriangle(moebPid[2], moebPid[3], moebPid[1])
     && moebiusMesh->AddFaceTriangle(moebPid[2], moebPid[4], moebPid[3])
     && moebiusMesh->AddFaceTriangle(moebPid[4], moebPid[5], moebPid[3])
     && moebiusMesh->AddFaceTriangle(moebPid[4], moebPid[1], moebPid[5]) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Adding one more triangle makes the surface non-orientable. "
    "It should return false" << std::endl;

  if( !moebiusMesh->AddFaceTriangle(moebPid[1], moebPid[0], moebPid[5] ) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

   std::cout << "AddFaceTest passed" << std::endl;
   return EXIT_SUCCESS;
}



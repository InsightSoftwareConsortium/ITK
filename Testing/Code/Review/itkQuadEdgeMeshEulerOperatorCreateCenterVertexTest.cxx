/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest.cxx
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

#include <string>

#include "itkQuadEdgeMesh.h"
#include "itkQuadEdgeMeshLineCell.h"
#include "itkQuadEdgeMeshPolygonCell.h"

#include "itkQuadEdgeMeshFunctionBase.h"
#include "itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest( int, char * [] )
{
  
  typedef itk::QuadEdgeMesh< double, 3 >                      MeshType;
  typedef MeshType::Pointer                                   MeshPointer;
  typedef MeshType::QEType                                    QEType;
  typedef MeshType::PointIdentifier                           PointIdentifier;
  typedef MeshType::PointType                                 PointType;
  typedef MeshType::CellType                                  CellType;
  typedef itk::QuadEdgeMeshLineCell< CellType >               LineCellType;

  typedef itk::QuadEdgeMeshEulerOperatorCreateCenterVertexFunction< MeshType,
    QEType> CreateCenterVertex;
 
  /////////////////////////////////////////
  //
  //          Create Center Vertex
  //
  /////////////////////////////////////////
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
  //    |        __/ |        __/ |        __//|        __/ |
  //    |     __/    |     __/    |     __/  / |     __/    |
  //    |  __/       |  __/       |  __/ __x/  |  __/       |
  //    | /          | /          | /___/   \_ | /          |
  //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
  
  std::cout << "Checking CreateCenterVertex." << std::endl;

  MeshPointer mesh = MeshType::New();
  CreateSquareTriangularMesh<MeshType>( mesh );

  CreateCenterVertex::Pointer createCenterVertex = CreateCenterVertex::New( );
#ifndef NDEBUG
  std::cout << "     " << "Test No Mesh Input";
  if( createCenterVertex->Evaluate( (QEType*)1 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
#endif
  
  (void)createCenterVertex->GetNameOfClass(); 

  createCenterVertex->SetInput( mesh );
#ifndef NDEBUG
  std::cout << "     " << "Test QE Input with no left face";
  QEType* dummy = new QEType;
  if( createCenterVertex->Evaluate( dummy ) ) 
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  delete dummy;
  std::cout << "OK" << std::endl;

  std::cout << "     " << "Test No QE Input";
  if( createCenterVertex->Evaluate( (QEType*)0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
#endif

  std::cout << "     ";
  std::cout << "Create a center vertex of a triangle (possible).";
  if( !createCenterVertex->Evaluate( mesh->FindEdge( 8, 2 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 26, 59, 34, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( createCenterVertex->GetNewPointID( ) ).GetValence( ) != 3
)
    {
    std::cout << "FAILED, wrong valence of "
       << mesh->GetPoint( createCenterVertex->GetNewPointID( ) ).GetValence( )
       << " for vertex "
       << createCenterVertex->GetNewPointID( )
       << "." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << ".OK" << std::endl;
  // test with a bigger polygon
  //
  //   20 --------- 21 --------- 22 --------- 23 --------- 24
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   15 --------- 16 --------- 17 --------- 18 --------- 19
  //    |        __/ | \__        |        __/ |        __/ |
  //    |     __/    |    \__     |     __/    |     __/    |
  //    |  __/       |       \__  |  __/       |  __/       |
  //    | /          |          \ | /          | /          |
  //   10 --------- 11---------- xx --------- 13 --------- 14
  //    |        __/ |        __/ | \__        |        __/ |
  //    |     __/    |     __/    |    \__     |     __/    |
  //    |  __/       |  __/       |       \__  |  __/       |
  //    | /          | /          |          \ | /          |
  //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
  
  CreateSquareTriangularMesh<MeshType>( mesh );
  
  std::cout << "     ";
  std::cout << "Create a center vertex of a Polygon (possible).";
  
  // this create the 16-17-18-13-8-7-6-11 polygon.
  mesh->LightWeightDeleteEdge( mesh->FindEdge( 11, 12 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge(  6, 12 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge(  7, 12 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge(  7, 13 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 13 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 18 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 17 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge( 11, 17 ) );
  mesh->DeletePoint( 12 );
  mesh->AddFace( mesh->FindEdge( 6, 7 ) );
  if( !createCenterVertex->Evaluate( mesh->FindEdge( 6,  7 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }   
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 25, 56, 32, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( createCenterVertex->GetNewPointID( ) ).GetValence( ) != 8 )
    {
    std::cout << "FAILED, wrong valence of "
       << mesh->GetPoint( createCenterVertex->GetNewPointID( ) ).GetValence( )
       << " for vertex "
       << createCenterVertex->GetNewPointID( )
       << "." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << ".OK" << std::endl;
  std::cout << "Checking CreateCenterVertex." << "OK" << std::endl << std::endl;
  return EXIT_SUCCESS;
}

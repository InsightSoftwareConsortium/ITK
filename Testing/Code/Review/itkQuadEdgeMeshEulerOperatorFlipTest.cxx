/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorFlipTest.cxx
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
#include "itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int itkQuadEdgeMeshEulerOperatorFlipTest( int , char * [] )
{
  
  typedef itk::QuadEdgeMesh< double, 3 >                      MeshType;
  typedef MeshType::Pointer                                   MeshPointer;
  typedef MeshType::QEType                                    QEType;
  typedef MeshType::PointIdentifier                           PointIdentifier;
  typedef MeshType::PointType                                 PointType;
  typedef MeshType::CellType                                  CellType;
  typedef itk::QuadEdgeMeshLineCell< CellType >               LineCellType;
  
  typedef itk::QuadEdgeMeshEulerOperatorFlipEdgeFunction< MeshType, QEType>
    FlipEdge;
  
  MeshPointer  mesh = MeshType::New();
  CreateSquareTriangularMesh<MeshType>( mesh );
  FlipEdge::Pointer flipEdge = FlipEdge::New( );
  std::cout << flipEdge << std::endl;

#ifndef NDEBUG
  if( flipEdge->Evaluate( (QEType*)1 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
#endif
  
  std::cout << flipEdge->GetNameOfClass() << std::endl; 

  flipEdge->SetInput( mesh );

#ifndef NDEBUG
  std::cout << "     " << "Test QE Input not internal";
  QEType* dummy = new QEType;
  if( flipEdge->Evaluate( dummy ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  delete dummy;
  std::cout << "OK" << std::endl;
  std::cout << "     " << "Test No QE Input";
  if( flipEdge->Evaluate( (QEType*)0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
#endif

  mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 18 ) );
  mesh->AddFace( mesh->FindEdge( 17 ,12 ) );
  std::cout << "     " << "Flip an edge with a polygonal face (impossible)";
  QEType* tempFlippedEdge = flipEdge->Evaluate( mesh->FindEdge( 12 , 17 ) ); 
  if( tempFlippedEdge )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  
  CreateSquareTriangularMesh<MeshType>( mesh );
  std::cout << "     " << "Flip an edge (possible)";
  tempFlippedEdge = flipEdge->Evaluate( mesh->FindEdge( 12 , 6 ) ); 
  if( !tempFlippedEdge )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  // The number of edges and faces must be unchanged:
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 25, 56, 32, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 12 ).GetValence( ) != 5 )
    {
    std::cout << "FAILED [wrong valence of "
               << mesh->GetPoint( 12 ).GetValence( )
               << " for vertex 12 ]." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 6 ).GetValence( ) != 5 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 6 ).GetValence( )
              << " for vertex 6 ]." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 11 ).GetValence( ) != 7 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 11 ).GetValence( )
              << " for vertex 11 ]." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 7 ).GetValence( ) != 7 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 7 ).GetValence( )
              << " for vertex 7 ]." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << ".OK" << std::endl;
   // Checking invariance (i.e. FlipEdge is it's own inverse):
  std::cout << "     " << "Check FlipEdge(FlipEdge()) invariance (possible for triangles).";
  if( !flipEdge->Evaluate( tempFlippedEdge ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  // The number of edges and faces must be unchanged:
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 25, 56, 32, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 12 ).GetValence( ) != 6 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 12 ).GetValence( )
              << " for vertex 12 ]." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 6 ).GetValence( ) != 6 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 6 ).GetValence( )
              << " for vertex 6 ]." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 11 ).GetValence( ) != 6 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 11 ).GetValence( )
              << " for vertex 11 ]." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 7 ).GetValence( ) != 6 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 7 ).GetValence( )
              << " for vertex 7 ]." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
  std::cout << "Checking FlipEdge." << "OK" << std::endl << std::endl;
  
  return EXIT_SUCCESS;
}

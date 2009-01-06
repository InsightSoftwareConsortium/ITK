/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorSplitEdgeTest.cxx
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
#include "itkQuadEdgeMeshEulerOperatorSplitEdgeFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int itkQuadEdgeMeshEulerOperatorSplitEdgeTest( int, char * [] )
{
  
  typedef itk::QuadEdgeMesh< double, 3 >                      MeshType;
  typedef MeshType::Pointer                                   MeshPointer;
  typedef MeshType::QEType                                    QEType;
  typedef MeshType::PointIdentifier                           PointIdentifier;
  typedef MeshType::PointType                                 PointType;
  typedef MeshType::CellType                                  CellType;
  typedef itk::QuadEdgeMeshLineCell< CellType >               LineCellType;

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
  if( splitEdge->Evaluate( (QEType*)0 ) )
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

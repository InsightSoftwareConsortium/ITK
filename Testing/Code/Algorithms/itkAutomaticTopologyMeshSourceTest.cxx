/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAutomaticTopologyMeshSourceTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <math.h>
#include <time.h>

#include <iostream>
#include <set>

#include "itkMesh.h"
#include "itkAutomaticTopologyMeshSource.h"

int
itkAutomaticTopologyMeshSourceTest(int, char* [] )
{

  // Declare the type of the Mesh
  typedef itk::Mesh<double>                         MeshType;
  typedef MeshType::PointType                       PointType;
  typedef MeshType::CellType                        CellType;

  typedef itk::AutomaticTopologyMeshSource< MeshType >   MeshSourceType;
  typedef MeshSourceType::IdentifierType                 IdentifierType;
  typedef MeshSourceType::IdentifierArrayType            IdentifierArrayType;

  MeshSourceType::Pointer meshSource;

  meshSource = MeshSourceType::New();

  // Begin using various AddPoint functions to add the vertices of a
  // cube.

  // Point expressed as an itk::Point.

  PointType p;

  p[ 0 ] = 0;
  p[ 1 ] = 0;
  p[ 2 ] = 0;
  meshSource->AddPoint( p );

  // Point expressed as a C array.

  float a[3];
  a[ 0 ] = 1;
  a[ 1 ] = 0;
  a[ 2 ] = 0;
  meshSource->AddPoint( a );

  // Points expressed using their coordinates.
  meshSource->AddPoint( 0, 1, 0 );
  meshSource->AddPoint( 1, 1, 0 );
  meshSource->AddPoint( 0, 0, 1 );
  meshSource->AddPoint( 1, 0, 1 );
  meshSource->AddPoint( 0, 1, 1 );
  meshSource->AddPoint( 1, 1, 1 );

  // Done adding vertices of a cube.

  // Add a cell of each type using an Array of point IDs.  Here we use
  // the fact that the IDs are assigned in order, and the fact that
  // the Add[Cell] methods only use the first N entries of the array.

  IdentifierArrayType idArray( 8 );
  {
  for( IdentifierType i = 0; i < 8; i++ )
    {
    idArray[ i ] = i;
    }
  }

  // PROBLEM LINE
  meshSource->AddLine( idArray[0], idArray[1] );

  // Print out the resulting mesh data.
  std::cout << MeshType::Pointer(meshSource->GetOutput()) << std::endl;

  return EXIT_SUCCESS;

}


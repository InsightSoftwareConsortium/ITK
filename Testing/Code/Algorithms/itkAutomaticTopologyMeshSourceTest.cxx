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
#include <iostream>
#include <time.h>

#include "itkMesh.h"
#include "itkAutomaticTopologyMeshSource.h"

itk::Mesh<double>* TheMesh;
int itkAutomaticTopologyMeshSourceTest(int, char* [] )
{

  // Define the dimension of the mesh
  const unsigned int Dimension = 3;

  // Declare the type of the Mesh
  typedef itk::Mesh<double>                         MeshType;
  typedef MeshType::PointType                       PointType;

  typedef itk::AutomaticTopologyMeshSource< MeshType >   MeshSourceType;
  typedef MeshSourceType::CellArrayType                  CellArrayType;

  MeshSourceType::Pointer meshSource = MeshSourceType::New();

  // The number distinct points added is known at compile time, but
  // this should help keep track as the test is modified.
  int numPointsAdded = 0;
  int numCellsAdded = 0;

  // Add some points expressed as itk::Points.

  PointType p;

  p[ 0 ] = 1;
  p[ 1 ] = 0;
  p[ 2 ] = 0;
  meshSource->AddPoint( p );
  numPointsAdded++;
  std::cout << meshSource->GetOutput()->GetNumberOfPoints() << std::endl;

  TheMesh = meshSource->GetOutput();
  p[ 0 ] = 2;
  p[ 1 ] = 0;
  p[ 2 ] = 0;
  meshSource->AddPoint( p );
  numPointsAdded++;
  std::cout << meshSource->GetOutput()->GetNumberOfPoints() << std::endl;

  // Make sure it catches if the same point is added twice.
  p[ 0 ] = 1;
  p[ 1 ] = 0;
  p[ 2 ] = 0;
  meshSource->AddPoint( p );
  // Don't incr numPointsAdded.
  std::cout << meshSource->GetOutput()->GetNumberOfPoints() << std::endl;

  // Now add a point expressed as a C array.

  float a[3];

  a[ 0 ] = 3;
  a[ 1 ] = 0;
  a[ 2 ] = 0;
  meshSource->AddPoint( a );
  numPointsAdded++;
  std::cout << meshSource->GetOutput()->GetNumberOfPoints() << std::endl;

  // Now add a vertex for each existing point.

  unsigned int i;
  for( i = 0; i < meshSource->GetOutput()->GetNumberOfPoints(); i++ )
    {
    meshSource->AddVertex( i );
    numCellsAdded++;
    }
  std::cout << meshSource->GetOutput()->GetNumberOfPoints() << std::endl;

  // Try to add a Vertex with a point that's already added (this
  // shouldn't work)
  meshSource->AddVertex( a );
  // Don't incr either counter
  std::cout << meshSource->GetOutput()->GetNumberOfPoints() << std::endl;

  // Add a vertex specified by a new point.

  p[ 0 ] = 4;
  p[ 1 ] = 0;
  p[ 2 ] = 0;
  meshSource->AddVertex( p );
  numPointsAdded++;
  numCellsAdded++;
  std::cout << meshSource->GetOutput()->GetNumberOfPoints() << std::endl;

  // Add a vertex specified by a C array of coords.

  a[ 0 ] = 5;
  a[ 1 ] = 0;
  a[ 2 ] = 0;
  meshSource->AddVertex( a );
  numPointsAdded++;
  numCellsAdded++;
  std::cout << meshSource->GetOutput()->GetNumberOfPoints() << std::endl;

  // Add lines specified by the ID's of already specified points.
  CellArrayType lineCell( 2 );
  lineCell[ 0 ] = 0;
  lineCell[ 1 ] = 1;
  meshSource->AddLine( lineCell );
  numCellsAdded++;

  unsigned long lineArray[] = { 1, 2 };
  meshSource->AddLine( lineArray );
  numCellsAdded++;
  
  try
    {
    meshSource->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown during Update() " << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  // Check that the right number of points has been added.

  int numPoints = meshSource->GetOutput()->GetNumberOfPoints();
  if( numPoints != numPointsAdded )
    {
    std::cerr << "Mesh shows " << numPoints << " points, but "
              << numPointsAdded << " were added." << std::endl;
    return EXIT_FAILURE;
    }

  // Check that the right number of cells has been added.

  int numCells = meshSource->GetOutput()->GetNumberOfCells();
  if( numCells != numCellsAdded )
    {
    std::cerr << "Mesh shows " << numCells << " cells, but " << numCellsAdded
              << " were added." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << MeshType::Pointer(meshSource->GetOutput()) << std::endl;

  return EXIT_SUCCESS;

}





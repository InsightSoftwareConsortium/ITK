/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSimplexMeshVolumeCalculatorTest.cxx
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
 
#include <math.h>
#include <iostream>

#include "itkMesh.h"
#include "itkSimplexMesh.h"
#include "itkSimplexMeshGeometry.h"
#include "itkSimplexMeshVolumeCalculator.h"
#include "itkDefaultDynamicMeshTraits.h"

int itkSimplexMeshVolumeCalculatorTest(int , char *[] )
{ 
  
  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3, double, double, double> MeshTraits;

  typedef itk::SimplexMesh<double,3,MeshTraits>           SimplexMeshType;

  typedef itk::SimplexMeshGeometry                        SimplexMeshGeometryType;

  typedef SimplexMeshType::CellType                       CellInterfaceType;

  SimplexMeshType::Pointer simplexMesh = SimplexMeshType::New();

  typedef  SimplexMeshType::NeighborListType              NeighborsListType;

  NeighborsListType* neighbors = NULL;

  /**
   * Define the 3d geometric positions for 8 points in a cube.
   */
  SimplexMeshType::CoordRepType testPointCoords[8][3]
    = { {0,0,0}, {9,0,0}, {9,0,9}, {0,0,9},
        {0,9,0}, {9,9,0}, {9,9,9}, {0,9,9} };
  

  /**
   * Typedef the generic cell type for the mesh.  It is an abstract class,
   * so we can only use information from it, like get its pointer type.
   */
  typedef SimplexMeshType::CellType       CellType;
  typedef CellType::CellAutoPointer       CellAutoPointer;
  typedef SimplexMeshType::PointType      PointType;

  /**
   * Add our test points to the mesh.
   * simplexMesh->SetPoint(pointId, point)
   * Note that the constructor for Point is public, and takes an array
   * of coordinates for the point.
   */
  for(int i=0; i < 8 ; ++i)
    {
    simplexMesh->SetPoint(i, PointType(testPointCoords[i]));
    simplexMesh->SetGeometryData(i, new SimplexMeshGeometryType );
    }

  /** 
   * Specify the method used for allocating cells
   */
   simplexMesh->SetCellsAllocationMethod( SimplexMeshType::CellsAllocatedDynamicallyCellByCell );


   /**
    * Excercise the AddEdge method
    */
  simplexMesh->AddEdge( 0, 1 );
  simplexMesh->AddEdge( 0, 3 );
  simplexMesh->AddEdge( 0, 4 );
  simplexMesh->AddEdge( 1, 2 );
  simplexMesh->AddEdge( 1, 5 );
  simplexMesh->AddEdge( 2, 3 );
  simplexMesh->AddEdge( 2, 6 );
  simplexMesh->AddEdge( 3, 7 );
  simplexMesh->AddEdge( 4, 5 );
  simplexMesh->AddEdge( 4, 7 );
  simplexMesh->AddEdge( 5, 6 );
  simplexMesh->AddEdge( 6, 7 );

   /**
    * Excercise the AddNeighbor method
    */
  simplexMesh->AddNeighbor( 0, 1 );
  simplexMesh->AddNeighbor( 0, 3 );
  simplexMesh->AddNeighbor( 0, 4 );
  simplexMesh->AddNeighbor( 1, 2 );
  simplexMesh->AddNeighbor( 1, 5 );
  simplexMesh->AddNeighbor( 2, 3 );
  simplexMesh->AddNeighbor( 2, 6 );
  simplexMesh->AddNeighbor( 3, 7 );
  simplexMesh->AddNeighbor( 4, 5 );
  simplexMesh->AddNeighbor( 4, 7 );
  simplexMesh->AddNeighbor( 5, 6 );
  simplexMesh->AddNeighbor( 6, 7 );


  // Now add the symmetric relationships
  simplexMesh->AddNeighbor( 1, 0 );
  simplexMesh->AddNeighbor( 3, 0 );
  simplexMesh->AddNeighbor( 4, 0 );
  simplexMesh->AddNeighbor( 2, 1 );
  simplexMesh->AddNeighbor( 5, 1 );
  simplexMesh->AddNeighbor( 3, 2 );
  simplexMesh->AddNeighbor( 6, 2 );
  simplexMesh->AddNeighbor( 7, 3 );
  simplexMesh->AddNeighbor( 5, 4 );
  simplexMesh->AddNeighbor( 7, 4 );
  simplexMesh->AddNeighbor( 6, 5 );
  simplexMesh->AddNeighbor( 7, 6 );


  typedef itk::SimplexMeshVolumeCalculator< 
                 SimplexMeshType > VolumeCalculatorType;

    
  VolumeCalculatorType::Pointer calculator = VolumeCalculatorType::New();

  calculator->SetSimplexMesh( simplexMesh );
  calculator->Compute();

  double volume = calculator->GetVolume();

  std::cout << " volume = " << volume << std::endl;

  const double knownVolume = 10.0;
  
  if( fabs( volume - knownVolume ) > 1e-4 )
    {
    std::cerr << "Error in the Volume computation " << std::endl;
    std::cerr << "We expected " << knownVolume << std::endl;
    std::cerr << "But we got  " << volume << std::endl;
    return EXIT_FAILURE;
    }
    

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;

}





/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSimplexMeshTest.cxx
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
#include <time.h>

#include "itkMesh.h"
#include "itkHexahedronCell.h"
#include "itkSimplexMesh.h"
#include "itkSimplexMeshGeometry.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkTimeProbe.h"

int itkSimplexMeshTest(int , char *[] )
{ 
  
  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3, double, double, double> MeshTraits;

  typedef itk::SimplexMesh<double,3,MeshTraits>           SimplexMeshType;

  typedef SimplexMeshType::CellType                       CellInterfaceType;
  typedef itk::HexahedronCell< CellInterfaceType >        HexaCellType;

  SimplexMeshType::Pointer simplexMesh = SimplexMeshType::New();

  typedef  SimplexMeshType::NeighborListType              NeighborsListType;

  NeighborsListType* neighbors;

  /**
   * Define the 3d geometric positions for 8 points in a cube.
   */
  SimplexMeshType::CoordRepType testPointCoords[8][3]
    = { {0,0,0}, {9,0,0}, {9,0,9}, {0,0,9},
        {0,9,0}, {9,9,0}, {9,9,9}, {0,9,9} };
  
  /**
   * List the points that the hexahedron will use from the mesh.
   */
  unsigned long hexaPoints[8] = {0,1,2,3,4,5,6,7};
 

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
    }

  /** 
   * Specify the method used for allocating cells
   */
   simplexMesh->SetCellsAllocationMethod( SimplexMeshType::CellsAllocatedDynamicallyCellByCell );

  /**
   * Create an hexahedron cell
   */
  CellAutoPointer testCell; 
  testCell.TakeOwnership( new HexaCellType ); // polymorphism
  testCell->SetPointIds(hexaPoints);
  simplexMesh->SetCell(1, testCell ); // Internally transfers ownership to the mesh


  itk::TimeProbe timeProbe;

  for (int i=0; i < 2; i++)
    {  
    timeProbe.Start();
    for (int pointIndex = 0; pointIndex < simplexMesh->GetPoints()->Size(); pointIndex++)
      {
      neighbors = simplexMesh->GetNeighbors( pointIndex, i );
      }
    timeProbe.Stop();
    if (neighbors)
      {
      std::cout << "Rigidity: " << i << ", neighbor list size: " << neighbors->size() << std::endl;
      }
    std::cout << ", Elapsed time (for getting neighbors): " << timeProbe.GetMeanTime() << std::endl;
    }

    

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;

}





/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolygonCellTest.cxx
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

#include "itkMesh.h"
#include "itkPolygonCell.h"

#include <iostream>
#include <string>


int itkPolygonCellTest(int, char* [] )
{

 
  /**
   * Define a mesh type that stores a PixelType of "int".  Use the defaults for
   * the other template parameters.
   */
  typedef itk::Mesh<int>  MeshType;
  typedef MeshType::CellTraits  CellTraits;

  /**
   * Define a few cell types which uses a PixelType of "int".  Again,
   * use the defaults for the other parameters.  Note that a cell's template
   * parameters must match those of the mesh into which it is inserted.
   */
  typedef itk::CellInterface< int, CellTraits >           CellInterfaceType;
  typedef itk::PolygonCell<CellInterfaceType>             PolygonCellType;

   
  /**
   * Typedef the generic cell type for the mesh.  It is an abstract class,
   * so we can only use information from it, like get its pointer type.
   */
  typedef MeshType::CellType              CellType;
  typedef CellType::CellAutoPointer       CellAutoPointer;

  /**
   * The type of point stored in the mesh. Because mesh was instantiated
   * with defaults (itkDefaultStaticMeshTraits), the point dimension is 3 and
   * the coordinate representation is float.
   */
  typedef MeshType::PointType  PointType;

  
  /**
   * Create the mesh through its object factory.
   */
  MeshType::Pointer mesh = MeshType::New();  
  mesh->DebugOn();

  /**
   * Define the 3d geometric positions for 8 points in a cube.
   */
  MeshType::CoordRepType testPointCoords[8][3]
    = { {0,0,0}, {9,0,0}, {9,0,9}, {0,0,9},
        {0,9,0}, {9,9,0}, {9,9,9}, {0,9,9} };
 
  /**
   * Add our test points to the mesh.
   * mesh->SetPoint(pointId, point)
   * Note that the constructor for Point is public, and takes an array
   * of coordinates for the point.
   */
  for(int i=0; i < 8 ; ++i)
    {
    mesh->SetPoint(i, PointType(testPointCoords[i]));
    }

  /** 
   * Specify the method used for allocating cells
   */
   mesh->SetCellsAllocationMethod( MeshType::CellsAllocatedDynamicallyCellByCell );

  /**
   * Create the test cell. Note that testCell is a generic auto
   * pointer to a cell; in this example it ends up pointing to
   * different types of cells.
   */
  CellAutoPointer testCell; 
  PolygonCellType * newcell = new PolygonCellType;
  testCell.TakeOwnership( newcell ); // polymorphism

  /**
   * List the points that the polygon will use from the mesh.
   */
  unsigned long polygon1Points[4] = {0,1,2,3};
 
  /**
   * Assign the points to the tetrahedron through their identifiers.
   */
  testCell->SetPointIds(polygon1Points);

  /**
   * Add the test cell to the mesh.
   * mesh->SetCell(cellId, cell)
   */
  mesh->SetCell(0, testCell ); // Transfer ownership to the mesh
  std::cout << "PolygonCell pointer = " << (void const *)testCell.GetPointer() << std::endl;
  std::cout << "PolygonCell Owner   = " << testCell.IsOwner() << std::endl;
  
  {
  std::cout << "Test MakeCopy" << std::endl;

  CellAutoPointer anotherCell;

  testCell->MakeCopy( anotherCell );

  if( anotherCell->GetNumberOfPoints() != testCell->GetNumberOfPoints() )
    {
    std::cerr << "Make Copy failed !" << std::endl;
    return EXIT_FAILURE;
    }
  }

  /**
   * Exercise the methods AddPointId() and RemovePointId() 
   */
  {
    unsigned int np = newcell->GetNumberOfPoints();

    newcell->AddPointId( 100 );
    if( newcell->GetNumberOfPoints() != np+1 )
      {
      std::cerr << "AddPointId failed !" << std::endl;
      return EXIT_FAILURE;
      }

    newcell->RemovePointId( 100 );
    if( newcell->GetNumberOfPoints() != np )
      {
      std::cerr << "RemovePointId failed !" << std::endl;
      return EXIT_FAILURE;
      }
  }
  
  return EXIT_SUCCESS;  
}


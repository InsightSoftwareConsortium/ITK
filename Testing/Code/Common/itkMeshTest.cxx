/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshTest.cxx
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

#include "itkMesh.h"
#include "itkTetrahedronCell.h"
#include "itkHexahedronCell.h"
#include "itkQuadraticEdgeCell.h"
#include "itkQuadraticTriangleCell.h"
#include "itkBoundingBox.h"
#include "itkFileOutputWindow.h"

#include <iostream>
#include <string>

/**
 * Some typedefs to make things easier.
 */

/**
 * Define a mesh type that stores a PixelType of "int".  Use the defaults
 * for the other template parameters.
 */
typedef itk::Mesh<int>  MeshType;
typedef MeshType::CellTraits  CellTraits;

/**
 * Define a few cell types which uses a PixelType of "int".  Again,
 * use the defaults for the other parameters.  Note that a cell's template
 * parameters must match those of the mesh into which it is inserted.
 */
typedef itk::CellInterface< int, CellTraits >           CellInterfaceType;
typedef itk::LineBoundary<CellInterfaceType>            LineBoundaryType;
typedef itk::TetrahedronCell<CellInterfaceType>         TetraCellType;
typedef itk::HexahedronCell<CellInterfaceType>          HexaCellType;
typedef itk::QuadraticEdgeCell<CellInterfaceType>       QuadraticEdgeCellType;
typedef itk::QuadraticTriangleCell<CellInterfaceType>   QuadraticTriangleCellType;

/**
 * Typedef the generic cell type for the mesh.  It is an abstract class,
 * so we can only use information from it, like get its pointer type.
 */
typedef MeshType::CellType              CellType;
typedef CellType                        BoundaryType;
typedef CellType::CellAutoPointer       CellAutoPointer;
typedef BoundaryType::SelfAutoPointer   BoundaryAutoPointer;

/**
 * The type of point stored in the mesh. Because mesh was instantiated
 * with defaults (itkDefaultStaticMeshTraits), the point dimension is 3 and
 * the coordinate representation is float.
 */
typedef MeshType::PointType  PointType;

/**
 * The mesh that is created consists of a single hexahedron and a single
 * tetrahedron. (The tetra is inside of the hex.)
 */



int itkMeshTest(int, char* [] )
{
  itk::FileOutputWindow::Pointer fow = itk::FileOutputWindow::New();
  fow->SetInstance(fow);

  /**
   * Define the 3d geometric positions for 8 points in a cube.
   */
  MeshType::CoordRepType testPointCoords[8][3]
    = { {0,0,0}, {9,0,0}, {9,0,9}, {0,0,9},
        {0,9,0}, {9,9,0}, {9,9,9}, {0,9,9} };
  
  /**
   * List the points that the tetrahedron will use from the mesh.
   */
  unsigned long tetraPoints[4] = {0,1,2,4};
  
  /**
   * List the points that the hexahedron will use from the mesh.
   */
  unsigned long hexaPoints[8] = {0,1,2,3,4,5,6,7};
  
  
  /**
   * Create the mesh through its object factory.
   */
  MeshType::Pointer mesh = MeshType::New();  

  mesh->DebugOn();

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
  testCell.TakeOwnership(  new TetraCellType ); // polymorphism

  /**
   * Assign the points to the tetrahedron through their identifiers.
   */
  testCell->SetPointIds(tetraPoints);

  /**
   * Add the test cell to the mesh.
   * mesh->SetCell(cellId, cell)
   */
  mesh->SetCell(0, testCell ); // Transfer ownership to the mesh
  std::cout << "TetrahedronCell pointer = " << (void const *)testCell.GetPointer() << std::endl;
  std::cout << "TetrahedronCell Owner   = " << testCell.IsOwner() << std::endl;
  
  /**
   * Create another test cell.
   */
  testCell.TakeOwnership( new HexaCellType ); // polymorphism
  testCell->SetPointIds(hexaPoints);
  mesh->SetCell(1, testCell ); // Internally transfers ownership to the mesh


  CellAutoPointer hexaCell;
  if(  mesh->GetCell(1, hexaCell) )
    {
    std::cout << "Hexahedron cell recovered" << std::endl;
    std::cout << "GetNameOfClass()    = " << hexaCell->GetNameOfClass() << std::endl;
    std::cout << "GetNumberOfPoints() = " << hexaCell->GetNumberOfPoints() << std::endl;
    }
  else
    {
    std::cout << "Failure: hexahedron cell was not recovered" << std::endl;
    return EXIT_FAILURE;
    }

  CellAutoPointer cellPointer;
  /**
   * Try getting one of the hexahedron's faces.
   */
  const bool faceExists = mesh->GetCellBoundaryFeature(
                2,    // Topological dimension of boundary.
                1,    // CellIdentifier.
                0,    // CellFeatureIdentifier
                cellPointer ); // CellPointer to return the result

  std::cout << typeid( cellPointer ).name() << std::endl;
  std::cout << typeid( cellPointer.GetPointer() ).name() << std::endl;
  std::cout << "GetCellBoundaryFeature() return AutoPointer owner = " << cellPointer.IsOwner() << std::endl;
  
  HexaCellType::FaceType * quad = 0;
  try
    {
    quad = dynamic_cast<HexaCellType::FaceType *>(  cellPointer.GetPointer() );
    std::cout << "Quad face recovered " << std::endl;
    std::cout << quad->GetNameOfClass() << std::endl;
    }
  catch(...)
    {
    std::cout << "CellPointer cannot be down-cast to a QuadCellType" << std::endl;
    quad = 0;
    }
  if( quad )
    {
    std::cout << "CellPointer was safely down-casted to a QuadCellType" << std::endl;
    }

  if( faceExists ) 
    {
    std::cout << cellPointer->GetNumberOfPoints() << std::endl;
    std::cout << cellPointer->GetNameOfClass() << std::endl;
    }
  else 
    {
    std::cout << "Hexahedron face couldn't be extracted " << std::endl;
    }
  
  /**
   * Allocate an explicity boundary line.
   */
  BoundaryAutoPointer boundLine; 
  boundLine.TakeOwnership(  new LineBoundaryType ); // polymorphism
  
  /**
   * We don't want the hexahedron to consider the tetrahedron a neighbor
   * across its first edge, so don't add the tetrahedron as a using cell.
   */
  boundLine->AddUsingCell(1);
  boundLine->SetPointId(0,0);
  boundLine->SetPointId(1,1);

  mesh->SetBoundariesAllocationMethod(MeshType::BoundariesAllocatedDynamicallyCellByCell);
  mesh->SetBoundary(1,            // Topological dimension of boundary.
        0,                        // Boundary identifier.
        boundLine);               // Pointer to explicit boundary.
  
  mesh->SetBoundaryAssignment(1,  // Topologoical dimension.
            1,                    // CellIdentifier
            0,                    // CellFeatureIdentifier
            0);                   // Boundary identifier.  
  std::cout << "boundLine.IsOwner() = " << boundLine.IsOwner() << std::endl;
  std::cout << "boundLine.GetPointer() = " << boundLine.GetPointer() << std::endl;
  
  /**
   * Try getting the hexahedron's neighbor through its first edge.
   * This should be the test tetrahedron, except that we have done an
   * explicit assignment which removes this.
   */
  std::set<MeshType::CellIdentifier>  neighborSet;
  std::set<MeshType::CellIdentifier>::iterator cell;
  mesh->GetCellBoundaryFeatureNeighbors(
    1,              // Topological dimension of feature.
    1,              // CellIdentifier
    0,              // CellFeatureIdentifier
    &neighborSet); // Where to put result.

  std::cout << "Neighbors (hex edge 0):" << std::endl;
  for(cell = neighborSet.begin(); cell != neighborSet.end(); ++cell)
    {
    std::cout << "Id " << *cell << ": ";
    CellAutoPointer cellPointer;

    if( mesh->GetCell( *cell, cellPointer ) )
      {
      std::cout << cellPointer->GetNameOfClass();
      }
    std::cout << std::endl;
    }
  
  /**
   * Try getting the hexahedron's neighbor through its second edge.
   * This should be the test tetrahedron. (Because the boundary is
   * not defined explicitly, we use implicit relationships to determine
   * neighbors. In this case, bit the tetrahedron and hexahedron share
   * the two points defining the edge and are therefore considered 
   * neighbors.)
   */
  mesh->GetCellBoundaryFeatureNeighbors(
    1,              // Topological dimension of feature.
    1,              // CellIdentifier
    1,              // CellFeatureIdentifier
    &neighborSet); // Where to put result.

  std::cout << "Neighbors (hex edge 1):" << std::endl;
  for(cell = neighborSet.begin(); cell != neighborSet.end(); ++cell)
    {
    std::cout << "Id " << *cell << ": ";
    CellAutoPointer cellPointer;

    if( mesh->GetCell(*cell, cellPointer) )
      {
      std::cout << cellPointer->GetNameOfClass();
      }
    std::cout << std::endl;
    }
 

  /**
   * Try getting the tetrahedrons's neighbor through its fourth edge.
   * This should be the test hexahedron. The boundaries are implicit
   * as in the previous example.
   */
  mesh->GetCellBoundaryFeatureNeighbors(
    1,              // Topological dimension of feature.
    0,              // CellIdentifier
    3,              // CellFeatureIdentifier
    &neighborSet); // Where to put result.

  std::cout << "Neighbors (tet edge 3):" << std::endl;
  for(cell = neighborSet.begin(); cell != neighborSet.end(); ++cell)
    {
    std::cout << "Id " << *cell << ": ";
    CellAutoPointer cellPointer;

    if( mesh->GetCell(*cell, cellPointer) )
      {
      std::cout << cellPointer->GetNameOfClass();
      }
    std::cout << std::endl;
    }

  /**
   * Create a higher order  test cell.
   */
  { // Create a local scope
    testCell.TakeOwnership( new QuadraticEdgeCellType ); // polymorphism
    unsigned long quadraticEdgePoints[3] = {0,1,2};
    testCell->SetPointIds(quadraticEdgePoints);
    mesh->SetCell(2, testCell ); // Internally transfers ownership to the mesh

    CellAutoPointer quadraticdEdgeCell;
    if(  mesh->GetCell(2, quadraticdEdgeCell) )
      {
      std::cout << "Quadratic Edge cell recovered" << std::endl;
      std::cout << "GetNameOfClass()    = " << quadraticdEdgeCell->GetNameOfClass() << std::endl;
      std::cout << "GetNumberOfPoints() = " << quadraticdEdgeCell->GetNumberOfPoints() << std::endl;
      }
    else
      {
      std::cout << "Failure: QuadraticEdge cell was not recovered" << std::endl;
      return EXIT_FAILURE;
      }

    CellAutoPointer vertexPointer;
    /**
     * Try getting one of the QuadraticEdge's vertices.
     */
    const bool vertexExists = mesh->GetCellBoundaryFeature(
                  0,    // Topological dimension of boundary.
                  2,    // CellIdentifier.
                  0,    // CellFeatureIdentifier
                  vertexPointer ); // CellPointer to return the result

    std::cout << typeid( vertexPointer ).name() << std::endl;
    std::cout << typeid( vertexPointer.GetPointer() ).name() << std::endl;
    std::cout << "GetCellBoundaryFeature() return AutoPointer owner = " << vertexPointer.IsOwner() << std::endl;
    
    QuadraticEdgeCellType::VertexType * vertex = 0;
    try
      {
      vertex = dynamic_cast<QuadraticEdgeCellType::VertexType *>(  vertexPointer.GetPointer() );
      std::cout << "Vertex from the QuadraticEdge recovered " << std::endl;
      std::cout << vertex->GetNameOfClass() << std::endl;
      }
    catch(...)
      {
      std::cout << "CellPointer cannot be down-cast to a VertexCellType" << std::endl;
      vertex = 0;
      }
    if( vertex )
      {
      std::cout << "CellPointer was safely down-casted to a VertexCellType" << std::endl;
      }
    if( vertexExists ) 
      {
      std::cout << "Vertex number of points = " << vertexPointer->GetNumberOfPoints() << std::endl;
      std::cout << "Vertex name of class    = " << vertexPointer->GetNameOfClass() << std::endl;
      }
    else 
      {
      std::cout << "Vertex of the QuadraticEdge couldn't be extracted " << std::endl;
      }
  } // end of local scope for this part of the test. 


  /**
   * Create a higher order triangular test cell.
   */
  { // Create a local scope

    // In this block, we overwrite a cell at a particular id.
    // To avoid a memory leak, we must first grab ownership of the
    // current cell at that id so that the memory for the original
    // cell will be deleted when we leave this scope
    CellAutoPointer cellToDelete;
    mesh->GetCell(2, cellToDelete);
    cellToDelete.TakeOwnership();
    
    // Now we can construct a new cell and overwrite the id
    testCell.TakeOwnership(new QuadraticTriangleCellType); // polymorphism;
    unsigned long quadraticTrianglePoints[3] = {0,1,2};
    testCell->SetPointIds(quadraticTrianglePoints);
    mesh->SetCell(2, testCell ); // Internally transfers ownership to the mesh
    std::cout << "QuadraticTriangleCell pointer = " << (void*)testCell.GetPointer() << std::endl;
    std::cout << "QuadraticTriangleCell Owner   = " << testCell.IsOwner() << std::endl;

    CellAutoPointer quadraticdTriangleCell;

    if(  mesh->GetCell(2, quadraticdTriangleCell) )
      {
      std::cout << "Quadratic Triangle cell recovered" << std::endl;
      std::cout << "GetNameOfClass()    = " << quadraticdTriangleCell->GetNameOfClass() << std::endl;
      std::cout << "GetNumberOfPoints() = " << quadraticdTriangleCell->GetNumberOfPoints() << std::endl;
      }
    else
      {
      std::cout << "Failure: QuadraticTriangle cell was not recovered" << std::endl;
      return EXIT_FAILURE;
      }


    CellAutoPointer vertexPointer;

    /**
     * Try getting one of the QuadraticTriangle's vertices.
     */
    const bool vertexExists = mesh->GetCellBoundaryFeature(
                  0,    // Topological dimension of boundary.
                  2,    // CellIdentifier.
                  0,    // CellFeatureIdentifier
                  vertexPointer ); // CellPointer to return the result

    std::cout << typeid( vertexPointer ).name() << std::endl;
    std::cout << typeid( vertexPointer.GetPointer() ).name() << std::endl;
    std::cout << "GetCellBoundaryFeature() return AutoPointer owner = " << vertexPointer.IsOwner() << std::endl;

    QuadraticTriangleCellType::VertexType * vertex = 0;
    try
      {
      vertex = dynamic_cast<QuadraticTriangleCellType::VertexType *>(  vertexPointer.GetPointer() );
      std::cout << "Vertex from the QuadraticTriangle recovered " << std::endl;
      std::cout << vertex->GetNameOfClass() << std::endl;
      }
    catch(...)
      {
      std::cout << "CellPointer cannot be down-cast to a VertexCellType" << std::endl;
      vertex = 0;
      }
    if( vertex )
      {
      std::cout << "CellPointer was safely down-casted to a VertexCellType" << std::endl;
      }
    if( vertexExists ) 
      {
      std::cout << "Vertex number of points = " << vertexPointer->GetNumberOfPoints() << std::endl;
      std::cout << "Vertex name of class    = " << vertexPointer->GetNameOfClass() << std::endl;
      }
    else 
      {
      std::cout << "Vertex of the QuadraticTriangle couldn't be extracted " << std::endl;
      }

    /**
     * Try getting one of the QuadraticTriangle's edges.
     */
    CellAutoPointer edgePointer;
    const bool edgeExists = mesh->GetCellBoundaryFeature(
                  1,    // Topological dimension of boundary.
                  2,    // CellIdentifier.
                  0,    // CellFeatureIdentifier
                  edgePointer ); // CellPointer to return the result

    std::cout << typeid( edgePointer ).name() << std::endl;
    std::cout << typeid( edgePointer.GetPointer() ).name() << std::endl;
    std::cout << "GetCellBoundaryFeature() return AutoPointer owner = " << edgePointer.IsOwner() << std::endl;
    
    QuadraticTriangleCellType::EdgeType * edge = 0;
    try
      {
      edge = dynamic_cast<QuadraticTriangleCellType::EdgeType *>(  edgePointer.GetPointer() );
      std::cout << "Vertex from the QuadraticTriangle recovered " << std::endl;
      std::cout << edge->GetNameOfClass() << std::endl;
      }
    catch(...)
      {
      std::cout << "CellPointer cannot be down-cast to a VertexCellType" << std::endl;
      edge = 0;
      }
    if( edge )
      {
      std::cout << "CellPointer was safely down-casted to a VertexCellType" << std::endl;
      }
    if( edgeExists ) 
      {
      std::cout << "Edge number of points = " << edgePointer->GetNumberOfPoints() << std::endl;
      std::cout << "Edge name of class    = " << edgePointer->GetNameOfClass() << std::endl;

      // Evaluate The Shape functions for a particular parametric point
      CellType::ParametricCoordArrayType parametricCoordinates(1);
      CellType::ShapeFunctionsArrayType  weights( edgePointer->GetNumberOfPoints() );
      parametricCoordinates[0] = 0.25;
      edgePointer->EvaluateShapeFunctions( parametricCoordinates, weights );
      std::cout << "Shape Function weights = " << weights << std::endl;
      }
    else 
      {
      std::cerr << "Edge of the QuadraticTriangle couldn't be extracted " << std::endl;
      }
  } // end of local scope for this part of the test.



  /**
   * Perform some geometric operations (coordinate transformations)
   * to see if they are working.
   */
  MeshType::CoordRepType coords[MeshType::PointDimension];
  MeshType::PointIdentifier pointId;
  mesh->FindClosestPoint(coords,&pointId);

  /**
   * Compute the bounding box of the mesh
   */
  typedef itk::BoundingBox<MeshType::PointIdentifier,MeshType::PointDimension,
    MeshType::CoordRepType,MeshType::PointsContainer> BoundingBox;

  BoundingBox::Pointer bbox(BoundingBox::New());
  bbox->SetPoints(mesh->GetPoints());
  bbox->ComputeBoundingBox();
  std::cout << bbox << std::endl;


  return 0;  
}


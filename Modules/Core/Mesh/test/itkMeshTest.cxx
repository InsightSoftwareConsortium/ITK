/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkMesh.h"
#include "itkTetrahedronCell.h"
#include "itkHexahedronCell.h"
#include "itkQuadraticTriangleCell.h"
#include "itkFileOutputWindow.h"

#include <iostream>

namespace itkMeshTestTypes {
  // this namespace helps to isolate the types defined blow
  // when all the code is included in the test driver.

/**
 * Some typedefs to make things easier.
 */

/**
 * Define a mesh type that stores a PixelType of "int".  Use the defaults
 * for the other template parameters.
 */
typedef itk::Mesh<int>        MeshType;
typedef MeshType::CellTraits  CellTraits;


/**
 * Define a few cell types which uses a PixelType of "int".  Again,
 * use the defaults for the other parameters.  Note that a cell's template
 * parameters must match those of the mesh into which it is inserted.
 */
typedef itk::CellInterface< int, CellTraits >           CellInterfaceType;
typedef itk::LineCell<CellInterfaceType>                LineCellType;
typedef itk::TetrahedronCell<CellInterfaceType>         TetraCellType;
typedef itk::HexahedronCell<CellInterfaceType>          HexaCellType;
typedef itk::QuadraticEdgeCell<CellInterfaceType>       QuadraticEdgeCellType;
typedef itk::QuadraticTriangleCell<CellInterfaceType>   QuadraticTriangleCellType;

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
 * The mesh that is created consists of a single hexahedron and a single
 * tetrahedron. (The tetra is inside of the hex.)
 */


// create a class to store counts of cells found in the visit pass
class CountClass
{
public:
  CountClass()
    {
      m_Tetra =0;
      m_QuadraticEdgeCell =0;
      m_QuadraticTriangleCellType =0;
    }
  int m_Tetra;
  int m_QuadraticEdgeCell;
  int m_QuadraticTriangleCellType;
};


// Create a class that can be used to visit cells of
// different types via overloading the Visit method
class VisitCells
{
public:
  void SetCountClass(CountClass* c)
    {
      m_CountClass = c;
    }

  void Visit(unsigned long , TetraCellType*)
    {
      m_CountClass->m_Tetra++;
    }
  void Visit(unsigned long , QuadraticEdgeCellType*)
    {
      m_CountClass->m_QuadraticEdgeCell++;
    }

  void Visit(unsigned long , QuadraticTriangleCellType*)
    {
      m_CountClass->m_QuadraticTriangleCellType++;
    }
  virtual ~VisitCells() {}

  CountClass* m_CountClass;
  VisitCells()
  {
    m_CountClass = ITK_NULLPTR;
  }
};

typedef itk::CellInterfaceVisitorImplementation<
  int, MeshType::CellTraits,
  TetraCellType,
  VisitCells> TetraCellVisitor;

typedef itk::CellInterfaceVisitorImplementation<
  int, MeshType::CellTraits,
  QuadraticEdgeCellType,
  VisitCells> QuadraticEdgeCellVisitor;

typedef itk::CellInterfaceVisitorImplementation<
  int, MeshType::CellTraits,
  QuadraticTriangleCellType,
  VisitCells> QuadraticTriangleCellVisitor;

}


int itkMeshTest(int, char* [] )
{

  using namespace itkMeshTestTypes; // open the namespace here.
                                    // this is safe because only happens localy.

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
  MeshType::PointIdentifier tetraPoints[4] = {0,1,2,4};

  /**
   * List the points that the hexahedron will use from the mesh.
   */
  MeshType::PointIdentifier hexaPoints[8] = {0,1,2,3,4,5,6,7};


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
  for(int i=0; i < 8; ++i)
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

  CellAutoPointer cellPointer0;
  /**
   * Try getting one of the hexahedron's faces.
   */
  const bool faceExists = mesh->GetCellBoundaryFeature(
                2,    // Topological dimension of boundary.
                1,    // CellIdentifier.
                0,    // CellFeatureIdentifier
                cellPointer0 ); // CellPointer to return the result

  std::cout << typeid( cellPointer0 ).name() << std::endl;
  std::cout << typeid( cellPointer0.GetPointer() ).name() << std::endl;
  std::cout << "GetCellBoundaryFeature() return AutoPointer owner = " << cellPointer0.IsOwner() << std::endl;

  HexaCellType::FaceType * quad;
  try
    {
    quad = dynamic_cast<HexaCellType::FaceType *>(  cellPointer0.GetPointer() );
    std::cout << "Quad face recovered " << std::endl;
    std::cout << quad->GetNameOfClass() << std::endl;
    }
  catch(...)
    {
    std::cout << "CellPointer cannot be down-cast to a QuadCellType" << std::endl;
    quad = ITK_NULLPTR;
    }
  if( quad )
    {
    std::cout << "CellPointer was safely down-casted to a QuadCellType" << std::endl;
    }

  if( faceExists )
    {
    std::cout << cellPointer0->GetNumberOfPoints() << std::endl;
    std::cout << cellPointer0->GetNameOfClass() << std::endl;
    }
  else
    {
    std::cout << "Hexahedron face couldn't be extracted " << std::endl;
    }

  /**
   * Allocate an explicit boundary line.
   */
  CellAutoPointer boundLine;
  boundLine.TakeOwnership(  new LineCellType );

  /**
   * We don't want the hexahedron to consider the tetrahedron a neighbor
   * across its first edge, so don't add the tetrahedron as a using cell.
   */
  boundLine->AddUsingCell(1);
  boundLine->SetPointId(0,0);
  boundLine->SetPointId(1,1);

  mesh->SetCell( 2,             // New ID for new cell
                 boundLine);    // New cell being added

  mesh->SetBoundaryAssignment(1,  // Topological dimension.
            1,                    // CellIdentifier
            0,                    // CellFeatureIdentifier
            2);                   // Cell ID of boundary
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
   * Try querying the for the number of neighbors the hexahedron has
   * through its second edge, without getting the actual set of
   * boundaries.  (The result should be 1.  Note that the quads and
   * triangles that bound the 3D cells haven't been added, so they
   * don't show up as additional neighbors.)
   */
  int numberOfNeighbors = mesh->GetCellBoundaryFeatureNeighbors(
    1,              // Topological dimension of feature.
    1,              // CellIdentifier
    1,              // CellFeatureIdentifier
    ITK_NULLPTR);          // We don't want the neighbors themselves (yet)
  std::cout << "Number of neighbors (hex edge 1): "
            << numberOfNeighbors << ". " << std::endl;

  /**
   * Try getting the hexahedron's neighbor through its second edge.
   * This should be the test tetrahedron. (Because the boundary is
   * not defined explicitly, we use implicit relationships to determine
   * neighbors. In this case, the tetrahedron and hexahedron share
   * the two points defining the edge and are therefore considered
   * neighbors.)
   */
  mesh->GetCellBoundaryFeatureNeighbors(
    1,              // Topological dimension of feature.
    1,              // CellIdentifier
    1,              // CellFeatureIdentifier
    &neighborSet);  // Where to put result.

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

    // In this block, we overwrite a cell at a particular id.
    // To avoid a memory leak, we must first grab ownership of the
    // current cell at that id so that the memory for the original
    // cell will be deleted when we leave this scope
    CellAutoPointer cellToDelete;
    mesh->GetCell(2, cellToDelete);
    cellToDelete.TakeOwnership();

    testCell.TakeOwnership( new QuadraticEdgeCellType ); // polymorphism

    MeshType::PointIdentifier quadraticEdgePoints[3] = {0,1,2};
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

    QuadraticEdgeCellType::VertexType * vertex;
    try
      {
      vertex = dynamic_cast<QuadraticEdgeCellType::VertexType *>(  vertexPointer.GetPointer() );
      std::cout << "Vertex from the QuadraticEdge recovered " << std::endl;
      std::cout << vertex->GetNameOfClass() << std::endl;
      }
    catch(...)
      {
      std::cout << "CellPointer cannot be down-cast to a VertexCellType" << std::endl;
      vertex = ITK_NULLPTR;
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
    MeshType::PointIdentifier quadraticTrianglePoints[3] = {0,1,2};
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

    QuadraticTriangleCellType::VertexType * vertex;
    try
      {
      vertex = dynamic_cast<QuadraticTriangleCellType::VertexType *>(  vertexPointer.GetPointer() );
      std::cout << "Vertex from the QuadraticTriangle recovered " << std::endl;
      std::cout << vertex->GetNameOfClass() << std::endl;
      }
    catch(...)
      {
      std::cout << "CellPointer cannot be down-cast to a VertexCellType" << std::endl;
      vertex = ITK_NULLPTR;
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

    QuadraticTriangleCellType::EdgeType * edge;
    try
      {
      edge = dynamic_cast<QuadraticTriangleCellType::EdgeType *>(  edgePointer.GetPointer() );
      std::cout << "Vertex from the QuadraticTriangle recovered " << std::endl;
      std::cout << edge->GetNameOfClass() << std::endl;
      }
    catch(...)
      {
      std::cout << "CellPointer cannot be down-cast to a VertexCellType" << std::endl;
      edge = ITK_NULLPTR;
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
   * Compute the bounding box of the mesh
   */
  typedef itk::BoundingBox<MeshType::PointIdentifier,MeshType::PointDimension,
    MeshType::CoordRepType,MeshType::PointsContainer> BoundingBox;

  BoundingBox::Pointer bbox(BoundingBox::New());
  bbox->SetPoints(mesh->GetPoints());
  bbox->ComputeBoundingBox();

  /**
   * Set up some visitors
   */
  MeshType::CellType::MultiVisitor::Pointer mv =
    MeshType::CellType::MultiVisitor::New();
  /**
   * Create a class to hold the counts of each cell type
   */
  CountClass counts;
  /**
   * Create a visitor for each cell type and set the counts class for the visitor
   */
  TetraCellVisitor::Pointer cv = TetraCellVisitor::New();
  cv->SetCountClass(&counts);
  QuadraticEdgeCellVisitor::Pointer ev = QuadraticEdgeCellVisitor::New();
  ev->SetCountClass(&counts);
  QuadraticTriangleCellVisitor::Pointer tv = QuadraticTriangleCellVisitor::New();
  tv->SetCountClass(&counts);
  mv->AddVisitor(cv);
  mv->AddVisitor(ev);
  mv->AddVisitor(tv);

  // Now ask the mesh to accept the multivisitor which
  // will Call Visit for each cell in the mesh that matches the
  // cell types of the visitors added to the MultiVisitor
  mesh->Accept(mv);
  // print the counts found
  std::cout << "Number of TetraCellType " << counts.m_Tetra << "\n";
  std::cout << "Number of QuadraticEdgeCellType " << counts.m_QuadraticEdgeCell << "\n";
  std::cout << "Number of QuadraticTriangleCellType " << counts.m_QuadraticTriangleCellType << "\n";

  std::cout << bbox << std::endl;

  std::cout << mesh << std::endl;


  // Exercising the Graft method
  MeshType::Pointer newMesh = MeshType::New();
  newMesh->Graft( mesh );

  if( newMesh->GetNumberOfPoints() != mesh->GetNumberOfPoints() )
    {
    std::cerr << "Graft failed !, different number of points" << std::endl;
    return EXIT_FAILURE;
    }

  if( newMesh->GetNumberOfCells() != mesh->GetNumberOfCells() )
    {
    std::cerr << "Graft failed !, different number of cells" << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << newMesh << std::endl;

  return EXIT_SUCCESS;
}

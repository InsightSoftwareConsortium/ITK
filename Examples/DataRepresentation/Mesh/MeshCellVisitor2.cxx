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

//  Software Guide : BeginLatex
//
//  The following section illustrates a realistic example of the use of Cell
//  visitors on the \doxygen{Mesh}. A set of different visitors is defined
//  here, each visitor associated with a particular type of cell. All the
//  visitors are registered with a MultiVisitor class which is passed to the
//  mesh.
//
//  The first step is to include the \code{CellInterfaceVisitor} header file.
//
//  \index{itk::Mesh!CellVisitor}
//  \index{itk::Mesh!CellInterfaceVisitor}
//  \index{CellVisitor}
//  \index{CellInterfaceVisitor}
//
//  Software Guide : EndLatex


#include "itkMesh.h"
#include "itkLineCell.h"
#include "itkTetrahedronCell.h"


// Software Guide : BeginCodeSnippet
#include "itkCellInterfaceVisitor.h"
// Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The typical mesh types are now declared.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float                             PixelType;
  typedef itk::Mesh< PixelType, 3 >         MeshType;

  typedef MeshType::CellType                CellType;

  typedef itk::VertexCell< CellType >       VertexType;
  typedef itk::LineCell< CellType >         LineType;
  typedef itk::TriangleCell< CellType >     TriangleType;
  typedef itk::TetrahedronCell< CellType >  TetrahedronType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Then, custom CellVisitor classes should be declared. The only requirement
  //  on the declaration of each visitor class is to provide a method named
  //  \code{Visit()}. This method expects as arguments a cell identifier and a
  //  pointer to the \emph{specific} cell type for which this visitor is
  //  intended.
  //
  //  \index{itk::Mesh!CellInterfaceVisitor}
  //  \index{CellInterfaceVisitor!requirements}
  //  \index{CellInterfaceVisitor!Visit()}
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  The following Vertex visitor simply prints out the identifier of the
  //  point with which the cell is associated. Note that the cell uses the
  //  method \code{GetPointId()} without any arguments. This method is only
  //  defined on the VertexCell.
  //
  //  \index{itk::CellInterface!GetPointId()}
  //  \index{GetPointId()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
class CustomVertexVisitor
{
public:
  void Visit(unsigned long cellId, VertexType * t )
    {
    std::cout << "cell " << cellId << " is a Vertex " << std::endl;
    std::cout << "    associated with point id = ";
    std::cout << t->GetPointId() << std::endl;
    }
  virtual ~CustomVertexVisitor() {}
};
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  The following Line visitor computes the length of the line.  Note
//  that this visitor is slightly more complicated since it needs to get
//  access to the actual mesh in order to get point coordinates from the
//  point identifiers returned by the line cell. This is done by holding a
//  pointer to the mesh and querying the mesh each time point coordinates are
//  required. The mesh pointer is set up in this case with the
//  \code{SetMesh()} method.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
class CustomLineVisitor
{
public:
  CustomLineVisitor():m_Mesh( 0 ) {}
  virtual ~CustomLineVisitor() {}

  void SetMesh( MeshType * mesh ) { m_Mesh = mesh; }

  void Visit(unsigned long cellId, LineType * t )
    {
    std::cout << "cell " << cellId << " is a Line " << std::endl;
    LineType::PointIdIterator pit = t->PointIdsBegin();
    MeshType::PointType p0;
    MeshType::PointType p1;
    m_Mesh->GetPoint( *pit++, &p0 );
    m_Mesh->GetPoint( *pit++, &p1 );
    const double length = p0.EuclideanDistanceTo( p1 );
    std::cout << " length = " << length << std::endl;
    }

private:
  MeshType::Pointer m_Mesh;
};
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  The Triangle visitor below prints out the identifiers of its points.
//  Note the use of the \code{PointIdIterator} and the \code{PointIdsBegin()}
//  and \code{PointIdsEnd()} methods.
//
//  \index{CellInterface!PointIdsBegin()}
//  \index{CellInterface!PointIdsEnd()}
//  \index{CellInterface!iterating points}
//  \index{PointIdsBegin()}
//  \index{PointIdsEnd()}
//
//  Software Guide : EndLatex


#ifndef __CustomTriangleVisitor
#define __CustomTriangleVisitor
// Software Guide : BeginCodeSnippet
class CustomTriangleVisitor
{
public:
  void Visit(unsigned long cellId, TriangleType * t )
    {
    std::cout << "cell " << cellId << " is a Triangle " << std::endl;
    LineType::PointIdIterator pit = t->PointIdsBegin();
    LineType::PointIdIterator end = t->PointIdsEnd();
    while( pit != end )
      {
      std::cout << "  point id = " << *pit << std::endl;
      ++pit;
      }
    }
  virtual ~CustomTriangleVisitor() {}
};
// Software Guide : EndCodeSnippet
#endif

//  Software Guide : BeginLatex
//
//  The TetrahedronVisitor below simply returns the number of faces on this
//  figure. Note that \code{GetNumberOfFaces()} is a method exclusive of 3D
//  cells.
//
//  \index{GetNumberOfFaces()!TetrahedronCell}
//  \index{TetrahedronCell!GetNumberOfFaces()}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
class CustomTetrahedronVisitor
{
public:
  void Visit(unsigned long cellId, TetrahedronType * t )
    {
    std::cout << "cell " << cellId << " is a Tetrahedron " << std::endl;
    std::cout << "  number of faces = ";
    std::cout << t->GetNumberOfFaces() << std::endl;
    }
  virtual ~CustomTetrahedronVisitor() {}
};
// Software Guide : EndCodeSnippet


int main(int, char *[])
{
  MeshType::Pointer  mesh = MeshType::New();

  // Creating the points and inserting them in the mesh
  //
  MeshType::PointType   point0;
  MeshType::PointType   point1;
  MeshType::PointType   point2;
  MeshType::PointType   point3;

  point0[0] = -1; point0[1] = -1; point0[2] = -1;
  point1[0] =  1; point1[1] =  1; point1[2] = -1;
  point2[0] =  1; point2[1] = -1; point2[2] =  1;
  point3[0] = -1; point3[1] =  1; point3[2] =  1;

  mesh->SetPoint( 0, point0 );
  mesh->SetPoint( 1, point1 );
  mesh->SetPoint( 2, point2 );
  mesh->SetPoint( 3, point3 );


  // Creating and associating the Tetrahedron
  //
  CellType::CellAutoPointer cellpointer;

  cellpointer.TakeOwnership( new TetrahedronType );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 1 );
  cellpointer->SetPointId( 2, 2 );
  cellpointer->SetPointId( 3, 3 );
  mesh->SetCell( 0, cellpointer );


  // Creating and associating the Triangles
  //
  cellpointer.TakeOwnership( new TriangleType );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 1 );
  cellpointer->SetPointId( 2, 2 );
  mesh->SetCell( 1, cellpointer );

  cellpointer.TakeOwnership( new TriangleType );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 2 );
  cellpointer->SetPointId( 2, 3 );
  mesh->SetCell( 2, cellpointer );

  cellpointer.TakeOwnership( new TriangleType );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 3 );
  cellpointer->SetPointId( 2, 1 );
  mesh->SetCell( 3, cellpointer );

  cellpointer.TakeOwnership( new TriangleType );
  cellpointer->SetPointId( 0, 3 );
  cellpointer->SetPointId( 1, 2 );
  cellpointer->SetPointId( 2, 1 );
  mesh->SetCell( 4, cellpointer );


  // Creating and associating the Edges
  //
  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 1 );
  mesh->SetCell( 5, cellpointer );

  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 1 );
  cellpointer->SetPointId( 1, 2 );
  mesh->SetCell( 6, cellpointer );

  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 2 );
  cellpointer->SetPointId( 1, 0 );
  mesh->SetCell( 7, cellpointer );

  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 1 );
  cellpointer->SetPointId( 1, 3 );
  mesh->SetCell( 8, cellpointer );

  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 3 );
  cellpointer->SetPointId( 1, 2 );
  mesh->SetCell( 9, cellpointer );

  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 3 );
  cellpointer->SetPointId( 1, 0 );
  mesh->SetCell( 10, cellpointer );


  // Creating and associating the Vertices
  //
  cellpointer.TakeOwnership( new VertexType );
  cellpointer->SetPointId( 0, 0 );
  mesh->SetCell( 11, cellpointer );

  cellpointer.TakeOwnership( new VertexType );
  cellpointer->SetPointId( 0, 1 );
  mesh->SetCell( 12, cellpointer );

  cellpointer.TakeOwnership( new VertexType );
  cellpointer->SetPointId( 0, 2 );
  mesh->SetCell( 13, cellpointer );

  cellpointer.TakeOwnership( new VertexType );
  cellpointer->SetPointId( 0, 3 );
  mesh->SetCell( 14, cellpointer );


  // Simple verification of the number of points and cells inserted
  //
  std::cout << "# Points= " << mesh->GetNumberOfPoints() << std::endl;
  std::cout << "# Cell  = " << mesh->GetNumberOfCells() << std::endl;


  //  Software Guide : BeginLatex
  //
  // \index{itk::Mesh!Cell\-Interface\-Visitor\-Implementation}
  // \index{itk::Mesh!CellInterfaceVisitor}
  // \index{itk::Mesh!CellVisitor}
  // \index{CellVisitor}
  //
  //  With the cell visitors we proceed now to instantiate CellVisitor
  //  implementations. The visitor classes defined above are used as template
  //  arguments of the cell visitor implementation.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CellInterfaceVisitorImplementation<
      PixelType, MeshType::CellTraits, VertexType,
      CustomVertexVisitor > VertexVisitorInterfaceType;

  typedef itk::CellInterfaceVisitorImplementation<
      PixelType, MeshType::CellTraits, LineType,
      CustomLineVisitor > LineVisitorInterfaceType;

  typedef itk::CellInterfaceVisitorImplementation<
      PixelType, MeshType::CellTraits, TriangleType,
      CustomTriangleVisitor > TriangleVisitorInterfaceType;

  typedef itk::CellInterfaceVisitorImplementation<
      PixelType, MeshType::CellTraits, TetrahedronType,
      CustomTetrahedronVisitor > TetrahedronVisitorInterfaceType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Note that the actual \code{CellInterfaceVisitorImplementation} is
  //  templated over the PixelType, the CellTraits, the CellType to be visited
  //  and the Visitor class defining what to do with the cell.
  //
  //  A visitor implementation class can now be created using the normal
  //  invocation to its \code{New()} method and assigning the result to a
  //  \doxygen{SmartPointer}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  VertexVisitorInterfaceType::Pointer  vertexVisitor =
                                   VertexVisitorInterfaceType::New();

  LineVisitorInterfaceType::Pointer  lineVisitor =
                                   LineVisitorInterfaceType::New();

  TriangleVisitorInterfaceType::Pointer  triangleVisitor =
                                   TriangleVisitorInterfaceType::New();

  TetrahedronVisitorInterfaceType::Pointer  tetrahedronVisitor =
                                   TetrahedronVisitorInterfaceType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Remember that the LineVisitor requires the pointer to the mesh object
  //  since it needs to get access to actual point coordinates. This is done by
  //  invoking the \code{SetMesh()} method defined above.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  lineVisitor->SetMesh( mesh );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  // Looking carefully you will notice that the \code{SetMesh()} method is
  // declared in \code{CustomLineVisitor} but we are invoking it on
  // \code{LineVisitorInterfaceType}. This is possible thanks to the way in
  // which the VisitorInterfaceImplementation is defined. This class derives
  // from the visitor type provided by the user as the fourth template
  // parameter. \code{LineVisitorInterfaceType} is then a derived class of
  // \code{CustomLineVisitor}.
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  The set of visitors should now be registered with the MultiVisitor class
  //  that will walk through the cells and delegate action to every registered
  //  visitor when the appropriate cell type is encountered. The following
  //  lines create a MultiVisitor object.
  //
  //  \index{CellMultiVisitorType}
  //  \index{MultiVisitor}
  //  \index{itk::Mesh!MultiVisitor}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef CellType::MultiVisitor CellMultiVisitorType;
  CellMultiVisitorType::Pointer multiVisitor = CellMultiVisitorType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Every visitor implementation is registered with the Mesh using the
  //  \code{AddVisitor()} method.
  //
  //  \index{itk::Mesh!AddVisitor()}
  //  \index{AddVisitor()!itk::Mesh}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  multiVisitor->AddVisitor( vertexVisitor      );
  multiVisitor->AddVisitor( lineVisitor        );
  multiVisitor->AddVisitor( triangleVisitor    );
  multiVisitor->AddVisitor( tetrahedronVisitor );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally, the iteration over the cells is triggered by calling the method
  //  \code{Accept()} on the Mesh class.
  //
  //  \index{itk::Mesh!Accept()}
  //  \index{Accept()!itk::Mesh!}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  mesh->Accept( multiVisitor );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \code{Accept()} method will iterate over all the cells and for each
  //  one will invite the MultiVisitor to attempt an action on the cell. If no
  //  visitor is interested on the current cell type, the cell is just ignored
  //  and skipped.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}

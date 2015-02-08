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
//  In order to facilitate access to particular cell types, a convenience
//  mechanism has been built-in on the \doxygen{Mesh}. This mechanism is
//  based on the \emph{Visitor Pattern} presented  in \cite{Gamma1995}. The
//  visitor pattern is designed to facilitate the process of walking through an
//  heterogeneous list of objects sharing a common base class.
//
//  The first requirement for using the \code{CellVisitor} mechanism it to
//  include the \code{CellInterfaceVisitor} header file.
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
  //  Then, a custom CellVisitor class should be declared. In this particular
  //  example, the visitor class is intended to act only on \code{TriangleType}
  //  cells. The only requirement on the declaration of the visitor class is that
  //  it must provide a method named \code{Visit()}. This method expects as
  //  arguments a cell identifier and a pointer to the \emph{specific} cell type
  //  for which this visitor is intended. Nothing prevents a visitor class from
  //  providing \code{Visit()} methods for several different cell types.  The
  //  multiple methods will be differentiated by the natural C++ mechanism of
  //  function overload. The following code illustrates a minimal cell visitor
  //  class.
  //
  //  \index{itk::Mesh!CellInterfaceVisitor}
  //  \index{CellInterfaceVisitor!requirements}
  //  \index{CellInterfaceVisitor!Visit()}
  //
  //  Software Guide : EndLatex


#ifndef __CustomTriangleVisitor
#define __CustomTriangleVisitor
// Software Guide : BeginCodeSnippet
class CustomTriangleVisitor
{
public:
  typedef itk::TriangleCell<CellType>      TriangleType;
  void Visit(unsigned long cellId, TriangleType * t )
    {
    std::cout << "Cell # " << cellId << " is a TriangleType ";
    std::cout << t->GetNumberOfPoints() << std::endl;
    }
  CustomTriangleVisitor() {}
  virtual ~CustomTriangleVisitor() {}
};
// Software Guide : EndCodeSnippet
#endif

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
  //  This newly defined class will now be used to instantiate a cell visitor.
  //  In this particular example we create a class \code{CustomTriangleVisitor}
  //  which will be invoked each time a triangle cell is found while the mesh
  //  iterates over the cells.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CellInterfaceVisitorImplementation<
                              PixelType,
                              MeshType::CellTraits,
                              TriangleType,
                              CustomTriangleVisitor
                                       > TriangleVisitorInterfaceType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Note that the actual \code{CellInterfaceVisitorImplementation} is
  //  templated over the PixelType, the CellTraits, the CellType to be visited
  //  and the Visitor class that defines with will be done with the cell.
  //
  //  A visitor implementation class can now be created using the normal
  //  invocation to its \code{New()} method and assigning the result to a
  //  \doxygen{SmartPointer}.
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  TriangleVisitorInterfaceType::Pointer  triangleVisitor =
                                   TriangleVisitorInterfaceType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Many different visitors can be configured in this way. The set of all
  //  visitors can be registered with the MultiVisitor class provided for the
  //  mesh. An instance of the MultiVisitor class will walk through the cells and delegate
  //  action to every registered visitor when the appropriate cell type is
  //  encountered.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef CellType::MultiVisitor CellMultiVisitorType;
  CellMultiVisitorType::Pointer multiVisitor = CellMultiVisitorType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The visitor is registered with the Mesh using the \code{AddVisitor()}
  //  method.
  //
  //  \index{itk::Mesh!AddVisitor()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  multiVisitor->AddVisitor( triangleVisitor );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally, the iteration over the cells is triggered by calling the method
  //  \code{Accept()} on the \code{itk::Mesh}.
  //
  //  \index{itk::Mesh!Accept()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  mesh->Accept( multiVisitor );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \code{Accept()} method will iterate over all the cells and for each
  //  one will invite the MultiVisitor to attempt an action on the cell. If no
  //  visitor is interested on the current cell type the cell is just ignored
  //  and skipped.
  //
  //  MultiVisitors make it possible to add behavior to the cells without having to
  //  create new methods on the cell types or creating a complex visitor class
  //  that knows about every CellType.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}

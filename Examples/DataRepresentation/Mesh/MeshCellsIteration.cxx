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
//  Cells are stored in the \doxygen{Mesh} as pointers to a generic cell
//  \doxygen{CellInterface}.  This implies that only the virtual methods
//  defined on this base cell class can be invoked. In order to use methods
//  that are specific to each cell type it is necessary to down-cast the
//  pointer to the actual type of the cell.  This can be done safely by taking
//  advantage of the \code{GetType()} method that allows to identify the actual
//  type of a cell.
//
//  Let's start by assuming a mesh defined with one tetrahedron and all its
//  boundary faces. That is, four triangles, six edges and four vertices.
//
//  Software Guide : EndLatex


#include "itkMesh.h"
#include "itkLineCell.h"
#include "itkTetrahedronCell.h"


int main(int, char *[])
{
  typedef float                             PixelType;
  typedef itk::Mesh< PixelType, 3 >         MeshType;

  typedef MeshType::CellType                CellType;

  typedef itk::VertexCell< CellType >       VertexType;
  typedef itk::LineCell< CellType >         LineType;
  typedef itk::TriangleCell< CellType >     TriangleType;
  typedef itk::TetrahedronCell< CellType >  TetrahedronType;

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


  std::cout << "# Points= " << mesh->GetNumberOfPoints() << std::endl;
  std::cout << "# Cell  = " << mesh->GetNumberOfCells() << std::endl;


  //  Software Guide : BeginLatex
  //
  //  The cells can be visited using CellsContainer iterators . The iterator
  //  \code{Value()} corresponds to a raw pointer to the \code{CellType} base
  //  class.
  //
  // \index{itk::Mesh!CellsContainer}
  // \index{itk::Mesh!CellIterator}
  // \index{itk::Mesh!GetCells()}
  // \index{CellsContainer!Begin()}
  // \index{CellsContainer!End()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef MeshType::CellsContainer::ConstIterator  CellIterator;

  CellIterator cellIterator = mesh->GetCells()->Begin();
  CellIterator cellEnd      = mesh->GetCells()->End();

  while( cellIterator != cellEnd )
    {
    CellType * cell = cellIterator.Value();
    std::cout << cell->GetNumberOfPoints() << std::endl;
    ++cellIterator;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  In order to perform down-casting in a safe manner, the cell type can be
  //  queried first using the \code{GetType()} method.  Codes for the cell
  //  types have been defined with an \code{enum} type on the
  //  \code{itkCellInterface.h} header file.  These codes are :
  //
  //  \begin{itemize}
  //  \item VERTEX\_CELL
  //  \item LINE\_CELL
  //  \item TRIANGLE\_CELL
  //  \item QUADRILATERAL\_CELL
  //  \item POLYGON\_CELL
  //  \item TETRAHEDRON\_CELL
  //  \item HEXAHEDRON\_CELL
  //  \item QUADRATIC\_EDGE\_CELL
  //  \item QUADRATIC\_TRIANGLE\_CELL
  //  \end{itemize}
  //
  //  The method \code{GetType()} returns one of these codes. It is then
  //  possible to test the type of the cell before down-casting its pointer to
  //  the actual type. For example, the following code visits all the cells in
  //  the mesh and tests which ones are actually of type
  //  \code{LINE\_CELL}. Only those cells are down-casted to \code{LineType}
  //  cells and a method specific for the \code{LineType} is invoked.
  //
  //  Software Guide : EndLatex

  std::cout << "Visiting the Line cells : " << std::endl;

  // Software Guide : BeginCodeSnippet
  cellIterator = mesh->GetCells()->Begin();
  cellEnd      = mesh->GetCells()->End();

  while( cellIterator != cellEnd )
    {
    CellType * cell = cellIterator.Value();
    if( cell->GetType() == CellType::LINE_CELL )
      {
      LineType * line = static_cast<LineType *>( cell );
      std::cout << "dimension = " << line->GetDimension();
      std::cout << " # points = " << line->GetNumberOfPoints();
      std::cout << std::endl;
      }
    ++cellIterator;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  In order to perform different actions on different cell types a
  //  \code{switch} statement can be used with cases for every cell type.
  //  The following code illustrates an iteration over the cells and the
  //  invocation of different methods on each cell type.
  //
  //  Software Guide : EndLatex

  std::cout << "Visiting several cell types : " << std::endl;

  // Software Guide : BeginCodeSnippet
  cellIterator = mesh->GetCells()->Begin();
  cellEnd      = mesh->GetCells()->End();

  while( cellIterator != cellEnd )
    {
    CellType * cell = cellIterator.Value();
    switch( cell->GetType() )
      {
      case CellType::VERTEX_CELL:
        {
        std::cout << "VertexCell : " << std::endl;
        VertexType * line = dynamic_cast<VertexType *>( cell );
        std::cout << "dimension = " << line->GetDimension()      << std::endl;
        std::cout << "# points  = " << line->GetNumberOfPoints() << std::endl;
        break;
        }
      case CellType::LINE_CELL:
        {
        std::cout << "LineCell : " << std::endl;
        LineType * line = dynamic_cast<LineType *>( cell );
        std::cout << "dimension = " << line->GetDimension()      << std::endl;
        std::cout << "# points  = " << line->GetNumberOfPoints() << std::endl;
        break;
        }
      case CellType::TRIANGLE_CELL:
        {
        std::cout << "TriangleCell : " << std::endl;
        TriangleType * line = dynamic_cast<TriangleType *>( cell );
        std::cout << "dimension = " << line->GetDimension()      << std::endl;
        std::cout << "# points  = " << line->GetNumberOfPoints() << std::endl;
        break;
        }
      default:
        {
        std::cout << "Cell with more than three points" << std::endl;
        std::cout << "dimension = " << cell->GetDimension()      << std::endl;
        std::cout << "# points  = " << cell->GetNumberOfPoints() << std::endl;
        break;
        }
      }
    ++cellIterator;
    }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}

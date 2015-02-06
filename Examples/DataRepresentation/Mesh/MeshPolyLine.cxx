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
//  This section illustrates how to represent a classical \emph{PolyLine}
//  structure using the \doxygen{Mesh}
//
//  \index{itk::Mesh!PolyLine}
//
//  Software Guide : EndLatex


//  Software Guide : BeginLatex
//
//  A PolyLine only involves zero and one dimensional cells, which are
//  represented by the \doxygen{VertexCell} and the \doxygen{LineCell}.
//
//  \index{itk::LineCell!header}
//  \index{itk::VertexCell!header}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkMesh.h"
#include "itkLineCell.h"
// Software Guide : EndCodeSnippet


int main(int, char *[])
{
  //  Software Guide : BeginLatex
  //
  //  Then the PixelType is defined and the mesh type is instantiated with it.
  //  Note that the dimension of the space is two in this case.
  //
  //  \index{itk::Mesh!Instantiation}
  //  \index{itk::Mesh!PixelType}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float                             PixelType;
  typedef itk::Mesh< PixelType, 2 >         MeshType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The cell type can now be instantiated using the traits
  //  taken from the Mesh.
  //
  //  \index{itk::LineCell!Instantiation}
  //  \index{itk::VertexCell!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef MeshType::CellType                CellType;
  typedef itk::VertexCell< CellType >       VertexType;
  typedef itk::LineCell< CellType >         LineType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The mesh is created and the points associated with the vertices are
  //  inserted.  Note that there is an important distinction between the
  //  points in the mesh and the \doxygen{VertexCell} concept. A VertexCell
  //  is a cell of dimension zero. Its main difference as compared to a point
  //  is that the cell can be aware of neighborhood relationships with other
  //  cells. Points are not aware of the existence of cells. In fact, from
  //  the pure topological point of view, the coordinates of points in the
  //  mesh are completely irrelevant.  They may as well be absent from the
  //  mesh structure altogether.  VertexCells on the other hand are necessary
  //  to represent the full set of neighborhood relationships on the
  //  Polyline.
  //
  //  In this example we create a polyline connecting the four vertices of a
  //  square by using three of the square sides.
  //
  //  \index{itk::Mesh!New()}
  //  \index{itk::Mesh!SetPoint()}
  //  \index{itk::Mesh!PointType}
  //  \index{itk::Mesh!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MeshType::Pointer  mesh = MeshType::New();

  MeshType::PointType   point0;
  MeshType::PointType   point1;
  MeshType::PointType   point2;
  MeshType::PointType   point3;

  point0[0] = -1; point0[1] = -1;
  point1[0] =  1; point1[1] = -1;
  point2[0] =  1; point2[1] =  1;
  point3[0] = -1; point3[1] =  1;

  mesh->SetPoint( 0, point0 );
  mesh->SetPoint( 1, point1 );
  mesh->SetPoint( 2, point2 );
  mesh->SetPoint( 3, point3 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We proceed now to create the cells, associate them with the points and
  //  insert them on the mesh.
  //
  //  \index{itk::AutoPointer!TakeOwnership()}
  //  \index{CellAutoPointer!TakeOwnership()}
  //  \index{CellType!creation}
  //  \index{itk::Mesh!SetCell()}
  //  \index{itk::LineCell!Instantiation}
  //  \index{itk::LineCell!SetPointId()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  CellType::CellAutoPointer cellpointer;

  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 1 );
  mesh->SetCell( 0, cellpointer );

  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 1 );
  cellpointer->SetPointId( 1, 2 );
  mesh->SetCell( 1, cellpointer );

  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 2 );
  cellpointer->SetPointId( 1, 0 );
  mesh->SetCell( 2, cellpointer );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally the zero dimensional cells represented by the
  //  \doxygen{VertexCell} are created and inserted in the mesh.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new VertexType );
  cellpointer->SetPointId( 0, 0 );
  mesh->SetCell( 3, cellpointer );

  cellpointer.TakeOwnership( new VertexType );
  cellpointer->SetPointId( 0, 1 );
  mesh->SetCell( 4, cellpointer );

  cellpointer.TakeOwnership( new VertexType );
  cellpointer->SetPointId( 0, 2 );
  mesh->SetCell( 5, cellpointer );

  cellpointer.TakeOwnership( new VertexType );
  cellpointer->SetPointId( 0, 3 );
  mesh->SetCell( 6, cellpointer );
  // Software Guide : EndCodeSnippet


  // Print out the number of points and the number of cells.
  std::cout << "# Points= " << mesh->GetNumberOfPoints() << std::endl;
  std::cout << "# Cell  = " << mesh->GetNumberOfCells() << std::endl;


  //  Software Guide : BeginLatex
  //
  //  At this point the Mesh contains four points and three cells.  The
  //  points can be visited using PointContainer iterators.
  //
  // \index{itk::Mesh!PointsContainer}
  // \index{itk::Mesh!PointIterator}
  // \index{itk::Mesh!GetPoints()}
  // \index{PointsContainer!Begin()}
  // \index{PointsContainer!End()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef MeshType::PointsContainer::ConstIterator  PointIterator;
  PointIterator pointIterator = mesh->GetPoints()->Begin();
  PointIterator pointEnd      = mesh->GetPoints()->End();

  while( pointIterator != pointEnd )
    {
    std::cout << pointIterator.Value() << std::endl;
    ++pointIterator;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The cells can be visited using CellsContainer iterators.
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
  //  Note that cells are stored as pointer to a generic cell type that is the
  //  base class of all the specific cell classes. This means that at this
  //  level we can only have access to the virtual methods defined in the
  //  \code{CellType}.
  //
  //  The point identifiers to which the cells have been associated can be
  //  visited using iterators defined in the \code{CellType} trait. The
  //  following code illustrates the use of the PointIdIterator. The
  //  \code{PointIdsBegin()} method returns the iterator to the first
  //  point-identifier in the cell.  The \code{PointIdsEnd()} method returns
  //  the iterator to the past-end point-identifier in the cell.
  //
  //  \index{CellType!PointIdsBegin()}
  //  \index{CellType!PointIdsEnd()}
  //  \index{CellType!PointIdIterator}
  //  \index{PointIdIterator}
  //  \index{PointIdsBegin()}
  //  \index{PointIdsEnd()}
  //
  //  Software Guide : EndLatex

  cellIterator = mesh->GetCells()->Begin();
  cellEnd      = mesh->GetCells()->End();

  while( cellIterator != cellEnd )
    {
    CellType * cell = cellIterator.Value();

    std::cout << "cell with " << cell->GetNumberOfPoints();
    std::cout << " points   " << std::endl;

    // Software Guide : BeginCodeSnippet
    typedef CellType::PointIdIterator     PointIdIterator;

    PointIdIterator pointIditer = cell->PointIdsBegin();
    PointIdIterator pointIdend  = cell->PointIdsEnd();

    while( pointIditer != pointIdend )
      {
      std::cout << *pointIditer << std::endl;
      ++pointIditer;
      }
    // Software Guide : EndCodeSnippet

    ++cellIterator;
    }


  //  Software Guide : BeginLatex
  //
  //  Note that the point-identifier is obtained from the iterator using the
  //  more traditional \code{*iterator} notation instead the \code{Value()}
  //  notation used by cell-iterators.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}

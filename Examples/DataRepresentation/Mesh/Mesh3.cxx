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
//  Just as custom data can be associated with points in the mesh,
//  it is also possible to associate custom data with cells. The type of the
//  data associated with the cells can be different from the data type
//  associated with points. By default, however, these two types are the same.
//  The following example illustrates how to access data associated with cells.
//  The approach is analogous to the one used to access point data.
//
//  \index{itk::Mesh!Cell data}
//
//  Software Guide : EndLatex


//  Software Guide : BeginLatex
//
//  Consider the example of a mesh containing lines on which values are
//  associated with each line. The mesh and cell header files should be
//  included first.
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
  //  Then the \code{PixelType} is defined and the mesh type is
  //  instantiated with it.
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
  //  The \doxygen{LineCell} type can now be instantiated using the traits
  //  taken from the Mesh.
  //
  //  \index{itk::LineCell!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef MeshType::CellType                CellType;
  typedef itk::LineCell< CellType >         LineType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Let's now create a Mesh and insert some points into it. Note that the
  //  dimension of the points matches the dimension of the Mesh. Here we insert
  //  a sequence of points that look like a plot of the $\log()$ function.  We
  //  add the \code{vnl\_math::eps} value in order to avoid numerical errors when
  //  the point id is zero. The value of \code{vnl\_math::eps} is the difference
  //  between 1.0 and the least value greater than 1.0 that is representable in
  //  this computer.
  //
  //  \index{itk::Mesh!New()}
  //  \index{itk::Mesh!SetPoint()}
  //  \index{itk::Mesh!PointType}
  //  \index{itk::Mesh!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MeshType::Pointer  mesh = MeshType::New();

  typedef MeshType::PointType PointType;
  PointType point;

  const unsigned int numberOfPoints = 10;
  for(unsigned int id=0; id<numberOfPoints; id++)
    {
    point[0] = static_cast<PointType::ValueType>( id ); // x
    point[1] = std::log( static_cast<double>( id ) + itk::Math::eps );    // y
    mesh->SetPoint( id, point );
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  A set of line cells is created and associated with the existing points by
  //  using point identifiers. In this simple case, the point identifiers can
  //  be deduced from cell identifiers since the line cells are ordered in the
  //  same way.
  //
  //  \index{itk::AutoPointer!TakeOwnership()}
  //  \index{CellAutoPointer!TakeOwnership()}
  //  \index{CellType!creation}
  //  \index{itk::Mesh!SetCell()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  CellType::CellAutoPointer line;
  const unsigned int numberOfCells = numberOfPoints-1;
  for(unsigned int cellId=0; cellId<numberOfCells; cellId++)
    {
    line.TakeOwnership(  new LineType  );
    line->SetPointId( 0, cellId   ); // first point
    line->SetPointId( 1, cellId+1 ); // second point
    mesh->SetCell( cellId, line );   // insert the cell
    }
  // Software Guide : EndCodeSnippet


  std::cout << "Points = " << mesh->GetNumberOfPoints() << std::endl;
  std::cout << "Cells  = " << mesh->GetNumberOfCells()  << std::endl;


  //  Software Guide : BeginLatex
  //
  //  Data associated with cells is inserted in the \doxygen{Mesh} by using
  //  the \code{SetCellData()} method.  It requires the user to provide an
  //  identifier and the value to be inserted. The identifier should match one
  //  of the inserted cells. In this simple example, the square of the cell
  //  identifier is used as cell data. Note the use of \code{static\_cast} to
  //  \code{PixelType} in the assignment.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  for(unsigned int cellId=0; cellId<numberOfCells; cellId++)
    {
    mesh->SetCellData( cellId, static_cast<PixelType>( cellId * cellId ) );
    }

  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Cell data can be read from the Mesh with the
  //  \code{GetCellData()} method. It requires the user to provide the
  //  identifier of the cell for which the data is to be retrieved. The user
  //  should provide also a valid pointer to a location where the data can be
  //  copied.
  //
  //  \index{itk::Mesh!GetCellData()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  for(unsigned int cellId=0; cellId<numberOfCells; ++cellId)
    {
    PixelType value = static_cast<PixelType>(0.0);
    mesh->GetCellData( cellId, &value );
    std::cout << "Cell " << cellId << " = " << value << std::endl;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Neither \code{SetCellData()} or \code{GetCellData()} are efficient ways
  //  to access cell data. More efficient access to cell data can be achieved
  //  by using the Iterators built into the \code{CellDataContainer}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef MeshType::CellDataContainer::ConstIterator CellDataIterator;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Note that the \code{ConstIterator} is used here because the data is only
  //  going to be read.  This approach is exactly the same already illustrated
  //  for getting access to point data. The iterator to the first cell data
  //  item can be obtained with the \code{Begin()} method of the
  //  \code{CellDataContainer}. The past-end iterator is returned by the \code{End()}
  //  method. The cell data container itself can be obtained from the mesh with
  //  the method \code{GetCellData()}.
  //
  //  \index{itk::Mesh!Iterating cell data}
  //  \index{itk::Mesh!GetCellData()}
  //  \index{CellDataContainer!Begin()}
  //  \index{CellDataContainer!End()}
  //  \index{CellDataContainer!Iterator}
  //  \index{CellDataContainer!ConstIterator}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  CellDataIterator cellDataIterator  = mesh->GetCellData()->Begin();
  CellDataIterator end               = mesh->GetCellData()->End();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally, a standard loop is used to iterate over all the cell data
  //  entries. Note the use of the \code{Value()} method to get the
  //  value associated with the data entry. \code{PixelType} elements are copied into the
  //  local variable \code{cellValue}.
  //
  //  \index{CellDataIterator!Value()}
  //  \index{CellDataIterator!increment}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  while( cellDataIterator != end )
    {
    PixelType cellValue = cellDataIterator.Value();
    std::cout << cellValue << std::endl;
    ++cellDataIterator;
    }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}

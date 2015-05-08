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
//  The \doxygen{Mesh} class is intended to represent shapes in space.  It
//  derives from the \doxygen{PointSet} class and hence inherits all the
//  functionality related to points and access to the pixel-data associated
//  with the points.  The mesh class is also $N$-dimensional which
//  allows a great flexibility in its use.
//
//  In practice a \code{Mesh} class can be seen as a \code{PointSet} to
//  which cells (also known as elements) of many different dimensions and
//  shapes have been added. Cells in the mesh are defined in terms of the
//  existing points using their point-identifiers.
//
//  As with \code{PointSet}, a \code{Mesh} object may be \emph{static}
//  or \emph{dynamic}. The first is used when the number of
//  points in the set is known in advance and not expected
//  to change as a consequence of the manipulations performed on the
//  set. The dynamic style, on the other hand, is intended to support
//  insertion and removal of points in an efficient manner. In addition
//  to point management, the distinction facilitates optimization of
//  performance and memory management of cells.
//
//  \index{itk::Mesh}
//  \index{itk::Mesh!Static}
//  \index{itk::Mesh!Dynamic}
//  \index{itk::Mesh!Header file}
//
//  In order to use the Mesh class, its header file should be included.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkMesh.h"
// Software Guide : EndCodeSnippet

int main(int, char *[])
{

  //  Software Guide : BeginLatex
  //
  //  Then, the type associated with the points must be selected and used for
  //  instantiating the Mesh type.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   float   PixelType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \code{Mesh} type extensively uses the capabilities provided by
  //  \href{http://www.boost.org/more/generic_programming.html}{Generic
  //  Programming}. In particular, the \code{Mesh} class is parameterized over
  //  \code{PixelType}, spatial dimension, and (optionally) a parameter set
  //  called \code{MeshTraits}.  \code{PixelType} is the type of the
  //  value associated with each point (just as is done with \code{PointSet}).
  //  The following illustrates a typical instantiation of \code{Mesh}.
  //
  //  \index{itk::Mesh!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 3;
  typedef itk::Mesh< PixelType, Dimension > MeshType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Meshes typically require large amounts of memory. For this reason, they
  //  are reference counted objects, managed using \doxygen{SmartPointers}. The
  //  following line illustrates how a mesh is created by invoking the
  //  \code{New()} method on \code{MeshType} and assigning the result to a
  //  \code{SmartPointer}.
  //
  //  \index{itk::Mesh!New()}
  //  \index{itk::Mesh!Pointer()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MeshType::Pointer mesh = MeshType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Management of points in a \code{Mesh} is identical to that in a \code{PointSet}.
  //  The type of point associated with the mesh can be obtained through the
  //  \code{PointType} trait. The following code shows the creation of points
  //  compatible with the mesh type defined above and the assignment of values
  //  to its coordinates.
  //
  //  \index{itk::Mesh!PointType}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MeshType::PointType p0;
  MeshType::PointType p1;
  MeshType::PointType p2;
  MeshType::PointType p3;

  p0[0]= -1.0; p0[1]= -1.0; p0[2]= 0.0; // first  point ( -1, -1, 0 )
  p1[0]=  1.0; p1[1]= -1.0; p1[2]= 0.0; // second point (  1, -1, 0 )
  p2[0]=  1.0; p2[1]=  1.0; p2[2]= 0.0; // third  point (  1,  1, 0 )
  p3[0]= -1.0; p3[1]=  1.0; p3[2]= 0.0; // fourth point ( -1,  1, 0 )
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The points can now be inserted into the \code{Mesh} using the \code{SetPoint()}
  //  method. Note that points are copied into the mesh structure, meaning
  //  that the local instances of the points can now be modified without
  //  affecting the Mesh content.
  //
  //  \index{itk::Mesh!SetPoint()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  mesh->SetPoint( 0, p0 );
  mesh->SetPoint( 1, p1 );
  mesh->SetPoint( 2, p2 );
  mesh->SetPoint( 3, p3 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The current number of points in a mesh can be queried with the
  //  \code{GetNumberOfPoints()} method.
  //
  //  \index{itk::Mesh!GetNumberOfPoints()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Points = " << mesh->GetNumberOfPoints() << std::endl;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The points can now be efficiently accessed using the Iterator to the
  //  \code{PointsContainer} as was done in the previous section for the
  //  PointSet.
  //
  //  \index{PointsContainer!Iterator}
  //  \index{itk::Mesh!GetPoints()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef MeshType::PointsContainer::Iterator PointsIterator;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  A point iterator is initialized to the first point with the
  //  \code{Begin()} method of the \code{PointsContainer}.
  //
  //  \index{PointsContainer!Begin()}
  //  \index{itk::Mesh!GetPoints()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointsIterator  pointIterator = mesh->GetPoints()->Begin();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \code{++} operator is used to advance the iterator from one
  //  point to the next. The value associated with the \code{Point} to which
  //  the iterator is pointing is obtained with the \code{Value()} method.
  //  The loop for walking through all the points is controlled by comparing
  //  the current iterator with the iterator returned by the \code{End()}
  //  method of the \code{PointsContainer}. The following illustrates the
  //  typical loop for walking through the points of a mesh.
  //
  //  \index{PointsContainer!End()}
  //  \index{PointsContainer!Iterator}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointsIterator end = mesh->GetPoints()->End();
  while( pointIterator != end )
    {
    MeshType::PointType p = pointIterator.Value();  // access the point
    std::cout << p << std::endl;                    // print the point
    ++pointIterator;                                // advance to next point
    }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}

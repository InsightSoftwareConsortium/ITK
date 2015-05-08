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
//  The \doxygen{Mesh} class supports the representation of formal
//  topologies. In particular the concept of \emph{K-Complex} can be
//  correctly represented in the Mesh. An informal definition of K-Complex
//  may be as follows: a K-Complex is a topological structure in which for
//  every cell of dimension $N$, its boundary faces (which are cells of
//  dimension $N-1$) also belong to the structure.
//
//  This section illustrates how to instantiate a K-Complex structure using the
//  mesh. The example structure is composed of one tetrahedron, its
//  four triangle faces, its six line edges and its four vertices.
//
//  \index{itk::Mesh!K-Complex}
//
//  Software Guide : EndLatex


//  Software Guide : BeginLatex
//
//  The header files of all the cell types involved should be loaded along with
//  the header file of the mesh class.
//
//  \index{itk::LineCell!header}
//  \index{itk::VertexCell!header}
//  \index{itk::TriangleCell!header}
//  \index{itk::TetrahedronCell!header}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkMesh.h"
#include "itkLineCell.h"
#include "itkTetrahedronCell.h"
// Software Guide : EndCodeSnippet


int main(int, char *[])
{
  //  Software Guide : BeginLatex
  //
  //  Then the PixelType is defined and the mesh type is instantiated with it.
  //  Note that the dimension of the space is three in this case.
  //
  //  \index{itk::Mesh!Instantiation}
  //  \index{itk::Mesh!PixelType}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float                             PixelType;
  typedef itk::Mesh< PixelType, 3 >         MeshType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The cell type can now be instantiated using the traits
  //  taken from the Mesh.
  //
  //  \index{itk::LineCell!Instantiation}
  //  \index{itk::VertexCell!Instantiation}
  //  \index{itk::TriangleCell!Instantiation}
  //  \index{itk::TetrahedronCell!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef MeshType::CellType                CellType;
  typedef itk::VertexCell< CellType >       VertexType;
  typedef itk::LineCell< CellType >         LineType;
  typedef itk::TriangleCell< CellType >     TriangleType;
  typedef itk::TetrahedronCell< CellType >  TetrahedronType;
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
  //  K-Complex.
  //
  //  The geometrical coordinates of the nodes of a regular tetrahedron can be
  //  obtained by taking every other node from a regular cube.
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

  point0[0] = -1; point0[1] = -1; point0[2] = -1;
  point1[0] =  1; point1[1] =  1; point1[2] = -1;
  point2[0] =  1; point2[1] = -1; point2[2] =  1;
  point3[0] = -1; point3[1] =  1; point3[2] =  1;

  mesh->SetPoint( 0, point0 );
  mesh->SetPoint( 1, point1 );
  mesh->SetPoint( 2, point2 );
  mesh->SetPoint( 3, point3 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We proceed now to create the cells, associate them with the points and
  //  insert them on the mesh. Starting with the tetrahedron we write the
  //  following code.
  //
  //  \index{itk::AutoPointer!TakeOwnership()}
  //  \index{CellAutoPointer!TakeOwnership()}
  //  \index{CellType!creation}
  //  \index{itk::Mesh!SetCell()}
  //  \index{itk::TetrahedronCell!Instantiation}
  //  \index{itk::TetrahedronCell!SetPointId()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  CellType::CellAutoPointer cellpointer;

  cellpointer.TakeOwnership( new TetrahedronType );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 1 );
  cellpointer->SetPointId( 2, 2 );
  cellpointer->SetPointId( 3, 3 );
  mesh->SetCell( 0, cellpointer );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Four triangular faces are created and associated with the mesh now.
  //  The first triangle connects points {0,1,2}.
  //
  //  \index{itk::TriangleCell!Instantiation}
  //  \index{itk::TriangleCell!SetPointId()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new TriangleType );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 1 );
  cellpointer->SetPointId( 2, 2 );
  mesh->SetCell( 1, cellpointer );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The second triangle connects points { 0, 2, 3 }.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new TriangleType );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 2 );
  cellpointer->SetPointId( 2, 3 );
  mesh->SetCell( 2, cellpointer );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The third triangle connects points { 0, 3, 1 }.
  //
  //  Software Guide : EndLatex

   // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new TriangleType );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 3 );
  cellpointer->SetPointId( 2, 1 );
  mesh->SetCell( 3, cellpointer );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The fourth triangle connects points { 3, 2, 1 }.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new TriangleType );
  cellpointer->SetPointId( 0, 3 );
  cellpointer->SetPointId( 1, 2 );
  cellpointer->SetPointId( 2, 1 );
  mesh->SetCell( 4, cellpointer );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Note how the \code{CellAutoPointer} is reused every time. Reminder: the
  //  \doxygen{AutoPointer} loses ownership of the cell when it is passed as
  //  an argument of the \code{SetCell()} method. The AutoPointer is attached
  //  to a new cell by using the \code{TakeOwnership()} method.
  //
  //  The construction of the K-Complex continues now with the creation of the
  //  six lines on the tetrahedron edges.
  //
  //  \index{itk::LineCell!Instantiation}
  //  \index{itk::LineCell!SetPointId()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
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
  // Software Guide : EndCodeSnippet


  // Print out the number of points and the number of cells.
  std::cout << "# Points= " << mesh->GetNumberOfPoints() << std::endl;
  std::cout << "# Cell  = " << mesh->GetNumberOfCells() << std::endl;


  //  Software Guide : BeginLatex
  //
  //  At this point the Mesh contains four points and fifteen cells enumerated
  //  from 0 to 14.  The points can be visited using PointContainer iterators.
  //
  // \index{itk::Mesh!PointsContainer}
  // \index{itk::Mesh!PointsIterators}
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
  // \index{itk::Mesh!CellsIterators}
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
  //  following code illustrates the use of the PointIdIterators. The
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


  //  Software Guide : BeginLatex
  //
  //  Up to here, the topology of the K-Complex is not completely defined since
  //  we have only introduced the cells. ITK allows the user to define
  //  explicitly the neighborhood relationships between cells. It is clear that
  //  a clever exploration of the point identifiers could have allowed a user
  //  to figure out the neighborhood relationships. For example, two triangle
  //  cells sharing the same two point identifiers will probably be neighbor
  //  cells. Some of the drawbacks on this implicit discovery of neighborhood
  //  relationships is that it takes computing time and that some applications
  //  may not accept the same assumptions. A specific case is surgery
  //  simulation. This application typically simulates bistoury cuts
  //  in a mesh representing an organ. A small cut in the surface may be made
  //  by specifying that two triangles are not considered to be neighbors any
  //  more.
  //
  //  Neighborhood relationships are represented in the mesh by the
  //  notion of \emph{BoundaryFeature}. Every cell has an internal list of
  //  cell-identifiers pointing to other cells that are considered to be its
  //  neighbors. Boundary features are classified by dimension. For example, a
  //  line will have two boundary features of dimension zero corresponding to
  //  its two vertices. A tetrahedron will have boundary features of dimension
  //  zero, one and two, corresponding to its four vertices, six edges and four
  //  triangular faces. It is up to the user to specify the connections between
  //  the cells.
  //
  //  \index{BoundaryFeature}
  //  \index{CellBoundaryFeature}
  //  \index{itk::Mesh!BoundaryFeature}
  //
  //  Let's take in our current example the tetrahedron cell that was
  //  associated with the cell-identifier \code{0} and assign to it the four
  //  vertices as boundaries of dimension zero. This is done by invoking the
  //  \code{SetBoundaryAssignment()} method on the Mesh class.
  //
  //  \index{itk::Mesh!SetBoundaryAssignment()}
  //  \index{SetBoundaryAssignment()!itk::Mesh}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MeshType::CellIdentifier cellId = 0;  // the tetrahedron

  int dimension = 0;                    // vertices

  MeshType::CellFeatureIdentifier featureId = 0;

  mesh->SetBoundaryAssignment( dimension, cellId, featureId++, 11 );
  mesh->SetBoundaryAssignment( dimension, cellId, featureId++, 12 );
  mesh->SetBoundaryAssignment( dimension, cellId, featureId++, 13 );
  mesh->SetBoundaryAssignment( dimension, cellId, featureId++, 14 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \code{featureId} is simply a number associated with the sequence of
  //  the boundary cells of the same dimension in a specific cell. For example,
  //  the zero-dimensional features of a tetrahedron are its four vertices.
  //  Then the zero-dimensional feature-Ids for this cell will range from zero
  //  to three. The one-dimensional features of the tetrahedron are its six
  //  edges, hence its one-dimensional feature-Ids will range from zero to
  //  five. The two-dimensional features of the tetrahedron are its four
  //  triangular faces. The two-dimensional feature ids will then range from
  //  zero to three. The following table summarizes the use on indices for
  //  boundary assignments.
  //
  //  \begin{center}
  //  \begin{tabular}{ | c || c | c | c | }
  //  \hline
  //  Dimension & CellType & FeatureId range & Cell Ids \\ \hline\hline
  //  0 & VertexCell & [0:3] & \{11,12,13,14\} \\   \hline
  //  1 & LineCell & [0:5] & \{5,6,7,8,9,10\} \\   \hline
  //  2 & TriangleCell & [0:3] & \{1,2,3,4\} \\ \hline
  //  \end{tabular}
  //  \end{center}
  //
  //  In the code example above, the values of featureId range from zero to
  //  three. The cell identifiers of the triangle cells in this example are the
  //  numbers \{1,2,3,4\}, while the cell identifiers of the vertex cells are
  //  the numbers \{11,12,13,14\}.

  //
  //  Let's now assign one-dimensional boundary features of the tetrahedron.
  //  Those are the line cells with identifiers  \{5,6,7,8,9,10\}. Note that the
  //  feature identifier is reinitialized to zero since the count is
  //  independent for each dimension.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  cellId    = 0;  // still the tetrahedron
  dimension = 1;  // one-dimensional features = edges
  featureId = 0;  // reinitialize the count

  mesh->SetBoundaryAssignment( dimension, cellId, featureId++,  5 );
  mesh->SetBoundaryAssignment( dimension, cellId, featureId++,  6 );
  mesh->SetBoundaryAssignment( dimension, cellId, featureId++,  7 );
  mesh->SetBoundaryAssignment( dimension, cellId, featureId++,  8 );
  mesh->SetBoundaryAssignment( dimension, cellId, featureId++,  9 );
  mesh->SetBoundaryAssignment( dimension, cellId, featureId++, 10 );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Finally we assign the two-dimensional boundary features of the
  //  tetrahedron. These are the four triangular cells with identifiers
  //  \{1,2,3,4\}. The featureId is reset to zero since feature-Ids are
  //  independent on each dimension.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  cellId    = 0;  // still the tetrahedron
  dimension = 2;  // two-dimensional features = triangles
  featureId = 0;  // reinitialize the count

  mesh->SetBoundaryAssignment( dimension, cellId, featureId++,  1 );
  mesh->SetBoundaryAssignment( dimension, cellId, featureId++,  2 );
  mesh->SetBoundaryAssignment( dimension, cellId, featureId++,  3 );
  mesh->SetBoundaryAssignment( dimension, cellId, featureId++,  4 );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  At this point we can query the tetrahedron cell for information about its
  //  boundary features. For example, the number of boundary features of each
  //  dimension can be obtained with the method
  //  \code{GetNumberOfBoundaryFeatures()}.
  //
  //  \index{itk::Mesh!CellFeatureCount}
  //  \index{itk::Mesh!GetNumberOfBoundaryFeatures()}
  //  \index{GetNumberOfBoundaryFeatures()!itk::Mesh}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  cellId = 0; // still the tetrahedron

  MeshType::CellFeatureCount n0;  // number of zero-dimensional features
  MeshType::CellFeatureCount n1;  // number of  one-dimensional features
  MeshType::CellFeatureCount n2;  // number of  two-dimensional features

  n0 = mesh->GetNumberOfCellBoundaryFeatures( 0, cellId );
  n1 = mesh->GetNumberOfCellBoundaryFeatures( 1, cellId );
  n2 = mesh->GetNumberOfCellBoundaryFeatures( 2, cellId );
  // Software Guide : EndCodeSnippet


  std::cout << "Number of boundary features of the cellId= " << cellId << std::endl;
  std::cout << "Dimension 0 = " << n0 << std::endl;
  std::cout << "Dimension 1 = " << n1 << std::endl;
  std::cout << "Dimension 2 = " << n2 << std::endl;


  //  Software Guide : BeginLatex
  //
  //  The boundary assignments can be recovered with the method
  //  \code{GetBoundaryAssigment()}. For example, the zero-dimensional features
  //  of the tetrahedron can be obtained with the following code.
  //
  // \index{itk::Mesh!GetBoundaryAssignment()}
  // \index{GetBoundaryAssignment()!itk::Mesh}
  //
  //  Software Guide : EndLatex

  std::cout << "Boundary features of dimension 0 " << std::endl;

  // Software Guide : BeginCodeSnippet
  dimension = 0;
  for(unsigned int b0=0; b0 < n0; b0++)
    {
    MeshType::CellIdentifier id;
    bool found = mesh->GetBoundaryAssignment( dimension, cellId, b0, &id );
    if( found ) std::cout << id << std::endl;
    }
  // Software Guide : EndCodeSnippet

  dimension = 1;
  std::cout << "Boundary features of dimension " << dimension << std::endl;
  for(unsigned int b1=0; b1 < n1; b1++)
    {
    MeshType::CellIdentifier id;
    bool found = mesh->GetBoundaryAssignment( dimension, cellId, b1, &id );
    if( found )
      {
      std::cout << id << std::endl;
      }
    }

  dimension = 2;
  std::cout << "Boundary features of dimension " << dimension << std::endl;
  for(unsigned int b2=0; b2 < n2; b2++)
    {
    MeshType::CellIdentifier id;
    bool found = mesh->GetBoundaryAssignment( dimension, cellId, b2, &id );
    if( found )
      {
      std::cout << id << std::endl;
      }
    }


  //  Software Guide : BeginLatex
  //
  //  The following code illustrates how to set the edge boundaries for one of
  //  the triangular faces.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  cellId     =  2;    // one of the triangles
  dimension  =  1;    // boundary edges
  featureId  =  0;    // start the count of features

  mesh->SetBoundaryAssignment( dimension, cellId, featureId++,  7 );
  mesh->SetBoundaryAssignment( dimension, cellId, featureId++,  9 );
  mesh->SetBoundaryAssignment( dimension, cellId, featureId++, 10 );
  // Software Guide : EndCodeSnippet

  std::cout << "In cell Id = " << cellId << std::endl;
  std::cout << "Boundary features of dimension " << dimension;
  n1 = mesh->GetNumberOfCellBoundaryFeatures( dimension, cellId);
  std::cout << " = " << n1 << std::endl;
  for(unsigned int b1=0; b1 < n1; b1++)
    {
    MeshType::CellIdentifier id;
    bool found = mesh->GetBoundaryAssignment( dimension, cellId, b1, &id );
    if( found )
      {
      std::cout << id << std::endl;
      }
    }

  return EXIT_SUCCESS;
}

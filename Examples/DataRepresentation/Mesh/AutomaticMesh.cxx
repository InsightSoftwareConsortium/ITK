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
//  The \doxygen{Mesh} class is extremely general and flexible, but
//  there is some cost to convenience.  If convenience is exactly what
//  you need, then it is possible to get it, in exchange for some of
//  that flexibility, by means of the
//  \doxygen{AutomaticTopologyMeshSource} class.  This class
//  automatically generates an explicit K-Complex, based on the cells
//  you add.  It explicitly includes all boundary information, so that
//  the resulting mesh can be easily traversed.  It merges all shared
//  edges, vertices, and faces, so no geometric feature appears more
//  than once.
//
//  This section shows how you can use the AutomaticTopologyMeshSource to
//  instantiate a mesh representing a K-Complex.  We will first generate the
//  same tetrahedron from Section~\ref{sec:MeshKComplex}, after which we will
//  add a hollow one to illustrate some additional features of the mesh
//  source.
//
//  \index{itk::Mesh!K-Complex}
//  \index{itk::AutomaticTopologyMeshSource}
//
//  Software Guide : EndLatex

//  Software Guide : BeginLatex
//
//  The header files of all the cell types involved should be loaded along with
//  the header file of the mesh class.
//
//  \index{itk::AutomaticTopologyMeshSource!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkTriangleCell.h"
#include "itkAutomaticTopologyMeshSource.h"
// Software Guide : EndCodeSnippet

int
main(int, char *[])
{
  //  Software Guide : BeginLatex
  //
  //  We then define the necessary types and instantiate the mesh
  //  source.  Two new types are \code{IdentifierType} and
  //  \code{IdentifierArrayType}.  Every cell in a mesh has an
  //  identifier, whose type is determined by the mesh traits.
  //  AutomaticTopologyMeshSource requires that the identifier
  //  type of all vertices and cells be \code{unsigned long}, which is
  //  already the default.  However, if you created a new mesh traits
  //  class to use string tags as identifiers, the resulting mesh
  //  would not be compatible with \doxygen{AutomaticTopologyMeshSource}.
  //  An \code{IdentifierArrayType} is simply an \doxygen{Array}
  //  of \code{IdentifierType} objects.
  //
  //  \index{itk::AutomaticTopologyMeshSource!IdentifierType}
  //  \index{itk::AutomaticTopologyMeshSource!IdentifierArrayType}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float                             PixelType;
  typedef itk::Mesh< PixelType, 3 >         MeshType;

  typedef MeshType::PointType               PointType;

  typedef itk::AutomaticTopologyMeshSource< MeshType >   MeshSourceType;
  typedef MeshSourceType::IdentifierArrayType            IdentifierArrayType;

  MeshSourceType::Pointer meshSource;

  meshSource = MeshSourceType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Now let us generate the tetrahedron.  The following line of code
  //  generates all the vertices, edges, and faces, along with the
  //  tetrahedral solid, and adds them to the mesh along with the
  //  connectivity information.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  meshSource->AddTetrahedron(
    meshSource->AddPoint( -1, -1, -1 ),
    meshSource->AddPoint(  1,  1, -1 ),
    meshSource->AddPoint(  1, -1,  1 ),
    meshSource->AddPoint( -1,  1,  1 )
  );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The function
  //  \code{AutomaticTopologyMeshSource::AddTetrahedron()}
  //  takes point identifiers as parameters; the identifiers must
  //  correspond to points that have already been added.
  //  \code{AutomaticTopologyMeshSource::AddPoint()} returns
  //  the appropriate identifier type for the point being added.  It
  //  first checks to see if the point is already in the mesh.  If so,
  //  it returns the ID of the point in the mesh, and if not, it
  //  generates a new unique ID, adds the point with that ID, and
  //  returns the ID.
  //
  //  \index{itk::AutomaticTopologyMeshSource!AddPoint()}
  //  \index{itk::AutomaticTopologyMeshSource!AddTetrahedron()}
  //
  //  Actually, \code{AddTetrahedron()} behaves in the same way.  If
  //  the tetrahedron has already been added, it leaves the mesh
  //  unchanged and returns the ID that the tetrahedron already has.
  //  If not, it adds the tetrahedron (and all its faces, edges, and
  //  vertices), and generates a new ID, which it returns.
  //
  //  It is also possible to add all the points first, and then add a
  //  number of cells using the point IDs directly.  This approach
  //  corresponds with the way the data is stored in many file formats
  //  for 3D polygonal models.
  //
  //  First we add the points (in this case the vertices of a larger
  //  tetrahedron).  This example also illustrates that
  //  \code{AddPoint()} can take a single \code{PointType} as a
  //  parameter if desired, rather than a sequence of floats.  Another
  //  possibility (not illustrated) is to pass in a C-style array.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointType p;
  IdentifierArrayType idArray( 4 );

  p[ 0 ] = -2;
  p[ 1 ] = -2;
  p[ 2 ] = -2;
  idArray[ 0 ] = meshSource->AddPoint( p );

  p[ 0 ] =  2;
  p[ 1 ] =  2;
  p[ 2 ] = -2;
  idArray[ 1 ] = meshSource->AddPoint( p );

  p[ 0 ] =  2;
  p[ 1 ] = -2;
  p[ 2 ] =  2;
  idArray[ 2 ] = meshSource->AddPoint( p );

  p[ 0 ] = -2;
  p[ 1 ] =  2;
  p[ 2 ] =  2;
  idArray[ 3 ] = meshSource->AddPoint( p );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Now we add the cells.  This time we are just going to create the
  //  boundary of a tetrahedron, so we must add each face separately.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  meshSource->AddTriangle( idArray[0], idArray[1], idArray[2] );
  meshSource->AddTriangle( idArray[1], idArray[2], idArray[3] );
  meshSource->AddTriangle( idArray[2], idArray[3], idArray[0] );
  meshSource->AddTriangle( idArray[3], idArray[0], idArray[1] );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Actually, we could have called, e.g., \code{AddTriangle( 4, 5, 6
  //  )}, since IDs are assigned sequentially starting at zero, and
  //  \code{idArray[0]} contains the ID for the fifth point added.
  //  But you should only do this if you are confident that you know
  //  what the IDs are.  If you add the same point twice and don't
  //  realize it, your count will differ from that of the mesh source.
  //
  //  You may be wondering what happens if you call, say,
  //  \code{AddEdge(0, 1)} followed by \code{AddEdge(1, 0)}.  The
  //  answer is that they do count as the same edge, and so only one
  //  edge is added.  The order of the vertices determines an
  //  orientation, and the first orientation specified is the one that
  //  is kept.
  //
  //  Once you have built the mesh you want, you can access it by
  //  calling \code{GetOutput()}.  Here we send it to \code{cout},
  //  which prints some summary data for the mesh.
  //
  //  Software Guide : EndLatex

  MeshType::Pointer mesh = meshSource->GetOutput();
  std::cout << mesh << std::endl;

  //  Software Guide : BeginLatex
  //
  //  In contrast to the case with typical filters, \code{GetOutput()} does
  //  not trigger an update process.  The mesh is always maintained in a
  //  valid state as cells are added, and can be accessed at any time.  It
  //  would, however, be a mistake to modify the mesh by some other means
  //  until AutomaticTopologyMeshSource is done with it, since the mesh
  //  source would then have an inaccurate record of which points and cells
  //  are currently in the mesh.
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;

}

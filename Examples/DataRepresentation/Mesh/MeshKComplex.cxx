/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MeshKComplex.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//
//  The Mesh class supports the representation of formal topologies. In
//  particular the concept of \emph{K-Complex} can be correctly reprented in
//  the Mesh. An informal definition of K-complex may be as follows: A
//  K-Complex is a topological structure in which for every cell of dimension
//  $N$, its boundary faces which are cells of dimension $N-1$ also belong to
//  the structure.
//
//  This section illustrates how to instantiate a K-Complex structure using the
//  \code{itk::Mesh}. The example structure is composed of one tetrahedron, its
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
#include "itkVertexCell.h"
#include "itkLineCell.h"
#include "itkTriangleCell.h"
#include "itkTetrahedronCell.h"
// Software Guide : EndCodeSnippet


int main()
{


  //  Software Guide : BeginLatex
  //  
  //  Then the PixelType is defined and the mesh type is instantiated with it.
  //  Note that the dimension of the space is 3 in this case.
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
  //  inserted.  Note that there is an important distintion between the points
  //  in the mesh and the VertexCell concept. A VertexCell is a cell of
  //  dimension zero. Its main difference with a point is that the cell can be
  //  aware of neighborhood relationships with other cells. Points are not
  //  aware of the existence of cells. In fact, from the pure topological point
  //  of view, the coordinates of points in the mesh are completely irrelevant.
  //  They may as well be absent from the mesh structure altogether.
  //  VertexCells on the other hand are necessary to represent the full set of
  //  neighborhood relationships on the K-Complex.
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

  point0[0] = -1; point0[1] = -1; point0[1] = -1; 
  point1[0] =  1; point1[1] =  1; point1[1] = -1; 
  point2[0] =  1; point2[1] = -1; point2[1] =  1; 
  point3[0] = -1; point3[1] =  1; point3[1] =  1; 

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
  cellpointer->SetPointId( 0, 1 );
  cellpointer->SetPointId( 0, 2 );
  cellpointer->SetPointId( 0, 3 );
  mesh->SetCell( 0, cellpointer );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  Four trianglular faces are created and associated with the mesh now.
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
  //  The second triangle connects points { 0, 2, 3 }
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
  //  The third triangle connects points { 0, 3, 1 }
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
  //  The fourth triangle connects points { 3, 2, 1 }
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new TriangleType );
  cellpointer->SetPointId( 0, 3 );
  cellpointer->SetPointId( 1, 2 );
  cellpointer->SetPointId( 2, 1 );
  mesh->SetCell( 3, cellpointer );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  Note how the \code{CellAutoPointer} is reused every time. The autopointer
  //  losses ownership of the cell when it is passed as argument of the
  //  \code{SetCell()} method. The AutoPointer is attached to a new cell by
  //  using the \code{TakeOwnership()} method. 
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
  mesh->SetCell( 4, cellpointer );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 1 );
  cellpointer->SetPointId( 1, 2 );
  mesh->SetCell( 5, cellpointer );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 2 );
  cellpointer->SetPointId( 1, 0 );
  mesh->SetCell( 6, cellpointer );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 1 );
  cellpointer->SetPointId( 1, 3 );
  mesh->SetCell( 7, cellpointer );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 3 );
  cellpointer->SetPointId( 1, 2 );
  mesh->SetCell( 8, cellpointer );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new LineType );
  cellpointer->SetPointId( 0, 3 );
  cellpointer->SetPointId( 1, 0 );
  mesh->SetCell( 9, cellpointer );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  Finally the zero dimensional cells represented by the VertexCell are
  //  created and inserted in the mesh.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new VertexType );
  cellpointer->SetPointId( 0, 0 );
  mesh->SetCell( 10, cellpointer );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new VertexType );
  cellpointer->SetPointId( 0, 1 );
  mesh->SetCell( 11, cellpointer );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new VertexType );
  cellpointer->SetPointId( 0, 2 );
  mesh->SetCell( 12, cellpointer );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  cellpointer.TakeOwnership( new VertexType );
  cellpointer->SetPointId( 0, 3 );
  mesh->SetCell( 13, cellpointer );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  At this point the Mesh contains four points and fourteen cells.
  //
  //  Software Guide : EndLatex 

  std::cout << "# Points= " << mesh->GetNumberOfPoints() << std::endl;
  std::cout << "# Cell  = " << mesh->GetNumberOfCells() << std::endl;




  return 0;

}


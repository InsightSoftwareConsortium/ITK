/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MeshCellVisitor.cxx
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
//  Cells are stored in the \code{itk::Mesh} as pointers to a generic cell.
//  This implies that only the virtual methods defined on this base cell class
//  can be invoked. In order to use methods that are specific of each cell type
//  it is necessary to down-cast the pointer to the actual type of the cell.
//  This can be done safely by taking advantage of the C++ RTTI mechanism
//  \footnote{RTTI stands for Run Time Type Information}.
//
//  Let's start by assuming a mesh defined with one tetrahedron and all its
//  boundary faces. That is, four triangles, six edges and four vertices.
//
//  \index{RTTI}
//
//  Software Guide : EndLatex 


#include "itkMesh.h"
#include "itkVertexCell.h"
#include "itkLineCell.h"
#include "itkTriangleCell.h"
#include "itkTetrahedronCell.h"




// Software Guide : BeginCodeSnippet
#include "itkCellInterfaceVisitor.h"
// Software Guide : EndCodeSnippet


  typedef float                             PixelType;
  typedef itk::Mesh< PixelType, 3 >         MeshType;

  typedef MeshType::CellType                CellType;

  typedef itk::VertexCell< CellType >       VertexType;
  typedef itk::LineCell< CellType >         LineType;
  typedef itk::TriangleCell< CellType >     TriangleType;
  typedef itk::TetrahedronCell< CellType >  TetrahedronType;

  // Software Guide : BeginCodeSnippet
  class CustomTriangleVisitor
    {
    public:
      typedef itk::TriangleCell<CellType>      TriangleType;

    public:
    void Visit(unsigned long cellId, TriangleType * t )
      {
      CellType::PointIdIterator pit = t->PointIdsBegin();
      CellType::PointIdIterator end = t->PointIdsEnd();
      std::cout << t->GetNumberOfPoints() << " points = ";
      while( pit != end )
        {
        std::cout <<  *pit << "  ";
        ++pit;
        }
      std::cout << std::endl;
      }
    };
  // Software Guide : EndCodeSnippet

int main()
{


  MeshType::Pointer  mesh = MeshType::New();



  //
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




  //
  // Creating and associating the Tetrahedron
  //
  CellType::CellAutoPointer cellpointer;

  cellpointer.TakeOwnership( new TetrahedronType );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 1 );
  cellpointer->SetPointId( 2, 2 );
  cellpointer->SetPointId( 3, 3 );
  mesh->SetCell( 0, cellpointer );



  //
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




  //
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




  //
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
  //  Two mechanisms can be used to safely down-cast the pointer to the actual
  //  cell type. The first is a built-in mechanism in ITK that allows the user
  //  to query the type of the Cell. Codes for the cell types have been defined
  //  with an \code{enum} type on the \code{itkCellInterface.h} header file.
  //  These codes are : VERTEX\_CELL, LINE\_CELL, TRIANGLE\_CELL,
  //  QUADRILATERAL\_CELL, POLYGON\_CELL, TETRAHEDRON\_CELL, HEXAHEDRON\_CELL,
  //  QUADRATIC\_EDGE\_CELL, QUADRATIC\_TRIANGLE\_CELL. The Cell type is returned
  //  by the method \code{GetType()}. It is then possible to test the type of
  //  the cell before down-casting its pointer to the actual type. For example,
  //  the following code visits all the cells in the mesh and test which ones
  //  are actually of type LINE\_CELL. Only those cells are down-casted to
  //  LineType cells and a method specific for the LineType are invoked.
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
      std::cout << "dimension = " << line->GetDimension()      << std::endl;
      std::cout << "# points  = " << line->GetNumberOfPoints() << std::endl;
      }
    ++cellIterator;
    }
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  A more convinient approach for visiting the cells in the mesh is provided
  //  by the implementation of the \emph{Visitor Pattern} discussed in
  //  \cite{Gamma1995}. The visitor pattern is designed to facilitate to walk
  //  through an heterogeneous list of object sharing a common base class. A
  //  visitor class is defined and passed to the Mesh.The mesh will iterate
  //  through its cells and attempt to pass every cell to the visitor. The
  //  visitor class is capable of recognizing the cells of its interest and
  //  operation only on them.
  //
  // \index{itk::Mesh!CellInterfaceVisitorImplementation}
  // \index{itk::Mesh!CellInterfaceVisitor}
  // \index{itk::Mesh!CellVisitor}
  // \index{CellVisitor}
  //
  //  The following code illustrates how to define a cell visitor. In this
  //  particular example we create a class \code{CustomTriangleVisitor} which
  //  will be invoked each time a triangle cell is found while the mesh
  //  iterates over the cells.
  //
  //  Software Guide : EndLatex 


  typedef CustomTriangleVisitor CustomVisitorType;



  // Software Guide : BeginCodeSnippet
  typedef itk::CellInterfaceVisitorImplementation<
                              PixelType, 
                              MeshType::CellTraits, 
                              TriangleType,
                              CustomVisitorType
                                       > TriangleVisitorInterfaceType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  TriangleVisitorInterfaceType::Pointer  triangleVisitor =
                                   TriangleVisitorInterfaceType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  A multivisitor class must be instantiated 
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
  //  The iteration over the cells is triggered by calling the method
  //  \code{Accept()} on the \code{itk::Mesh}.
  // 
  //  \index{itk::Mesh!Accept()}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  mesh->Accept( multiVisitor );
  // Software Guide : EndCodeSnippet

  

  return 0;

}


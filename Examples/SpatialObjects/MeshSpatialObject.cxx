/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MeshSpatialObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


#include <itkDefaultDynamicMeshTraits.h>
#include <itkMesh.h>
#include <itkTetrahedronCell.h>

// Software Guide : BeginLatex
//
// \index{itk::MeshSpatialObject}
//
// A \doxygen{MeshSpatialObject} contains a \doxygen{Mesh} but adds the
// notion of spatial transformations and parent-child hierarchy. 
// This example shows how to create a MeshSpatiaObject, create a binary image from it
// and how to write the mesh on disk.
// Let's begin by including the appropriate header file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include <itkMeshSpatialObject.h>
#include <itkSpatialObjectReader.h>
#include <itkSpatialObjectWriter.h>
#include <itkSpatialObjectToImageFilter.h>
// Software Guide : EndCodeSnippet

int main(int, char * [] ) 
{
  // Software Guide : BeginLatex
  //
  // The MeshSpatialObject is a "Wrapper" to the \doxygen{Mesh} so we first 
  // create an actual mesh. 
  // First we define some standard type definitions:
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet  
  typedef itk::DefaultDynamicMeshTraits< float , 3, 3 > MeshTrait;
  typedef itk::Mesh<float,3,MeshTrait>                  MeshType;
  typedef MeshType::CellTraits                          CellTraits;
  typedef itk::CellInterface< float, CellTraits >       CellInterfaceType;
  typedef itk::TetrahedronCell<CellInterfaceType>       TetraCellType;
  typedef MeshType::PointType                           PointType;
  typedef MeshType::CellType                            CellType;
  typedef CellType::CellAutoPointer                     CellAutoPointer;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  MeshType::Pointer myMesh = MeshType::New();

  MeshType::CoordRepType testPointCoords[4][3]
    = { {0,0,0}, {9,0,0}, {9,9,0}, {0,0,9} };

  unsigned long tetraPoints[4] = {0,1,2,4};
  int i;
  for(i=0; i < 4 ; ++i)
    {
    myMesh->SetPoint(i, PointType(testPointCoords[i]));
    }

  myMesh->SetCellsAllocationMethod( MeshType::CellsAllocatedDynamicallyCellByCell );
  CellAutoPointer testCell1; 
  testCell1.TakeOwnership(  new TetraCellType ); 
  testCell1->SetPointIds(tetraPoints);
  // Software Guide : EndCodeSnippet
  
  // Software Guide : BeginCodeSnippet
  myMesh->SetCell(0, testCell1 );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We then create a MeshSpatialObject which is templated over the type of mesh...
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet  
  typedef itk::MeshSpatialObject<MeshType>     MeshSpatialObjectType;
  MeshSpatialObjectType::Pointer myMeshSpatialObject = MeshSpatialObjectType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // ... and pass the Mesh pointer to the MeshSpatialObject
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet  
  myMeshSpatialObject->SetMesh(myMesh);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The actual pointer to the passed mesh can be retreived using the
  // GetMesh() function.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet  
  myMeshSpatialObject->GetMesh();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // Like any other SpatialObjects. The GetBoundingBox(), ValueAt(),
  // IsInside() functions can be used to retrieve important information.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet  
  std::cout << "Mesh bounds : " << myMeshSpatialObject->GetBoundingBox()->GetBounds() << std::endl;
  MeshSpatialObjectType::PointType myPhysicalPoint;
  myPhysicalPoint.Fill(1);
  std::cout << "Is my physical point inside? : " << myMeshSpatialObject->IsInside(myPhysicalPoint) << std::endl;
  // Software Guide : EndCodeSnippet  

  // Software Guide : BeginLatex
  // Now that we have defined the MeshSpatialObject, you can save the actual mesh onto disk
  // using the \doxygen{SpatialObjectWriter}. We need to specify the type of Mesh we are
  // writing.
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet 
  typedef itk::SpatialObjectWriter<3,float,MeshTrait> WriterType;
  WriterType::Pointer writer = WriterType::New();
  // Software Guide : EndCodeSnippet 

  // Software Guide : BeginLatex
  // Then we set the mesh spatial object and the name of the file and call the
  // the Update() function.
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet 
  writer->SetInput(myMeshSpatialObject);
  writer->SetFileName("myMesh.meta");
  writer->Update();
  // Software Guide : EndCodeSnippet 

  // Software Guide : BeginLatex
  // Reading the saved mesh is done using the \doxygen{SpatialObjectReader}
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet 
  typedef itk::SpatialObjectReader<3,float,MeshTrait> ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  // Software Guide : EndCodeSnippet 

  // Software Guide : BeginLatex
  // We set the name of the file we want to read and call update
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet 
  reader->SetFileName("myMesh.meta");
  reader->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We show now how to create a binary image of a MeshSpatialObject
  // using the \doxygen{SpatialObjectToImageFilter}. The resulting image
  // will have ones inside and zeros outside the mesh.
  // First we define and instantiate the SpatialObjectToImageFilter.
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::Image<unsigned char,3> ImageType;
  typedef itk::GroupSpatialObject<3> GroupType;
  typedef itk::SpatialObjectToImageFilter< GroupType, ImageType >   
    SpatialObjectToImageFilterType;
  SpatialObjectToImageFilterType::Pointer imageFilter = 
    SpatialObjectToImageFilterType::New();
  // Software Guide : EndCodeSnippet 

  // Software Guide : BeginLatex
  // Then we pass the output of the reader, i.e the MeshSpatialObject to the
  // filter.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  imageFilter->SetInput(  reader->GetGroup()  );
  // Software Guide : EndCodeSnippet 

  //  Software Guide : BeginLatex 
  //  Finally we trigger the execution of the filter by calling the
  //  \code{Update()} method.
  //  \index{itk::SpatialObjectToImageFilter!Update()}
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  imageFilter->Update();
  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex 
  //  Then we can get the resulting binary image using the GetOutput() function.
  //  Software Guide : EndLatex 
  //  Software Guide : BeginCodeSnippet
  ImageType::Pointer myBinaryMeshImage = imageFilter->GetOutput();
  //  Software Guide : EndCodeSnippet
  
  return EXIT_SUCCESS;

}

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ReadWriteSpatialObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// \index{itk::ReadWriteSpatialObject}
// Reading and Writing SpatialObjects is a fairly simple task. MetaIO is used to 
// convert current SpatialObjects into a file format.
//
// We first include the appropriate header files
//
// Software Guide : EndLatex 

#include "itkEllipseSpatialObject.h"

// Software Guide : BeginCodeSnippet
#include "itkSpatialObjectWriter.h"
#include "itkSpatialObjectReader.h"
// Software Guide : EndCodeSnippet

int main( int argc, char *argv[] )
{
 
// Software Guide : BeginLatex
//
// Next, we create a SpatialObjectWriter which is templated over the dimension 
// of the object(s) we want to write.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet 
  typedef itk::SpatialObjectWriter<3> WriterType;
  WriterType::Pointer writer = WriterType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We create an \doxygen{EllipseSpatialObject}.
//
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::EllipseSpatialObject<3> EllipseType;
  EllipseType::Pointer ellipse = EllipseType::New();
  ellipse->SetRadius(3);
// Software Guide : EndCodeSnippet
// Software Guide : BeginLatex
//
// Finally, we set to the writer the object to write using the SetInput() method and we set the name of the file  with 
// SetFileName() and, finally, qw call the Update() method.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  writer->SetInput(ellipse);
  writer->SetFileName("ellipse.meta");
  writer->Update();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// No we are ready to open the freshly created object.
//
// We first create a SpatialObjectReader which is also templated over the dimension of the object in the file
// This means that the file should contain only objects with the same dimension.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet 
  typedef itk::SpatialObjectReader<3> ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Then, we set the name of the file to read using SetFileName() and we call the Update() method to read the file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  reader->SetFileName("ellipse.meta");
  reader->Update();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// To get the objects in the file you can call the GetScene() function or the GetGroup() function.
// GetScene() returns an pointer to an \doxygen{Scene}.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  ReaderType::SceneType * scene = reader->GetScene();
  std::cout << "Number of objects in the scene: ";
  std::cout << scene->GetNumberOfObjects() << std::endl;
  ReaderType::GroupType * group = reader->GetGroup();
// Software Guide : EndCodeSnippet

  return 0;
}

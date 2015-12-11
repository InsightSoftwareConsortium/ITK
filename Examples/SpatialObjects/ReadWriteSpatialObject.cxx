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

// Software Guide : BeginLatex
//
// \index{itk::ReadWriteSpatialObject}
//
// Reading and writing SpatialObjects is a fairly simple task. The classes
// \doxygen{SpatialObjectReader} and \doxygen{SpatialObjectWriter} are used
// to read and write these objects, respectively. (Note these classes
// make use of the MetaIO auxiliary I/O routines and therefore have a
// \code{.meta} file suffix.)
//
// We begin this example by including the appropriate header files.
//
// Software Guide : EndLatex

// The following include to make Visual Studio 6 happy
#include "itkImageHelper.h"

// Software Guide : BeginCodeSnippet
#include "itkSpatialObjectReader.h"
#include "itkSpatialObjectWriter.h"
#include "itkEllipseSpatialObject.h"

// Software Guide : EndCodeSnippet

int main( int , char *[] )
{

// Software Guide : BeginLatex
//
// Next, we create a SpatialObjectWriter that is templated over the dimension
// of the object(s) we want to write.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::SpatialObjectWriter<3> WriterType;
  WriterType::Pointer writer = WriterType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// For this example, we create an \doxygen{EllipseSpatialObject}.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::EllipseSpatialObject<3> EllipseType;
  EllipseType::Pointer ellipse = EllipseType::New();
  ellipse->SetRadius(3);
// Software Guide : EndCodeSnippet
// Software Guide : BeginLatex
//
// Finally, we set to the writer the object to write using the
// \code{SetInput()} method and we set the name of the file with
// \code{SetFileName()} and call the \code{Update()} method to actually write
// the information.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  writer->SetInput(ellipse);
  writer->SetFileName("ellipse.meta");
  writer->Update();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Now we are ready to open the freshly created object. We first create a
// SpatialObjectReader which is also templated over the dimension of the
// object in the file. This means that the file should contain only objects
// with the same dimension.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::SpatialObjectReader<3> ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next we set the name of the file to read using \code{SetFileName()} and we
// call the \code{Update()} method to read the file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  reader->SetFileName("ellipse.meta");
  reader->Update();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// To get the objects in the file you can call the \code{GetScene()} method
// or the \code{GetGroup()} method.  \code{GetScene()} returns an pointer to
// a \doxygen{SceneSpatialObject}.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ReaderType::SceneType * scene = reader->GetScene();
  std::cout << "Number of objects in the scene: ";
  std::cout << scene->GetNumberOfObjects() << std::endl;
  ReaderType::GroupType * group = reader->GetGroup();
  std::cout << "Number of objects in the group: ";
  std::cout << group->GetNumberOfChildren() << std::endl;
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;

}

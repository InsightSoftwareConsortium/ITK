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
// \index{itk::SceneSpatialObject}
// This example describes how to use the \doxygen{SceneSpatialObject}.
// A SceneSpatialObject contains a collection of SpatialObjects. This
// example begins by including the appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSceneSpatialObject.h"
// Software Guide : EndCodeSnippet
#include "itkEllipseSpatialObject.h"

int main( int , char *[] )
{
// Software Guide : BeginLatex
//
// An SceneSpatialObject is templated over the dimension of the
// space which requires all the objects referenced by the SceneSpatialObject to
// have the same dimension.
//
// First we define some type definitions and we create the SceneSpatialObject.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::SceneSpatialObject<3> SceneSpatialObjectType;
  SceneSpatialObjectType::Pointer scene = SceneSpatialObjectType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Then we create two \doxygen{EllipseSpatialObject}s.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::EllipseSpatialObject<3> EllipseType;
  EllipseType::Pointer ellipse1 = EllipseType::New();
  ellipse1->SetRadius(1);
  ellipse1->SetId(1);
  EllipseType::Pointer ellipse2 = EllipseType::New();
  ellipse2->SetId(2);
  ellipse2->SetRadius(2);
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Then we add the two ellipses into the SceneSpatialObject.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  scene->AddSpatialObject(ellipse1);
  scene->AddSpatialObject(ellipse2);
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// We can query the number of object in the SceneSpatialObject with the
// \code{GetNumberOfObjects()} function. This function takes two optional
// arguments: the depth at which we should count the number of objects
// (default is set to infinity) and the name of the object to count (default
// is set to ITK\_NULLPTR).  This allows the user to count, for example, only
// ellipses.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  std::cout << "Number of objects in the SceneSpatialObject = ";
  std::cout << scene->GetNumberOfObjects() << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The \code{GetObjectById()} returns the first object in the
// SceneSpatialObject that has the specified identification number.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  std::cout << "Object in the SceneSpatialObject with an ID == 2: "
            << std::endl;
  scene->GetObjectById(2)->Print(std::cout);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Objects can also be removed from the SceneSpatialObject using the
// \code{RemoveSpatialObject()} function.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  scene->RemoveSpatialObject(ellipse1);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The list of current objects in the SceneSpatialObject can be retrieved
// using the \code{GetObjects()} method.  Like the
// \code{GetNumberOfObjects()} method, \code{GetObjects()} can take two
// arguments: a search depth and a matching name.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  SceneSpatialObjectType::ObjectListType * myObjectList =  scene->GetObjects();
  std::cout << "Number of objects in the SceneSpatialObject = ";
  std::cout << myObjectList->size() << std::endl;
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// In some cases, it is useful to define the hierarchy by using
// \code{ParentId()} and the current identification number. This results in
// having a flat list of SpatialObjects in the SceneSpatialObject. Therefore,
// the SceneSpatialObject provides the \code{FixHierarchy()} method which
// reorganizes the Parent-Child hierarchy based on identification numbers.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  scene->FixHierarchy();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The scene can also be cleared by using the \code{Clear()} function.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  scene->Clear();
// Software Guide : EndCodeSnippet


  return EXIT_SUCCESS;
}

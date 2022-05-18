/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
// \index{itk::GroupSpatialObject - Continued}
//
// This example describes how to use the \doxygen{GroupSpatialObject} as a
// replacement to ITKv4's \code{SceneSpatialObject}. This
// example begins by including the appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkGroupSpatialObject.h"
// Software Guide : EndCodeSnippet
#include "itkEllipseSpatialObject.h"

int
main(int, char *[])
{
  // Software Guide : BeginLatex
  //
  // A \code{GroupSpatialObject} is templated over the dimension of the
  // space which requires all the objects referenced by the
  // \code{GroupSpatialObject} to have the same dimension.
  //
  // First we define some type definitions and we create the
  // \code{GroupSpatialObject}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using GroupSpatialObjectType = itk::GroupSpatialObject<3>;
  auto scene = GroupSpatialObjectType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Then we create two \doxygen{EllipseSpatialObject}s.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using EllipseType = itk::EllipseSpatialObject<3>;
  auto ellipse1 = EllipseType::New();
  ellipse1->SetRadiusInObjectSpace(1);
  ellipse1->SetId(1);
  auto ellipse2 = EllipseType::New();
  ellipse2->SetId(2);
  ellipse2->SetRadiusInObjectSpace(2);
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Then we add the two ellipses into the \code{GroupSpatialObject}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  scene->AddChild(ellipse1);
  scene->AddChild(ellipse2);
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We can query the number of object in the \code{GroupSpatialObject} with
  // the \code{GetNumberOfObjects()} function. This function takes two
  // optional arguments: the depth at which we should count the number of
  // objects (default is set to infinity) and the name of the object to count
  // (default is set to ITK\_NULLPTR).  This allows the user to count, for
  // example, only ellipses.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Number of objects in the GroupSpatialObject = ";
  std::cout << scene->GetNumberOfChildren() << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \code{GetObjectById()} returns the first object in the
  // \code{GroupSpatialObject} that has the specified identification number.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Object in the GroupSpatialObject with an ID == 2: "
            << std::endl;
  scene->GetObjectById(2)->Print(std::cout);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Objects can also be removed from the \code{GroupSpatialObject} using the
  // \code{RemoveChild()} function.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  scene->RemoveChild(ellipse1);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The list of current objects in the \code{GroupSpatialObject} can be
  // retrieved using the \code{GetChildren()} method.  Like the
  // \code{GetNumberOfChildren()} method, \code{GetChildren()} can take two
  // arguments: a search depth and a matching name.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  GroupSpatialObjectType::ObjectListType * myObjectList =
    scene->GetChildren();
  std::cout << "Number of children in the GroupSpatialObject = ";
  std::cout << myObjectList->size() << std::endl;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // In some cases, it is useful to define the hierarchy by using
  // \code{ParentId()} and the current identification number. This results in
  // having a flat list of SpatialObjects in the \code{GroupSpatialObject}.
  // Therefore, the \code{GroupSpatialObject} provides the
  // \code{FixParentChildHierarchyUsingParentIds()} method which
  // reorganizes the Parent-Child hierarchy based on identification numbers.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  scene->FixParentChildHierarchyUsingParentIds();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The scene can also be cleared by using the \code{RemoveAllChildren()}
  // function.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  scene->RemoveAllChildren();
  // Software Guide : EndCodeSnippet


  return EXIT_SUCCESS;
}

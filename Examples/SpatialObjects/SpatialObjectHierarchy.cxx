/*=========================================================================
 *
 *  Copyright NumFOCUS
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
// \index{itk::SpatialObjectHierarchy}
// This example describes how \doxygen{SpatialObject} can form a hierarchy.
// This first example also shows how to create and manipulate
// spatial objects.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSpatialObject.h"
// Software Guide : EndCodeSnippet

int
main(int, char *[])
{

  // Software Guide : BeginLatex
  //
  // First, we create two spatial objects and give them the names \code{First
  // Object} and \code{Second Object}, respectively.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using SpatialObjectType = itk::SpatialObject<3>;

  SpatialObjectType::Pointer object1 = SpatialObjectType::New();
  object1->GetProperty().SetName("First Object");

  SpatialObjectType::Pointer object2 = SpatialObjectType::New();
  object2->GetProperty().SetName("Second Object");
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We then add the second object to the first one by using the
  // \code{AddChild()} method.  As a result \code{object2} becomes a
  // child of \code{object1}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  object1->AddChild(object2);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Whenever the parameters of an object, including its parent-child
  // relationships are changed, we must then call the \code{Update()}
  // method so that the object-to-parent
  // transforms and bounding box internally managed by each object are
  // maintained.   Calling \code{Update()} on an object automatically causes
  // the \code{Update()} function of each child to be called.
  //
  // Software Guide : EndLatex
  //
  // Software Guide : BeginCodeSnippet
  object1->Update();
  // Software Guide : EndCodeSnippet
  //
  // Then, we can query if an object has a parent by using the HasParent()
  // method. If it has one, the \code{GetParent()} method returns a constant
  // pointer to the parent.  In our case, if we ask the parent's name of the
  // object2 we should obtain: \code{First Object}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  if (object2->HasParent())
  {
    std::cout << "Name of the parent of the object2: ";
    std::cout << object2->GetParent()->GetProperty().GetName() << std::endl;
  }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // To access the list of children of the object, the \code{GetChildren()}
  // method returns a pointer to the (STL) list of children.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SpatialObjectType::ChildrenListType * childrenList = object1->GetChildren();
  std::cout << "object1 has " << childrenList->size() << " child"
            << std::endl;

  SpatialObjectType::ChildrenListType::const_iterator it =
    childrenList->begin();
  while (it != childrenList->end())
  {
    std::cout << "Name of the child of the object 1: ";
    std::cout << (*it)->GetProperty().GetName() << std::endl;
    ++it;
  }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Do NOT forget to delete the list of children since the
  // \code{GetChildren()} function creates an internal list.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  delete childrenList;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // An object can also be removed by using the \code{RemoveChild()}
  // method, and then calling \code{Update()} on the now-orphaned child.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  object1->RemoveChild(object2);
  object2->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Then, we can query the number of children an object has with the
  // \code{GetNumberOfChildren()} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Number of children for object1: ";
  std::cout << object1->GetNumberOfChildren() << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \code{Clear()} method erases all the information regarding the object
  // as well as the data. This method is usually overloaded by
  // derived classes.  Note that the Parent-Child relationships of the object
  // are NOT reset when \code{Clear()} is called; however, the
  // object-to-parent transform is reset to Identity.  As a result,
  // \code{Update()} should be called before the object is re-used, to
  // re-compute convenience member variables and values.  To remove the
  // children of a node, use the \code{RemoveAllChildren()} function.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  object1->Clear();
  object1->RemoveAllChildren();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The output of this first example looks like the following:
  // \small
  // \begin{verbatim}
  //   Name of the parent of the object2: First Object
  //   object1 has 1 child
  //   Name of the child of the object 1: Second Object
  //   Number of children for object1: 0
  // \end{verbatim}
  // \normalsize
  // Software Guide : EndLatex

  return EXIT_SUCCESS;
}

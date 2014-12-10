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
// \index{itk::SpatialObjectTreeContainer}
// This example describes how to use the \doxygen{SpatialObjectTreeContainer}
// to form a hierarchy of SpatialObjects.
// First we include the appropriate header file.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSpatialObjectTreeContainer.h"
// Software Guide : EndCodeSnippet

#include "itkGroupSpatialObject.h"
#include "itkLevelOrderTreeIterator.h"

int main( int , char *[] )
{
  // Software Guide : BeginLatex
  //
  // Next we define the type of node and the type of tree we plan to use.
  // Both are templated over the dimensionality of the space.
  // Let's create a 2-dimensional tree.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GroupSpatialObject< 2 >         NodeType;
  typedef itk::SpatialObjectTreeContainer< 2 > TreeType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Then, we can create three nodes and set their corresponding identification
  // numbers (using \code{SetId}).
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  NodeType::Pointer object0 = NodeType::New();
  object0->SetId(0);
  NodeType::Pointer object1 = NodeType::New();
  object1->SetId(1);
  NodeType::Pointer object2 = NodeType::New();
  object2->SetId(2);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The hierarchy is formed using the \code{AddSpatialObject()} function.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  object0->AddSpatialObject(object1);
  object1->AddSpatialObject(object2);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // After instantiation of the tree we set its root
  // using the \code{SetRoot()} function.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TreeType::Pointer tree = TreeType::New();
  tree->SetRoot(object0.GetPointer());
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The tree iterators described in a previous section of this guide can be used to parse the
  // hierarchy. For example, via an \doxygen{LevelOrderTreeIterator} templated over the type of tree,
  // we can parse the hierarchy of SpatialObjects. We set the maximum level to 10
  // which is enough in this case since our hierarchy is only 2 deep.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  itk::LevelOrderTreeIterator<TreeType> levelIt(tree,10);
  levelIt.GoToBegin();
  while(!levelIt.IsAtEnd())
    {
    std::cout << levelIt.Get()->GetId() << " ("<< levelIt.GetLevel()
      << ")" << std::endl;
    ++levelIt;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Tree iterators can also be used to add spatial objects to the hierarchy. Here we show
  // how to use the \doxygen{PreOrderTreeIterator} to add a fourth object to the tree.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  NodeType::Pointer object4 = NodeType::New();
  itk::PreOrderTreeIterator<TreeType> preIt( tree );
  preIt.Add(object4.GetPointer());
  // Software Guide : EndCodeSnippet


  return EXIT_SUCCESS;
}

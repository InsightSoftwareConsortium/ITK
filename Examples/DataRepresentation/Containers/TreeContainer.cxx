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
// \index{itk::TreeContainer}
//
// This example demonstrates use of the \doxygen{TreeContainer} class and
// associated \code{TreeIterator}s.
// \code{TreeContainer} implements the notion of a tree, which is a branching
// data structure composed of nodes and edges, where the edges indicate
// a parent/child relationship between nodes.  Each node may have exactly one
// parent, except for the root node, which has none.  A tree must have
// exactly one root node, and a node may not be its own parent.  To round out
// the vocabulary used to discuss this data structure, two nodes
// sharing the same parent node are called ``siblings,'' a childless node
// is termed a ``leaf,'' and a ``forest'' is a collection of disjoint trees.
// Note that in the present implementation, it is the user's responsibility
// to enforce these relationships, as no checking is done to ensure a
// cycle-free tree.  \code{TreeContainer} is templated over the type of node,
// affording the user great flexibility in using the structure for their
// particular problem.
//
// Let's begin by including the appropriate header files.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkTreeContainer.h"
#include "itkChildTreeIterator.h"
#include "itkLeafTreeIterator.h"
#include "itkLevelOrderTreeIterator.h"
#include "itkInOrderTreeIterator.h"
#include "itkPostOrderTreeIterator.h"
#include "itkRootTreeIterator.h"
#include "itkTreeIteratorClone.h"
// Software Guide : EndCodeSnippet

int main(int, char* [])
{

  // Software Guide : BeginLatex
  //
  // We first instantiate a tree with \code{int} node type.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  typedef int                          NodeType;
  typedef itk::TreeContainer<NodeType> TreeType;
  TreeType::Pointer tree = TreeType::New();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Next we set the value of the root node using \code{SetRoot()}.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  tree->SetRoot(0);
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Nodes may be added to the tree using the \code{Add()} method,
  // where the first argument is the value of the new node, and the second
  // argument is the value of the parent node.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  tree->Add(1,0);
  tree->Add(2,0);
  tree->Add(3,0);
  tree->Add(4,2);
  tree->Add(5,2);
  tree->Add(6,5);
  tree->Add(7,1);
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // If two nodes have the same value, it is ambiguous which node is intended
  // to be the parent of the new node; in this case, the first node with that
  // value is selected.  As will be demonstrated shortly, this ambiguity can be avoided
  // by constructing the tree with \code{TreeIterator}s.
  //
  // Let's begin by defining a \doxygen{ChildTreeIterator}.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  itk::ChildTreeIterator<TreeType> childIt(tree);
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Before discussing the particular features of this iterator, however, we will
  // illustrate features common to all \code{TreeIterator}s, which inherit
  // from \doxygen{TreeIteratorBase}.  Basic use follows the convention of other
  // iterators in ITK, relying on the \code{GoToBegin()} and \code{IsAtEnd()} methods.
  // The iterator is advanced using the prefix increment \code{++} operator,
  // whose behavior naturally depends on the particular iterator being used.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  for (childIt.GoToBegin(); !childIt.IsAtEnd(); ++childIt)
    {
    std::cout << childIt.Get() << std::endl;
    }
  std::cout << std::endl;
  // Software Guide : EndCodeSnippet


  childIt.GoToBegin();


  // Software Guide : BeginLatex
  //
  // Note that, though not illustrated here, trees may also be traversed using
  // the \code{GoToParent()} and \code{GoToChild()} methods.
  //
  // \code{TreeIterator}s have a number of useful functions for testing properties
  // of the current node.  For example, \code{GetType()} returns an enumerated type
  // corresponding to the type of the particular iterator being used. These types
  // are as follows:
  //
  // \code{UNDEFIND}, \code{PREORDER}, \code{INORDER}, \code{POSTORDER}, \code{LEVELORDER},
  // \code{CHILD}, \code{ROOT}, and \code{LEAF}.
  //
  // In the following snippet, we test whether the iterator is of type \code{CHILD},
  // and return from the program indicating failure if the test returns \code{false}.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  if(childIt.GetType() != itk::TreeIteratorBase<TreeType>::CHILD)
    {
    std::cerr << "Error: The iterator was not of type CHILD." << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The value associated with the node can be retrieved and modified using
  // \code{Get()} and \code{Set()} methods:
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  int oldValue = childIt.Get();
  std::cout << "The node's value is " << oldValue << std::endl;
  int newValue = 2;
  childIt.Set(newValue);
  std::cout << "Now, the node's value is " << childIt.Get() << std::endl;
  // Software Guide : EndCodeSnippet

  childIt.Set(oldValue);

  // Software Guide : BeginLatex
  //
  // A number of member functions are defined allowing the user to query
  // information about the current node's parent/child relationships:
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Is this a leaf node? " << childIt.IsLeaf() << std::endl;
  std::cout << "Is this the root node? " << childIt.IsRoot() << std::endl;
  std::cout << "Does this node have a parent? " << childIt.HasParent()
            << std::endl;
  std::cout << "How many children does this node have? "
            << childIt.CountChildren() << std::endl;
  std::cout << "Does this node have a child 1? " << childIt.HasChild(1)
            << std::endl;
  // Software Guide : EndCodeSnippet

  std::cout << std::endl;

  // Software Guide : BeginLatex
  //
  // In addition to traversing the tree and querying for information, \code{TreeIterator}s
  // can alter the structure of the tree itself.  For example, a node can be added
  // using the \code{Add()} methods, child nodes can be removed using the
  // \code{RemoveChild()} method, and the current node can be removed using the
  // \code{Remove()} method.  Each of these methods returns a bool indicating whether
  // the alteration was successful.
  //
  // To illustrate this, in the following snippet we clear the tree of all nodes, and then
  // repopulate it using the iterator.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  tree->Clear();

  itk::PreOrderTreeIterator<TreeType> it(tree);

  it.GoToBegin();

  it.Add(0);
  it.Add(1);
  it.Add(2);
  it.Add(3);
  it.GoToChild(2);
  it.Add(4);
  it.Add(5);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Every \code{TreeIterator} has a \code{Clone()} function which returns
  // a copy of the current iterator. Note that the user should delete
  // the created iterator by hand.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  itk::TreeIteratorBase<TreeType>* childItClone = childIt.Clone();
  delete childItClone;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Alternatively, \doxygen{TreeIteratorClone} can be used to create a generic copy of
  // an iterator.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  typedef itk::TreeIteratorBase<TreeType>      IteratorType;
  typedef itk::TreeIteratorClone<IteratorType> IteratorCloneType;
  IteratorCloneType anotherChildItClone = childIt;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We now turn our attention to features of the specific \code{TreeIterator}
  // specializations.  \code{ChildTreeIterator}, for example, provides a way
  // to iterate through all the children of a node.
  //
  // Software Guide : EndLatex

  std::cout << "ChildTreeIterator:" << std::endl;

  // Software Guide : BeginCodeSnippet
  for (childIt.GoToBegin(); !childIt.IsAtEnd(); ++childIt)
    {
    std::cout << childIt.Get();
    }
  std::cout << std::endl;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The \doxygen{LeafTreeIterator} iterates through the leaves of the tree.
  //
  // Software Guide : EndLatex

  std::cout << "LeafTreeIterator:" << std::endl;

  // Software Guide : BeginCodeSnippet
  itk::LeafTreeIterator<TreeType> leafIt(tree);
  for (leafIt.GoToBegin(); !leafIt.IsAtEnd(); ++leafIt)
    {
    std::cout << leafIt.Get() << std::endl;
    }
  std::cout << std::endl;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex

  // \doxygen{LevelOrderTreeIterator} takes three arguments in its constructor:
  // the tree to be traversed, the maximum depth (or `level'), and the starting node.
  // Naturally, this iterator provides a method for returning the current level.
  //
  // Software Guide : EndLatex

  std::cout << "LevelOrderTreeIterator:" << std::endl;

  // Software Guide : BeginCodeSnippet
  itk::LevelOrderTreeIterator<TreeType> levelIt(tree,10,tree->GetNode(0));
  for (levelIt.GoToBegin(); !levelIt.IsAtEnd(); ++levelIt)
    {
    std::cout << levelIt.Get()
              << " ("<< levelIt.GetLevel() << ")"
              << std::endl;
    }
  std::cout << std::endl;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // \doxygen{InOrderTreeIterator} iterates through the tree
  // from left to right.
  //
  // Software Guide : EndLatex

  std::cout << "InOrderTreeIterator:" << std::endl;

  // Software Guide : BeginCodeSnippet
  itk::InOrderTreeIterator<TreeType> inOrderIt(tree);
  for (inOrderIt.GoToBegin(); !inOrderIt.IsAtEnd(); ++inOrderIt)
    {
    std::cout << inOrderIt.Get() << std::endl;
    }
  std::cout << std::endl;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // \doxygen{PreOrderTreeIterator} iterates through the tree
  // from left to right but do a depth first search.
  //
  // Software Guide : EndLatex

  std::cout << "PreOrderTreeIterator:" << std::endl;

  // Software Guide : BeginCodeSnippet
  itk::PreOrderTreeIterator<TreeType> preOrderIt(tree);
  for (preOrderIt.GoToBegin(); !preOrderIt.IsAtEnd(); ++preOrderIt)
    {
    std::cout << preOrderIt.Get() << std::endl;
    }
  std::cout << std::endl;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The \doxygen{PostOrderTreeIterator} iterates through the tree
  // from left to right but goes from the leaves to the root in the search.
  //
  // Software Guide : EndLatex

  std::cout << "PostOrderTreeIterator:" << std::endl;

  // Software Guide : BeginCodeSnippet
  itk::PostOrderTreeIterator<TreeType> postOrderIt(tree);
  for (postOrderIt.GoToBegin(); !postOrderIt.IsAtEnd(); ++postOrderIt)
    {
    std::cout << postOrderIt.Get() << std::endl;
    }
  std::cout << std::endl;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The \doxygen{RootTreeIterator} goes from one node to the
  // root. The second arguments is the starting node. Here we go from the leaf
  // node (value = 6) up to the root.
  //
  // Software Guide : EndLatex

  std::cout << "RootTreeIterator:" << std::endl;

  // Software Guide : BeginCodeSnippet
  itk::RootTreeIterator<TreeType> rootIt(tree,tree->GetNode(4));
  for (rootIt.GoToBegin(); !rootIt.IsAtEnd(); ++rootIt)
    {
    std::cout << rootIt.Get() << std::endl;
    }
  std::cout << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;

}

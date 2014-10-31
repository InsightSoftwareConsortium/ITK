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
// This example shows how to use the \doxygen{TreeContainer} and the
// associated TreeIterators.
// The \doxygen{TreeContainer} implements the notion of tree and is
// templated over the type of node so it can virtually handle any
// objects. Each node is supposed to have only one parent so no cycle
// is present in the tree. No checking is done to ensure a cycle-free
// tree.
//
// Let's begin by including the appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkTreeContainer.h"
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
  // First, we create a tree of integers.
  // The TreeContainer is templated over the type of nodes.
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
  // Then we use the \code{Add()} function to add nodes to the tree
  // The first argument is the value of the new node and the second
  // argument is the value of the parent node. If two nodes have
  // the same values then the first one is picked. In this particular
  // case it is better to use an iterator to fill the tree.
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
  // We define an \doxygen{LevelOrderTreeIterator} to parse the tree in level order.
  // This particular iterator takes three arguments. The first one is the actual tree
  // to be parsed, the second one is the maximum depth level and the third one is the
  // starting node. The \code{GetNode()} function return a node given its value. Once
  // again the first node that corresponds to the value is returned.
  //
  // Software Guide : EndLatex

  std::cout << "LevelOrderTreeIterator:" << std::endl;

  // Software Guide : BeginCodeSnippet
  itk::LevelOrderTreeIterator<TreeType> levelIt(tree,10,tree->GetNode(2));
  levelIt.GoToBegin();
  while(!levelIt.IsAtEnd())
    {
    std::cout << levelIt.Get()
              << " ("<< levelIt.GetLevel()
              << ")" << std::endl;
    ++levelIt;
    }
  std::cout << std::endl;
  // Software Guide : EndCodeSnippet
  levelIt.GoToBegin();
  // Software Guide : BeginLatex
  //
  // The TreeIterators have useful functions to test the property of the current
  // pointed node. Among these functions: \code{IsLeaf{}} returns true if the current
  // node is a leaf, \code{IsRoot{}} returns true if the node is a root,
  // \code{HasParent{}} returns true if the node has a parent and
  // \code{CountChildren{}} returns the number of children for this particular node.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  levelIt.IsLeaf();
  levelIt.IsRoot();
  levelIt.HasParent();
  levelIt.CountChildren();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \doxygen{ChildTreeIterator} provides another way to iterate through a tree
  // by listing all the children of a node.
  //
  // Software Guide : EndLatex
  std::cout << "ChildTreeIterator:" << std::endl;
  // Software Guide : BeginCodeSnippet
  itk::ChildTreeIterator<TreeType> childIt(tree);
  childIt.GoToBegin();
  while(!childIt.IsAtEnd())
    {
    std::cout << childIt.Get() << std::endl;
    ++childIt;
    }
  std::cout << std::endl;
  // Software Guide : EndCodeSnippet
  childIt.GoToBegin();
  // Software Guide : BeginLatex
  //
  // The \code{GetType()} function returns the type of iterator used.
  // The list of enumerated types is as follow:
  // PREORDER, INORDER, POSTORDER, LEVELORDER, CHILD, ROOT and LEAF.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  if(childIt.GetType() != itk::TreeIteratorBase<TreeType>::CHILD)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Every TreeIterator has a \code{Clone()} function which returns
  // a copy of the current iterator. Note that the user should delete
  // the created iterator by hand.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  childIt.GoToParent();
  itk::TreeIteratorBase<TreeType>* childItClone = childIt.Clone();
  delete childItClone;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \doxygen{LeafTreeIterator} iterates through the leaves of the tree.
  //
  // Software Guide : EndLatex
  std::cout << "LeafTreeIterator:" << std::endl;
  // Software Guide : BeginCodeSnippet
  itk::LeafTreeIterator<TreeType> leafIt(tree);
  leafIt.GoToBegin();
  while(!leafIt.IsAtEnd())
    {
    std::cout << leafIt.Get() << std::endl;
    ++leafIt;
    }
  std::cout << std::endl;
  // Software Guide : EndCodeSnippet
  // Software Guide : BeginLatex
  //
  // The \doxygen{InOrderTreeIterator} iterates through the tree
  // in the order from left to right.
  //
  // Software Guide : EndLatex
  std::cout << "InOrderTreeIterator:" << std::endl;
  // Software Guide : BeginCodeSnippet
  itk::InOrderTreeIterator<TreeType> InOrderIt(tree);
  InOrderIt.GoToBegin();
  while(!InOrderIt.IsAtEnd())
    {
    std::cout << InOrderIt.Get() << std::endl;
    ++InOrderIt;
    }
  std::cout << std::endl;
  // Software Guide : EndCodeSnippet
  // Software Guide : BeginLatex
  //
  // The \doxygen{PreOrderTreeIterator} iterates through the tree
  // from left to right but do a depth first search.
  //
  // Software Guide : EndLatex
  std::cout << "PreOrderTreeIterator:" << std::endl;
  // Software Guide : BeginCodeSnippet
  itk::PreOrderTreeIterator<TreeType> PreOrderIt(tree);
  PreOrderIt.GoToBegin();
  while(!PreOrderIt.IsAtEnd())
    {
    std::cout << PreOrderIt.Get() << std::endl;
    ++PreOrderIt;
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
  itk::PostOrderTreeIterator<TreeType> PostOrderIt(tree);
  PostOrderIt.GoToBegin();
  while(!PostOrderIt.IsAtEnd())
    {
    std::cout << PostOrderIt.Get() << std::endl;
    ++PostOrderIt;
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
  itk::RootTreeIterator<TreeType> RootIt(tree,tree->GetNode(6));
  RootIt.GoToBegin();
  while(!RootIt.IsAtEnd())
    {
    std::cout << RootIt.Get() << std::endl;
    ++RootIt;
    }
  std::cout << std::endl;
  // Software Guide : EndCodeSnippet
  // Software Guide : BeginLatex
  //
  // All the nodes of the tree can be removed by using the
  // \code{Clear()} function.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  tree->Clear();
  // Software Guide : EndCodeSnippet
  // Software Guide : BeginLatex
  //
  // We show how to use a TreeIterator to form a tree by creating nodes.
  // The \code{Add()} function is used to add a node and put a value on it.
  // The \code{GoToChild()} is used to jump to a node.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  itk::PreOrderTreeIterator<TreeType> PreOrderIt2(tree);
  PreOrderIt2.Add(0);
  PreOrderIt2.Add(1);
  PreOrderIt2.Add(2);
  PreOrderIt2.Add(3);
  PreOrderIt2.GoToChild(2);
  PreOrderIt2.Add(4);
  PreOrderIt2.Add(5);
  // Software Guide : EndCodeSnippet
  // Software Guide : BeginLatex
  //
  // The \doxygen{TreeIteratorClone} can be used to have a generic copy of
  // an iterator.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::TreeIteratorBase<TreeType>      IteratorType;
  typedef itk::TreeIteratorClone<IteratorType> IteratorCloneType;
  itk::PreOrderTreeIterator<TreeType> anIterator(tree);
  IteratorCloneType aClone = anIterator;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}

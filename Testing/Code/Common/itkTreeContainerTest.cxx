/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTreeContainerTest.cxx
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


/**
 * This is a test file for the itkTreeContainer class.
 */
#include "itkTreeContainer.h"
#include "itkChildTreeIterator.h"
#include "itkLeafTreeIterator.h"
#include "itkLevelOrderTreeIterator.h"
#include "itkInOrderTreeIterator.h"
#include "itkPostOrderTreeIterator.h"
#include "itkPreOrderTreeIterator.h"
#include "itkRootTreeIterator.h"
#include "itkTreeIteratorClone.h"

int itkTreeContainerTest(int, char* [])
{
  typedef int NodeType;
  typedef itk::TreeContainer<NodeType> TreeType;  
  TreeType::Pointer tree = TreeType::New();
  tree->SetRoot(0);
  tree->Add(1,0);
  tree->Add(2,0);
  tree->Add(3,0);
  tree->Add(4,2);
  tree->Add(5,2);
  tree->Add(6,5);
  tree->Add(7,1);

  std::cout << "Testing PrintSelf: " << std::endl;
  std::cout << tree << std::endl;
  std::cout << "[SUCESS]" << std::endl;

  // LevelOrderTreeIterator Test
  std::cout << "Testing LevelOrderTreeIterator: " << std::endl; 
  itk::LevelOrderTreeIterator<TreeType> levelIt(tree,10,tree->GetNode(2));
  levelIt.GoToBegin();
  while(!levelIt.IsAtEnd())
    {
    std::cout << levelIt.Get() << " ("<< levelIt.GetLevel() << ")" << std::endl;;
    ++levelIt;
    }

  // Test some functionalities of TreeIteratorBase
  // IsLeaf()
  std::cout << "Testing IsLeaf(): "; 
  levelIt.GoToBegin();
  if(levelIt.IsLeaf())
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
   std::cout << "[SUCESS]" << std::endl;

  // IsRoot()
  std::cout << "Testing IsRoot(): "; 
  if(!levelIt.IsRoot())
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
   std::cout << "[SUCESS]" << std::endl;

  // HasParent()
  std::cout << "Testing HasParent(): "; 
  if(!levelIt.HasParent())
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
   std::cout << "[SUCESS]" << std::endl;

  // CountChildren()
  std::cout << "Testing CountChildren(): "; 
  if(levelIt.CountChildren()!=2)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
   std::cout << "[SUCESS]" << std::endl;


  // ChildTreeIterator Test
  std::cout << "Testing ChildTreeIterator: " << std::endl; 
  itk::ChildTreeIterator<TreeType> childIt(tree);
  childIt.GoToBegin();
  while(!childIt.IsAtEnd())
    {
    std::cout << childIt.Get() << std::endl;;
    ++childIt;
    }
  std::cout << std::endl;
    
  std::cout << "Starting to another branch: " << std::endl;
  itk::ChildTreeIterator<TreeType> childIt2(tree,tree->GetNode(2));
  childIt2.GoToBegin();
  while(!childIt2.IsAtEnd())
    {
    std::cout << childIt2.Get() << std::endl;;
    ++childIt2;
    }

  childIt2.GoToBegin();

  std::cout << "Testing other features : ";

  if(childIt2.GetType() != itk::TreeIteratorBase<TreeType>::CHILD)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }

  if(!childIt2.GoToParent())
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[SUCESS]" << std::endl;

  std::cout << "Testing Clone() : ";
  itk::TreeIteratorBase<TreeType>* childItClone = childIt2.Clone();
  if(!childItClone)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  delete childItClone;

  std::cout << "[SUCESS]" << std::endl;

  // LeafTreeIterator Test
  std::cout << "Testing LeafTreeIterator: " << std::endl; 
  itk::LeafTreeIterator<TreeType> leafIt(tree);
  leafIt.GoToBegin();
  while(!leafIt.IsAtEnd())
    {
    std::cout << leafIt.Get() << std::endl;;
    ++leafIt;
    }

  std::cout << std::endl;
  std::cout << "[SUCESS]" << std::endl;


  // InOrderTreeIterator Test
  std::cout << "Testing InOrderTreeIterator: " << std::endl; 
  itk::InOrderTreeIterator<TreeType> InOrderIt(tree);
  InOrderIt.GoToBegin();
  while(!InOrderIt.IsAtEnd())
    {
    std::cout << InOrderIt.Get() << std::endl;;
    ++InOrderIt;
    }
  std::cout << std::endl;
  std::cout << "[SUCESS]" << std::endl;

  
  // PostOrderTreeIterator Test
  std::cout << "Testing PostOrderTreeIterator: " << std::endl; 
  itk::PostOrderTreeIterator<TreeType> PostOrderIt(tree);
  PostOrderIt.GoToBegin();
  while(!PostOrderIt.IsAtEnd())
    {
    std::cout << PostOrderIt.Get() << std::endl;;
    ++PostOrderIt;
    }
  std::cout << std::endl;
  std::cout << "[SUCESS]" << std::endl;

  // RootTreeIterator Test
  std::cout << "Testing RootTreeIterator: " << std::endl; 
  itk::RootTreeIterator<TreeType> RootIt(tree,tree->GetNode(6));
  RootIt.GoToBegin();
  while(!RootIt.IsAtEnd())
    {
    std::cout << RootIt.Get() << std::endl;;
    ++RootIt;
    }
  std::cout << std::endl;
  std::cout << "[SUCESS]" << std::endl;


  // PreOrderTreeIterator Test
  std::cout << "Testing PreOrderTreeIterator: " << std::endl; 
  itk::PreOrderTreeIterator<TreeType> PreOrderIt(tree);
  PreOrderIt.GoToBegin();
  while(!PreOrderIt.IsAtEnd())
    {
    std::cout << PreOrderIt.Get() << std::endl;;
    ++PreOrderIt;
    }
  std::cout << std::endl;
  std::cout << "[SUCESS]" << std::endl;


  /** Now create the tree using iterators */
  std::cout << "Testing clear tree" << std::endl;
  if(tree->Clear())
    {
    std::cout << "[SUCESS]" << std::endl;
    }
  else
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << tree << std::endl;

  // Testing the tree creation using iterators
  itk::PreOrderTreeIterator<TreeType> PreOrderIt2(tree);
  PreOrderIt2.Add(0);
  PreOrderIt2.Add(1);
  PreOrderIt2.Add(2);
  PreOrderIt2.Add(3);
  PreOrderIt2.GoToChild(2);
  PreOrderIt2.Add(4);
  PreOrderIt2.Add(5);

  std::cout << tree << std::endl;
  std::cout << "[SUCESS]" << std::endl;
  // Try to add a bench of leaves
  itk::LeafTreeIterator<TreeType> LeafIt2(tree);
  LeafIt2.GoToBegin();
  int i=6;
  while(!LeafIt2.IsAtEnd())
    {
    LeafIt2.Add(i);
    i++;
    ++LeafIt2;
    if(!LeafIt2.IsAtEnd())
      {
      ++LeafIt2;
      }
    }
  std::cout << tree << std::endl;
  std::cout << "[SUCESS]" << std::endl;


  // Test the TreeIteratorClone
  typedef itk::TreeIteratorBase<TreeType> IteratorType;
  typedef itk::TreeIteratorClone<IteratorType> MyIteratorClone;
  itk::PreOrderTreeIterator<TreeType> anIterator(tree);
  MyIteratorClone aClone = anIterator;
  ++aClone; // step forward in the cloned iterator, anIterator unchanged.
  itk::PreOrderTreeIterator<TreeType> anotherIterator(tree);
  aClone = anotherIterator; // the memory for the cloned version of  "anIterator" is freed.
  ++aClone; // step forward in the cloned iterator, but now in post-order!
 
  std::cout << "Test Done." << std::endl;
  return EXIT_SUCCESS;
}


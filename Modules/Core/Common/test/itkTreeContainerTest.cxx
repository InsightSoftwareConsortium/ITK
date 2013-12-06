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

/**
 * This is a test file for the itkTreeContainer class.
 */
#include "itkTreeContainer.h"
#include "itkChildTreeIterator.h"
#include "itkLeafTreeIterator.h"
#include "itkLevelOrderTreeIterator.h"
#include "itkInOrderTreeIterator.h"
#include "itkPostOrderTreeIterator.h"
#include "itkRootTreeIterator.h"
#include "itkTreeIteratorClone.h"
#include "itkTestingMacros.h"

int itkTreeContainerTest(int, char* [])
{
  typedef int                          NodeType;
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

  unsigned int expectedIndex = 0;

  std::cout << "Testing PrintSelf: " << std::endl;
  std::cout << tree << std::endl;
  std::cout << "[SUCCESS]" << std::endl;

  // LevelOrderTreeIterator Test
  std::cout << "Testing LevelOrderTreeIterator: " << std::endl;
  itk::LevelOrderTreeIterator<TreeType> levelIt(tree,10,tree->GetNode(2));
  levelIt.GoToBegin();
  while(!levelIt.IsAtEnd())
    {
    std::cout << levelIt.Get() << " ("<< levelIt.GetLevel() << ")" << std::endl;
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
   std::cout << "[SUCCESS]" << std::endl;

  // IsRoot()
  std::cout << "Testing IsRoot(): ";
  if(!levelIt.IsRoot())
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
   std::cout << "[SUCCESS]" << std::endl;

  // HasParent()
  std::cout << "Testing HasParent(): ";
  if(!levelIt.HasParent())
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
   std::cout << "[SUCCESS]" << std::endl;

  // CountChildren()
  std::cout << "Testing CountChildren(): ";
  if(levelIt.CountChildren() != 2)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
   std::cout << "[SUCCESS]" << std::endl;


  // ChildTreeIterator Test
  std::cout << "Testing ChildTreeIterator: " << std::endl;
  NodeType expectedChild[] = {1, 2, 3};
  expectedIndex = 0;
  itk::ChildTreeIterator<TreeType> childIt(tree);
  childIt.GoToBegin();
  while(!childIt.IsAtEnd())
    {
    std::cout << childIt.Get() << std::endl;
    TEST_EXPECT_EQUAL( childIt.Get(), expectedChild[expectedIndex] );
    ++expectedIndex;
    ++childIt;
    }
  std::cout << std::endl;

  std::cout << "Starting to another branch: " << std::endl;
  itk::ChildTreeIterator<TreeType> childIt2(tree,tree->GetNode(2));
  childIt2.GoToBegin();
  while(!childIt2.IsAtEnd())
    {
    std::cout << childIt2.Get() << std::endl;
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

  std::cout << "[SUCCESS]" << std::endl;

  std::cout << "Testing Clone() : ";
  itk::TreeIteratorBase<TreeType>* childItClone = childIt2.Clone();
  if(!childItClone)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  delete childItClone;

  std::cout << "[SUCCESS]" << std::endl;

  // LeafTreeIterator Test
  std::cout << "Testing LeafTreeIterator: " << std::endl;
  NodeType expectedLeaf[] = {7, 4, 6, 3};
  expectedIndex = 0;
  itk::LeafTreeIterator<TreeType> leafIt(tree);
  leafIt.GoToBegin();
  while(!leafIt.IsAtEnd())
    {
    std::cout << leafIt.Get() << std::endl;
    TEST_EXPECT_EQUAL( leafIt.Get(), expectedLeaf[expectedIndex] );
    ++expectedIndex;
    ++leafIt;
    }

  std::cout << std::endl;
  std::cout << "[SUCCESS]" << std::endl;


  // InOrderTreeIterator Test
  std::cout << "Testing InOrderTreeIterator: " << std::endl;
  NodeType expectedInOrder[] = {0, 1, 7, 2, 4, 5, 6, 3};
  expectedIndex = 0;
  itk::InOrderTreeIterator<TreeType> InOrderIt(tree);
  InOrderIt.GoToBegin();
  while(!InOrderIt.IsAtEnd())
    {
    std::cout << InOrderIt.Get() << std::endl;
    TEST_EXPECT_EQUAL( InOrderIt.Get(), expectedInOrder[expectedIndex] );
    ++expectedIndex;
    ++InOrderIt;
    }
  std::cout << std::endl;
  std::cout << "[SUCCESS]" << std::endl;


  // PostOrderTreeIterator Test
  std::cout << "Testing PostOrderTreeIterator: " << std::endl;
  NodeType expectedPostOrder[] = {7, 1, 4, 6, 5, 2, 3, 0};
  expectedIndex = 0;
  itk::PostOrderTreeIterator<TreeType> PostOrderIt(tree);
  PostOrderIt.GoToBegin();
  while(!PostOrderIt.IsAtEnd())
    {
    std::cout << PostOrderIt.Get() << std::endl;
    TEST_EXPECT_EQUAL( PostOrderIt.Get(), expectedPostOrder[expectedIndex] );
    ++expectedIndex;
    ++PostOrderIt;
    }
  std::cout << std::endl;
  std::cout << "[SUCCESS]" << std::endl;

  // RootTreeIterator Test
  std::cout << "Testing RootTreeIterator: " << std::endl;
  NodeType expectedRootTree[] = {6, 5, 2, 0};
  expectedIndex = 0;
  itk::RootTreeIterator<TreeType> RootIt(tree,tree->GetNode(6));
  RootIt.GoToBegin();
  while(!RootIt.IsAtEnd())
    {
    std::cout << RootIt.Get() << std::endl;
    TEST_EXPECT_EQUAL( RootIt.Get(), expectedRootTree[expectedIndex] );
    ++expectedIndex;
    ++RootIt;
    }
  std::cout << std::endl;
  std::cout << "[SUCCESS]" << std::endl;


  // PreOrderTreeIterator Test
  std::cout << "Testing PreOrderTreeIterator: " << std::endl;
  NodeType expectedPreOrder[] = {0, 1, 7, 2, 4, 5, 6, 3};
  expectedIndex = 0;
  itk::PreOrderTreeIterator<TreeType> PreOrderIt(tree);
  PreOrderIt.GoToBegin();
  while(!PreOrderIt.IsAtEnd())
    {
    std::cout << PreOrderIt.Get() << std::endl;
    TEST_EXPECT_EQUAL( PreOrderIt.Get(), expectedPreOrder[expectedIndex] );
    ++expectedIndex;
    ++PreOrderIt;
    }
  std::cout << std::endl;
  std::cout << "[SUCCESS]" << std::endl;


  /** Now create the tree using iterators */
  std::cout << "Testing clear tree" << std::endl;
  if(tree->Clear())
    {
    std::cout << "[SUCCESS]" << std::endl;
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
  std::cout << "[SUCCESS]" << std::endl;
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
  std::cout << "[SUCCESS]" << std::endl;


  // Test the TreeIteratorClone
  typedef itk::TreeIteratorBase<TreeType>      IteratorType;
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

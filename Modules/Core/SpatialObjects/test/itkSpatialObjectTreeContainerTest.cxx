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
#include "itkGroupSpatialObject.h"
#include "itkSpatialObjectTreeContainer.h"
#include "itkLevelOrderTreeIterator.h"
#include <iostream>

int itkSpatialObjectTreeContainerTest(int, char* [])
{

  typedef itk::GroupSpatialObject<2>         NodeType;
  typedef itk::SpatialObjectTreeContainer<2> TreeType;

  NodeType::Pointer object0 = NodeType::New();
  object0->SetId(0);
  NodeType::Pointer object1 = NodeType::New();
  object1->SetId(1);
  NodeType::Pointer object2 = NodeType::New();
  object2->SetId(2);
  NodeType::Pointer object3 = NodeType::New();
  object3->SetId(3);
  NodeType::Pointer object4 = NodeType::New();
  object4->SetId(4);
  NodeType::Pointer object5 = NodeType::New();
  object5->SetId(5);
  NodeType::Pointer object6 = NodeType::New();
  object6->SetId(6);
  NodeType::Pointer object7 = NodeType::New();
  object7->SetId(7);

  object0->AddSpatialObject(object1);
  object0->AddSpatialObject(object2);
  object0->AddSpatialObject(object3);
  object1->AddSpatialObject(object7);
  object2->AddSpatialObject(object4);
  object2->AddSpatialObject(object5);
  object5->AddSpatialObject(object6);

  TreeType::Pointer tree = TreeType::New();
  tree->Print(std::cout);

  tree->SetRoot(object0.GetPointer());

  // LevelOrderTreeIterator Test
  std::cout << "Testing LevelOrderTreeIterator: " << std::endl;
  itk::LevelOrderTreeIterator<TreeType> levelIt(tree,10);
  levelIt.GoToBegin();
  while(!levelIt.IsAtEnd())
    {
    std::cout << levelIt.Get()->GetId() << " ("<< levelIt.GetLevel() << ")" << std::endl;
    ++levelIt;
    }
  std::cout << std::endl;
  std::cout << "[SUCCESS]" << std::endl;


  std::cout << "Testing adding to tree by iterator (PreOrderTreeIterator): " << std::endl;
  NodeType::Pointer object8 = NodeType::New();
  object8->SetId(8);
  itk::PreOrderTreeIterator<TreeType> preIt( tree );
  preIt.Add(object8.GetPointer());
//if the following line is used instead of the previous line the correct node type is created and the test passes
//  preIt.GetNode()->AddChild(object8->GetTreeNode());
  preIt.GoToBegin();
  while(!preIt.IsAtEnd())
    {
    if(preIt.Get()->GetId() == 8)
      {
      break;
      }
    ++preIt;
    }
  if(preIt.Get()->GetId() != 8)
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing node type of SpatialObject added to tree by iterator (PreOrderTreeIterator): " << std::endl;
  const itk::SpatialObjectTreeNode<2>* spatialObjectTreeNode =
    dynamic_cast<const itk::SpatialObjectTreeNode<2>*>(preIt.GetNode());
  if(spatialObjectTreeNode==ITK_NULLPTR)
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  tree->Print(std::cout);
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Test Done." << std::endl;
  return EXIT_SUCCESS;
}

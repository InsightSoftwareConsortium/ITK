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

#include <fstream>
#include "itkCommand.h"
#include <iostream>


/**
 * This is a test file for the itkTreeContainer class.
 */

#include "itkTreeContainer.h"
#include "itkChildTreeIterator.h"
#include "itkLevelOrderTreeIterator.h"
#include "itkInOrderTreeIterator.h"
#include "itkPostOrderTreeIterator.h"
#include "itkRootTreeIterator.h"

class TestData
{
public:
  TestData()
  {
    m_Value = 0;
    m_Counter++;
  }

  TestData(const int valueIn)
  {
    m_Value = valueIn;
    m_Counter++;
  }

  TestData(const TestData& t)
  {
    m_Value = t.m_Value;
    m_Counter++;
  }

  ~TestData()
  {
    m_Value = 0;
    m_Counter--;

    if(m_Counter == 0)
      {
      std::cout << "last Data destroyed" << std::endl;
      }
  }

  int Get() const
  {
    return m_Value;
  }

  void Set( int v )
  {
    m_Value = v;
  }

  TestData &operator=(const TestData& t)
  {
    m_Value = t.m_Value;
    std::cout << "assignation: " << m_Value << std::endl;
    return *this;
  }

  bool operator==(const TestData& t)
  {
    if( &t == this )
      {
      return true;
      }
    return false;
  }

  friend std::ostream& operator<<( std::ostream& o, TestData& t);
  friend std::istream& operator>>( std::istream& i, TestData& t);

private:
  int        m_Value;
  static int m_Counter;
};

int TestData::m_Counter = 0;

std::ostream& operator<<( std::ostream& o, TestData& t)
{
  return o << "( TestData Nr. " << t.Get() << " )";
}

std::istream& operator>>( std::istream& i, TestData& t )
{
  char s[10];
  int value;

  i >> s; // {
  i >> s; // TestData
  i >> s; // Nr.
  i >> s; // }
  i >> value;

  t.Set( value );
  return i;
}


typedef TestData*                           NodePointerType;
typedef std::list<NodePointerType>          NodeListType;
typedef itk::TreeContainer<NodePointerType> TreeType;

class TreeChangeListener : public itk::Command
{
public:
  typedef TreeChangeListener             Self;
  typedef itk::SmartPointer<Self>        Pointer;
  typedef itk::SmartPointer<const Self>  ConstPointer;
  itkNewMacro(Self);

  virtual void Execute( itk::Object*, const itk::EventObject& event ) ITK_OVERRIDE
  {
    std::cout << event.GetEventName() << std::endl;
  }

  virtual void Execute( const itk::Object *, const itk::EventObject &event ) ITK_OVERRIDE
  {
    std::cout << event.GetEventName();

    const itk::TreeChangeEvent<TreeType>* e = static_cast<const itk::TreeChangeEvent<TreeType> * >(&event);

    std::cout << *e->GetChangePosition().Get()  << std::endl;
  }

protected:
  TreeChangeListener() {};
};

/*
    CreateTree_1()

                                                    [1]

                                                 *   *   *
                                              *    *   *     *
                                          *      *       *       *
                                        [11]    [12]    [13]    [14]
                                      * * *      * * *
                                  *    *  *      *  *    *
                               *     *    *      *    *     *
                            [111] [112] [113]  [121] [122] [123]
                                                             *
                                                             *
                                                             *
                                                            [9]
                                                            * *
                                                           *   *
                                                          *     *
                                                         [91]  [92]
                                                        * * *
                                                      *   *   *
                                                    *     *     *
                                                 [991]  [912]  [913]
 */
TreeType::Pointer CreateTree_1(NodeListType& internalList)
{
  std::cout << "create tree" << std::endl;

  TreeType::Pointer tree = TreeType::New();


  TreeChangeListener::Pointer treeChangeListener = TreeChangeListener::New();
  itk::TreeAddEvent<TreeType> ev;
  unsigned long tag = tree->AddObserver( ev, treeChangeListener );


  itk::PostOrderTreeIterator<TreeType> it( tree );

  TestData* newNode = new TestData(1);
  internalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(11);
  internalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(12);
  internalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(13);
  internalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(14);
  internalList.push_back(newNode);
  it.Add(newNode);
  it.GoToChild(0);
  newNode = new TestData(111);
  internalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(112);
  internalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(113);
  internalList.push_back(newNode);
  it.Add(newNode);
  it.GoToParent();
  it.GoToChild(1);
  newNode = new TestData(121);
  internalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(122);
  internalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(123);
  internalList.push_back(newNode);
  it.Add(newNode);
  tree->RemoveObserver( tag );

  std::cout << "end create tree" << std::endl;
  return tree;
}

/*
    CreateTree_2()

                                                             *
                                                             *
                                                             *
                                                            [9]
                                                            * *
                                                           *   *
                                                          *     *
                                                         [91]  [92]
                                                        * * *
                                                      *   *   *
                                                    *     *     *
                                                 [991]  [912]  [913]
 */
TreeType::Pointer CreateTree_2(NodeListType& internalList)
{
  std::cout << "create tree 2" << std::endl;

  TreeType::Pointer tree = TreeType::New();
  TestData* newNode = new TestData(9);
  internalList.push_back(newNode);
  tree->SetRoot(newNode);
  itk::PostOrderTreeIterator<TreeType> it( tree );

  newNode = new TestData(91);
  internalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(92);
  internalList.push_back(newNode);
  it.Add(newNode);
  it.GoToChild( 0 );
  newNode = new TestData(991);
  internalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(912);
  internalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(913);
  internalList.push_back(newNode);
  it.Add(newNode);

  std::cout << "end create tree 2" << std::endl;
  return tree;
}

int IteratorTest( itk::TreeIteratorBase<TreeType>& i )
{
  int sum = 0;

  // childIt.GoToBegin();
  while(!i.IsAtEnd())
  {
    sum += i.Get()->Get();
    std::cout << i.Get()->Get() << std::endl;
    ++i;
  }

  return sum;
}

/** Print the results */
int PrintResult( int result, int value )
{
  if ( result == value )
  {
    std::cout << "[SUCCESS]" << std::endl;
    return 1;
  }
  else
  {
    std::cout << "[FAILURE]" << std::endl;
    return 0;
  }
}

/**
 * Test
 */
int itkTreeContainerTest2(int, char* [])
{
  int result;
  int testCounter = 0;
  NodeListType internalList; // used to delete the pointer in the tree

  TreeType::Pointer tree = CreateTree_1(internalList);

  std::cout << "\nPreOrderTreeIterator:" << std::endl;
  itk::PreOrderTreeIterator<TreeType> preOrderIt( tree );
  result = IteratorTest( preOrderIt );
  testCounter += PrintResult( result, 753 );

  std::cout << "\nLevelOrderTreeIterator:" << std::endl;
  itk::LevelOrderTreeIterator<TreeType> levelIt(tree);
  result = IteratorTest( levelIt );
  testCounter += PrintResult( result, 753 );

  std::cout << "\nInOrderTreeIterator:" << std::endl;
  itk::InOrderTreeIterator<TreeType> inOrderIt(tree);
  result = IteratorTest( inOrderIt );
  testCounter += PrintResult( result, 753 );

  std::cout << "\nPostOrderTreeIterator:" << std::endl;
  itk::PostOrderTreeIterator<TreeType> postOrderIt(tree);
  result = IteratorTest( postOrderIt );
  testCounter += PrintResult( result, 753 );

  std::cout << "\nChildTreeIterator Test 1:" << std::endl;
  itk::ChildTreeIterator<TreeType> childIt_1(tree);
  result = IteratorTest( childIt_1 );
  testCounter += PrintResult( result, 50 );

  std::cout << "\nChildTreeIterator Test 2:" << std::endl;
  itk::ChildTreeIterator<TreeType> childIt_2(tree);
  childIt_2.GoToChild(0);
  result = IteratorTest( childIt_2 );
  testCounter += PrintResult( result, 336 );

  std::cout << "\nChildTreeIterator Test 3:" << std::endl;
  itk::ChildTreeIterator<TreeType> childIt_3(tree);
  childIt_3.GoToChild(1);
  result = IteratorTest( childIt_3 );
  testCounter += PrintResult( result, 366 );

  std::cout << "\nChildTreeIterator Test 4:" << std::endl;
  itk::ChildTreeIterator<TreeType> childIt_4(tree);
  childIt_4.GoToChild(1);
  childIt_4.GoToChild(2);
  result = IteratorTest( childIt_4 );
  testCounter += PrintResult( result, 0 );

  std::cout << "\nRootTreeIterator Test 1:" << std::endl;
  itk::RootTreeIterator<TreeType> rootIt_1(tree);
  rootIt_1.GoToChild(1);
  rootIt_1.GoToChild(2);
  result = IteratorTest( rootIt_1 );
  testCounter += PrintResult( result, 136 );

  // creat tree 2
  itk::PreOrderTreeIterator<TreeType> iterator_123( tree );
  iterator_123.GoToChild( 1 );
  iterator_123.GoToChild( 2 );
  TreeType::Pointer tree_2 = CreateTree_2(internalList);
  iterator_123.Add( *tree_2.GetPointer() );

  std::cout << "\nTree1 + Tree2:" << std::endl;
  itk::PreOrderTreeIterator<TreeType> preOrderIt_Tree_1_2( tree );
  result = IteratorTest( preOrderIt_Tree_1_2 );
  testCounter += PrintResult( result, 3761 );

  TreeChangeListener::Pointer treeChangeListener = TreeChangeListener::New();
  itk::TreeChangeEvent<TreeType> ev;
  unsigned long tag = tree->AddObserver( ev, treeChangeListener );
  tree->Clear();
  tree->RemoveObserver( tag );

  // Delete the list of pointer to TestData
  std::list<TestData*>::iterator it = internalList.begin();

  while(it != internalList.end())
    {
    std::list<TestData*>::iterator it2 = it;
    ++it;
    TestData* t = *it2;
    internalList.erase(it2);
    delete t;
    }

  if ( testCounter == 10 )
    {
    std::cout << "TEST DONE" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "TEST [FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
}

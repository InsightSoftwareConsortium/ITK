/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTreeContainerTest2.cxx
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

#include <fstream>
#include <itkCommand.h> 
#include "itkMacro.h"
#include <itkObject.h>
#include <iostream>
#include <itkObjectFactory.h>
#include <itkSmartPointer.h>


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

class TestData {
    int value;
    static int counter;

public:
    TestData() {
        counter++;
    }

    TestData(const int valueIn) {
        this->value = valueIn;
        counter++;
    };

    TestData(const TestData& t) {
        this->value = t.value;
        counter++;
    };

    ~TestData() {
        value = 0;
        counter--;

        if(counter == 0)
            std::cout << "last Data destroyed" << std::endl;
    };

    int Get() const {
        return value;
    };

    void Set( int v ) {
        value = v;
    };


    TestData &operator=(const TestData& t) {
        value = t.value;
        std::cout << "assignation: " << value << std::endl;
        return *this;
    }

    bool operator==(const TestData& t) {

        if( &t == this )
            return true;
        return false;
    };

    friend std::ostream& operator<<( std::ostream& o, TestData& t);
    friend std::istream& operator>>( std::istream& i, TestData& t);
};

int TestData::counter = 0;

/**
 *
 */
std::ostream& operator<<( std::ostream& o, TestData& t) {

    return o << "( TestData Nr. " << t.Get() << " )";
}

/**
 *
 */
std::istream& operator>>( std::istream& i, TestData& t ) {

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


typedef TestData* NodeType;
std::list<TestData*> InternalList;// used to delete the pointer in the tree
typedef itk::TreeContainer<NodeType> TreeType;

class TreeChangeListener : public itk::Command {

protected:
  TreeChangeListener(){};
public:

  typedef TreeChangeListener Self;
  typedef itk::SmartPointer<Self>  Pointer;
  typedef itk::SmartPointer<const Self>  ConstPointer;
  itkNewMacro(Self);
  
  /**
   *
   */
  void  Execute (itk::Object*, const itk::EventObject& event ) {

       std::cout << event.GetEventName() << std::endl;
  }

  /**
   *
   */
  virtual void  Execute (const itk::Object *, const itk::EventObject &event) {
  
    std::cout << event.GetEventName();

    const itk::TreeChangeEvent<TreeType>* e = dynamic_cast<const itk::TreeChangeEvent<TreeType>*>(&event);

    if ( e )
      std::cout << *e->GetChangePosition().Get()  << std::endl;
  }
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
TreeType::Pointer CreateTree_1()
{
  std::cout << "create tree" << std::endl;

  TreeType::Pointer tree = TreeType::New();

  
  TreeChangeListener::Pointer treeChangeListener = TreeChangeListener::New();
  itk::TreeAddEvent<TreeType> ev;
  unsigned long tag = tree->AddObserver( ev, treeChangeListener );
  

  itk::PostOrderTreeIterator<TreeType> it( tree );

  TestData* newNode = new TestData(1);
  InternalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(11);
  InternalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(12);
  InternalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(13);
  InternalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(14);
  InternalList.push_back(newNode);
  it.Add(newNode);
  it.GoToChild(0);
  newNode = new TestData(111);
  InternalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(112);
  InternalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(113);
  InternalList.push_back(newNode);
  it.Add(newNode);
  it.GoToParent();
  it.GoToChild(1);
  newNode = new TestData(121);
  InternalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(122);
  InternalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(123);
  InternalList.push_back(newNode);
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
TreeType::Pointer CreateTree_2()
{
  std::cout << "create tree 2" << std::endl;

  TreeType::Pointer tree = TreeType::New();
  TestData* newNode = new TestData(9);
  InternalList.push_back(newNode);
  tree->SetRoot(newNode);
  itk::PostOrderTreeIterator<TreeType> it( tree );

  newNode = new TestData(91);
  InternalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(92);
  InternalList.push_back(newNode);
  it.Add(newNode);
  it.GoToChild( 0 );
  newNode = new TestData(991);
  InternalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(912);
  InternalList.push_back(newNode);
  it.Add(newNode);
  newNode = new TestData(913);
  InternalList.push_back(newNode);
  it.Add(newNode);

  std::cout << "end create tree 2" << std::endl;
  return tree;
}


/**
 *
 */
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
    std::cout << "[SUCESS]" << std::endl;
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
  TreeType::Pointer tree = CreateTree_1();

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
  TreeType::Pointer tree_2 = CreateTree_2();
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
  std::list<TestData*>::iterator it = InternalList.begin();

  while(it != InternalList.end())
    {
    std::list<TestData*>::iterator it2 = it;
    it++;
    TestData* t = *it2;
    InternalList.erase(it2);
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


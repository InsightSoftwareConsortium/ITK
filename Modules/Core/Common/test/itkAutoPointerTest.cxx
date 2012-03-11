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

#include <iostream>

#include "itkAutoPointer.h"

class TestObject
 {
  public:
    typedef TestObject                   Self;
    typedef itk::AutoPointer<Self>       AutoPointer;
    typedef itk::AutoPointer<const Self> ConstAutoPointer;
    TestObject() { std::cout << "TestObject Contructed" << std::endl; }
    virtual ~TestObject() { std::cout << "TestObject Destructed" << std::endl; }
    const char * GetClassName(void) const { return "my Class name is TestObject"; }
  };


int itkAutoPointerTest(int, char* [] )
{

  TestObject * obj = new TestObject;

  TestObject::AutoPointer ptr1;
  ptr1.TakeOwnership( obj );

  std::cout << "after assignment from raw pointer" << std::endl;
  std::cout << "ptr1 IsOwner = " << ptr1.IsOwner() << std::endl;

  std::cout << ptr1->GetClassName() << std::endl;

  TestObject::AutoPointer ptr2( ptr1 );

  std::cout << "after copy constructor " << std::endl;
  std::cout << "ptr1 IsOwner = " << ptr1.IsOwner() << std::endl;
  std::cout << "ptr2 IsOwner = " << ptr2.IsOwner() << std::endl;

  ptr2.Reset();
  std::cout << "after Reset " << std::endl;
  std::cout << "ptr2 IsOwner = " << ptr2.IsOwner() << std::endl;

  ptr1.TakeOwnership( new TestObject );
  std::cout << "after assignment from raw pointer" << std::endl;
  std::cout << "ptr1 IsOwner = " << ptr1.IsOwner() << std::endl;

  // The following test exercise the methods but don't validate the results
  if( ptr1 == ptr2 )
    {
    std::cout << "AutoPointers are equal " << std::endl;
    }
  if( ptr1 > ptr2 )
    {
    std::cout << "ptr1 > ptr2" << std::endl;
    }
  if( ptr1 < ptr2 )
    {
    std::cout << "ptr1 < ptr2" << std::endl;
    }


  TestObject::ConstAutoPointer cptr1;
  cptr1.TakeOwnership( new TestObject );


  TestObject::ConstAutoPointer cptr2( cptr1 );


  return EXIT_SUCCESS;
}

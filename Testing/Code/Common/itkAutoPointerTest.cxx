/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAutoPointerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkAutoPointer.h"

class TestObject
 {
  public:
    typedef TestObject Self;
    typedef itk::AutoPointer<Self> AutoPointer;      
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


  
  // The following test excercise the methods but don't validate the results
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



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


int itkAutoPointerTest(int, char**)
{

  TestObject * obj = new TestObject;
  TestObject::AutoPointer ptr1;
  ptr1 = obj;

  std::cout << ptr1->GetClassName() << std::endl;

  TestObject::AutoPointer ptr2( ptr1 );

  ptr2.Reset();

  ptr1 = new TestObject;

  
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

  
  TestObject::ConstAutoPointer cptr1( new TestObject );

  
  TestObject::ConstAutoPointer cptr2( cptr1 );


  return EXIT_SUCCESS;
}



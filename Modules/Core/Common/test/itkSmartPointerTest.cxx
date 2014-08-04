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
#include <cstdlib>
#include "itkSmartPointer.h"
#include "itkMacro.h"

class itkTestObject
{
public:
  typedef itk::SmartPointer<itkTestObject> Pointer;
  static itkTestObject::Pointer New();
  virtual void Register()
    {
    std::cout << "Register " << *this << " count:"
              << (m_ReferenceCount+1) << "  " << std::endl;
    m_ReferenceCount++;
    }
  virtual void UnRegister()
    {
    std::cout << "UnRegister " << this << " count:"
              << (m_ReferenceCount-1) << "  " << std::endl;

    m_ReferenceCount--;
    if ( m_ReferenceCount == 0 )
      {
      delete this;
      }
    }
    inline friend std::ostream &operator << (std::ostream &os,
                                             itkTestObject const& o)
    {
      os << "itkTestObject " << (void const * )&o << " " << o.m_ReferenceCount;
      return os;
    }

  std::ostream& Print(std::ostream& os) const
    {
    os << "itkTestObject " << (void const * )this << " " << this->m_ReferenceCount;
    return os;
    }

protected:
  itkTestObject()
    {
    m_ReferenceCount = 0;
    std::cout << "construct itkTestObject " << *this << std::endl;
    }
  virtual ~itkTestObject()
    {
    std::cout << "destruct itkTestObject " << *this << std::endl;
    }

private:
  unsigned int m_ReferenceCount;
};

itkTestObject::Pointer itkTestObject::New()
{
  return itkTestObject::Pointer(new itkTestObject);
}

class itkTestObjectSubClass : public itkTestObject
{
public:
  typedef itk::SmartPointer<itkTestObjectSubClass> Pointer;
  static Pointer New();
};


itkTestObjectSubClass::Pointer itkTestObjectSubClass::New()
{
  return itkTestObjectSubClass::Pointer(new itkTestObjectSubClass);
}

// This SHOULD NOT be used in ITK, all functions
// should take raw pointers as arguments
void TestUpCastPointer(itkTestObject::Pointer)
{
}

// Test a function that takes an itkTestObject raw pointer
void TestUpCast(itkTestObject*)
{
}


int itkSmartPointerTest(int, char* [] )
{
  // Create a base class pointer to a child class
  itkTestObject::Pointer to(itkTestObjectSubClass::New());
  // test the safe down cast and create a child class Pointer object
  itkTestObjectSubClass::Pointer sc
    = dynamic_cast<itkTestObjectSubClass*>(to.GetPointer());
  // Test the up cast with a function that takes a pointer
  TestUpCast(sc);
  // Test calling a function that takes a SmartPointer as
  // an argument, note, that you have to get the raw pointer
  // for this to work
  TestUpCastPointer(sc.GetPointer());

  // This will not work, but only in construction
  //itkTestObject::Pointer p = sc;

  // For construction use the constructor call:
  // Test casting up the tree, note no explict cast is required
  itkTestObject::Pointer p(sc);

  // No cast is required for assignment
  p = sc;

  std::cout <<"second test" << std::endl;
  {
  itkTestObject::Pointer o1 = itkTestObject::New();
  std::cout << "o1 " << &o1 << std::endl;
  itkTestObject::Pointer o2 = itkTestObject::New();
  std::cout << "o2 " << &o2 << std::endl;
  itkTestObject::Pointer o3 = itkTestObject::New();
  std::cout << "o3 " << &o3 << std::endl;
  itkTestObject::Pointer o4 = itkTestObject::New();
  std::cout << "o4 " << &o4 << std::endl;

  o1 = o2;
  o2 = o3;
  o4 = o1;
  if ( o1 < o2 )
    {
    std::cout << "o1 is < o2 " << o1 << " " << o2 << std::endl;
    }
  else
    {
    std::cout << "o1 is not < o2 " << &o1 << " " << &o2 << std::endl;
    }
  }
  std::cout <<"end second test" << std::endl << std::endl;


  // check test of null pointer
  itkTestObject::Pointer q(ITK_NULLPTR);
  std::cout << q;

  std::cout <<"first test" << std::endl;
  {
  itkTestObject::Pointer o1 = itkTestObject::New();
  if(o1.IsNull())
    {
    return EXIT_FAILURE;
    }

  // use argument dependent loop up for swap in itk namespace
  swap(q,o1);

  }
  std::cout <<"end first test" << std::endl << std::endl;
  return EXIT_SUCCESS;
}

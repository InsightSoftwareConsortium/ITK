/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkSmartPointerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include <iostream>
#include "itkSmartPointer.h"

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
      os << "itkTestObject " << (void*)&o << " " << o.m_ReferenceCount; 
      return os;
    }
  
  std::ostream& Print(std::ostream& os) const
    {
    os << "itkTestObject " << (void*)this << " " << this->m_ReferenceCount; 
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
void TestUpCastPointer(itkTestObject::Pointer p)
{
}

// Test a function that takes an itkTestObject raw pointer
void TestUpCast(itkTestObject* p)
{
}


int main()
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

  std::cout <<"first test" << std::endl;
  {
  itkTestObject::Pointer o1 = itkTestObject::New();
  }
  std::cout <<"end first test" << std::endl << std::endl;
  return 0;
}



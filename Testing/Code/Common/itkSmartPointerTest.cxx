/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkSmartPointerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>
#include "itkSmartPointer.h"

class itkTestObject
{
public:
  typedef itkSmartPointer<itkTestObject> Pointer;
  static itkTestObject::Pointer New();
  void Register()
    {
    std::cout << "Register " << *this << " count:" 
              << (m_ReferenceCount+1) << "  " << std::endl;
    m_ReferenceCount++;
    }
  void UnRegister()
    {
    std::cout << "UnRegister " << *this << " count:" 
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
  
  std::ostream Print(std::ostream& os) const
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
  ~itkTestObject() 
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



int main()
{
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



/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadTest.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMLoadTest_h
#define __itkFEMLoadTest_h

#include "itkFEMLoadElementBase.h"

namespace itk {
namespace fem {




/**
 * \class LoadTest
 * \brief Example to show how to define templated load classes.
 *
 * \note The class must be instantiated, before the object factory can
 *       produce more objects of this class. Instantiate the specific
 *       derived classes with: "template class LoadTest<...>;" where required.
 */
template<class TClass>
class LoadTest : public LoadElement
{
FEM_CLASS(LoadTest,LoadElement)
public:

  /**
   * Default constructor
   */
  LoadTest() {}

  /**
   * Some data that this load defines.
   */
  TClass data;

  virtual void Read( std::istream& f, void* info )
  {
    Superclass::Read(f,info);
  }
  void Write( std::ostream& f ) const
  {
    // call the parent's write function
    Superclass::Write(f);
  }

private:
  /** Dummy static int that enables automatic registration
      with FEMObjectFactory. */
  static const int DummyCLID;

};




// Provide the templated code for CLID function, that is 
// otherwise generated automaticly with FEM_CLASS_REGISTER
// macro.
template<class TClass>
int LoadTest<TClass>::CLID(void) 
{
  static const int CLID_ = FEMOF::Register( LoadTest::NewB, (std::string("LoadTest(")
                +typeid(TClass).name()+")").c_str());
  return CLID_;
}

// Make sure that the class is registered with FEMObjectFactory
// by calling CLID() static member function each time the class
// is instantiated for a specific template parameter TClass.
template<class TClass>
const int LoadTest<TClass>::DummyCLID=LoadTest<TClass>::CLID();






}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadTest_h

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationTestBar2D.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMLoadImplementationTestBar2D_h
#define __itkFEMLoadImplementationTestBar2D_h

#include "itkFEMElementBar2D.h"
#include "itkFEMLoadTest.h"

namespace itk {
namespace fem {




/**
 * This is an example of how to define the implementation of a templated
 * Load class. Since the Load class is templated, its implementation must
 * also be templated. Due to limitations of MS compiler, we define this
 * implementation function as a static function inside a templated class.
 *
 * To make things easier to use, we template the class over the whole
 * templated load class and not only over the template parameters required
 * to define the templated Load class.
 *
 * You must manually instantiate this class to register the load
 * implementation function with the VisitorDispatcher. The
 * instantiation is normally done like:
 *     typedef LoadTest<...> MyLoadTestClass;
 *     template class LoadTestImplementationBar2D<MyLoadTestClass>;
 */
template<class TLoadClass>
class LoadTestImplementationBar2D
{
public:
  static Element::LoadVectorType impl(Bar2D::ConstPointer element, Element::LoadElementPointer load)
  {
    // We must dynamically cast the given load pointer to the
    // correct templated load class, which is given as
    // template parameter.
    typename TLoadClass::Pointer l0=dynamic_cast<TLoadClass*>(&*load);
    if ( !l0 ) throw FEMException(__FILE__, __LINE__, "FEM error");

    cout<<"Load object's data:"<<l0->data<<"\n";

    Element::VectorType F(element->GetNumberOfDegreesOfFreedom());

    return F;
  }
private:
  static const bool registered;
};

// When the templated load implementation function is instantiated,
// it will automatically be registered with the VisitorDispatcher so 
// that it is called as required.
// Instantiating the implementation function will also instantiate the
// corresponding Load class.
template<class TLoadClass>
const bool LoadTestImplementationBar2D<TLoadClass>::registered=
  VisitorDispatcher<Bar2D,Element::LoadElementType,Bar2D::VectorType (*)(Bar2D::ConstPointer,ElementNew::LoadElementPointer)>
  ::RegisterVisitor((TLoadClass*)0, &LoadTestImplementationBar2D<TLoadClass>::impl);




}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadImplementationTestBar2D_h

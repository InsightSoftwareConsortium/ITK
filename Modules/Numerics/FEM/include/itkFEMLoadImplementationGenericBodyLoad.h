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
#ifndef __itkFEMLoadImplementationGenericBodyLoad_h
#define __itkFEMLoadImplementationGenericBodyLoad_h

#include "itkFEMElementBase.h"
#include "itkFEMLoadGrav.h"

namespace itk {
namespace fem {

/**
 * \class LoadImplementationGenericBodyLoad
 * \brief Class that holds a templated generic body load implementation.
 *
 * The only accessable part of this class is a static function HandleLoad.
 * This is the function that should be passed to the VisitorDispatcher
 * when registering a load with the element class. The function is templated
 * over the a pointer to an element class, and can therefore be registered
 * with any element class.
 *
 * Function HandleLoad is declared within a class only to avoid problems with
 * MS compiler. The real gravity load implementation is in static member
 * function Implementation, which is automatically called within HandleLoad
 * function.
 *
 * \note Declare any additional general implementations of loads in a\
 *       similar way as here.
 * \ingroup ITK-FEM
 */
class LoadImplementationGenericBodyLoad
{
public:
  /**
   * Template parameter should be a const pointer type pointing to a class
   * that is derived from the Element base class. The template parameter
   * is normally automatically determined.
   *
   * FIXME: Add concept checking.
   */
  template<class TElementClassConstPointer>
  static void HandleLoad(TElementClassConstPointer e, Element::LoadPointer l, Element::VectorType& Fe)
    {
    // Check if we really got a LoadGrav object
    LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*l);
    if ( !l0 )
      {
      // Passed load object was not of class LoadGrav!
      throw FEMException(__FILE__, __LINE__, "FEM error");
      }

    // Statically cast the passed pointer to the element base class and
    // call the real load implementation with the correct pointer types.
    // If cast fails, the passed pointer was of incompatible class.
    Implementation(static_cast<Element::ConstPointer>(e),l0,Fe);
    }

private:
  /**
   * Handle LoadGrav in element by integrating over the element domain.
   * This implementation requires that the element has the shape functions
   * and integration routines defined.
   *
   * It is also assumed, that element's local DOFs are numbered with respect
   * to node ID. If this is not the case, you should not use this function.
   */
  static void Implementation(Element::ConstPointer element, LoadGrav::Pointer load, Element::VectorType& Fe);

  /**
   * Private constructor prohibits creation of objects of this class
   */
  LoadImplementationGenericBodyLoad();
};

#ifdef _MSC_VER
// Declare a static dummy function to prevent a MSVC 6.0 SP5 from crashing.
// I have no idea why things don't work when this is not declared, but it
// looks like this declaration makes compiler forget about some of the
// troubles it has with templates.
static void Dummy( void );
#endif // #ifdef _MSC_VER

}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadImplementationGenericBodyLoad_h

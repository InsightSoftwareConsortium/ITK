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
#ifndef __itkFEMLoadImplementationGenericLandmarkLoad_h
#define __itkFEMLoadImplementationGenericLandmarkLoad_h

#include "itkFEMElementBase.h"
#include "itkFEMLoadLandmark.h"

namespace itk {
namespace fem {

/**
 * \class LoadImplementationGenericLandmarkLoad
 * \brief Class that holds a templated generic landmark load implementation.
 *
 * For more info see class LoadImplementationGenericBodyLoad.
 * \ingroup ITK-FEM
 */
class LoadImplementationGenericLandmarkLoad
{
public:
  template<class TElementClassConstPointer>
  static void HandleLoad(TElementClassConstPointer e, Element::LoadPointer l, Element::VectorType& Fe)
    {
    // Check if we really got an object of correct class
    LoadLandmark::Pointer l0=dynamic_cast<LoadLandmark*>(&*l);
    if ( !l0 )
      {
      // Class of passed load object was not compatible!
      throw FEMException(__FILE__, __LINE__, "FEM error");
      }

    // Statically cast the passed pointer to the element base class and
    // call the real load implementation with the correct pointer types.
    // If cast fails, the passed pointer was of incompatible class.
    Implementation(static_cast<Element::ConstPointer>(e),l0,Fe);
    }

private:
  /**
   * Handle LoadLandmark in element by integrating over the element domain.
   * This implementation requires that the element has the shape functions
   * and integration routines defined.
   *
   * It is also assumed, that element's local DOFs are numbered with respect
   * to node ID. If this is not the case, you should not use this function.
   */
  static void Implementation(Element::ConstPointer element, LoadLandmark::Pointer load, Element::VectorType& Fe);

  /**
   * Private constructor prohibits creation of objects of this class
   */
  LoadImplementationGenericLandmarkLoad();
};

#ifdef _MSC_VER
// Declare a static dummy function to prevent a MSVC 6.0 SP5 from crashing.
// I have no idea why things don't work when this is not declared, but it
// looks like this declaration makes compiler forget about some of the
// troubles it has with templates.
static void Dummy( void );
#endif // #ifdef _MSC_VER

}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadImplementationGenericLandmarkLoad_h

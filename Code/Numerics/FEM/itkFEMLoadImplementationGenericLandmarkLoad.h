/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationGenericLandmarkLoad.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

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
 */
class LoadImplementationGenericLandmarkLoad
{
public:
  template<class TElementClassConstPointer>
  inline static Element::VectorType HandleLoad(TElementClassConstPointer e, Element::LoadElementPointer l)
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
    return Implementation(e,l0);
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
  static Element::VectorType Implementation(Element::ConstPointer element, LoadLandmark::Pointer load);

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

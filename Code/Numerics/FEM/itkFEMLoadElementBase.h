/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadElementBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMLoadElementBase_h
#define __itkFEMLoadElementBase_h

#include "itkFEMLoadBase.h"

namespace itk {
namespace fem {




/**
 * \class LoadElement
 * \brief Virtual element load base class.
 *
 * This load class defines an external load that acts on elements in a system.
 * The vector with pointers to elements defines, on which elements
 * the load acts. The derived load classes should provide members, that allow the
 * Element's class Fe() member function to uniquely transform the load into nodal loads.
 * No special requirements are enforced on those members.
 * 
 * Ultimately, when assembling the right hand side of the master equation (master force vector)
 * the Element's Fe() member funtion is called with the pointer to the LoadElement class that is
 * prescribed on that element. Fe() function shuld dynamically cast this pointer to specific
 * load class, which it can handle and return the element's force vector.
 */
class LoadElement : public Load
{
FEM_CLASS(LoadElement,Load)
public:
  /**
   * Float type used in Element and derived classes
   */
  typedef Element::Float Float;

  /**
   * Type of array of pointers to element objects
   */
  typedef std::vector<Element::ConstPointer> ElementPointersVectorType;
  ElementPointersVectorType el;  /** pointers to element objects on which the load acts */

  virtual void Read( std::istream& f, void* info );
  void Write( std::ostream& f ) const;

  // FIXME: should clear vector, not zero it
  LoadElement() : el(0) {}

};

FEM_CLASS_INIT(LoadElement)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadElementBase_h

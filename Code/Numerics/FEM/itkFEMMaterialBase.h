/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMMaterialBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMMaterialBase_h
#define __itkFEMMaterialBase_h

#include "itkFEMLightObject.h"
#include "itkFEMPArray.h"

namespace itk {
namespace fem {




/**
 * \class Material
 * \brief Base class for storing all the implicit material and other properties
          required to fully define the element class.
 *
 * When specifying materials for particular element, you should use
 * MaterialStandard class or derive your own class (using Material
 * or MaterialStandard as a base class) if your Element requires
 * special properties or constants.
 *
 * Material base class doesn't define any data member.
 * Everything usefull is stored in derived clases. This class
 * is here just to group all material classes together and access
 * them via this base class.
 */
class Material : public FEMLightObject
{
FEM_ABSTRACT_CLASS(Material,FEMLightObject)
public:
  /**
   * Array class that holds special pointers to objects of all Material classes
   */
  typedef FEMPArray<Self> ArrayType;

};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMMaterialBase_h

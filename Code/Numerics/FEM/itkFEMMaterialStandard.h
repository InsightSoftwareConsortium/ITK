/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMMaterialStandard.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMMaterialStandard_h
#define __itkFEMMaterialStandard_h

#include "itkFEMMaterialBase.h"

namespace itk {
namespace fem {




/**
 * \class MaterialStandard
 * \brief Standard material class
 *
 * This class includes material and other kind of properties required to
 * define any standard element in FEM toolkit. Most of the Elements
 * can use this material class.
 */
class MaterialStandard : public Material {
FEM_CLASS(MaterialStandard,Material)
public:
  virtual void Read(std::istream& f, void* info);
  virtual void Write(std::ostream& f ) const;

  /* Data members of MaterialStandard class */

  /**
   * Young modulus
   */
  double E;

  /**
   * Cross section area of a line element
   */
  double A;  // 

  /**
   * Moment of inertia
   */
  double I;

  /**
   * Poisson's ratio
   */
  double ni;
  
  /*
   * ... we can add properties here as required without the influence on the already defined elements
   */

};

FEM_CLASS_INIT(MaterialStandard)



}} // end namespace itk::fem

#endif // #ifndef __itkFEMMaterialStandard_h

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC0LinearTriangularMembrane.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement2DC0LinearTriangularMembrane_h
#define __itkFEMElement2DC0LinearTriangularMembrane_h

#include "itkFEMElement2DC0LinearTriangular.h"
#include "itkFEMElement2DMembrane.h"

namespace itk {
namespace fem {




/**
 * \class Element2DC0LinearTriangularMembrane
 * \brief 3-noded finite element class in 2D space for linear elasticity problem.
 *
 * This element is combined from Element2DC0LinearTriangular and Element2DMembrane.
 */
class Element2DC0LinearTriangularMembrane : public Element2DMembrane<Element2DC0LinearTriangular>
{
FEM_CLASS(Element2DC0LinearTriangularMembrane,Element2DMembrane<Element2DC0LinearTriangular>)
public:

  HANDLE_ELEMENT_LOADS();

  /**
   * Default constructor only clears the internal storage
   */
  Element2DC0LinearTriangularMembrane();

  /**
   * Construct an element by specifying pointers to
   * 3 points and a material.
   */
  Element2DC0LinearTriangularMembrane(
      NodeIDType n1_, 
      NodeIDType n2_,
      NodeIDType n3_,
      Material::ConstPointer p_ );

}; // class Element2DC0LinearTriangularMembrane 

FEM_CLASS_INIT(Element2DC0LinearTriangularMembrane)




}} // end namespace itk::fem

#endif  // #ifndef __itkFEMElement2DC0LinearTriangularMembrane_h

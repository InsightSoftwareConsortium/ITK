/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC0LinearQuadrilateralMembrane.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement2DC0LinearQuadrilateralMembrane_h
#define __itkFEMElement2DC0LinearQuadrilateralMembrane_h

#include "itkFEMElement2DC0LinearQuadrilateral.h"
#include "itkFEMElement2DMembrane.h"

namespace itk {
namespace fem {




/**
 * \class Element2DC0LinearQuadrilateralMembrane
 * \brief 4-noded finite element class in 2D space for linear elasticity problem
 */
class Element2DC0LinearQuadrilateralMembrane : public Element2DMembrane<Element2DC0LinearQuadrilateral>
{
FEM_CLASS(Element2DC0LinearQuadrilateralMembrane,Element2DMembrane<Element2DC0LinearQuadrilateral>)
public:

  HANDLE_ELEMENT_LOADS();

  /**
   * Default constructor only clears the internal storage
   */
  Element2DC0LinearQuadrilateralMembrane();

  /**
   * Construct an element by specifying pointers to
   * 4 points and a material.
   */
  Element2DC0LinearQuadrilateralMembrane(
      NodeIDType n1_, 
      NodeIDType n2_,
      NodeIDType n3_,
      NodeIDType n4_,
      Material::ConstPointer p_ );

}; // class Element2DC0LinearQuadrilateralMembrane

FEM_CLASS_INIT(Element2DC0LinearQuadrilateralMembrane)




}} // end namespace itk::fem

#endif  // #ifndef __itkFEMElement2DC0LinearQuadrilateralMembrane_h

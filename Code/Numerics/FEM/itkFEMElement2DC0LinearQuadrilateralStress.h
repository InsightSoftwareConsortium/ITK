/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC0LinearQuadrilateralStress.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement2DC0LinearQuadrilateralStress_h
#define __itkFEMElement2DC0LinearQuadrilateralStress_h

#include "itkFEMElement2DC0LinearQuadrilateral.h"
#include "itkFEMElement2DStress.h"

namespace itk {
namespace fem {




/**
 * \class Element2DC0LinearQuadrilateralStress
 * \brief 4-noded finite element class in 2D space for linear elasticity problem
 */
class Element2DC0LinearQuadrilateralStress : public Element2DStress<Element2DC0LinearQuadrilateral>
{
FEM_CLASS(Element2DC0LinearQuadrilateralStress,Element2DStress<Element2DC0LinearQuadrilateral>)
public:

  HANDLE_ELEMENT_LOADS();

  /**
   * Default constructor only clears the internal storage
   */
  Element2DC0LinearQuadrilateralStress();

  /**
   * Construct an element by specifying pointers to
   * 4 points and a material.
   */
  Element2DC0LinearQuadrilateralStress(
      NodeIDType n1_, 
      NodeIDType n2_,
      NodeIDType n3_,
      NodeIDType n4_,
      Material::ConstPointer p_ );

}; // class Element2DC0LinearQuadrilateralStress

FEM_CLASS_INIT(Element2DC0LinearQuadrilateralStress)




}} // end namespace itk::fem

#endif  // #ifndef __itkFEMElement2DC0LinearQuadrilateralStress_h

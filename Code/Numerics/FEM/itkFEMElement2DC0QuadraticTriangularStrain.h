/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC0QuadraticTriangularStrain.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement2DC0QuadraticTriangularStrain_h
#define __itkFEMElement2DC0QuadraticTriangularStrain_h

#include "itkFEMElement2DC0QuadraticTriangular.h"
#include "itkFEMElement2DStrain.h"

namespace itk {
namespace fem {




/**
 * \class Element2DC0QuadraticTriangularStrain
 * \brief 3-noded finite element class in 2D space for linear elasticity problem.
 *
 * This element is combined from Element2DC0LinearTriangular and Element2DStrain.
 */
class Element2DC0QuadraticTriangularStrain : public Element2DStrain<Element2DC0QuadraticTriangular>
{
FEM_CLASS(Element2DC0QuadraticTriangularStrain,Element2DStrain<Element2DC0QuadraticTriangular>)
public:

  HANDLE_ELEMENT_LOADS();

  /**
   * Default constructor only clears the internal storage
   */
  Element2DC0QuadraticTriangularStrain();

  /**
   * Construct an element by specifying pointers to
   * 3 points and a material.
   */
  Element2DC0QuadraticTriangularStrain(
      NodeIDType n1_, 
      NodeIDType n2_,
      NodeIDType n3_,
      NodeIDType n4_, 
      NodeIDType n5_,
      NodeIDType n6_,
      Material::ConstPointer p_ );

}; // class Element2DC0QuadraticTriangularStrain 

FEM_CLASS_INIT(Element2DC0QuadraticTriangularStrain)




}} // end namespace itk::fem

#endif  // #ifndef __itkFEMElement2DC0QuadraticTriangularStrain_h

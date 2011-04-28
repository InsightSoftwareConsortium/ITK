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
#ifndef __itkFEMElement2DC0LinearTriangularStress_h
#define __itkFEMElement2DC0LinearTriangularStress_h

#include "itkFEMElement2DC0LinearTriangular.h"
#include "itkFEMElement2DStress.h"

namespace itk {
namespace fem {

/**
 * \class Element2DC0LinearTriangularStress
 * \brief 3-noded finite element class in 2D space for linear elasticity problem.
 *
 * This element is combined from Element2DC0LinearTriangular and Element2DStress.
 * \ingroup ITK-FEM
 */
class Element2DC0LinearTriangularStress : public Element2DStress<Element2DC0LinearTriangular>
{
FEM_CLASS(Element2DC0LinearTriangularStress,Element2DStress<Element2DC0LinearTriangular>)
public:

  HANDLE_ELEMENT_LOADS();

  /**
   * Default constructor only clears the internal storage
   */
  Element2DC0LinearTriangularStress();

  /**
   * Construct an element by specifying pointers to
   * 3 points and a material.
   */
  Element2DC0LinearTriangularStress(
      NodeIDType n1_,
      NodeIDType n2_,
      NodeIDType n3_,
      Material::ConstPointer p_ );

}; // class Element2DC0LinearTriangularStress

FEM_CLASS_INIT(Element2DC0LinearTriangularStress)

}} // end namespace itk::fem

#endif  // #ifndef __itkFEMElement2DC0LinearTriangularStress_h

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

#ifndef itkFEMElement2DC0LinearTriangularStress_h
#define itkFEMElement2DC0LinearTriangularStress_h

#include "itkFEMElement2DC0LinearTriangular.h"
#include "itkFEMElement2DStress.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element2DC0LinearTriangularStress
 * \brief 3-noded finite element class in 2D space for linear elasticity problem.
 *
 * This element is combined from Element2DC0LinearTriangular and Element2DStress.
 * \ingroup ITKFEM
 * This class combines the geometry of the FE problem defined in
 * \link Element2DC0LinearTriangular\endlink
 * and the physics of the problem defined in
 * \link Element2DStrain\endlink
 *
 * \sa Element2DC0LinearTriangularMembrane
 * \sa Element2DC0LinearTriangularStrain
 */
class ITKFEM_EXPORT Element2DC0LinearTriangularStress : public Element2DStress<Element2DC0LinearTriangular>
{
public:
  /** Standard class typedefs. */
  typedef Element2DC0LinearTriangularStress            Self;
  typedef Element2DStress<Element2DC0LinearTriangular> Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element2DC0LinearTriangularStress, Element2DStress<Element2DC0LinearTriangular> );

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  virtual::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE;

  /**
   * Default constructor only clears the internal storage
   */
  Element2DC0LinearTriangularStress();

  /**
   * Construct an element by specifying pointers to
   * 3 points and a material.
   */
  Element2DC0LinearTriangularStress(NodeIDType n1_,
                                    NodeIDType n2_,
                                    NodeIDType n3_,
                                    Material::ConstPointer p_);

protected:
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

};  // class Element2DC0LinearTriangularStress

}
}  // end namespace itk::fem

#endif  // #ifndef itkFEMElement2DC0LinearTriangularStress_h

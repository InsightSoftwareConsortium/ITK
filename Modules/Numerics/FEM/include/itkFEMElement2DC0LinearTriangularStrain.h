/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkFEMElement2DC0LinearTriangularStrain_h
#define itkFEMElement2DC0LinearTriangularStrain_h

#include "itkFEMElement2DC0LinearTriangular.h"
#include "itkFEMElement2DStrain.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element2DC0LinearTriangularStrain
 * \brief 3-noded finite element class in 2D space for linear elasticity problem.
 *
 * This element is combined from Element2DC0LinearTriangular and Element2DStrain.
 * \ingroup ITKFEM
 * The ordering of the nodes is counter clockwise. That is the nodes
 * should be defined in the following order:
 *
 *  (0,1)
 *  2
 *  *
 *  |\
 *  | \
 *  |  \
 *  |   \
 *  |    \
 *  |     \
 *  *------*
 *  0      1
 *  (0,0)  (0,1)
 *
 * This class combines the geometry of the FE problem defined in
 * \link Element2DC0LinearTriangular\endlink
 * and the physics of the problem defined in
 * \link Element2DStrain\endlink
 *
 * \sa Element2DC0LinearTriangularMembrane
 * \sa Element2DC0LinearTriangularStress
 */
class ITKFEM_EXPORT Element2DC0LinearTriangularStrain : public Element2DStrain<Element2DC0LinearTriangular>
{
public:
  /** Standard class type aliases. */
  using Self = Element2DC0LinearTriangularStrain;
  using Superclass = Element2DStrain<Element2DC0LinearTriangular>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element2DC0LinearTriangularStrain, Element2DStrain<Element2DC0LinearTriangular>);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  itk::LightObject::Pointer
  CreateAnother() const override;

  /**
   * Default constructor only clears the internal storage
   */
  Element2DC0LinearTriangularStrain();

  /**
   * Construct an element by specifying pointers to
   * 3 points and a material.
   */
  Element2DC0LinearTriangularStrain(NodeIDType n1_, NodeIDType n2_, NodeIDType n3_, Material::ConstPointer p_);

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

}; // class Element2DC0LinearTriangularStrain
} // end namespace fem
} // end namespace itk

#endif // itkFEMElement2DC0LinearTriangularStrain_h

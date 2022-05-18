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

#ifndef itkFEMElement2DC0LinearLineStress_h
#define itkFEMElement2DC0LinearLineStress_h

#include "itkFEMElement2DC0LinearLine.h"
#include "itkFEMElement1DStress.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element2DC0LinearLineStress
 * \brief 2-noded finite element class in 2D space for linear elasticity problem.
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Element2DC0LinearLineStress : public Element1DStress<Element2DC0LinearLine>
{
public:
  /** Standard class type aliases. */
  using Self = Element2DC0LinearLineStress;
  using Superclass = Element1DStress<Element2DC0LinearLine>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element2DC0LinearLineStress, Element1DStress<Element2DC0LinearLine>);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  itk::LightObject::Pointer
  CreateAnother() const override;

  /**
   * Default constructor only clears the internal storage
   */
  Element2DC0LinearLineStress();

  /**
   * Construct an element by specifying pointers to
   * 4 points and a material.
   */
  Element2DC0LinearLineStress(NodeIDType n1_, NodeIDType n2_, Material::ConstPointer p_);

  /**
   * Consistent mass matrix for a line element.
   * See any finite element book for Consistent mass matrix definition.
   */
  void
  GetMassMatrix(MatrixType & Me) const override;

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

}; // class Element2DC0LinearLineStress
} // end namespace fem
} // end namespace itk

#endif // itkFEMElement2DC0LinearLineStress_h

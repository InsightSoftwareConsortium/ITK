/*=========================================================================
 *
 * Copyright Insight Software Consortium
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 *=========================================================================*/

#ifndef itkFEMElement3DC0LinearTriangularMembrane_h
#define itkFEMElement3DC0LinearTriangularMembrane_h

#include "itkFEMElement3DC0LinearTriangular.h"
#include "itkFEMElement3DMembrane.h"
#include "itkFEMElement3DMembrane1DOF.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element3DC0LinearTriangularMembrane
 * \brief 3-noded finite element class in 3D space for surface membrane problem.
 *
 * This element is combined from Element3DC0LinearTriangular and Element3DMembrane.
 * A membrane element in three dimensional is an isotropic homogeneous
 * element through a small thickness. The elements have three translational
 * degrees of freedom at each node.
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Element3DC0LinearTriangularMembrane : public Element3DMembrane<Element3DC0LinearTriangular>
{
public:
  /** Standard class type aliases. */
  using Self = Element3DC0LinearTriangularMembrane;
  using Superclass = Element3DMembrane<Element3DC0LinearTriangular>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element3DC0LinearTriangularMembrane, Element3DMembrane<Element3DC0LinearTriangular>);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  ::itk::LightObject::Pointer
  CreateAnother() const override;

  /**
   * Default constructor only clears the internal storage
   */
  Element3DC0LinearTriangularMembrane();

  /**
   * Construct an element by specifying pointers to
   * 3 points and a material.
   */
  Element3DC0LinearTriangularMembrane(NodeIDType n1_, NodeIDType n2_, NodeIDType n3_, Material::ConstPointer p_);

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

}; // class Element3DC0LinearTriangularMembrane
} // end namespace fem
} // end namespace itk

#endif // itkFEMElement3DC0LinearTriangularMembrane_h

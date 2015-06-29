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

#ifndef itkFEMElement2DC0LinearTriangularMembrane_h
#define itkFEMElement2DC0LinearTriangularMembrane_h

#include "itkFEMElement2DC0LinearTriangular.h"
#include "itkFEMElement2DMembrane.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element2DC0LinearTriangularMembrane
 * \brief 3-noded finite element class in 2D space.
 *
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
 * The constitutive equation used is from membrane bending energy.
 * This class combines the geometry of the FE problem defined in
 * \link Element2DC0LinearTriangular\endlink
 * and the physics of the problem defined in
 * \link Element2DMembrane\endlink
 *
 * \sa Element2DC0LinearTriangularStrain
 * \sa Element2DC0LinearTriangularStress
 *
 * This element is combined from Element2DC0LinearTriangular and Element2DMembrane.
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Element2DC0LinearTriangularMembrane : public Element2DMembrane<Element2DC0LinearTriangular>
{
public:
  /** Standard class typedefs. */
  typedef Element2DC0LinearTriangularMembrane            Self;
  typedef Element2DMembrane<Element2DC0LinearTriangular> Superclass;
  typedef SmartPointer<Self>                             Pointer;
  typedef SmartPointer<const Self>                       ConstPointer;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element2DC0LinearTriangularMembrane, Element2DMembrane<Element2DC0LinearTriangular> );

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  virtual::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE;

  /**
   * Default constructor only clears the internal storage
   */
  Element2DC0LinearTriangularMembrane();

  /**
   * Construct an element by specifying pointers to
   * 3 points and a material.
   */
  Element2DC0LinearTriangularMembrane(NodeIDType n1_, NodeIDType n2_, NodeIDType n3_, Material::ConstPointer p_);

protected:
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

};  // class Element2DC0LinearTriangularMembrane

}
}  // end namespace itk::fem

#endif  // #ifndef itkFEMElement2DC0LinearTriangularMembrane_h

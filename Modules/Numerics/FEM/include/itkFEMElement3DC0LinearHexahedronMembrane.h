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

#ifndef itkFEMElement3DC0LinearHexahedronMembrane_h
#define itkFEMElement3DC0LinearHexahedronMembrane_h

#include "itkFEMElement3DC0LinearHexahedron.h"
#include "itkFEMElement3DMembrane.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element3DC0LinearHexahedronMembrane
 * \brief 8-noded finite element class in 3D space.
 * The constitutive equation used is from membrane bending energy.
 *
 *
 * This class combines the geometry of the FE problem defined in
 * \link Element3DC0LinearHexahedron\endlink
 * and the physics of the problem defined in
 * \link Element3DMembrane\endlink
 *
 * \sa Element3DC0LinearHexahedronStrain
 *
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Element3DC0LinearHexahedronMembrane : public Element3DMembrane<Element3DC0LinearHexahedron>
{
public:
  /** Standard class type aliases. */
  using Self = Element3DC0LinearHexahedronMembrane;
  using Superclass = Element3DMembrane<Element3DC0LinearHexahedron>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element3DC0LinearHexahedronMembrane, Element3DMembrane<Element3DC0LinearHexahedron>);

  /**
   * CreateAnother method will clone the existing instance of this type,
   * including its internal member variables.
   */
  ::itk::LightObject::Pointer
  CreateAnother() const override;

  /**
   * Default constructor only clears the internal storage
   */
  Element3DC0LinearHexahedronMembrane();

  /**
   * Construct an element by specifying pointers to
   * an array of 8 points and a material.
   */
  Element3DC0LinearHexahedronMembrane(NodeIDType ns_[], Material::ConstPointer p_);

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};

} // end namespace fem
} // end namespace itk

#endif // itkFEMElement3DC0LinearHexahedronMembrane_h

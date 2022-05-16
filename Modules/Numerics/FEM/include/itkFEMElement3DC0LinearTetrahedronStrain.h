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

#ifndef itkFEMElement3DC0LinearTetrahedronStrain_h
#define itkFEMElement3DC0LinearTetrahedronStrain_h

#include "itkFEMElement3DC0LinearTetrahedron.h"
#include "itkFEMElement3DStrain.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element3DC0LinearTetrahedronStrain
 * \brief 4-noded finite element class in 3D space for linear elasticity problem
 *
 * This class combines the geometry of the FE problem defined in
 * \link Element3DC0LinearTetrahedron\endlink
 * and the physics of the problem defined in
 * \link Element3DStrain\endlink
 *
 * \sa Element3DC0LinearTetrahedronMembrane
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Element3DC0LinearTetrahedronStrain : public Element3DStrain<Element3DC0LinearTetrahedron>
{
public:
  /** Standard class type aliases. */
  using Self = Element3DC0LinearTetrahedronStrain;
  using Superclass = Element3DStrain<Element3DC0LinearTetrahedron>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element3DC0LinearTetrahedronStrain, Element3DStrain<Element3DC0LinearTetrahedron>);

  /**
   * CreateAnother method will clone the existing instance of this type,
   * including its internal member variables.
   */
  itk::LightObject::Pointer
  CreateAnother() const override;

  /**
   * Default constructor only clears the internal storage
   */
  Element3DC0LinearTetrahedronStrain();

  /**
   * Construct an element by specifying pointers to
   * an array of 4 points and a material.
   */
  Element3DC0LinearTetrahedronStrain(NodeIDType ns_[], Material::ConstPointer p_);

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

}; // class Element3DC0LinearTetrahedronStrain
} // end namespace fem
} // end namespace itk

#endif // itkFEMElement3DC0LinearTetrahedronStrain_h

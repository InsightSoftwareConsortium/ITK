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

#ifndef itkFEMElement3DC0LinearHexahedronStrain_h
#define itkFEMElement3DC0LinearHexahedronStrain_h

#include "itkFEMElement3DC0LinearHexahedron.h"
#include "itkFEMElement3DStrain.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element3DC0LinearHexahedronStrain
 * \brief 8-noded finite element class in 3D space.
 * The constitutive equation used is from linear elasticity theory.
 *
 *
 * This class combines the geometry of the FE problem defined in
 * \link Element3DC0LinearHexahedron\endlink
 * and the physics of the problem defined in
 * \link Element3DStrain\endlink
 *
 * \sa Element3DC0LinearHexahedronMembrane
 *
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Element3DC0LinearHexahedronStrain : public Element3DStrain<Element3DC0LinearHexahedron>
{
public:
  /** Standard class typedefs. */
  typedef Element3DC0LinearHexahedronStrain            Self;
  typedef Element3DStrain<Element3DC0LinearHexahedron> Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element3DC0LinearHexahedronStrain, Element3DStrain<Element3DC0LinearHexahedron> );

  /**
   * CreateAnother method will clone the existing instance of this type,
   * including its internal member variables.
   */
  virtual::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE;

  /**
   * Default constructor only clears the internal storage
   */
  Element3DC0LinearHexahedronStrain();

  /**
   * Construct an element by specifying pointers to
   * an array of 8 points and a material.
   */
  Element3DC0LinearHexahedronStrain(NodeIDType ns_[], Material::ConstPointer p_);

protected:
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

};  // class Element3DC0LinearHexahedronStrain

}
}  // end namespace itk::fem

#endif  // #ifndef itkFEMElement3DC0LinearHexahedronStrain_h

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

#ifndef itkFEMElement3DC0LinearTetrahedronMembrane_h
#define itkFEMElement3DC0LinearTetrahedronMembrane_h

#include "itkFEMElement3DC0LinearTetrahedron.h"
#include "itkFEMElement3DMembrane.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element3DC0LinearTetrahedronMembrane
 * \brief 4-noded finite element class in 3D space.
 * The constitutive equation used is from membrane bending energy.
 *
 * This class combines the geometry of the FE problem defined in
 * \link Element3DC0LinearTetrahedron\endlink
 * and the physics of the problem defined in
 * \link Element3DMembrane\endlink
 *
 * \sa Element3DC0LinearTetrahedronStrain
 *
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Element3DC0LinearTetrahedronMembrane : public Element3DMembrane<Element3DC0LinearTetrahedron>
{
public:
  /** Standard class typedefs. */
  typedef Element3DC0LinearTetrahedronMembrane            Self;
  typedef Element3DMembrane<Element3DC0LinearTetrahedron> Superclass;
  typedef SmartPointer<Self>                              Pointer;
  typedef SmartPointer<const Self>                        ConstPointer;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element3DC0LinearTetrahedronMembrane, Element3DMembrane<Element3DC0LinearTetrahedron> );

  /**
   * CreateAnother method will clone the existing instance of this type,
   * including its internal member variables.
   */
  virtual::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE;

  /**
   * Default constructor only clears the internal storage
   */
  Element3DC0LinearTetrahedronMembrane();

  /**
   * Construct an element by specifying pointers to
   * an array of 4 points and a material.
   */
  Element3DC0LinearTetrahedronMembrane(NodeIDType ns_[], Material::ConstPointer p_);

protected:
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

};  // class Element3DC0LinearTetrahedronMembrane

}
}  // end namespace itk::fem

#endif  // #ifndef itkFEMElement3DC0LinearTetrahedronMembrane_h

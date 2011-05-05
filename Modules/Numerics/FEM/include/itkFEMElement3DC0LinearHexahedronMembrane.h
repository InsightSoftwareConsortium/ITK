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
#ifndef __itkFEMElement3DC0LinearHexahedronMembrane_h
#define __itkFEMElement3DC0LinearHexahedronMembrane_h

#include "itkFEMElement3DC0LinearHexahedron.h"
#include "itkFEMElement3DMembrane.h"

namespace itk {
namespace fem {

/**
 * \class Element3DC0LinearHexahedronMembrane
 * \brief 8-noded finite element class in 3D space for linear elasticity problem
 * \ingroup ITK-FEM
 */
class Element3DC0LinearHexahedronMembrane : public Element3DMembrane<Element3DC0LinearHexahedron>
{
FEM_CLASS(Element3DC0LinearHexahedronMembrane,Element3DMembrane<Element3DC0LinearHexahedron>)
public:

  HANDLE_ELEMENT_LOADS();

  /**
   * Default constructor only clears the internal storage
   */
  Element3DC0LinearHexahedronMembrane();

  /**
   * Construct an element by specifying pointers to
   * an array of 8 points and a material.
   */
  Element3DC0LinearHexahedronMembrane(
      NodeIDType ns_[],
      Material::ConstPointer p_ );

}; // class Element3DC0LinearHexahedronMembrane

FEM_CLASS_INIT(Element3DC0LinearHexahedronMembrane)

}} // end namespace itk::fem

#endif  // #ifndef __itkFEMElement3DC0LinearHexahedronMembrane_h

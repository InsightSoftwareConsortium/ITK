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
// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMElement3DC0LinearHexahedronStrain.h"

namespace itk {
namespace fem {


Element3DC0LinearHexahedronStrain
::Element3DC0LinearHexahedronStrain() : Superclass()
{
}

Element3DC0LinearHexahedronStrain
::Element3DC0LinearHexahedronStrain(
      NodeIDType ns_[],
      Material::ConstPointer m_) : Superclass()
{
  // Set the geometrical points
  for (int k=0; k<8; k++)
    {
    this->SetNode( k, ns_[k] );
    }

  /*
   * Initialize the pointer to material object and check that
   * we were given the pointer to the right class.
   * If the material class was incorrect an exception is thrown.
   */
  if( (m_mat=dynamic_cast<const MaterialLinearElasticity*>(&*m_)) == 0 )
    {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"Element3DC0LinearHexahedronStrain::Element3DC0LinearHexahedronStrain()");
    }
}

FEM_CLASS_REGISTER(Element3DC0LinearHexahedronStrain)

}} // end namespace itk::fem

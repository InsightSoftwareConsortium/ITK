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

#include "itkFEMLoadNoisyLandmark.h"

namespace itk
{
namespace fem
{

void
LoadNoisyLandmark::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Confidence: " << this->m_Confidence << '\n';
  os << indent << "Error Norm: " << this->m_ErrorNorm << '\n';
  os << indent << "Simulated Displacement: " << this->m_SimulatedDisplacement << '\n';
  os << indent << "Real Displacement: " << this->m_RealDisplacement << '\n';
  os << indent << "Shape Function: " << this->m_Shape << '\n';
  os << indent << "Outlier? " << this->m_IsOutlier << '\n';
  os << indent << "Out of Mesh? " << this->m_IsOutOfMesh << '\n';
  os << indent << "Has Structure Tensor? " << this->m_HasStructureTensor << '\n';
  os << indent << "Structure Tensor: " << this->m_StructureTensor << '\n';
  os << indent << "Landmark Tensor: " << this->m_LandmarkTensor << '\n';
}

} // end namespace fem
} // end namespace itk

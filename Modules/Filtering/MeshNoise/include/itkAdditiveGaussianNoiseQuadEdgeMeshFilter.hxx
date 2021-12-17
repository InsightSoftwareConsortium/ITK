/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkAdditiveGaussianNoiseQuadEdgeMeshFilter_hxx
#define itkAdditiveGaussianNoiseQuadEdgeMeshFilter_hxx

#include "itkNormalVariateGenerator.h"

namespace itk
{

template <typename TInputMesh, typename TOutputMesh>
AdditiveGaussianNoiseQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::AdditiveGaussianNoiseQuadEdgeMeshFilter()
{
  this->m_Mean = 0.0;
  this->m_Sigma = 1.0;
  this->m_Seed = 0;
}

template <typename TInputMesh, typename TOutputMesh>
void
AdditiveGaussianNoiseQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Mean: " << this->m_Mean << std::endl;
  os << indent << "Sigma: " << this->m_Sigma << std::endl;
}

template <typename TInputMesh, typename TOutputMesh>
void
AdditiveGaussianNoiseQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::GenerateData()
{
  typename TInputMesh::ConstPointer inputMesh = this->GetInput();
  typename TOutputMesh::Pointer     outputMesh = this->GetOutput();

  this->CopyInputMeshToOutputMesh();

  typename TOutputMesh::PointsContainer::Iterator it = outputMesh->GetPoints()->Begin();

  using GeneratorType = itk::Statistics::NormalVariateGenerator;
  GeneratorType::Pointer generator = GeneratorType::New();
  generator->Initialize(this->m_Seed);

  while (it != outputMesh->GetPoints()->End())
  {
    for (unsigned int d = 0; d < TOutputMesh::MeshTraits::PointDimension; ++d)
    {
      it.Value()[d] += (generator->GetVariate() * this->m_Sigma + this->m_Mean);
    }
    ++it;
  }
}
} // end namespace itk

#endif

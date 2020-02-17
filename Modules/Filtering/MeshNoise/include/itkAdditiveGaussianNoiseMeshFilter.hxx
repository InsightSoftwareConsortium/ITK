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
#ifndef itkAdditiveGaussianNoiseMeshFilter_hxx
#define itkAdditiveGaussianNoiseMeshFilter_hxx

#include "itkAdditiveGaussianNoiseMeshFilter.h"
#include "itkNormalVariateGenerator.h"

namespace itk
{

template <typename TInputMesh, typename TOutputMesh>
AdditiveGaussianNoiseMeshFilter<TInputMesh, TOutputMesh>::AdditiveGaussianNoiseMeshFilter()
{
  this->m_Mean = 0.0;
  this->m_Sigma = 1.0;
  this->m_Seed = 0;
}

template <typename TInputMesh, typename TOutputMesh>
void
AdditiveGaussianNoiseMeshFilter<TInputMesh, TOutputMesh>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Mean: " << this->m_Mean << std::endl;
  os << indent << "Sigma: " << this->m_Sigma << std::endl;
}

template <typename TInputMesh, typename TOutputMesh>
void
AdditiveGaussianNoiseMeshFilter<TInputMesh, TOutputMesh>::GenerateData()
{
  using InputPointsContainer = typename TInputMesh::PointsContainer;
  using OutputPointsContainer = typename TOutputMesh::PointsContainer;

  using InputPointsContainerConstPointer = typename TInputMesh::PointsContainerConstPointer;
  using OutputPointsContainerPointer = typename TOutputMesh::PointsContainerPointer;

  const InputMeshType * inputMesh = this->GetInput();
  OutputMeshPointer     outputMesh = this->GetOutput();

  if (!inputMesh)
  {
    itkExceptionMacro(<< "Missing Input Mesh");
  }

  if (!outputMesh)
  {
    itkExceptionMacro(<< "Missing Output Mesh");
  }

  outputMesh->SetBufferedRegion(outputMesh->GetRequestedRegion());

  InputPointsContainerConstPointer inPoints = inputMesh->GetPoints();
  OutputPointsContainerPointer     outPoints = outputMesh->GetPoints();

  outPoints->Reserve(inputMesh->GetNumberOfPoints());
  outPoints->Squeeze();

  typename InputPointsContainer::ConstIterator inputPoint = inPoints->Begin();
  typename OutputPointsContainer::Iterator     outputPoint = outPoints->Begin();

  unsigned int maxDimension = TInputMesh::MaxTopologicalDimension;

  using GeneratorType = itk::Statistics::NormalVariateGenerator;
  GeneratorType::Pointer generator = GeneratorType::New();
  generator->Initialize(this->m_Seed);

  while (inputPoint != inPoints->End())
  {
    for (unsigned int dim = 0; dim < maxDimension; ++dim)
    {
      outputPoint.Value()[dim] = inputPoint.Value()[dim] + generator->GetVariate() * this->m_Sigma + this->m_Mean;
    }
    ++inputPoint;
    ++outputPoint;
  }

  // Create duplicate references to the rest of data on the mesh
  this->CopyInputMeshToOutputMeshPointData();
  this->CopyInputMeshToOutputMeshCellLinks();
  this->CopyInputMeshToOutputMeshCells();
  this->CopyInputMeshToOutputMeshCellData();

  for (unsigned int dim = 0; dim < maxDimension; ++dim)
  {
    outputMesh->SetBoundaryAssignments(dim, inputMesh->GetBoundaryAssignments(dim));
  }
}

} // namespace itk

#endif

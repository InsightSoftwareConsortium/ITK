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
#ifndef itkSmoothingQuadEdgeMeshFilter_hxx
#define itkSmoothingQuadEdgeMeshFilter_hxx

#include "itkProgressReporter.h"

namespace itk
{
template <typename TInputMesh, typename TOutputMesh>
SmoothingQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::SmoothingQuadEdgeMeshFilter()
  : m_CoefficientsMethod(nullptr)
  , m_InputDelaunayFilter(InputOutputDelaunayConformingType::New())
  , m_OutputDelaunayFilter(OutputDelaunayConformingType::New())
  , m_NumberOfIterations(1)
  , m_RelaxationFactor(static_cast<OutputCoordType>(1.0))
{}

template <typename TInputMesh, typename TOutputMesh>
SmoothingQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::~SmoothingQuadEdgeMeshFilter() = default;

template <typename TInputMesh, typename TOutputMesh>
void
SmoothingQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::SetCoefficientsMethod(CoefficientsComputation * iMethod)
{
  m_CoefficientsMethod = iMethod;
  this->Modified();
}

template <typename TInputMesh, typename TOutputMesh>
void
SmoothingQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::GenerateData()
{
  const OutputPointIdentifier numberOfPoints = this->GetInput()->GetNumberOfPoints();

  ProgressReporter progress(this, 0, m_NumberOfIterations * (numberOfPoints + 1), 100);

  OutputMeshPointer mesh = OutputMeshType::New();

  const OutputPointsContainerPointer temp = OutputPointsContainer::New();
  temp->Reserve(numberOfPoints);

  if (this->m_DelaunayConforming)
  {
    m_InputDelaunayFilter->SetInput(this->GetInput());
    if (m_NumberOfIterations == 0)
    {
      m_InputDelaunayFilter->GraftOutput(this->GetOutput());
      m_InputDelaunayFilter->Update();
      this->GraftOutput(m_InputDelaunayFilter->GetOutput());
    }
    else
    {
      m_InputDelaunayFilter->Update();
      mesh = m_InputDelaunayFilter->GetOutput();
    }
  }
  else
  {
    if (m_NumberOfIterations == 0)
    {
      this->CopyInputMeshToOutputMesh();
    }
    else
    {
      CopyMeshToMesh(this->GetInput(), mesh.GetPointer());
    }
  }

  for (unsigned int iter = 0; iter < m_NumberOfIterations; ++iter)
  {
    OutputPointsContainerPointer points = mesh->GetPoints();

    for (OutputPointsContainerIterator it = points->Begin(); it != points->End(); ++it)
    {
      OutputPointType p = it.Value();
      OutputQEType *  qe = p.GetEdge();
      if (qe != nullptr)
      {
        OutputPointType  r = p;
        OutputVectorType v;
        v.Fill(0.0);
        OutputQEType *  qe_it = qe;
        OutputCoordType sum_coeff = 0.;
        do
        {
          OutputPointType q = mesh->GetPoint(qe_it->GetDestination());

          OutputCoordType coeff = (*m_CoefficientsMethod)(mesh, qe_it);
          sum_coeff += coeff;

          v += coeff * (q - p);
          qe_it = qe_it->GetOnext();
        } while (qe_it != qe);

        OutputCoordType den = 1.0 / static_cast<OutputCoordType>(sum_coeff);
        v *= den;

        r += m_RelaxationFactor * v;
        r.SetEdge(qe);
        temp->SetElement(it.Index(), r);
      }
      else
      {
        temp->SetElement(it.Index(), p);
      }

      progress.CompletedPixel();
    }

    mesh->SetPoints(temp);

    if (this->m_DelaunayConforming)
    {
      mesh->DisconnectPipeline();
      m_OutputDelaunayFilter->SetInput(mesh);

      if (iter + 1 == m_NumberOfIterations)
      {
        m_OutputDelaunayFilter->GraftOutput(this->GetOutput());
        m_OutputDelaunayFilter->Update();
        this->GraftOutput(m_OutputDelaunayFilter->GetOutput());
      }
      else
      {
        m_OutputDelaunayFilter->Update();
        mesh = m_OutputDelaunayFilter->GetOutput();
      }
    }

    progress.CompletedPixel();

    if (iter + 1 == m_NumberOfIterations)
    {
      this->GraftOutput(mesh);
    }
  }
}

template <typename TInputMesh, typename TOutputMesh>
void
SmoothingQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfBooleanMacro(DelaunayConforming);
  os << indent << "NumberOfIterations: " << m_NumberOfIterations << std::endl;
  os << indent << "RelaxationFactor: " << m_RelaxationFactor << std::endl;
}
} // namespace itk

#endif

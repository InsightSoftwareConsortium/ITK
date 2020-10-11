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
#ifndef itkDecimationQuadEdgeMeshFilter_h
#define itkDecimationQuadEdgeMeshFilter_h

#include "itkIntTypes.h"
#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"

namespace itk
{
/**
 * \class DecimationQuadEdgeMeshFilter
 * \brief
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInput, typename TOutput, typename TCriterion>
class DecimationQuadEdgeMeshFilter : public QuadEdgeMeshToQuadEdgeMeshFilter<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DecimationQuadEdgeMeshFilter);

  using Self = DecimationQuadEdgeMeshFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = QuadEdgeMeshToQuadEdgeMeshFilter<TInput, TOutput>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(DecimationQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

  using InputMeshType = TInput;
  using InputMeshPointer = typename InputMeshType::Pointer;

  using OutputMeshType = TOutput;
  using OutputMeshPointer = typename OutputMeshType::Pointer;

  using CriterionType = TCriterion;
  using CriterionPointer = typename CriterionType::Pointer;
  using MeasureType = typename CriterionType::MeasureType;
  using PriorityType = typename CriterionType::PriorityType;
  using PriorityQueueItemType = typename CriterionType::PriorityQueueWrapperType;

  itkSetObjectMacro(Criterion, CriterionType);

protected:
  DecimationQuadEdgeMeshFilter()
  {
    this->m_Iteration = 0;
    this->m_OutputMesh = nullptr;
  }

  ~DecimationQuadEdgeMeshFilter() override = default;

  CriterionPointer m_Criterion;
  SizeValueType    m_Iteration;

  void
  GenerateData() override
  {
    this->CopyInputMeshToOutputMesh();

    Initialize();
    FillPriorityQueue();
    m_Iteration = 0;
    this->m_OutputMesh = this->GetOutput();
    do
    {
      this->Extract();

      if (ProcessWithTopologicalGuarantee())
      {
        return;
      }

      ++m_Iteration;
    } while (!IsCriterionSatisfied());

    this->GetOutput()->SqueezePointsIds();
    this->GetOutput()->DeleteUnusedCellData();
  }

  virtual void
  Initialize()
  {}
  virtual void
  FillPriorityQueue() = 0;

  virtual void
  Extract() = 0;

  virtual bool
  ProcessWithoutAnyTopologicalGuarantee() = 0;

  virtual bool
  ProcessWithTopologicalGuarantee() = 0;

  virtual bool
  IsCriterionSatisfied() = 0;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    this->Superclass::PrintSelf(os, indent);
    os << indent << "Criterion: " << m_Criterion << std::endl;
  }

  /** Cache pointer to output to use in inner loops */
  OutputMeshType * m_OutputMesh;
};
} // namespace itk

#endif

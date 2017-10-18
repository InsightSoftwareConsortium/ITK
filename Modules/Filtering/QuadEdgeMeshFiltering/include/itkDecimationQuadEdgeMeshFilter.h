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
template< typename TInput, typename TOutput, typename TCriterion >
class DecimationQuadEdgeMeshFilter:
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInput, TOutput >
{
public:
  typedef DecimationQuadEdgeMeshFilter                        Self;
  typedef SmartPointer< Self >                                Pointer;
  typedef SmartPointer< const Self >                          ConstPointer;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInput, TOutput > Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(DecimationQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

  typedef TInput                          InputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;

  typedef TOutput                          OutputMeshType;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;

  typedef TCriterion                                       CriterionType;
  typedef typename CriterionType::Pointer                  CriterionPointer;
  typedef typename CriterionType::MeasureType              MeasureType;
  typedef typename CriterionType::PriorityType             PriorityType;
  typedef typename CriterionType::PriorityQueueWrapperType PriorityQueueItemType;

  itkSetObjectMacro(Criterion, CriterionType);

protected:
  DecimationQuadEdgeMeshFilter()
  {
    this->m_Iteration = 0;
    this->m_OutputMesh = ITK_NULLPTR;
  }

  ~DecimationQuadEdgeMeshFilter() ITK_OVERRIDE {}

  CriterionPointer m_Criterion;
  SizeValueType    m_Iteration;

  void GenerateData() ITK_OVERRIDE
  {
    this->CopyInputMeshToOutputMesh();

    Initialize();
    FillPriorityQueue();
    m_Iteration = 0;
    this->m_OutputMesh = this->GetOutput();
    do
      {
      this->Extract();

      if ( ProcessWithTopologicalGuarantee() )
        {
        return;
        }

      ++m_Iteration;
      }
    while ( !IsCriterionSatisfied() );

    this->GetOutput()->SqueezePointsIds();
  }

  virtual void Initialize() {}
  virtual void FillPriorityQueue() = 0;

  virtual void Extract() = 0;

  virtual bool ProcessWithoutAnyTopologicalGuarantee() = 0;

  virtual bool ProcessWithTopologicalGuarantee() = 0;

  virtual bool IsCriterionSatisfied() = 0;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    this->Superclass::PrintSelf(os, indent);
    os << indent << "Criterion: " << m_Criterion << std::endl;
  }

  /** Cache pointer to output to use in inner loops */
  OutputMeshType *m_OutputMesh;

private:
  DecimationQuadEdgeMeshFilter(const Self &);
  void operator=(const Self &);
};
}

#endif

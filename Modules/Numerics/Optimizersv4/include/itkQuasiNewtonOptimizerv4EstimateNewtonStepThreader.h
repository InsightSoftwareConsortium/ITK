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
#ifndef itkQuasiNewtonOptimizerv4EstimateNewtonStepThreader_h
#define itkQuasiNewtonOptimizerv4EstimateNewtonStepThreader_h

#include "itkDomainThreader.h"
#include "itkThreadedIndexedContainerPartitioner.h"

namespace itk
{
template <typename TInternalComputationValueType>
class ITK_FORWARD_EXPORT QuasiNewtonOptimizerv4Template;

/**
 *\class QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate
 * \brief Estimate the quasi-Newton step in a thread.
 * \ingroup ITKOptimizersv4
 * */
template <typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate
  : public DomainThreader<ThreadedIndexedContainerPartitioner,
                          QuasiNewtonOptimizerv4Template<TInternalComputationValueType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate);

  /** Standard class type aliases. */
  using Self = QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate;
  using Superclass =
    DomainThreader<ThreadedIndexedContainerPartitioner, QuasiNewtonOptimizerv4Template<TInternalComputationValueType>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate, DomainThreader);

  itkNewMacro(Self);

  using DomainType = typename Superclass::DomainType;
  using AssociateType = typename Superclass::AssociateType;
  using IndexRangeType = DomainType;

protected:
  void
  ThreadedExecution(const IndexRangeType & subrange, const ThreadIdType threadId) override;

  QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate() = default;
  ~QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate() override = default;
};

/** This helps to meet backward compatibility */
using QuasiNewtonOptimizerv4EstimateNewtonStepThreader =
  QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate<double>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkQuasiNewtonOptimizerv4EstimateNewtonStepThreader.hxx"
#endif

#endif

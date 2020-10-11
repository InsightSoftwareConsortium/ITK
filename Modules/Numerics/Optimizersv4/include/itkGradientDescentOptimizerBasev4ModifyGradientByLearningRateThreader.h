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
#ifndef itkGradientDescentOptimizerBasev4ModifyGradientByLearningRateThreader_h
#define itkGradientDescentOptimizerBasev4ModifyGradientByLearningRateThreader_h

#include "itkDomainThreader.h"
#include "itkThreadedIndexedContainerPartitioner.h"

namespace itk
{
template <typename TInternalComputationValueType>
class ITK_FORWARD_EXPORT GradientDescentOptimizerBasev4Template;

/**
 *\class GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate
 * \brief Modify the gradient by the learning rate for
 * GradientDescentOptimizerBasev4.
 * \ingroup ITKOptimizersv4
 */

template <typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate
  : public DomainThreader<ThreadedIndexedContainerPartitioner,
                          GradientDescentOptimizerBasev4Template<TInternalComputationValueType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate);

  /** Standard class type aliases. */
  using Self = GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate;
  using Superclass = DomainThreader<ThreadedIndexedContainerPartitioner,
                                    GradientDescentOptimizerBasev4Template<TInternalComputationValueType>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate, DomainThreader);

  itkNewMacro(Self);

  using DomainType = typename Superclass::DomainType;
  using AssociateType = typename Superclass::AssociateType;
  using IndexRangeType = DomainType;

protected:
  void
  ThreadedExecution(const IndexRangeType & subrange, const ThreadIdType threadId) override;

  GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate() = default;
  ~GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate() override = default;
};

/** This helps to meet backward compatibility */
using GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreader =
  GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate<double>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGradientDescentOptimizerBasev4ModifyGradientByLearningRateThreader.hxx"
#endif

#endif

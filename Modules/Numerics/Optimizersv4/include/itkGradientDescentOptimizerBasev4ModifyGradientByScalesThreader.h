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
#ifndef itkGradientDescentOptimizerBasev4ModifyGradientByScalesThreader_h
#define itkGradientDescentOptimizerBasev4ModifyGradientByScalesThreader_h

#include "itkDomainThreader.h"
#include "itkThreadedIndexedContainerPartitioner.h"

namespace itk
{
template <typename TInternalComputationValueType>
class ITK_FORWARD_EXPORT GradientDescentOptimizerBasev4Template;

/**
 *\class GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate
 * \brief Modify the gradient by the parameter scales for
 * GradientDescentOptimizerBasev4.
 * \ingroup ITKOptimizersv4
 */

template <typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate
  : public DomainThreader<ThreadedIndexedContainerPartitioner,
                          GradientDescentOptimizerBasev4Template<TInternalComputationValueType>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate);

  /** Standard class type aliases. */
  using Self = GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate;
  using Superclass = DomainThreader<ThreadedIndexedContainerPartitioner,
                                    GradientDescentOptimizerBasev4Template<TInternalComputationValueType>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate, DomainThreader);

  itkNewMacro(Self);

  using DomainType = typename Superclass::DomainType;
  using AssociateType = typename Superclass::AssociateType;
  using IndexRangeType = DomainType;

protected:
  void
  ThreadedExecution(const IndexRangeType & subrange, const ThreadIdType threadId) override;

  GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate() = default;
  ~GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate() override = default;
};

/** This helps to meet backward compatibility */
using GradientDescentOptimizerBasev4ModifyGradientByScalesThreader =
  GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate<double>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGradientDescentOptimizerBasev4ModifyGradientByScalesThreader.hxx"
#endif

#endif

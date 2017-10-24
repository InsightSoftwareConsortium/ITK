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
#ifndef itkGradientDescentOptimizerBasev4ModifyGradientByLearningRateThreader_h
#define itkGradientDescentOptimizerBasev4ModifyGradientByLearningRateThreader_h

#include "itkDomainThreader.h"
#include "itkThreadedIndexedContainerPartitioner.h"

namespace itk
{
template<typename TInternalComputationValueType>
class ITK_FORWARD_EXPORT GradientDescentOptimizerBasev4Template;

/** \class GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate
 * \brief Modify the gradient by the learning rate for
 * GradientDescentOptimizerBasev4.
 * \ingroup ITKOptimizersv4
 */

template<typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate
  : public DomainThreader< ThreadedIndexedContainerPartitioner, GradientDescentOptimizerBasev4Template<TInternalComputationValueType> >
{
public:
  /** Standard class typedefs. */
  typedef GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate              Self;
  typedef DomainThreader< ThreadedIndexedContainerPartitioner,
                          GradientDescentOptimizerBasev4Template<TInternalComputationValueType> >  Superclass;
  typedef SmartPointer< Self >                                                                    Pointer;
  typedef SmartPointer< const Self >                                                              ConstPointer;

  itkTypeMacro( GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate, DomainThreader );

  itkNewMacro( Self );

  typedef typename Superclass::DomainType             DomainType;
  typedef typename Superclass::AssociateType          AssociateType;
  typedef DomainType                                  IndexRangeType;

protected:
  virtual void ThreadedExecution( const IndexRangeType & subrange,
                                  const ThreadIdType threadId ) ITK_OVERRIDE;

  GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate() {}
  virtual ~GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate);
};

/** This helps to meet backward compatibility */
typedef GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate<double> GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreader;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientDescentOptimizerBasev4ModifyGradientByLearningRateThreader.hxx"
#endif

#endif

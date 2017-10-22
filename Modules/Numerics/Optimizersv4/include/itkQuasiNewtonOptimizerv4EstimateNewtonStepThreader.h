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
#ifndef itkQuasiNewtonOptimizerv4EstimateNewtonStepThreader_h
#define itkQuasiNewtonOptimizerv4EstimateNewtonStepThreader_h

#include "itkDomainThreader.h"
#include "itkThreadedIndexedContainerPartitioner.h"

namespace itk
{
template<typename TInternalComputationValueType>
class ITK_FORWARD_EXPORT QuasiNewtonOptimizerv4Template;

/** \class QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate
 * \brief Estimate the quasi-Newton step in a thread.
 * \ingroup ITKOptimizersv4
 * */
template<typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate
  : public DomainThreader< ThreadedIndexedContainerPartitioner, QuasiNewtonOptimizerv4Template<TInternalComputationValueType> >
{
public:
  /** Standard class typedefs. */
  typedef QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate                                  Self;
  typedef DomainThreader< ThreadedIndexedContainerPartitioner, QuasiNewtonOptimizerv4Template<TInternalComputationValueType> >
                                                                                                    Superclass;
  typedef SmartPointer< Self >                                                                      Pointer;
  typedef SmartPointer< const Self >                                                                ConstPointer;

  itkTypeMacro( QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate, DomainThreader );

  itkNewMacro( Self );

  typedef typename Superclass::DomainType     DomainType;
  typedef typename Superclass::AssociateType  AssociateType;
  typedef DomainType                          IndexRangeType;

protected:
  virtual void ThreadedExecution( const IndexRangeType & subrange,
                                  const ThreadIdType threadId ) ITK_OVERRIDE;

  QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate() {}
  virtual ~QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate);
};

/** This helps to meet backward compatibility */
typedef QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate<double> QuasiNewtonOptimizerv4EstimateNewtonStepThreader;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuasiNewtonOptimizerv4EstimateNewtonStepThreader.hxx"
#endif

#endif

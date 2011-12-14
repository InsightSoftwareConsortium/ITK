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
#ifndef __itkGradientDescentOptimizerBasev4ModifyGradientByScalesThreader_h
#define __itkGradientDescentOptimizerBasev4ModifyGradientByScalesThreader_h

#include "itkDomainThreader.h"
#include "itkThreadedIndexedContainerPartitioner.h"

namespace itk
{

class GradientDescentOptimizerBasev4;

/** \class GradientDescentOptimizerBasev4ModifyGradientByScalesThreader
 * \brief Modify the gradient by the parameter scales for
 * GradientDescentOptimizerBasev4.
 * \ingroup ITKOptimizersv4
 */
class GradientDescentOptimizerBasev4ModifyGradientByScalesThreader
  : public DomainThreader< ThreadedIndexedContainerPartitioner, GradientDescentOptimizerBasev4 >
{
public:
  /** Standard class typedefs. */
  typedef GradientDescentOptimizerBasev4ModifyGradientByScalesThreader Self;
  typedef DomainThreader< ThreadedIndexedContainerPartitioner, GradientDescentOptimizerBasev4 >
                                                                           Superclass;
  typedef SmartPointer< Self >                                             Pointer;
  typedef SmartPointer< const Self >                                       ConstPointer;

  itkTypeMacro( GradientDescentOptimizerBasev4ModifyGradientByScalesThreader, DomainThreader );

  itkNewMacro( Self );

  typedef Superclass::DomainType    DomainType;
  typedef Superclass::AssociateType AssociateType;
  typedef DomainType                IndexRangeType;

protected:
  virtual void ThreadedExecution( const IndexRangeType & subrange,
                                  const ThreadIdType threadId );

  GradientDescentOptimizerBasev4ModifyGradientByScalesThreader() {}
  virtual ~GradientDescentOptimizerBasev4ModifyGradientByScalesThreader() {}

private:
  GradientDescentOptimizerBasev4ModifyGradientByScalesThreader( const Self & ); // purposely not implemented
  void operator=( const Self & ); // purposely not implemented
};

} // end namespace itk

#endif

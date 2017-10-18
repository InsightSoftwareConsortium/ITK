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
#ifndef itkCostFunction_h
#define itkCostFunction_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkArray.h"
#include "itkOptimizerParameters.h"

namespace itk
{
/** \class CostFunction
 * \brief Base class for cost functions intended to be used with Optimizers.
 *
 * \ingroup Numerics Optimizers
 *
 * \ingroup ITKOptimizers
 */
template< typename TInternalComputationValueType >
class ITK_TEMPLATE_EXPORT CostFunctionTemplate:public Object
{
public:
  /** Standard class typedefs. */
  typedef CostFunctionTemplate       Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CostFunctionTemplate, Object);

  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef TInternalComputationValueType                        ParametersValueType;
  typedef OptimizerParameters< TInternalComputationValueType > ParametersType;

  /** Return the number of parameters required to compute
   *  this cost function.
   *  This method MUST be overloaded by derived classes. */
  virtual unsigned int GetNumberOfParameters(void) const  = 0;

protected:
  CostFunctionTemplate() {}
  virtual ~CostFunctionTemplate() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CostFunctionTemplate);
};

/** This helps to meet backward compatibility */
typedef CostFunctionTemplate<double> CostFunction;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCostFunction.hxx"
#endif

#endif

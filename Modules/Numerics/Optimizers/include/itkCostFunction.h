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
template <typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT CostFunctionTemplate : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CostFunctionTemplate);

  /** Standard class type aliases. */
  using Self = CostFunctionTemplate;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CostFunctionTemplate, Object);

  /**  ParametersType type alias.
   *  It defines a position in the optimization search space. */
  using ParametersValueType = TInternalComputationValueType;
  using ParametersType = OptimizerParameters<TInternalComputationValueType>;

  /** Return the number of parameters required to compute
   *  this cost function.
   *  This method MUST be overloaded by derived classes. */
  virtual unsigned int
  GetNumberOfParameters() const = 0;

protected:
  CostFunctionTemplate() = default;
  ~CostFunctionTemplate() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};

/** This helps to meet backward compatibility */
using CostFunction = CostFunctionTemplate<double>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCostFunction.hxx"
#endif

#endif

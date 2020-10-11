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
#ifndef itkNonLinearOptimizer_h
#define itkNonLinearOptimizer_h

#include "itkOptimizer.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class NonLinearOptimizer
 * \brief Wrap of the vnl_nonlinear_minimizer to be adapted
 *
 * This class is provided to support the structure of an
 * Optimizers Hierarchy. It is not intended to be instantiated.
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT NonLinearOptimizer : public Optimizer

{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NonLinearOptimizer);

  /** Standard class type aliases. */
  using Self = NonLinearOptimizer;
  using Superclass = Optimizer;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NonLinearOptimizer, Optimizer);

  /**  Types inherited from the superclass */
  using ParametersType = Superclass::ParametersType;
  using ScalesType = Superclass::ScalesType;

protected:
  NonLinearOptimizer() = default;
  ~NonLinearOptimizer() override;
};
} // end namespace itk

#endif

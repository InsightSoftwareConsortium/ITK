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
#ifndef itkKernelFunctionBase_h
#define itkKernelFunctionBase_h

#include "itkFunctionBase.h"
#include "itkConceptChecking.h"
#include "itkNumericTraits.h"
#include "itkMath.h"

namespace itk
{
/** \class KernelFunctionBase
 * \brief Kernel used for density estimation and nonparameteric regression.
 *
 * This class encapsulates the smoothing kernel used for statistical density
 * estimation and nonparameteric regression. The basic idea of the kernel
 * approach is to weight observations by a smooth function (the kernel)
 * to created a smoothed approximation.
 *
 * Reference:
 * Silverman, B. W. (1986) Density Estimation. London: Chapman and Hall.
 *
 * \ingroup Functions
 * \ingroup ITKCommon
 */
template <typename TRealValueType = double>
class KernelFunctionBase : public FunctionBase<TRealValueType, TRealValueType>
{
public:
  /** Standard class type aliases. */
  using Self = KernelFunctionBase;
  using Superclass = FunctionBase<TRealValueType, TRealValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using RealType = TRealValueType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(KernelFunctionBase, FunctionBase);

  /** Evaluate the function. Subclasses must implement this. */
  TRealValueType
  Evaluate(const TRealValueType & u) const override = 0;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(TRealValueTypeIsFloatingPointCheck, (Concept::IsFloatingPoint<TRealValueType>));
  // End concept checking
#endif

protected:
  KernelFunctionBase() = default;
  ~KernelFunctionBase() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }
};
} // end namespace itk

#endif // itkKernelFunctionBase_h

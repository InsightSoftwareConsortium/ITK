/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
 * \brief Kernel used for density estimation and nonparametric regression.
 *
 * This class encapsulates the smoothing kernel used for statistical density
 * estimation and nonparametric regression. The basic idea of the kernel
 * approach is to weight observations by a smooth function (the kernel)
 * to create a smoothed approximation \cite silverman1986.
 *
 * \ingroup Functions
 * \ingroup ITKCommon
 */
template <typename TRealValueType = double>
class ITK_TEMPLATE_EXPORT KernelFunctionBase : public FunctionBase<TRealValueType, TRealValueType>
{
public:
  /** Standard class type aliases. */
  using Self = KernelFunctionBase;
  using Superclass = FunctionBase<TRealValueType, TRealValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using RealType = TRealValueType;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(KernelFunctionBase);

  /** Evaluate the function. Subclasses must implement this. */
  TRealValueType
  Evaluate(const TRealValueType & u) const override = 0;

  itkConceptMacro(TRealValueTypeIsFloatingPointCheck, (Concept::IsFloatingPoint<TRealValueType>));

protected:
  KernelFunctionBase() = default;
  ~KernelFunctionBase() override = default;
};
} // end namespace itk

#endif // itkKernelFunctionBase_h

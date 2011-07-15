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
#ifndef __itkKernelFunction_h
#define __itkKernelFunction_h

#include "itkFunctionBase.h"
#include "vnl/vnl_math.h"

namespace itk
{
/** \class KernelFunction
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
class ITKCommon_EXPORT KernelFunction:public FunctionBase< double, double >
{
public:
  /** Standard class typedefs. */
  typedef KernelFunction                 Self;
  typedef FunctionBase< double, double > Superclass;
  typedef SmartPointer< Self >           Pointer;
  typedef SmartPointer< const Self >     ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(KernelFunction, FunctionBase);

  /** Evaluate the function. Subclasses must implement this. */
  virtual double Evaluate(const double & u) const = 0;

protected:
  KernelFunction();
  ~KernelFunction();
  void PrintSelf(std::ostream & os, Indent indent) const
  { Superclass::PrintSelf(os, indent); }
};
} // end namespace itk

#endif

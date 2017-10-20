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
#ifndef itkKernelFunction_h
#define itkKernelFunction_h

#include "itkKernelFunctionBase.h"

#ifndef ITKV3_COMPATIBILITY
#error "This file is only valid when ITKV3_COMPATIBILITY is turned on. Users are encouraged to convert to itk::KernelFunctionBase in ITKv4"
#endif

namespace itk
{
/** \class KernelFunction
 * \brief KernelFunction is for backward compatibility with ITKv3
 *
 * \deprecated
 * \ingroup Functions
 * \ingroup ITKV3Compatibility
 */
class ITKCommon_EXPORT KernelFunction:public KernelFunctionBase< double >
{
public:
  /** Standard class typedefs. */
  typedef KernelFunction               Self;
  typedef KernelFunctionBase< double > Superclass;
  typedef SmartPointer< Self >         Pointer;
  typedef SmartPointer< const Self >   ConstPointer;

  typedef Superclass::RealType RealType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(KernelFunction, FunctionBase);

  /** Evaluate the function. Subclasses must implement this. */
  virtual RealType Evaluate(const RealType & u) const ITK_OVERRIDE = 0;

protected:
  KernelFunction() {};
  virtual ~KernelFunction() {};
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  { Superclass::PrintSelf(os, indent); }
};
} // end namespace itk

#endif // itkKernelFunction_h

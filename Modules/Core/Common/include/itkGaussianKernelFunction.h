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
#ifndef itkGaussianKernelFunction_h
#define itkGaussianKernelFunction_h

#include "itkKernelFunctionBase.h"
#include "itkMath.h"
#include <cmath>

namespace itk
{
/** \class GaussianKernelFunction
 * \brief Gaussian kernel used for density estimation and nonparameteric
 *  regression.
 *
 * This class encapsulates a Gaussian smoothing kernel for
 * density estimation or nonparameteric regression.
 * See documentation for KernelFunctionBase for more details.
 *
 * \sa KernelFunctionBase
 *
 * \ingroup Functions
 * \ingroup ITKCommon
 */
template< typename TRealValueType = double >
class GaussianKernelFunction:public KernelFunctionBase<TRealValueType>
{
public:
  /** Standard class typedefs. */
  typedef GaussianKernelFunction             Self;
  typedef KernelFunctionBase<TRealValueType> Superclass;
  typedef SmartPointer< Self >               Pointer;

  typedef typename Superclass::RealType  RealType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianKernelFunction, KernelFunctionBase);

  /** Evaluate the function. */
  TRealValueType Evaluate(const TRealValueType & u) const ITK_OVERRIDE
  { return ( std::exp( static_cast< TRealValueType >(-0.5) * itk::Math::sqr(u) ) * m_Factor ); }

protected:
  GaussianKernelFunction(): m_Factor(  NumericTraits< TRealValueType >::OneValue() / std::sqrt(static_cast< TRealValueType >(2.0 * itk::Math::pi )) ) {};
  virtual ~GaussianKernelFunction() ITK_OVERRIDE {};
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  { Superclass::PrintSelf(os, indent); }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GaussianKernelFunction);

  const TRealValueType m_Factor;
};
} // end namespace itk

#endif

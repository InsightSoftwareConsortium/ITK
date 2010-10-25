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
#ifndef __itkBSplineDerivativeKernelFunction_h
#define __itkBSplineDerivativeKernelFunction_h

#include "itkBSplineKernelFunction.h"

namespace itk
{
/** \class BSplineDerivativeKernelFunction
 * \brief Derivative of a BSpline kernel used for density estimation and
 *  nonparameteric regression.
 *
 * This class encapsulates the derivative of a BSpline kernel for
 * density estimation or nonparameteric regression.
 * See documentation for KernelFunction for more details.
 *
 * This class is templated over the spline order.
 * \warning Evaluate is only implemented for spline order 1 to 4
 *
 * \sa KernelFunction
 *
 * \ingroup Functions
 */
template< unsigned int VSplineOrder = 3 >
class ITK_EXPORT BSplineDerivativeKernelFunction:public KernelFunction
{
public:
  /** Standard class typedefs. */
  typedef BSplineDerivativeKernelFunction Self;
  typedef KernelFunction                  Superclass;
  typedef SmartPointer< Self >            Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineDerivativeKernelFunction, KernelFunction);

  /** Enum of for spline order. */
  itkStaticConstMacro(SplineOrder, unsigned int, VSplineOrder);

  /** Evaluate the function. */
  inline double Evaluate(const double & u) const
  {
    return ( m_KernelFunction->Evaluate(u + 0.5)
             - m_KernelFunction->Evaluate(u - 0.5) );
  }

protected:

  typedef BSplineKernelFunction< itkGetStaticConstMacro(SplineOrder) - 1 >
  KernelType;

  BSplineDerivativeKernelFunction()
  {
    m_KernelFunction = KernelType::New();
  }

  ~BSplineDerivativeKernelFunction(){}
  void PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    os << indent  << "Spline Order: " << SplineOrder << std::endl;
  }

private:
  BSplineDerivativeKernelFunction(const Self &); //purposely not implemented
  void operator=(const Self &);                  //purposely not implemented

  typename KernelType::Pointer m_KernelFunction;
};
} // end namespace itk

#endif

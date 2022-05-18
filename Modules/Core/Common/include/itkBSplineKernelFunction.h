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
#ifndef itkBSplineKernelFunction_h
#define itkBSplineKernelFunction_h

#include "itkKernelFunctionBase.h"
#include "itkMath.h"

namespace itk
{
/** \class BSplineKernelFunction
 * \brief BSpline kernel used for density estimation and nonparametric
 *  regression.
 *
 * This class encapsulates BSpline kernel for
 * density estimation or nonparametric regression.
 * See documentation for KernelFunctionBase for more details.
 *
 * This class is templated over the spline order.
 * \warning Evaluate is only implemented for spline order 0 to 3
 *
 * \sa KernelFunctionBase
 *
 * \ingroup Functions
 * \ingroup ITKCommon
 */
template <unsigned int VSplineOrder = 3, typename TRealValueType = double>
class ITK_TEMPLATE_EXPORT BSplineKernelFunction : public KernelFunctionBase<TRealValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BSplineKernelFunction);

  /** Standard class type aliases. */
  using Self = BSplineKernelFunction;
  using Superclass = KernelFunctionBase<TRealValueType>;
  using Pointer = SmartPointer<Self>;

  using typename Superclass::RealType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineKernelFunction, KernelFunctionBase);

  /** Enum of for spline order. */
  static constexpr unsigned int SplineOrder = VSplineOrder;

  /** Evaluate the function. Faster than the `Evaluate` member function, because it is static (while `Evaluate` is
   * virtual). */
  static TRealValueType
  FastEvaluate(const TRealValueType u)
  {
    return Self::Evaluate(Dispatch<VSplineOrder>(), u);
  }

  /** Evaluate the function. */
  TRealValueType
  Evaluate(const TRealValueType & u) const override
  {
    return Self::FastEvaluate(u);
  }

protected:
  BSplineKernelFunction() = default;
  ~BSplineKernelFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "Spline Order: " << SplineOrder << std::endl;
  }

private:
  /** Structures to control overloaded versions of Evaluate */
  struct DispatchBase
  {};
  template <unsigned int>
  struct Dispatch : public DispatchBase
  {};

  /** Zeroth order spline. */
  inline static TRealValueType
  Evaluate(const Dispatch<0> &, const TRealValueType & u)
  {
    const TRealValueType absValue = itk::Math::abs(u);
    if (absValue < TRealValueType{ 0.5 })
    {
      return TRealValueType{ 1.0 };
    }
    else if (Math::ExactlyEquals(absValue, TRealValueType{ 0.5 }))
    {
      return TRealValueType{ 0.5 };
    }
    else
    {
      return TRealValueType{ 0.0 };
    }
  }

  /** First order spline */
  inline static TRealValueType
  Evaluate(const Dispatch<1> &, const TRealValueType & u)
  {
    const TRealValueType absValue = itk::Math::abs(u);
    if (absValue < TRealValueType{ 1.0 })
    {
      return TRealValueType{ 1.0 } - absValue;
    }
    else
    {
      return TRealValueType{ 0.0 };
    }
  }

  /** Second order spline. */
  inline static TRealValueType
  Evaluate(const Dispatch<2> &, const TRealValueType & u)
  {
    const TRealValueType absValue = itk::Math::abs(u);
    if (absValue < TRealValueType{ 0.5 })
    {
      const TRealValueType sqrValue = itk::Math::sqr(absValue);
      return TRealValueType{ 0.75 } - sqrValue;
    }
    else if (absValue < TRealValueType{ 1.5 })
    {
      const TRealValueType sqrValue = itk::Math::sqr(absValue);
      // NOTE: 1.0/8.0 == 0.125
      return (TRealValueType{ 9.0 } - TRealValueType{ 12.0 } * absValue + TRealValueType{ 4.0 } * sqrValue) *
             TRealValueType{ 0.125 };
    }
    else
    {
      return TRealValueType{ 0.0 };
    }
  }

  /**  Third order spline. */
  inline static TRealValueType
  Evaluate(const Dispatch<3> &, const TRealValueType & u)
  {
    const TRealValueType absValue = itk::Math::abs(u);
    if (absValue < TRealValueType{ 1.0 })
    {
      const TRealValueType sqrValue = itk::Math::sqr(absValue);
      return (TRealValueType{ 4.0 } - TRealValueType{ 6.0 } * sqrValue + TRealValueType{ 3.0 } * sqrValue * absValue) /
             TRealValueType{ 6.0 };
    }
    else if (absValue < TRealValueType{ 2.0 })
    {
      const TRealValueType sqrValue = itk::Math::sqr(absValue);
      return (TRealValueType{ 8.0 } - TRealValueType{ 12.0 } * absValue + TRealValueType{ 6.0 } * sqrValue -
              sqrValue * absValue) /
             TRealValueType{ 6.0 };
    }
    else
    {
      return TRealValueType{ 0.0 };
    }
  }

  /** Unimplemented spline order */
  inline static TRealValueType
  Evaluate(const DispatchBase &, const TRealValueType &)
  {
    itkGenericExceptionMacro("Evaluate not implemented for spline order " << SplineOrder);
  }
};
} // end namespace itk

#endif

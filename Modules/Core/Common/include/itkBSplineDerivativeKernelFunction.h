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
#ifndef itkBSplineDerivativeKernelFunction_h
#define itkBSplineDerivativeKernelFunction_h

#include "itkBSplineKernelFunction.h"
#include "itkMath.h"

namespace itk
{
/** \class BSplineDerivativeKernelFunction
 * \brief Derivative of a BSpline kernel used for density estimation and
 *  nonparametric regression.
 *
 * This class encapsulates the derivative of a BSpline kernel for
 * density estimation or nonparametric regression.
 * See documentation for KernelFunctionBase for more details.
 *
 * This class is templated over the spline order.
 * \warning Evaluate is only implemented for spline order 1 to 4
 *
 * \sa KernelFunctionBase
 *
 * \ingroup Functions
 * \ingroup ITKCommon
 */
template <unsigned int VSplineOrder = 3, typename TRealValueType = double>
class ITK_TEMPLATE_EXPORT BSplineDerivativeKernelFunction : public KernelFunctionBase<TRealValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BSplineDerivativeKernelFunction);

  /** Standard class type aliases. */
  using Self = BSplineDerivativeKernelFunction;
  using Superclass = KernelFunctionBase<TRealValueType>;
  using Pointer = SmartPointer<Self>;

  using typename Superclass::RealType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineDerivativeKernelFunction, KernelFunctionBase);

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
  BSplineDerivativeKernelFunction() = default;
  ~BSplineDerivativeKernelFunction() override = default;

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

  /** Evaluate the function:  zeroth order spline. */
  inline static TRealValueType
  Evaluate(const Dispatch<0> &, const TRealValueType & itkNotUsed(u))
  {
    return TRealValueType{ 0.0 };
  }

  /** Evaluate the function:  first order spline */
  inline static TRealValueType
  Evaluate(const Dispatch<1> &, const TRealValueType & u)
  {
    if (Math::ExactlyEquals(u, TRealValueType{ -1.0 }))
    {
      return TRealValueType{ 0.5 };
    }
    else if ((u > TRealValueType{ -1.0 }) && (u < TRealValueType{ 0.0 }))
    {
      return TRealValueType{ 1.0 };
    }
    else if (Math::ExactlyEquals(u, TRealValueType{ 0.0 }))
    {
      return TRealValueType{ 0.0 };
    }
    else if ((u > TRealValueType{ 0.0 }) && (u < TRealValueType{ 1.0 }))
    {
      return TRealValueType{ -1.0 };
    }
    else if (Math::ExactlyEquals(u, TRealValueType{ 1.0 }))
    {
      return TRealValueType{ -0.5 };
    }
    else
    {
      return TRealValueType{ 0.0 };
    }
  }

  /** Evaluate the function:  second order spline. */
  inline static TRealValueType
  Evaluate(const Dispatch<2> &, const TRealValueType & u)
  {
    if ((u > TRealValueType{ -0.5 }) && (u < TRealValueType{ 0.5 }))
    {
      return (TRealValueType{ -2.0 } * u);
    }
    else if ((u >= TRealValueType{ 0.5 }) && (u < TRealValueType{ 1.5 }))
    {
      return (TRealValueType{ -1.5 } + u);
    }
    else if ((u > TRealValueType{ -1.5 }) && (u <= TRealValueType{ -0.5 }))
    {
      return (TRealValueType{ 1.5 } + u);
    }
    else
    {
      return TRealValueType{ 0.0 };
    }
  }

  /** Evaluate the function:  third order spline. */
  inline static TRealValueType
  Evaluate(const Dispatch<3> &, const TRealValueType & u)
  {
    if ((u >= TRealValueType{ 0.0 }) && (u < TRealValueType{ 1.0 }))
    {
      return (TRealValueType{ -2.0 } * u + TRealValueType{ 1.5 } * u * u);
    }
    else if ((u > TRealValueType{ -1.0 }) && (u < TRealValueType{ 0.0 }))
    {
      return (TRealValueType{ -2.0 } * u - TRealValueType{ 1.5 } * u * u);
    }
    else if ((u >= TRealValueType{ 1.0 }) && (u < TRealValueType{ 2.0 }))
    {
      return (TRealValueType{ -2.0 } + TRealValueType{ 2.0 } * u - TRealValueType{ 0.5 } * u * u);
    }
    else if ((u > TRealValueType{ -2.0 }) && (u <= TRealValueType{ -1.0 }))
    {
      return (TRealValueType{ 2.0 } + TRealValueType{ 2.0 } * u + TRealValueType{ 0.5 } * u * u);
    }
    else
    {
      return TRealValueType{ 0.0 };
    }
  }

  /** Evaluate the function:  unimplemented spline order */
  inline static TRealValueType
  Evaluate(const DispatchBase &, const TRealValueType &)
  {
    itkGenericExceptionMacro("Evaluate not implemented for spline order " << SplineOrder);
  }
};
} // end namespace itk

#endif

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
class BSplineDerivativeKernelFunction : public KernelFunctionBase<TRealValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BSplineDerivativeKernelFunction);

  /** Standard class type aliases. */
  using Self = BSplineDerivativeKernelFunction;
  using Superclass = KernelFunctionBase<TRealValueType>;
  using Pointer = SmartPointer<Self>;

  using RealType = typename Superclass::RealType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineDerivativeKernelFunction, KernelFunctionBase);

  /** Enum of for spline order. */
  static constexpr unsigned int SplineOrder = VSplineOrder;

  /** Evaluate the function. */
  TRealValueType
  Evaluate(const TRealValueType & u) const override
  {
    return this->Evaluate(Dispatch<VSplineOrder>(), u);
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
  inline TRealValueType
  Evaluate(const Dispatch<0> &, const TRealValueType & itkNotUsed(u)) const
  {
    return NumericTraits<TRealValueType>::ZeroValue();
  }

  /** Evaluate the function:  first order spline */
  inline TRealValueType
  Evaluate(const Dispatch<1> &, const TRealValueType & u) const
  {
    if (Math::ExactlyEquals(u, -NumericTraits<TRealValueType>::OneValue()))
    {
      return static_cast<TRealValueType>(0.5);
    }
    else if ((u > -NumericTraits<TRealValueType>::OneValue()) && (u < NumericTraits<TRealValueType>::ZeroValue()))
    {
      return NumericTraits<TRealValueType>::OneValue();
    }
    else if (Math::ExactlyEquals(u, NumericTraits<TRealValueType>::ZeroValue()))
    {
      return NumericTraits<TRealValueType>::ZeroValue();
    }
    else if ((u > NumericTraits<TRealValueType>::ZeroValue()) && (u < NumericTraits<TRealValueType>::OneValue()))
    {
      return -NumericTraits<TRealValueType>::OneValue();
    }
    else if (Math::ExactlyEquals(u, NumericTraits<TRealValueType>::OneValue()))
    {
      return static_cast<TRealValueType>(-0.5);
    }
    else
    {
      return NumericTraits<TRealValueType>::ZeroValue();
    }
  }

  /** Evaluate the function:  second order spline. */
  inline TRealValueType
  Evaluate(const Dispatch<2> &, const TRealValueType & u) const
  {
    if ((u > static_cast<TRealValueType>(-0.5)) && (u < static_cast<TRealValueType>(0.5)))
    {
      return (static_cast<TRealValueType>(-2.0) * u);
    }
    else if ((u >= static_cast<TRealValueType>(0.5)) && (u < static_cast<TRealValueType>(1.5)))
    {
      return (static_cast<TRealValueType>(-1.5) + u);
    }
    else if ((u > static_cast<TRealValueType>(-1.5)) && (u <= static_cast<TRealValueType>(-0.5)))
    {
      return (static_cast<TRealValueType>(1.5) + u);
    }
    else
    {
      return NumericTraits<TRealValueType>::ZeroValue();
    }
  }

  /** Evaluate the function:  third order spline. */
  inline TRealValueType
  Evaluate(const Dispatch<3> &, const TRealValueType & u) const
  {
    if ((u >= NumericTraits<TRealValueType>::ZeroValue()) && (u < NumericTraits<TRealValueType>::OneValue()))
    {
      return (static_cast<TRealValueType>(-2.0) * u + static_cast<TRealValueType>(1.5) * u * u);
    }
    else if ((u > -NumericTraits<TRealValueType>::OneValue()) && (u < NumericTraits<TRealValueType>::ZeroValue()))
    {
      return (static_cast<TRealValueType>(-2.0) * u - static_cast<TRealValueType>(1.5) * u * u);
    }
    else if ((u >= NumericTraits<TRealValueType>::OneValue()) && (u < static_cast<TRealValueType>(2.0)))
    {
      return (static_cast<TRealValueType>(-2.0) + static_cast<TRealValueType>(2.0) * u -
              static_cast<TRealValueType>(0.5) * u * u);
    }
    else if ((u > static_cast<TRealValueType>(-2.0)) && (u <= -NumericTraits<TRealValueType>::OneValue()))
    {
      return (static_cast<TRealValueType>(2.0) + static_cast<TRealValueType>(2.0) * u +
              static_cast<TRealValueType>(0.5) * u * u);
    }
    else
    {
      return NumericTraits<TRealValueType>::ZeroValue();
    }
  }

  /** Evaluate the function:  unimplemented spline order */
  inline TRealValueType
  Evaluate(const DispatchBase &, const TRealValueType &) const
  {
    itkExceptionMacro("Evaluate not implemented for spline order " << SplineOrder);
  }
};
} // end namespace itk

#endif

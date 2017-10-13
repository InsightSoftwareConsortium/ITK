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
#ifndef itkBSplineKernelFunction_h
#define itkBSplineKernelFunction_h

#include "itkKernelFunctionBase.h"
#include "itkMath.h"

namespace itk
{
/** \class BSplineKernelFunction
 * \brief BSpline kernel used for density estimation and nonparameteric
 *  regression.
 *
 * This class enscapsulates BSpline kernel for
 * density estimation or nonparameteric regression.
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
template< unsigned int VSplineOrder = 3, typename TRealValueType = double >
class ITK_TEMPLATE_EXPORT BSplineKernelFunction:public KernelFunctionBase<TRealValueType>
{
public:
  /** Standard class typedefs. */
  typedef BSplineKernelFunction              Self;
  typedef KernelFunctionBase<TRealValueType> Superclass;
  typedef SmartPointer< Self >               Pointer;

  typedef typename Superclass::RealType  RealType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineKernelFunction, KernelFunctionBase);

  /** Enum of for spline order. */
  itkStaticConstMacro(SplineOrder, unsigned int, VSplineOrder);

  /** Evaluate the function. */
  TRealValueType Evaluate(const TRealValueType & u) const ITK_OVERRIDE
  {
    return this->Evaluate(Dispatch< VSplineOrder >(), u);
  }

protected:
  BSplineKernelFunction(){}
  virtual ~BSplineKernelFunction() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);
    os << indent  << "Spline Order: " << SplineOrder << std::endl;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineKernelFunction);

  /** Structures to control overloaded versions of Evaluate */
  struct DispatchBase {};
  template< unsigned int >
  struct Dispatch: public DispatchBase {};

  /** Zeroth order spline. */
  inline TRealValueType Evaluate(const Dispatch< 0 > &, const TRealValueType & u) const
  {
    const TRealValueType absValue = itk::Math::abs(u);
    if ( absValue  < static_cast< TRealValueType >(0.5) )
      {
      return NumericTraits< TRealValueType >::OneValue();
      }
    else if ( Math::ExactlyEquals(absValue, static_cast< TRealValueType >(0.5)) )
      {
      return static_cast< TRealValueType >(0.5);
      }
    else
      {
      return NumericTraits< TRealValueType >::ZeroValue();
      }
  }

  /** First order spline */
  inline TRealValueType Evaluate(const Dispatch< 1 > &, const TRealValueType & u) const
  {
    const TRealValueType absValue = itk::Math::abs(u);
    if ( absValue  < NumericTraits< TRealValueType >::OneValue() )
      {
      return NumericTraits< TRealValueType >::OneValue() - absValue;
      }
    else
      {
      return NumericTraits< TRealValueType >::ZeroValue();
      }
  }

  /** Second order spline. */
  inline TRealValueType Evaluate(const Dispatch< 2 > &, const TRealValueType & u) const
  {
    const TRealValueType absValue = itk::Math::abs(u);
    if ( absValue  < static_cast< TRealValueType >(0.5) )
      {
      const TRealValueType sqrValue = itk::Math::sqr(absValue);
      return static_cast< TRealValueType >(0.75) - sqrValue;
      }
    else if ( absValue < static_cast< TRealValueType >(1.5) )
      {
      const TRealValueType sqrValue = itk::Math::sqr(absValue);
      // NOTE: 1.0/8.0 == static_cast< TRealValueType >( 0.125 )
      return ( static_cast< TRealValueType >(9.0) - static_cast< TRealValueType >(12.0) * absValue
        + static_cast< TRealValueType >(4.0) * sqrValue ) * static_cast< TRealValueType >(0.125);
      }
    else
      {
      return NumericTraits< TRealValueType >::ZeroValue();
      }
  }

  /**  Third order spline. */
  inline TRealValueType Evaluate(const Dispatch< 3 > &, const TRealValueType & u) const
  {
    const TRealValueType absValue = itk::Math::abs(u);
    if ( absValue  < NumericTraits< TRealValueType >::OneValue() )
      {
      const TRealValueType sqrValue = itk::Math::sqr(absValue);
      return ( static_cast< TRealValueType >(4.0) - static_cast< TRealValueType >(6.0) * sqrValue
        + static_cast< TRealValueType >(3.0) * sqrValue * absValue ) / static_cast< TRealValueType >(6.0);
      }
    else if ( absValue < static_cast< TRealValueType >(2.0) )
      {
      const TRealValueType sqrValue = itk::Math::sqr(absValue);
      return ( static_cast< TRealValueType >(8.0) - static_cast< TRealValueType >(12.0) * absValue + static_cast< TRealValueType >(6.0) * sqrValue
               - sqrValue * absValue ) / static_cast< TRealValueType >(6.0);
      }
    else
      {
      return NumericTraits< TRealValueType >::ZeroValue();
      }
  }

  /** Unimplemented spline order */
  inline TRealValueType Evaluate(const DispatchBase &, const TRealValueType &) const
  {
    itkExceptionMacro( "Evaluate not implemented for spline order "
      << SplineOrder);
    return NumericTraits< TRealValueType >::ZeroValue(); // This is to avoid compiler warning about missing
                // return statement.  It should never be evaluated.
  }
};
} // end namespace itk

#endif

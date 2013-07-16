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
template< unsigned int VSplineOrder = 3, typename TRealValueType = double >
class BSplineDerivativeKernelFunction:public KernelFunctionBase<TRealValueType>
{
public:
  /** Standard class typedefs. */
  typedef BSplineDerivativeKernelFunction    Self;
  typedef KernelFunctionBase<TRealValueType> Superclass;
  typedef SmartPointer< Self >               Pointer;

  typedef typename Superclass::RealType  RealType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineDerivativeKernelFunction, KernelFunctionBase);

  /** Enum of for spline order. */
  itkStaticConstMacro(SplineOrder, unsigned int, VSplineOrder);

  /** Evaluate the function. */
  inline TRealValueType Evaluate( const TRealValueType & u ) const
    {
    return this->Evaluate( Dispatch< VSplineOrder >(), u );
    }

protected:
  BSplineDerivativeKernelFunction() {}
  virtual ~BSplineDerivativeKernelFunction(){}

  void PrintSelf(std::ostream & os, Indent indent) const
    {
    Superclass::PrintSelf(os, indent);
    os << indent  << "Spline Order: " << SplineOrder << std::endl;
    }

private:
  BSplineDerivativeKernelFunction(const Self &); //purposely not implemented
  void operator=(const Self &);                  //purposely not implemented

  /** Structures to control overloaded versions of Evaluate */
  struct DispatchBase {};
  template< unsigned int >
  struct Dispatch: public DispatchBase {};

  /** Evaluate the function:  zeroth order spline. */
  inline TRealValueType Evaluate( const Dispatch<0>&, const TRealValueType & itkNotUsed( u ) )
    const
    {
    return NumericTraits< TRealValueType >::Zero;
    }

  /** Evaluate the function:  first order spline */
  inline TRealValueType Evaluate( const Dispatch<1>&, const TRealValueType& u ) const
    {
    if( u == -NumericTraits< TRealValueType >::One )
      {
      return static_cast< TRealValueType >(0.5);
      }
    else if( ( u > -NumericTraits< TRealValueType >::One ) && ( u < NumericTraits< TRealValueType >::Zero ) )
      {
      return NumericTraits< TRealValueType >::One;
      }
    else if( u == NumericTraits< TRealValueType >::Zero )
      {
      return NumericTraits< TRealValueType >::Zero;
      }
    else if( ( u > NumericTraits< TRealValueType >::Zero ) && ( u < NumericTraits< TRealValueType >::One ) )
      {
      return -NumericTraits< TRealValueType >::One;
      }
    else if( u == NumericTraits< TRealValueType >::One )
      {
      return static_cast< TRealValueType >(-0.5);
      }
    else
      {
      return NumericTraits< TRealValueType >::Zero;
      }
    }

  /** Evaluate the function:  second order spline. */
  inline TRealValueType Evaluate( const Dispatch<2>&, const TRealValueType& u) const
    {
    if( ( u > static_cast< TRealValueType >(-0.5) ) && ( u < static_cast< TRealValueType >(0.5) ) )
      {
      return ( static_cast< TRealValueType >(-2.0) * u );
      }
    else if( ( u >= static_cast< TRealValueType >(0.5) ) && ( u < static_cast< TRealValueType >(1.5) ) )
      {
      return ( static_cast< TRealValueType >(-1.5) + u );
      }
    else if( ( u > static_cast< TRealValueType >(-1.5) ) && ( u <= static_cast< TRealValueType >(-0.5) ) )
      {
      return ( static_cast< TRealValueType >(1.5) + u );
      }
    else
      {
      return NumericTraits< TRealValueType >::Zero;
      }
    }

  /** Evaluate the function:  third order spline. */
  inline TRealValueType Evaluate( const Dispatch<3>&, const TRealValueType& u ) const
    {
    if( ( u >= NumericTraits< TRealValueType >::Zero ) && ( u < NumericTraits< TRealValueType >::One ) )
      {
      return ( static_cast< TRealValueType >(-2.0)* u + static_cast< TRealValueType >(1.5) * u * u );
      }
    else if( ( u > -NumericTraits< TRealValueType >::One ) && ( u < NumericTraits< TRealValueType >::Zero ) )
      {
      return ( static_cast< TRealValueType >(-2.0) * u - static_cast< TRealValueType >(1.5) * u * u );
      }
    else if( ( u >= NumericTraits< TRealValueType >::One ) && ( u < static_cast< TRealValueType >(2.0) ) )
      {
      return ( static_cast< TRealValueType >(-2.0) + static_cast< TRealValueType >(2.0) * u - static_cast< TRealValueType >(0.5) * u * u );
      }
    else if( ( u > static_cast< TRealValueType >(-2.0) ) && ( u <= -NumericTraits< TRealValueType >::One ) )
      {
      return ( static_cast< TRealValueType >(2.0) + static_cast< TRealValueType >(2.0) * u + static_cast< TRealValueType >(0.5) * u * u );
      }
    else
      {
      return NumericTraits< TRealValueType >::Zero;
      }
    }

  /** Evaluate the function:  unimplemented spline order */
  inline TRealValueType Evaluate( const DispatchBase&, const TRealValueType& ) const
    {
    itkExceptionMacro( "Evaluate not implemented for spline order " << SplineOrder );
    return NumericTraits< TRealValueType >::Zero; // This is to avoid compiler warning about missing
    // return statement. It should never be evaluated.
    }
};
} // end namespace itk

#endif

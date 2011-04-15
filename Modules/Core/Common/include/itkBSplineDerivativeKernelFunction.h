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
 * \ingroup ITK-Common
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
  inline double Evaluate( const double & u ) const
    {
    return this->Evaluate( Dispatch< VSplineOrder >(), u );
    }

protected:
  BSplineDerivativeKernelFunction() {}
  ~BSplineDerivativeKernelFunction(){}

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
  struct Dispatch: DispatchBase {};

  /** Evaluate the function:  zeroth order spline. */
  inline double Evaluate( const Dispatch<0>&, const double & itkNotUsed( u ) )
    const
    {
    return 0.0;
    }

  /** Evaluate the function:  first order spline */
  inline double Evaluate( const Dispatch<1>&, const double& u ) const
    {
    if( u == -1.0 )
      {
      return 0.5;
      }
    else if( ( u > -1.0 ) && ( u < 0.0 ) )
      {
      return 1.0;
      }
    else if( u == 0.0 )
      {
      return 0.0;
      }
    else if( ( u > 0.0 ) && ( u < 1.0 ) )
      {
      return -1.0;
      }
    else if( u == 1.0 )
      {
      return -0.5;
      }
    else
      {
      return 0.0;
      }
    }

  /** Evaluate the function:  second order spline. */
  inline double Evaluate( const Dispatch<2>&, const double& u) const
    {
    if( ( u > -0.5 ) && ( u < 0.5 ) )
      {
      return ( -2.0 * u );
      }
    else if( ( u >= 0.5 ) && ( u < 1.5 ) )
      {
      return ( -1.5 + u );
      }
    else if( ( u > -1.5 ) && ( u <= -0.5 ) )
      {
      return ( 1.5 + u );
      }
    else
      {
      return 0.0;
      }
    }

  /** Evaluate the function:  third order spline. */
  inline double Evaluate( const Dispatch<3>&, const double& u ) const
    {
    if( ( u >= 0.0 ) && ( u < 1.0 ) )
      {
      return ( -2.0* u + 1.5 * u * u );
      }
    else if( ( u > -1.0 ) && ( u < 0.0 ) )
      {
      return ( -2.0 * u - 1.5 * u * u );
      }
    else if( ( u >= 1.0 ) && ( u < 2.0 ) )
      {
      return ( -2.0 + 2.0 * u - 0.5 * u * u );
      }
    else if( ( u > -2.0 ) && ( u <= -1.0 ) )
      {
      return ( 2.0 + 2.0 * u + 0.5 * u * u );
      }
    else
      {
      return 0.0;
      }
    }

  /** Evaluate the function:  unimplemented spline order */
  inline double Evaluate( const DispatchBase&, const double& ) const
    {
    itkExceptionMacro( "Evaluate not implemented for spline order "
      << SplineOrder );
    return 0.0; // This is to avoid compiler warning about missing
    // return statement. It should never be evaluated.
    }
};
} // end namespace itk

#endif

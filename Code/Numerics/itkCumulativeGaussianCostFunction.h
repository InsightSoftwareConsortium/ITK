/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCumulativeGaussianCostFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCumulativeGaussianCostFunction_h
#define __itkCumulativeGaussianCostFunction_h

#include "itkMultipleValuedCostFunction.h"

namespace itk
{

/** \class CumulativeGaussianCostFunction
* \brief Cost function for the Cumulative Gaussian Optimizer.
*
* The Cumulative Gaussian is defined as the integral of
* a normalized Gaussian over the domain \f$ [-\infty, \infty] \f$. 
*
* \par Let G(x) be the normalized Gaussian defined as
* \f$ G(x) = \frac{1}{{\sigma \sqrt {2\pi } }}e^{ - \frac{{\left( {x -
* \mu } \right)^2 }}{{2\sigma ^2 }}} \f$.
* The Cumulative Gaussian, is acquired by integrating G(x) then scaling and
* offseting it by the lower asymptotes \f$ I_1 and upper \f$ I_2 \f$ \f$: 
* \f$ C\left( x \right) = I_1  + \frac{{I_2  - I_1 }}{2}\left( {1 +
* erf\left( {\frac{{x - \mu }}{{\sigma \sqrt 2 }}} \right)} \right) \f$, where
* \f$ C\left( { - \infty } \right) = I_1 \f$ and 
* \f$ C\left( \infty  \right) = I_2 \f$.
* 
* \par C(x) can only be tabulated since it's a variation of the
* error function. It is included in this class
* as the function EvaluateCumulativeGaussian, where the argument
* of the function is \f$ {\frac{{x - \mu }}{{\sigma \sqrt 2 }}} \f$.
*
* \ingroup Numerics Cost Functions
*/

class CumulativeGaussianCostFunction : public MultipleValuedCostFunction
{
public:

  /** Standard typedefs. */
  typedef CumulativeGaussianCostFunction    Self;
  typedef MultipleValuedCostFunction        Superclass;
  typedef SmartPointer<Self>                Pointer;
  typedef SmartPointer<const Self>          ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Array Typedefs. */
  typedef Superclass::ParametersType        ParametersType;
  typedef Superclass::MeasureType           MeasureType;
  typedef Superclass::DerivativeType        DerivativeType;

  /** The dimensions of parameter space; mean, standard deviation, lower and upper asymptotes. */
  enum {SpaceDimension = 4};

  /** Not necessary for this optimizer. */
  void GetDerivative( const ParametersType & parameters, DerivativeType & derivative) const {};

  /** Return the values evaluated for the given parameters. */
  MeasureType GetValue( const ParametersType & parameters ) const;
  
  /** Return a pointer of values evaluated for the given parameters. */
  MeasureType * GetValue( ParametersType & parameters );

  /** Calculate a fit error between the data and the fit curve. */
  double CalculateFitError(MeasureType * setTestArray);

  /** Given the argument of a Cumulative Gaussian, return its value. */
  double EvaluateCumulativeGaussian(double argument) const;

  /** Get the SpaceDimension. */
  unsigned int GetNumberOfParameters() const;

  /** Get the number Range Dimension. */
  unsigned int GetNumberOfValues() const;

  /** Initialize the arrays. */
  void Initialize(unsigned int rangeDimension);

  /** Set the original data array. */
  void SetOriginalDataArray(MeasureType * setOriginalDataArray);

protected:
  CumulativeGaussianCostFunction();
  virtual ~CumulativeGaussianCostFunction();

  void PrintSelf(std::ostream &os, Indent indent) const;

private:
 
  /** Pointer to the original data array. */
  MeasureType * m_OriginalDataArray;

  /** Number of data samples. */
  unsigned int m_RangeDimension;

  /** Different arrays. */
  mutable MeasureType m_Measure;
  mutable MeasureType * m_MeasurePointer;
  mutable ParametersType m_Parameters;
};

} // end namespace itk

#endif

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularStepGradientDescentOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegularStepGradientDescentOptimizer_h
#define __itkRegularStepGradientDescentOptimizer_h

#include "itkRegularStepGradientDescentBaseOptimizer.h"

namespace itk
{
  
/** \class RegularStepGradientDescentOptimizer
 * \brief Implement a gradient descent optimizer
 *
 * \ingroup Numerics  Optimizers
 *
 */
template <class TCostFunction>
class ITK_EXPORT RegularStepGradientDescentOptimizer : 
        public RegularStepGradientDescentBaseOptimizer< TCostFunction >
{
public:
  /** Standard class typedefs. */
  typedef RegularStepGradientDescentOptimizer  Self;
  typedef RegularStepGradientDescentBaseOptimizer<TCostFunction> Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( RegularStepGradientDescentOptimizer, 
                RegularStepGradientDescentBaseOptimizer );

  /** Cost function typedefs. */
  typedef          TCostFunction                CostFunctionType;
  typedef typename CostFunctionType::Pointer    CostFunctionPointer;
  
  /** Dimension of the Search Space */
  enum { SpaceDimension = Superclass::SpaceDimension };

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef typename Superclass::ParametersType ParametersType;

  /**  Measure type.
   *  It defines a type used to return the cost function value. */
  typedef typename Superclass::MeasureType MeasureType;

  /**  Derivative type.
   *  It defines a type used to return the cost function derivative.  */
  typedef typename Superclass::DerivativeType DerivativeType;

protected:
  RegularStepGradientDescentOptimizer() {};
  virtual ~RegularStepGradientDescentOptimizer() {};

  /** Advance one step along the corrected gradient taking into
   * account the steplength represented by factor.
   * This method is invoked by AdvanceOneStep. It is expected
   * to be overrided by optimization methods in non-vector spaces
   * \sa AdvanceOneStep */
  virtual void StepAlongGradient( 
                  double factor, 
                  const DerivativeType & transformedGradient );

private:
  RegularStepGradientDescentOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegularStepGradientDescentOptimizer.txx"
#endif

#endif




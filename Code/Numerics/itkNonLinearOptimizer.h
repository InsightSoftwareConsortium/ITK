/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNonLinearOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNonLinearOptimizer_h
#define __itkNonLinearOptimizer_h

#include "itkOptimizer.h"


namespace itk
{
  
/** \class NonLinearOptimizer
 * \brief Wrap of the vnl_nonlinear_minimizer to be adapted 
 *
 * \ingroup Numerics Optimizers
 */
template <class TCostFunction>
class ITK_EXPORT NonLinearOptimizer : public Optimizer<TCostFunction>

{
public:
  /** Standard class typedefs. */
  typedef NonLinearOptimizer  Self;
  typedef  Optimizer<TCostFunction>   Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( NonLinearOptimizer, Optimizer );

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef typename TCostFunction::ParametersType ParametersType;

  /**  Measure type.
   *  It defines a type used to return the cost function value. */
  typedef typename TCostFunction::MeasureType MeasureType;

  /**  Derivative type.
   *  It defines a type used to return the cost function derivative. */
  typedef typename TCostFunction::DerivativeType DerivativeType;

protected:
  NonLinearOptimizer() {};
  virtual ~NonLinearOptimizer() {};

private:
  NonLinearOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk


#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConjugateGradientOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConjugateGradientOptimizer_h
#define __itkConjugateGradientOptimizer_h

#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_conjugate_gradient.h"

namespace itk
{
  
/** \class ConjugateGradientOptimizer
 * \brief Wrap of the vnl_conjugate_gradient 
 *
 * \ingroup Numerics Optimizers
 */
template <class TCostFunction>
class ITK_EXPORT ConjugateGradientOptimizer : 
        public SingleValuedNonLinearVnlOptimizer< TCostFunction >

{
public:
  /** Standard class typedefs. */
  typedef ConjugateGradientOptimizer  Self;
  typedef SingleValuedNonLinearVnlOptimizer<TCostFunction> Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( ConjugateGradientOptimizer, SingleValuedNonLinearOptimizer );

  /** Internal Optimizer Type */
  typedef   vnl_conjugate_gradient InternalOptimizerType;

  /**  Parameters type.
   *  it defines a position in the optimization search space */
  typedef typename TCostFunction::ParametersType ParametersType;

  /**  Measure type.
   *  it defines a type used to return the cost function value  */
  typedef typename TCostFunction::MeasureType MeasureType;

  /**  Derivative type.
   *  it defines a type used to return the cost function derivative  */
  typedef typename TCostFunction::DerivativeType DerivativeType;

  /** Method for getting access to the internal optimizer */
  vnl_conjugate_gradient & GetOptimizer(void);

  /** Start optimization with an initial value. */
  void StartOptimization( void );

protected:
  ConjugateGradientOptimizer();
  virtual ~ConjugateGradientOptimizer() {};

private:
  ConjugateGradientOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  /**  The vnl optimization method for conjugate gradient. */
  InternalOptimizerType     m_ConjugateGradient;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConjugateGradientOptimizer.txx"
#endif

#endif




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
class ITK_EXPORT ConjugateGradientOptimizer : 
    public SingleValuedNonLinearVnlOptimizer

{
public:
  /** Standard class typedefs. */
  typedef ConjugateGradientOptimizer          Self;
  typedef SingleValuedNonLinearVnlOptimizer   Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( ConjugateGradientOptimizer, SingleValuedNonLinearOptimizer );

  /** InternalParameters typedef. */
  typedef   vnl_vector<double>     InternalParametersType;

  /** Internal Optimizer Type */
  typedef   vnl_conjugate_gradient InternalOptimizerType;

  /** Method for getting access to the internal optimizer */
  vnl_conjugate_gradient * GetOptimizer(void);

  /** Start optimization with an initial value. */
  void StartOptimization( void );

  /** Plug in a Cost Function into the optimizer  */
  virtual void SetCostFunction( SingleValuedCostFunction * costFunction );

  /** Return the number of iterations performed so far */
  unsigned long GetNumberOfIterations(void) const;
  unsigned long GetCurrentIteration(void) const;


protected:
  ConjugateGradientOptimizer();
  virtual ~ConjugateGradientOptimizer();

  typedef Superclass::CostFunctionAdaptorType   CostFunctionAdaptorType;

private:
  ConjugateGradientOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  /**  The vnl optimization method for conjugate gradient. */
  bool                         m_OptimizerInitialized;
  InternalOptimizerType      * m_VnlOptimizer;

};

} // end namespace itk


#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLBFGSOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLBFGSOptimizer_h
#define __itkLBFGSOptimizer_h

#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_lbfgs.h"

namespace itk
{
  
/** \class LBFGSOptimizer
 * \brief Wrap of the vnl_lbfgs minimizer to be adapted for Registration
 *
 * \ingroup Numerics Optimizers
 */
template <class TCostFunction>
class ITK_EXPORT LBFGSOptimizer : 
    public SingleValuedNonLinearVnlOptimizer<TCostFunction> 
{
public:
  /** Standard class typedefs. */
  typedef LBFGSOptimizer  Self;
  typedef   SingleValuedNonLinearOptimizer<TCostFunction> Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( LBFGSOptimizer, SingleValuedNonLinearOptimizer );

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef typename TCostFunction::ParametersType ParametersType;

  /**  Measure type.
   *  It defines a type used to return the cost function value. */
  typedef typename TCostFunction::MeasureType MeasureType;

  /**  Derivative type.
   *  It defines a type used to return the cost function derivative.  */
  typedef typename TCostFunction::DerivativeType DerivativeType;
 
  /** Internal optimizer type. */
  typedef   vnl_lbfgs       InternalOptimizerType;

  /** Method for getting access to the internal optimizer. */
  InternalOptimizerType & GetOptimizer(void);

  /** Start optimization with an initial value. */
  void StartOptimization( void );
 
protected:
  LBFGSOptimizer();
  virtual ~LBFGSOptimizer() {};

private:
  LBFGSOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  InternalOptimizerType     m_LBFGS;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLBFGSOptimizer.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAmoebaOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAmoebaOptimizer_h
#define __itkAmoebaOptimizer_h

#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_amoeba.h"
#include "vnl/vnl_cost_function.h"
#include "itkExceptionObject.h"

namespace itk
{
  
/** \class AmoebaOptimizer
 * \brief Wrap of the vnl_amoeba algorithm
 *
 * \ingroup Numerics Optimizers
 */
template <class TCostFunction>
class ITK_EXPORT AmoebaOptimizer : 
    public SingleValuedNonLinearVnlOptimizer<TCostFunction>

{
public:
  /** Standard "Self" typedef. */
  typedef AmoebaOptimizer  Self;
  typedef SingleValuedNonLinearVnlOptimizer<TCostFunction> Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( AmoebaOptimizer, NonLinearOptimizer );

  /** InternalParameters typedef. */
  typedef   vnl_vector<double>     InternalParametersType;

  /** InternalMeasure typedef. */
  typedef   vnl_vector<double>     InternalMeasureType;

  /** InternalGradient typedef. */
  typedef   vnl_matrix<double>     InternalDerivativeType;

  /** ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef typename TCostFunction::ParametersType    ParametersType;

  /** MeasureType typedef.
   *  It defines a type used to return the cost function value.  */
  typedef typename TCostFunction::MeasureType         MeasureType;

  /** GradientType typedef.
   *  It defines a type used to return the cost function derivative. */
  typedef typename TCostFunction::DerivativeType      DerivativeType;

  /** Internal optimizer type. */
  typedef   vnl_amoeba     InternalOptimizerType;

  /** Vnl cost function adaptor type. */
  typedef typename Superclass::VnlCostFunctionAdaptor     
                                   VnlCostFunctionAdaptorType;

  /** Method for getting access to the internal optimizer. */
  vnl_amoeba & GetOptimizer(void);

  /** Start optimization with an initial value. */
  void StartOptimization( void );

protected:
  AmoebaOptimizer();
  virtual ~AmoebaOptimizer() {};

private:
  AmoebaOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  InternalOptimizerType             m_Amoeba;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAmoebaOptimizer.txx"
#endif

#endif




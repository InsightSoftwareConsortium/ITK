/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevenbergMarquardtOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLevenbergMarquardtOptimizer_h
#define __itkLevenbergMarquardtOptimizer_h

#include "itkMultipleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_levenberg_marquardt.h"

namespace itk
{
  
/** \class LevenbergMarquardtOptimizer
 * \brief Wrap of the vnl_levenberg_marquardt 
 *
 * \ingroup Numerics Optimizers
 */
template <class TCostFunction>
class ITK_EXPORT LevenbergMarquardtOptimizer : 
          public MultipleValuedNonLinearVnlOptimizer<TCostFunction> 
{
public:
  /** Standard class typedefs. */
  typedef LevenbergMarquardtOptimizer  Self;
  typedef   MultipleValuedNonLinearVnlOptimizer<TCostFunction> Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( LevenbergMarquardtOptimizer, NonLinearOptimizer );
  
  /** InternalParameters typedef. */
  typedef   vnl_vector<double>     InternalParametersType;

  /** InternalMeasure typedef. */
  typedef   vnl_vector<double>     InternalMeasureType;

  /** InternalGradient typedef. */
  typedef   vnl_matrix<double>     InternalDerivativeType;

  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef typename TCostFunction::ParametersType    ParametersType;

  /**  MeasureType typedef.
   *  It defines a type used to return the cost function value. */
  typedef typename TCostFunction::MeasureType         MeasureType;

  /**  GradientType typedef.
   *  It defines a type used to return the cost function derivative. */
  typedef typename TCostFunction::DerivativeType      DerivativeType;

  /** Internal optimizer type. */
  typedef   vnl_levenberg_marquardt InternalOptimizerType;

  /** Method for getting access to the internal optimizer. */
  InternalOptimizerType & GetOptimizer(void);

  /** Start optimization with an initial value. */
  void StartOptimization( void ); 

protected:
  LevenbergMarquardtOptimizer();
  virtual ~LevenbergMarquardtOptimizer() {};

private:
  LevenbergMarquardtOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  InternalOptimizerType     m_LevenbergMarquardt;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevenbergMarquardtOptimizer.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevenbergMarquardtOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkLevenbergMarquardtOptimizer_h
#define __itkLevenbergMarquardtOptimizer_h

#include "itkMultipleValuedNonLinearOptimizer.h"
#include "vnl/algo/vnl_levenberg_marquardt.h"

namespace itk
{
  
/** \class LevenbergMarquardtOptimizer
 * \brief Wrap of the vnl_levenberg_marquardt 
 *
 */

template <class TCostFunction>
class ITK_EXPORT LevenbergMarquardtOptimizer : 
          public MultipleValuedNonLinearOptimizer<TCostFunction> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef LevenbergMarquardtOptimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef   MultipleValuedNonLinearOptimizer<TCostFunction> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;



 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( LevenbergMarquardtOptimizer, 
      NonLinearOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * VectorType typedef.
   */
  typedef   vnl_vector<double>     VectorType;

  /**
   * Internal Optimizer Type
   */
  typedef   vnl_levenberg_marquardt InternalOptimizerType;

  /**
   * Method for getting access to the internal optimizer
   */
  InternalOptimizerType & GetOptimizer(void);

  /**
   * Start optimization with an initial value
   */
  void StartOptimization( VectorType & );
 
protected:

  LevenbergMarquardtOptimizer();
  virtual ~LevenbergMarquardtOptimizer() {};
  LevenbergMarquardtOptimizer(const Self&) {}
  void operator=(const Self&) {}

  InternalOptimizerType     m_LevenbergMarquardt;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevenbergMarquardtOptimizer.txx"
#endif

#endif




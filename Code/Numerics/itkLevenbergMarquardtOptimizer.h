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

#include "itkNonLinearOptimizer.h"
#include "vnl/algo/vnl_levenberg_marquardt.h"

namespace itk
{
  
/** \class LevenbergMarquardtOptimizer
 * \brief Wrap of the vnl_levenberg_marquardt to be adapted for Registration
 *
 */

template <class TMetric>
class ITK_EXPORT LevenbergMarquardtOptimizer : 
    public NonLinearOptimizer<TMetric> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef LevenbergMarquardtOptimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef   NonLinearOptimizer<TMetric> Superclass;

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
   * Method for getting access to the internal optimizer
   */
  vnl_levenberg_marquardt & GetOptimizer(void);


protected:

  LevenbergMarquardtOptimizer();
  virtual ~LevenbergMarquardtOptimizer() {};
  LevenbergMarquardtOptimizer(const Self&) {}
  void operator=(const Self&) {}

  vnl_levenberg_marquardt     m_LevenbergMarquardt;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevenbergMarquardtOptimizer.txx"
#endif

#endif




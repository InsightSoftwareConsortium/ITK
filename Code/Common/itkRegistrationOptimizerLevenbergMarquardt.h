/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationOptimizerLevenbergMarquardt.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegistrationOptimizerLevenbergMarquardt_h
#define __itkRegistrationOptimizerLevenbergMarquardt_h

#include "itkRegistrationOptimizerNonLinear.h"
#include "vnl/algo/vnl_levenberg_marquardt.h"

namespace itk
{
  
/** \class RegistrationOptimizerLevenbergMarquardt
 * \brief Wrap of the vnl_levenberg_marquardt to be adapted for Registration
 *
 */

template <class TMetric>
class ITK_EXPORT RegistrationOptimizerLevenbergMarquardt : 
    public RegistrationOptimizerNonLinear<TMetric> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationOptimizerLevenbergMarquardt  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef   RegistrationOptimizerNonLinear<TMetric> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;



 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( RegistrationOptimizerLevenbergMarquardt, 
      RegistrationOptimizerNonLinear );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method for getting access to the internal optimizer
   */
  vnl_levenberg_marquardt & GetOptimizer(void);


protected:

  RegistrationOptimizerLevenbergMarquardt();
  virtual ~RegistrationOptimizerLevenbergMarquardt() {};
  RegistrationOptimizerLevenbergMarquardt(const Self&) {}
  void operator=(const Self&) {}

  vnl_levenberg_marquardt     m_LevenbergMarquardt;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationOptimizerLevenbergMarquardt.txx"
#endif

#endif




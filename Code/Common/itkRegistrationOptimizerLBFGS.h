/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationOptimizerLBFGS.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegistrationOptimizerLBFGS_h
#define __itkRegistrationOptimizerLBFGS_h

#include "itkRegistrationOptimizerNonLinear.h"
#include "vnl/algo/vnl_lbfgs.h"

namespace itk
{
  
/** \class RegistrationOptimizerLBFGS
 * \brief Wrap of the vnl_lbfgs minimizer to be adapted for Registration
 *
 */

template <class TMetric>
class ITK_EXPORT RegistrationOptimizerLBFGS : 
    public RegistrationOptimizerNonLinear<TMetric> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationOptimizerLBFGS  Self;

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
  itkTypeMacro( RegistrationOptimizerLBFGS, 
      RegistrationOptimizerNonLinear );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method for getting access to the internal optimizer
   */
  vnl_lbfgs & GetOptimizer(void);


protected:

  RegistrationOptimizerLBFGS();
  virtual ~RegistrationOptimizerLBFGS() {};
  RegistrationOptimizerLBFGS(const Self&) {}
  void operator=(const Self&) {}

  vnl_lbfgs     m_LBFGS;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationOptimizerLBFGS.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationOptimizerConjugateGradient.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegistrationOptimizerConjugateGradient_h
#define __itkRegistrationOptimizerConjugateGradient_h

#include "itkRegistrationOptimizerNonLinear.h"
#include "vnl/algo/vnl_conjugate_gradient.h"

namespace itk
{
  
/** \class RegistrationOptimizerConjugateGradient
 * \brief Wrap of the vnl_conjugate_gradient to be adapted for Registration
 *
 */

template <class TMetric>
class ITK_EXPORT RegistrationOptimizerConjugateGradient : 
    public RegistrationOptimizerNonLinear<TMetric> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationOptimizerConjugateGradient  Self;

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
  itkTypeMacro( RegistrationOptimizerConjugateGradient, 
      RegistrationOptimizerNonLinear );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method for getting access to the internal optimizer
   */
  vnl_conjugate_gradient & GetOptimizer(void);


protected:

  RegistrationOptimizerConjugateGradient();
  virtual ~RegistrationOptimizerConjugateGradient() {};
  RegistrationOptimizerConjugateGradient(const Self&) {}
  void operator=(const Self&) {}

  vnl_conjugate_gradient     m_ConjugateGradient;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationOptimizerConjugateGradient.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationOptimizerAmoeba.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegistrationOptimizerAmoeba_h
#define __itkRegistrationOptimizerAmoeba_h

#include "itkRegistrationOptimizerNonLinear.h"
#include "vnl/algo/vnl_amoeba.h"

namespace itk
{
  
/** \class RegistrationOptimizerAmoeba
 * \brief Wrap of the vnl_amoeba to be adapted for Registration
 *
 */

template <class TMetric>
class ITK_EXPORT RegistrationOptimizerAmoeba : 
    public RegistrationOptimizerNonLinear<TMetric> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationOptimizerAmoeba  Self;

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
  itkTypeMacro( RegistrationOptimizerAmoeba, 
      RegistrationOptimizerNonLinear );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method for getting access to the internal optimizer
   */
  vnl_amoeba & GetOptimizer(void);


protected:

  RegistrationOptimizerAmoeba();
  virtual ~RegistrationOptimizerAmoeba() {};
  RegistrationOptimizerAmoeba(const Self&) {}
  void operator=(const Self&) {}

  vnl_amoeba     m_Amoeba;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationOptimizerAmoeba.txx"
#endif

#endif




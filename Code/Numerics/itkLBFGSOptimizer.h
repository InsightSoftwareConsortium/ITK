/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLBFGSOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkLBFGSOptimizer_h
#define __itkLBFGSOptimizer_h

#include "itkNonLinearOptimizer.h"
#include "vnl/algo/vnl_lbfgs.h"

namespace itk
{
  
/** \class LBFGSOptimizer
 * \brief Wrap of the vnl_lbfgs minimizer to be adapted for Registration
 *
 */

template <class TMetric>
class ITK_EXPORT LBFGSOptimizer : 
    public NonLinearOptimizer<TMetric> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef LBFGSOptimizer  Self;

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
  itkTypeMacro( LBFGSOptimizer, 
      NonLinearOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method for getting access to the internal optimizer
   */
  vnl_lbfgs & GetOptimizer(void);


protected:

  LBFGSOptimizer();
  virtual ~LBFGSOptimizer() {};
  LBFGSOptimizer(const Self&) {}
  void operator=(const Self&) {}

  vnl_lbfgs     m_LBFGS;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLBFGSOptimizer.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAmoebaOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAmoebaOptimizer_h
#define __itkAmoebaOptimizer_h

#include "itkNonLinearOptimizer.h"
#include "vnl/algo/vnl_amoeba.h"

namespace itk
{
  
/** \class AmoebaOptimizer
 * \brief Wrap of the vnl_amoeba to be adapted for Registration
 *
 */

template <class TMetric>
class ITK_EXPORT AmoebaOptimizer : 
    public NonLinearOptimizer<TMetric> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef AmoebaOptimizer  Self;

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
  itkTypeMacro( AmoebaOptimizer, 
      NonLinearOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method for getting access to the internal optimizer
   */
  vnl_amoeba & GetOptimizer(void);


protected:

  AmoebaOptimizer();
  virtual ~AmoebaOptimizer() {};
  AmoebaOptimizer(const Self&) {}
  void operator=(const Self&) {}

  vnl_amoeba     m_Amoeba;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAmoebaOptimizer.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegistrationOptimizer_h
#define __itkRegistrationOptimizer_h

#include "itkObject.h"

namespace itk
{
  
/** \class RegistrationOptimizer
 * \brief Generic representation for an optimization method intended to be
 * used in registration 
 *
 * This Class wraps an optimization method and provides to it the 
 * appropiated inputs comming from a RegistrationMetric
 * In this way, the registration methods don't need to be aware of
 * the type of objects that are being registered, so the same optimizer
 * can be reused for a variety of applications
 *
 */

template <class TMetric>
class ITK_EXPORT RegistrationOptimizer : public Object 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationOptimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef   Object  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;


  /**
   *  Type of the Reference
   */
  typedef TMetric             MetricType;


  /**
   *  Type of the Mapper
   */
  typedef typename MetricType::MapperType  MapperType;


  /**
   *  Type of the Transformation
   */
  typedef typename MapperType::TransformationType  TransformationType;  
  

  /**
   *  Pointer type for the Metric 
   */
  typedef typename MetricType::Pointer    MetricPointer;


  /**
   *  Pointer type for the Mapper 
   */
  typedef typename MapperType::Pointer    MapperPointer;



 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegistrationOptimizer, Object);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method for Start the optimization
   */
   virtual void StartOptimization(void);
  

  /**
   * Connect the Metric 
   */
   void SetMetric( MetricType * );



protected:

  MetricPointer               m_Metric;

  RegistrationOptimizer();
  virtual ~RegistrationOptimizer() {};
  RegistrationOptimizer(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationOptimizer.txx"
#endif

#endif




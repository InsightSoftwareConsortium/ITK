/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkOptimizer_h
#define __itkOptimizer_h

#include "itkObject.h"
#include "itkObjectFactory.h"


namespace itk
{
  
/** \class Optimizer
 * \brief Generic representation for an optimization method intended to be
 * used in registration 
 *
 * This Class wraps an optimization method and provides to it the 
 * appropiated inputs comming from a SimilarityRegistrationMetric
 * In this way, the registration methods don't need to be aware of
 * the type of objects that are being registered, so the same optimizer
 * can be reused for a variety of applications
 *
 */

template <class TMetric>
class ITK_EXPORT Optimizer : public Object 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Optimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef   Object  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


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
  itkTypeMacro(Optimizer, Object);


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

  Optimizer();
  virtual ~Optimizer() {};
  Optimizer(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOptimizer.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMethod.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegistrationMethod_h
#define __itkRegistrationMethod_h

#include "itkObject.h"

namespace itk
{

/**
 * \class RegistrationMethod
 * \brief Base class for registration methods
 *
 * This Class define the generic interface for a registration method.
 * The basic elements of a registration method are:
 *   - Metric to compare the reference and the target
 *   - Transformation used to register the reference against the target
 *   - Optimization method used to search for the best transformation
 * 
 * Registration is not limited to Images, and for this reason
 * this class is templated over the type of the reference object,
 * the target object and the transformation. This types are obtained
 * from the Metric type, to reduce the number of redundant
 * template parameters
 *
 */

template <class TMetric, class TOptimizer >
class ITK_EXPORT RegistrationMethod : public Object 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationMethod  Self;


  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;


  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   *  Type of the Metric
   */
  typedef TMetric               MetricType;


  /**
   *  Type of the Optimizer 
   */
  typedef TOptimizer            OptimizerType;


  /**
   *  Type of the Reference
   */
  typedef typename MetricType::ReferenceType  ReferenceType;


  /**
   *  Type of the Metric
   */
  typedef typename MetricType::TargetType  TargetType;
 

  /**
   *  Type of the Mapper
   */
  typedef typename MetricType::MapperType  MapperType;


  /**
   *  Type of the Transformation
   */
  typedef typename MapperType::TransformationType  TransformationType;
 
  
  /**
   *  Pointer type for the Reference 
   */
  typedef typename ReferenceType::Pointer ReferencePointer;

  
  /**
   *  Pointer type for the Target 
   */
  typedef typename TargetType::Pointer TargetPointer;


  /**
   *  Pointer type for the Transformation
   */
  typedef typename TransformationType::Pointer TransformationPointer;


  /**
   *  Pointer type for the metric
   */
  typedef typename MetricType::Pointer        MetricPointer;


  /**
   *  Pointer type for the mapper
   */
  typedef typename MapperType::Pointer        MapperPointer;


  /**
   *  Pointer type for the optimizer 
   */
  typedef typename OptimizerType::Pointer     OptimizerPointer;


 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegistrationMethod, Object);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method that initiates the registration.
   */
   void StartRegistration(void);


  /**
   * Set the Target
   */
   void SetTarget( TargetType * );

   
  /**
   * Set the Reference
   */
   void SetReference( ReferenceType * );


  /**
   * Set the Transformation
   */
   void SetTransformation( TransformationType * );


  /**
   * Get the Reference
   */
   itkGetMacro( Reference, ReferencePointer );

   
  /**
   * Get the Target
   */
   itkGetMacro( Target, TargetPointer );


  /**
   * Get the Transformation
   */
   itkGetMacro( Transformation, TransformationPointer );


  /**
   * Get the Optimizer
   */
   itkGetMacro( Optimizer, OptimizerPointer );




protected:

  RegistrationMethod();
  virtual ~RegistrationMethod();
  RegistrationMethod(const Self&);
  const Self & operator=(const Self&);


private:

  TargetPointer              m_Target;
  ReferencePointer           m_Reference;
  TransformationPointer      m_Transformation;
  MapperPointer              m_Mapper;  
  MetricPointer              m_Metric;
  OptimizerPointer           m_Optimizer;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationMethod.txx"
#endif

#endif




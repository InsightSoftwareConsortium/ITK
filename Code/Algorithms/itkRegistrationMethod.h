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

#include "itkProcessObject.h"

namespace itk
{

/**
 * \class RegistrationMethod
 * \brief Base class for Registration Methods
 *
 * This Class define the generic interface for a registration method.
 * The basic elements of a registration method are defined in a Traits
 * class
 * 
 */

template <class TTraits>
class ITK_EXPORT RegistrationMethod : public ProcessObject 
{
public:
  /**
   * Standard "Self" typedef.
   */
   typedef RegistrationMethod  Self;


  /**
   * Standard "Superclass" typedef.
   */
   typedef ProcessObject  Superclass;


  /** 
   * Smart pointer typedef support 
   */
   typedef SmartPointer<Self>   Pointer;
   typedef SmartPointer<const Self>  ConstPointer;

  /**
   *  Type of the Target
   */
   typedef typename TTraits::TargetType              TargetType;
   typedef typename TargetType::Pointer              TargetPointer;

  /**
   *  Type of the Metric
   */
   typedef typename TTraits::MetricType              MetricType;

  /**
   *  Type of the Mapper
   */
   typedef typename TTraits::MapperType              MapperType;

  /**
   *  Type of the Transformation
   */
   typedef typename TTraits::TransformationType TransformationType;


  /**
   *  Type of the Reference
   */
   typedef typename TTraits::ReferenceType         ReferenceType;
   typedef typename ReferenceType::Pointer         ReferencePointer;


  /**
   *  Type of the Optimizer
   */
   typedef typename TTraits::OptimizerType         OptimizerType;


  /**
   *  Type of the Transformation parameters
   *  This is the same type used to represent the search
   *  space of the optimization algorithm
   */
   typedef typename TTraits::ParametersType         ParametersType;


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
   int StartRegistration(void);


  /**
   * Set the Target
   */
   void SetTarget( TargetType * Target );


  /**
   * Set the Reference
   */
   void SetReference( ReferenceType * Reference );


  /**
   * Set the Optimizer
   */
   itkSetObjectMacro( Optimizer,  OptimizerType );


  /**
   * Set the Metric
   */
   itkSetObjectMacro( Metric, MetricType );


  /**
   * Get the Reference
   */
   ReferencePointer GetReference( void );
   

  /**
   * Get the Target
   */
   TargetPointer     GetTarget( void );


  /**
   * Get the Optimizer
   */
   itkGetObjectMacro( Optimizer, OptimizerType );


  /**
   * Get the Metric
   */
   itkGetObjectMacro( Metric, MetricType );


protected:

  RegistrationMethod();
  virtual ~RegistrationMethod();
  RegistrationMethod(const Self&);
  const Self & operator=(const Self&);
 

private:

  typename MetricType::Pointer              m_Metric;
  typename OptimizerType::Pointer           m_Optimizer;

};


} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationMethod.txx"
#endif

#endif




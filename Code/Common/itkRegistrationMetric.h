/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegistrationMetric_h
#define __itkRegistrationMetric_h

#include "itkObject.h"

namespace itk
{
  
/** \class RegistrationMetric
 * \brief Computes similarity between two objects to be registered
 *
 * This Class is templated over the type of the objects to be registered and
 * over the type of transformation to be used.
 *
 * SmartPointer to this three objects are received, and using them, this
 * class computes a value(s) that measures the similarity of the target
 * against the reference object once the transformation is applied to it.
 *
 * The class is templated over the kind of value that can be produced as
 * measure of similarity. That allows to cover methods that produce residuals
 * as well as methods that produces just one double as result.
 */

template <class TReference, class TTarget, 
          class TMapper, class TMeasure,
          class TDerivative > 
class ITK_EXPORT RegistrationMetric : public Object 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationMetric  Self;

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
   *  Type of the Reference
   */
  typedef TReference            ReferenceType;


  /**
   *  Type of the Target
   */
  typedef TTarget               TargetType;
 

  /**
   *  Type of the Mapper
   */
  typedef TMapper               MapperType;
  

  /**
   *  Type of the measure
   */
  typedef TMeasure              MeasureType;
 

  /**
   *  Type of the measure
   */
  typedef TDerivative           DerivativeType;


  /**
   *  Pointer type for the Reference 
   */
  typedef typename ReferenceType::Pointer ReferencePointer;


  /**
   *  Pointer type for the Target 
   */
  typedef typename TargetType::Pointer TargetPointer;


  /**
   *  Pointer type for the Mapper
   */
  typedef typename MapperType::Pointer MapperPointer;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegistrationMetric, Object);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method for execute the algorithm
   */
   virtual void Compute(void);
  

  /**
   * Connect the Reference 
   */
   void SetReference( ReferenceType * );


  /**
   * Connect the Target 
   */
   void SetTarget( TargetType * );

   
  /**
   * Connect the Mapper
   */
   void SetMapper( MapperType * );


  /**
   * Get the Match Measure Value
   */
   itkGetMacro( MatchMeasure, MeasureType );


  /**
   * Get the Derivatives of the Match Measure
   */
   itkGetMacro( MatchMeasureDerivatives, DerivativeType );


protected:

  ReferencePointer            m_Reference;
  TargetPointer               m_Target;
  MapperPointer               m_Mapper;
  MeasureType                 m_MatchMeasure;
  DerivativeType              m_MatchMeasureDerivatives;

  RegistrationMetric();
  virtual ~RegistrationMetric() {};
  RegistrationMetric(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationMetric.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarityRegistrationMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkSimilarityRegistrationMetric_h
#define __itkSimilarityRegistrationMetric_h

#include "itkObject.h"

namespace itk
{
  
/** \class SimilarityRegistrationMetric
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

template <class TTarget,  class TMapper, 
          class TMeasure, class TDerivative > 
class ITK_EXPORT SimilarityRegistrationMetric : public Object 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef SimilarityRegistrationMetric  Self;

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
  typedef typename TMapper::DomainType     ReferenceType;
  typedef typename ReferenceType::Pointer  ReferencePointer;


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
  itkTypeMacro(SimilarityRegistrationMetric, Object);


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
   * Return the Reference 
   */
   ReferencePointer  GetReference( void );


  /**
   * Connect the Target 
   */
   itkSetObjectMacro( Target, TargetType );


  /**
   * Get the Target
   */
   itkGetObjectMacro( Target, TargetType );


  /**
   * Get the Match Measure Value
   */
   itkGetMacro( MatchMeasure, MeasureType );


  /**
   * Get the Derivatives of the Match Measure
   */
   itkGetMacro( MatchMeasureDerivatives, DerivativeType );

  
  /**
   * Connect the Mapper
   */
   itkSetObjectMacro( Mapper, MapperType );


   /**
    * Get a pointer to the Mapper
    */
   itkGetObjectMacro( Mapper, MapperType );


protected:

  SimilarityRegistrationMetric();
  virtual ~SimilarityRegistrationMetric() {};
  SimilarityRegistrationMetric(const Self&) {}
  void operator=(const Self&) {}

private:

  TargetPointer               m_Target;
  MapperPointer               m_Mapper;

protected:

  MeasureType                 m_MatchMeasure;
  DerivativeType              m_MatchMeasureDerivatives;


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimilarityRegistrationMetric.txx"
#endif

#endif




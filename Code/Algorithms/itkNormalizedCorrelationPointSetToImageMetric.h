/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizedCorrelationPointSetToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkNormalizedCorrelationPointSetToImageMetric_h
#define __itkNormalizedCorrelationPointSetToImageMetric_h

#include "itkSimilarityRegistrationMetric.h"
#include "itkCovariantVector.h"
#include "itkPoint.h"


namespace itk
{
  
/** \class NormalizedCorrelationPointSetToImageMetric
 * \brief Computes similarity between two objects to be registered
 *
 * This Class is templated over the type of the objects to be registered and
 * over the type of transformation to be used.
 *
 * SmartPointer to this three objects are received, and using them, this
 * class computes a value(s) that measures the similarity of the target
 * against the reference object once the transformation is applied to it.
 *
 */

template < class TTarget, class TMapper > 
class ITK_EXPORT NormalizedCorrelationPointSetToImageMetric : 
public SimilarityRegistrationMetric< typename TMapper::DomainType,
                                     TTarget, TMapper, double,
                                     CovariantVector<double, TMapper::SpaceDimension > >

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef NormalizedCorrelationPointSetToImageMetric  Self;

  /**
   * Space dimension is the dimension of parameters space
   */
  enum { SpaceDimension = TMapper::SpaceDimension };
  enum { RangeDimension = 9};


  /**
   *  Type of the match measure
   */
  typedef double			        MeasureType;
 

  /**
   *  Type of the derivative of the match measure
   */
  typedef CovariantVector<MeasureType,
                          SpaceDimension >  DerivativeType;


  /**
   * Standard "Superclass" typedef.
   */
  typedef SimilarityRegistrationMetric< 
                       typename TMapper::DomainType,
                       TTarget, TMapper,
                       MeasureType,DerivativeType >  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   *  Type of the Mapper
   */
  typedef TMapper							MapperType;
  
  /**
   *  Type of the Reference
   */
  typedef typename MapperType::DomainType     ReferenceType;


  /**
   *  Type of the Mapper Excepction
   */
  typedef typename TMapper::ExceptionType		  			MapperException;
 
  /**
   *  Type of the Target
   */
  typedef TTarget							TargetType;
 

  /**
   *  Pointer type for the Reference 
   */
  typedef typename ReferenceType::Pointer         ReferencePointer;


  /**
   *  Pointer type for the Target 
   */
  typedef typename TargetType::Pointer            TargetPointer;


  /**
   *  Pointer type for the Mapper
   */
  typedef typename MapperType::Pointer            MapperPointer;


  /**
   *  Parameters type
   */
  typedef typename  TMapper::ParametersType       ParametersType;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(NormalizedCorrelationPointSetToImageMetric, Object);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
 
  /**
   * Get the Derivatives of the Match Measure
   */
  const DerivativeType & GetDerivative( const ParametersType & parameters );

  /**
   *  Get the Value for SingleValue Optimizers
   */
  MeasureType    GetValue( const ParametersType & parameters );


  /**
   *  Get Value and Derivatives for MultipleValuedOptimizers
   */
   void GetValueAndDerivative( const ParametersType & parameters,
       MeasureType & Value, DerivativeType  & Derivative );

 
protected:

  NormalizedCorrelationPointSetToImageMetric();
  virtual ~NormalizedCorrelationPointSetToImageMetric() {};
  NormalizedCorrelationPointSetToImageMetric(const Self&) {}
  void operator=(const Self&) {}

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNormalizedCorrelationPointSetToImageMetric.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPatternIntensityImageToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkPatternIntensityImageToImageMetric_h
#define __itkPatternIntensityImageToImageMetric_h

#include "itkObject.h"
#include "itkVectorContainer.h"

namespace itk
{
  
/** \class PatternIntensityImageToImageMetric
 * \brief Computes similarity between two objects to be registered
 *
 * This metric was proposed in:
 *
 *  G.P. Penney, J. Weese, J.A. Little, P. Desmedt, D.L. Hill and D.J.Hawkes
 *  "A Comparision of similarity measures for use 
 *  in 2D-3D Medical Image Registration"
 *  IEEE Transactions on Medical Imaging, 1998, 
 *  vol 17, no. 4, august, pp 586-595
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

template < class TTarget, class TMapper,class TMeasure,
          class TDerivative > 
class ITK_EXPORT PatternIntensityImageToImageMetric : public Object 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef PatternIntensityImageToImageMetric  Self;

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
   *  Type of the match measure
   */
  typedef TMeasure			 MeasureType;
 

  /**
   *  Type of the derivative of the match measure
   */
  typedef VectorContainer<unsigned int,TDerivative>  DerivativeType;


  /**
   *  Pointer to the derivative of the match measure
   */
  typedef  typename DerivativeType::Pointer       DerivativePointer;



  typedef itk::VectorContainer<unsigned int,TMeasure> VectorMeasureType;

  typedef typename VectorMeasureType::Pointer      VectorMeasurePointer;

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
   *  Parameters type
   */
  typedef itk::VectorContainer<unsigned int,double> ParametersType;


  /**
   *  Pointer to Parameters type
   */
  typedef typename ParametersType::Pointer    ParametersPointer;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(PatternIntensityImageToImageMetric, Object);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
 
  /**
   * Connect the Target 
   */
  void SetTarget( TargetType * );

  /**
   * Connect the Mapper
   */
  void SetMapper( MapperType * );

  /**
   * Get the Derivatives of the Match Measure
   */
  DerivativePointer GetDerivative( void );

  /**
   *  Get the Value for SingleValue Optimizers
   */
  MeasureType    GetValue( void );

  /**
   *  Get the Value for MultipleValuedOptimizers
   */
   void  GetValue(VectorMeasureType::Pointer &);

  /**
   *  Get Value and Derivatives for MultipleValuedOptimizers
   */
   void GetValueAndDerivative(MeasureType & Value, DerivativeType  & Derivative );

  /**
   * Get Parameters
   */
   const ParametersType::Pointer & GetParameters( void ) const {return m_Parameters;}
  
  /**
   * Space dimension is the dimension of parameters space
   */
   enum { SpaceDimension = TMapper::SpaceDimension };    
   enum { RangeDimension = 9};

protected:

  ReferencePointer            m_Reference;
  TargetPointer               m_Target;
  MapperPointer               m_Mapper;
  MeasureType                 m_MatchMeasure;
  VectorMeasurePointer        m_VectorMatchMeasure;        
  DerivativePointer           m_MatchMeasureDerivatives;
  ParametersPointer           m_Parameters;

  PatternIntensityImageToImageMetric();
  virtual ~PatternIntensityImageToImageMetric() {};
  PatternIntensityImageToImageMetric(const Self&) {}
  void operator=(const Self&) {}
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPatternIntensityImageToImageMetric.txx"
#endif

#endif




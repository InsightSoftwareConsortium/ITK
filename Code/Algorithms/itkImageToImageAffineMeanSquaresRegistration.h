/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffineMeanSquaresRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkImageToImageAffineMeanSquaresRegistration_h
#define __itkImageToImageAffineMeanSquaresRegistration_h

#include "itkObject.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkMeanSquaresImageToImageMetric.h"
//#include "itkConjugateGradientOptimizer.h"
#include "itkLevenbergMarquardtOptimizer.h"
#include "itkImage.h"
#include "itkImageMapper.h"
#include "itkAffineRegistrationTransform.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkVectorContainer.h"

namespace itk
{

/**
 * \class ImageToImageAffineMeanSquaresRegistration
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

template <class TReference, class TTarget>
class ITK_EXPORT ImageToImageAffineMeanSquaresRegistration : public Object 
{
public:
  /**
   * Standard "Self" typedef.
   */
   typedef ImageToImageAffineMeanSquaresRegistration  Self;


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
   typedef TReference  ReferenceType;

  /**
   *  Type of the Metric
   */
   typedef TTarget TargetType;

  /**
   *  Type of the paramters
   */
   typedef itk::VectorContainer<unsigned int,double> ParametersType;

  /**
   *  Pointer to the parameters
   */
   typedef typename ParametersType::Pointer ParametersPointer;

  /**
   * Image Dimensions
   */
   enum {ImageDimension = ReferenceType::ImageDimension};

  /**
   *  Type of the Transformation
   */
   typedef AffineRegistrationTransform<double,ImageDimension>         TransformationType;
	  
  /**
   *  Type of the Mapper
   */
   typedef ImageMapper<ReferenceType,TransformationType>  MapperType;


  /**
   *  Type of the Metric
   */
   typedef MeanSquaresImageToImageMetric<TargetType, MapperType,double,double>   MetricType;


  /**
   *  Type of the Optimizer 
   */
   //typedef ConjugateGradientOptimizer<MetricType>          OptimizerType;
   typedef LevenbergMarquardtOptimizer<MetricType>           OptimizerType;

  /**
   *  Pointer type for the optimizer 
   */
   typedef OptimizerType::Pointer     OptimizerPointer;

  /**
   * Internal Optimizer type
   */
   typedef  OptimizerType::InternalOptimizerType  vnlOptimizerType;

  /**
   *  Pointer type for the Reference 
   */
   typedef ReferenceType::Pointer ReferencePointer;

  
  /**
   *  Pointer type for the Target 
   */
   typedef TargetType::Pointer TargetPointer;


  /**
   *  Pointer type for the Transformation
   */
   typedef  TransformationType::Pointer TransformationPointer;


  /**
   *  Pointer type for the metric
   */
   typedef  MetricType::Pointer        MetricPointer;


  /**
   *  Pointer type for the mapper
   */
   typedef MapperType::Pointer        MapperPointer;


  /**
   * Interpolation type
   */
   typedef itk::LinearInterpolateImageFunction<TargetType>  InterpolatorType;

  /** 
   * Run-time type information (and related methods).
   */
   itkTypeMacro(ImageToImageAffineMeanSquaresRegistration, Object);


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
   void SetTarget( TargetType * );

   
  /**
   * Set the Reference
   */
   void SetReference( ReferenceType * );

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

  /**
   * Get the Optimizer
   */
   itkGetMacro( Parameters, ParametersPointer );


protected:

  ImageToImageAffineMeanSquaresRegistration();
  virtual ~ImageToImageAffineMeanSquaresRegistration();
  ImageToImageAffineMeanSquaresRegistration(const Self&);
  const Self & operator=(const Self&);
 

private:

  TargetPointer              m_Target;
  ReferencePointer           m_Reference;
  TransformationPointer      m_Transformation;
  MapperPointer              m_Mapper;  
  MetricPointer              m_Metric;
  OptimizerPointer           m_Optimizer;
  ParametersPointer          m_Parameters;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageAffineMeanSquaresRegistration.txx"
#endif

#endif




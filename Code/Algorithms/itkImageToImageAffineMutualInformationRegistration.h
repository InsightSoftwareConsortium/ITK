/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffineMutualInformationRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkImageToImageAffineMutualInformationRegistration_h
#define __itkImageToImageAffineMutualInformationRegistration_h

#include "itkObject.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkMutualInformationImageToImageMetric.h"
#include "itkImage.h"
#include "itkImageMapper.h"
#include "itkAffineRegistrationTransform.h"
#include "itkVectorContainer.h"

namespace itk
{

/**
 * \class ImageToImageAffineMutualInformationRegistration
 * \brief Register two images using mutual information
 *
 * ImageToImageAffineMutualInformationRegistration performs an affine registration
 * between a target and reference image using mutual information. Specially,
 * it uses the optimization method of Viola and Wells to find the
 * best affine transform to register the reference image onto the target
 * image. The two images are defined via methods SetTarget() and SetReference().
 *
 * The mutual information value and derivatives are computed in the
 * MutualInformationImagetoImageMetric class.
 *
 * This class uses a simple stochastic gradient ascent scheme. Steps
 * are taken repeatedly taken that are proportional to the approximate
 * deriviative of the mutual information with respect to the affine
 * transform parameters. The stepsize is governed by the LearningRate
 * parameter. The default is 1.0. The LearningRate is set via the method
 * SetLearningRate(). Typically, the learning rate should be annealed
 * (decreased over time). In our experiments, we have found that a 
 * LearningRate of 1.0 works well for images normalized between 0 and 1.
 *
 * Since the parameters of the linear part is different in magnitude
 * to the parameters in the offset part, scaling is required
 * to improve convergence. Scaling weights of the parameter derivatives
 * can be set via SetScalingWeights(). Default is 0.001 for the parameters
 * in the linear part and 1.0 for parameters in the offset part.
 *
 * Implementaton of this class is based on:
 * Viola, P. and Wells III, W. (1997). 
 * "Alignment by Maximization of Mutual Information"
 * International Journal of Computer Vision, 24(2):137-154
 *
 * This class is templated over the reference image type and the
 * the target image type.
 *
 * \sa MutualInformationImageToImageMetric
 *
 */
template <class TReference, class TTarget>
class ITK_EXPORT ImageToImageAffineMutualInformationRegistration : public Object 
{
public:
  /**
   * Standard "Self" typedef.
   */
   typedef ImageToImageAffineMutualInformationRegistration  Self;

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
   *  Type of the Target
   */
   typedef TTarget TargetType;

  /**
   * Image Dimensions
   */
   enum {ImageDimension = ReferenceType::ImageDimension};

  /**
   *  Type of the Transformation
   */
   typedef AffineRegistrationTransform<double,ImageDimension>  TransformationType;
	  
  /**
   *  Type of the Mapper
   */
   typedef ImageMapper<ReferenceType,TransformationType>  MapperType;

  /**
   *  Type of the Metric
   */
   typedef MutualInformationImageToImageMetric<
    TargetType, MapperType, double, double>    MetricType;

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
   typedef  typename TransformationType::Pointer TransformationPointer;

  /**
   *  Pointer type for the metric
   */
   typedef  typename MetricType::Pointer  MetricPointer;

  /**
   *  Pointer type for the mapper
   */
   typedef typename MapperType::Pointer  MapperPointer;

  /**
   *  Type of the parameters
   */
   typedef typename MetricType::ParametersType  ParametersType;

  /**
   *  Pointer to the parameters
   */
   typedef typename ParametersType::Pointer ParametersPointer;

  /**
   * Type of the scaling weights
   */
   typedef itk::VectorContainer<unsigned int, double> ScalingWeightsType;

  /**
   * Pointer to the scaling weights
   */
   typedef typename ScalingWeightsType::Pointer ScalingWeightsPointer;


  /** 
   * Run-time type information (and related methods).
   */
   itkTypeMacro(ImageToImageAffineMutualInformationRegistration, Object);


  /**
   * Method for creation through the object factory.
   */
   itkNewMacro(Self);
  

  /**
   * Method that initiates the registration.
   */
   int StartRegistration( void );


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
   * Get the Metric
   */
   itkGetMacro( Metric, MetricPointer );

  /**
   * Set the transform parameters
   */
   void SetParameters( ParametersType * );

  /**
   * Get the transform parameters
   */
  const ParametersPointer& GetParameters( void ) const 
    { return m_Parameters; }
 
  // -------------------------------
  // Optimization related methods
  // -------------------------------
  /**
   * Set the scaling weights
   */
  void SetScalingWeights( ScalingWeightsType * );

  /**
   * Get the scaling weights
   */
  const ScalingWeightsType::Pointer & GetScalingWeights( void ) const
   { return m_ScalingWegihts; }

  /** 
   * Set the learning rate. This is used in the optimization scheme.
   * Typically, the learning rate needs to be annealed (decreased over
   * time). Default is 1.0.
   */
  itkSetClampMacro( LearningRate, double, 0.0,
    NumericTraits<double>::max() );

  /**
   * Get the learning rate.
   */
  itkGetMacro( LearningRate, double );

  /** 
   * Set the number of iterations. This determines the number of
   * iterations performed in the steepest descent optimization.
   * Default is 1000.
   */
  itkSetMacro( NumberOfIterations, unsigned int );

  /**
   * Get the number of iterations.
   */
  itkGetMacro( NumberOfIterations, unsigned int );

  /**
   * Turn screen dump on and off. Default is on.
   */
  itkSetMacro( ScreenDump, bool );
  itkBooleanMacro( ScreenDump );


protected:

  ImageToImageAffineMutualInformationRegistration();
  virtual ~ImageToImageAffineMutualInformationRegistration();
  ImageToImageAffineMutualInformationRegistration(const Self&);
  const Self & operator=(const Self&);
 

private:

  TargetPointer              m_Target;
  ReferencePointer           m_Reference;
  TransformationPointer      m_Transformation;
  MapperPointer              m_Mapper;  
  MetricPointer              m_Metric;
  ParametersPointer          m_Parameters;
  
  // -------------------------------
  // Optimization related variables
  // -------------------------------
  ScalingWeightsPointer      m_ScalingWeights;
  double                     m_LearningRate;
  unsigned int               m_NumberOfIterations;
  bool                       m_ScreenDump;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageAffineMutualInformationRegistration.txx"
#endif

#endif




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

#include "itkRegistrationMethod.h"
#include "itkMutualInformationImageToImageMetric.h"
#include "itkGradientDescentOptimizer.h"
#include "itkImageMapper.h"
#include "itkCenteredAffineRegistrationTransform.h"
#include "itkPoint.h"

namespace itk
{

/**
 *  Traits class that defines the different types to be
 *  used by this registration method
 */
template <class TReference, class TTarget>
class ITK_EXPORT ImageToImageAffineMutualInformationGradientDescentRegistrationTraits 
{
  
public:

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
   enum {ImageDimension = ReferenceType::ImageDimension,
         ParametersDimension = ImageDimension*(ImageDimension+1) };

  /**
   *  Type of the parameters
   */
   typedef Point<double,ParametersDimension>   ParametersType;

  /**
   *  Type of the Transformation
   */

   typedef CenteredAffineRegistrationTransform<
                double, 
                ImageDimension, 
                ParametersType >  TransformationType;

  /**
   *  Type of the Mapper
   */
   typedef ImageMapper<ReferenceType,TransformationType>  MapperType;

  /**
   *  Type of the Metric
   */
   typedef MutualInformationImageToImageMetric<TargetType, MapperType>   MetricType;


  /**
   *  Type of the Optimizer 
   */
   typedef GradientDescentOptimizer<MetricType>           OptimizerType;


};



/**
 * \class ImageToImageAffineMutualInformationRegistration
 * \brief Register two images using mutual information.
 *
 * ImageToImageAffineMutualInformationRegistration performs an
 * affine registration
 * between a target and reference image using mutual information.
 * It uses the optimization method of Viola and Wells to find the
 * best affine transform to register the reference image onto the target
 * image. The two images are defined via methods SetTarget()
 * and SetReference().
 *
 * The mutual information value and its derivatives are computed
 * using the MutualInformationImagetoImageMetric class.
 *
 * The registration uses a simple stochastic gradient ascent scheme. Steps
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
 * in the linear part and 1.0 for parameters for the offset part.
 *
 * Optimization performance can be improve by setting the transformation
 * centers to center of mass of the image. The transformation centers
 * can be specify via methods SetTargetTransformationCenter() and
 * SetReferenceTransformationCenter(). The default is the origin for
 * both centers.
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
class ITK_EXPORT ImageToImageAffineMutualInformationRegistration 
: public RegistrationMethod< 
   ImageToImageAffineMutualInformationGradientDescentRegistrationTraits<
               TReference,
               TTarget>  >
{
public:
  /**
   * Standard "Self" typedef.
   */
   typedef ImageToImageAffineMutualInformationRegistration  Self;

  /**
   * Standard "Superclass" typedef.
   */
   typedef RegistrationMethod< 
      ImageToImageAffineMutualInformationGradientDescentRegistrationTraits<
                                       TReference,
                                       TTarget>  >           Superclass;

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
   * Parmeters Dimensions
   */
   enum { ParametersDimension = ImageDimension * ( ImageDimension + 1 ) };

  /**
   * Parameters Type
   */
   typedef typename Superclass::ParametersType   ParametersType;

  /**
   *  Type of the Transformation
   */
   typedef typename Superclass::TransformationType    TransformationType;

  /**
   * Point Type
   */
  typedef typename TransformationType::PointType  PointType;

  /**
   *  Type of the Mapper
   */
   typedef typename Superclass::MapperType   MapperType;

  /**
   *  Type of the Metric
   */
   typedef typename Superclass::MetricType   MetricType;

  /**
   *  Type of the Optimizer 
   */
   typedef typename  Superclass::OptimizerType  OptimizerType;

  /**
   *  Pointer type for the optimizer 
   */
   typedef typename OptimizerType::Pointer  OptimizerPointer;

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
   * Run-time type information (and related methods).
   */
   itkTypeMacro(ImageToImageAffineMutualInformationRegistration,
       RegistrationMethod);

  /**
   * Method for creation through the object factory.
   */
   itkNewMacro(Self);

  /**
   * Method that initiates the registration.
   */
   int StartRegistration( void );


  /**
   * Set the transform parameters
   */
   void SetParameters( const ParametersType&  params )
    { m_Parameters = params; }

  /**
   * Get the transform parameters
   */
  const ParametersType& GetParameters( void ) const
    { return m_Parameters; }

  /**
   * Set the target image transformation center
   */
   void SetTargetTransformationCenter( const PointType& center );

  /**
   * Get the target image transformation center
   */
   PointType& GetTargetTransformationCenter( void ) const
    { return m_TargetTransformationCenter; }

  /**
   * Set the reference image transformation center
   */
   void SetReferenceTransformationCenter( const PointType& center );

  /**
   * Get the reference image transformation center
   */
   PointType& GetReferenceTransformationCenter( void ) const
    { return m_ReferenceTransformationCenter; }

  /**
   * Set the scaling weights
   */
  void SetScalingWeights( const ParametersType& weights )
   { m_ScalingWeights = weights; }

  /**
   * Get the scaling weights
   */
  const ParametersType& GetScalingWeights( void ) const
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

protected:

  ImageToImageAffineMutualInformationRegistration();
  virtual ~ImageToImageAffineMutualInformationRegistration();
  ImageToImageAffineMutualInformationRegistration(const Self&);
  const Self & operator=(const Self&);

private:

  ParametersType             m_Parameters;

  PointType                  m_TargetTransformationCenter;
  PointType                  m_ReferenceTransformationCenter;

  // -------------------------------
  // Optimization related variables
  // -------------------------------
  ParametersType             m_ScalingWeights;
  double                     m_LearningRate;
  unsigned int               m_NumberOfIterations;


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageAffineMutualInformationRegistration.txx"
#endif

#endif



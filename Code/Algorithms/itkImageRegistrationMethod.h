/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegistrationMethod.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageRegistrationMethod_h
#define __itkImageRegistrationMethod_h

#include "itkProcessObject.h"
#include "itkImage.h"
#include "itkImageToImageMetric.h"
#include "itkSingleValuedNonLinearOptimizer.h"

namespace itk
{

/** \class ImageRegistrationMethod
 * \brief Base class for Image Registration Methods
 *
 * This Class define the generic interface for a registration method.
 *
 * This class is templated over the type of the two image to be
 * registered. A generic Transform is used by this class. That allows
 * to select at run time the particular type of transformation that
 * is to be applied for registering the images.
 *
 * This method use a generic Metric in order to compare the two images.
 * the final goal of the registration method is to find the set of 
 * parameters of the Transformation that optimizes the metric.
 *
 * The registration method also support a generic optimizer that can
 * be selected at run-time. The only restriction for the optimizer is
 * that it should be able to operate in single-valued cost functions
 * given that the metrics used to compare images provide a single 
 * value as output.
 *
 * The terms : Fixed image and Moving image are used in this class
 * to indicate what image is being mapped by the transform.
 *
 * This class uses the coordinate system of the Fixed image as a reference
 * and searchs for a Transform that will map points from the space of the
 * Fixed image to the space of the Moving image.
 *
 * For doing so, a Metric will be continously applied to compare the Fixed
 * image with the Transformed Moving image. This process also requires to
 * interpolate values from the Moving image.
 *
 * \ingroup RegistrationFilters
 */
template <typename TFixedImage, typename TMovingImage>
class ITK_EXPORT ImageRegistrationMethod : public ProcessObject 
{
public:
  /** Standard class typedefs. */
  typedef ImageRegistrationMethod  Self;
  typedef ProcessObject  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageRegistrationMethod, ProcessObject);

  /**  Type of the Fixed image. */
  typedef          TFixedImage                     FixedImageType;
  typedef typename FixedImageType::ConstPointer    FixedImageConstPointer;

  /**  Type of the Moving image. */
  typedef          TMovingImage                     MovingImageType;
  typedef typename MovingImageType::ConstPointer    MovingImageConstPointer;

  /**  Type of the metric. */
  typedef ImageToImageMetric< FixedImageType,
                              MovingImageType >       MetricType;
  typedef typename MetricType::Pointer                MetricPointer;
  typedef typename MetricType::FixedImageRegionType   FixedImageRegionType;

  /**  Type of the Transform . */
  typedef  typename MetricType::TransformType      TransformType;
  typedef  typename TransformType::Pointer         TransformPointer;

  /**  Type of the Interpolator. */
  typedef  typename MetricType::InterpolatorType   InterpolatorType;
  typedef  typename InterpolatorType::Pointer      InterpolatorPointer;

  /**  Type of the optimizer. */
  typedef   SingleValuedNonLinearOptimizer         OptimizerType;

  /** Type of the Transformation parameters This is the same type used to
   *  represent the search space of the optimization algorithm */
  typedef  typename MetricType::TransformParametersType    ParametersType;

  /** Method that initiates the registration. */
  void StartRegistration(void);

  /** Set/Get the Fixed image. */
  itkSetConstObjectMacro( FixedImage, FixedImageType );
  itkGetConstObjectMacro( FixedImage, FixedImageType ); 

  /** Set/Get the Moving image. */
  itkSetConstObjectMacro( MovingImage, MovingImageType );
  itkGetConstObjectMacro( MovingImage, MovingImageType );

  /** Set/Get the Optimizer. */
  itkSetObjectMacro( Optimizer,  OptimizerType );
  itkGetObjectMacro( Optimizer,  OptimizerType );

  /** Set/Get the Metric. */
  itkSetObjectMacro( Metric, MetricType );
  itkGetObjectMacro( Metric, MetricType );

  /** Set/Get the Transfrom. */
  itkSetObjectMacro( Transform, TransformType );
  itkGetObjectMacro( Transform, TransformType );

  /** Set/Get the Interpolator. */
  itkSetObjectMacro( Interpolator, InterpolatorType );
  itkGetObjectMacro( Interpolator, InterpolatorType );

  /** Set/Get the initial transformation parameters. */
  virtual void SetInitialTransformParameters( const ParametersType & param );
  itkGetConstReferenceMacro( InitialTransformParameters, ParametersType );

  /** Get the last transformation parameters visited by 
   * the optimizer. */
  itkGetConstReferenceMacro( LastTransformParameters, ParametersType );

  /** Set the region of the fixed image to be considered as region of
   interest during the registration. This region will be passed to 
   the ImageMetric in order to restrict the metric computation to 
   consider only this region. 
   \warning The same region can also be set directly into the metric.
   please avoid to set the region in both places since this can lead
   to inconsistent configurations.  */
  void SetFixedImageRegion( const  FixedImageRegionType & region ); 

  /** Initialize by setting the interconnects between the components. */
  void Initialize() throw (ExceptionObject);

protected:
  ImageRegistrationMethod();
  virtual ~ImageRegistrationMethod() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  ImageRegistrationMethod(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  MetricPointer                    m_Metric;
  OptimizerType::Pointer           m_Optimizer;

  MovingImageConstPointer          m_MovingImage;
  FixedImageConstPointer           m_FixedImage;

  TransformPointer                 m_Transform;
  InterpolatorPointer              m_Interpolator;

  ParametersType                   m_InitialTransformParameters;
  ParametersType                   m_LastTransformParameters;
  
  bool                             m_FixedImageRegionDefined;
  FixedImageRegionType             m_FixedImageRegion;

};


} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegistrationMethod.txx"
#endif

#endif




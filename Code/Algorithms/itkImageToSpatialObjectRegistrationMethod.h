/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToSpatialObjectRegistrationMethod.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToSpatialObjectRegistrationMethod_h
#define __itkImageToSpatialObjectRegistrationMethod_h

#include "itkProcessObject.h"
#include "itkImage.h"
#include "itkImageToSpatialObjectMetric.h"
#include "itkSingleValuedNonLinearOptimizer.h"

namespace itk
{

/** \class ImageToSpatialObjectRegistrationMethod
 * \brief Base class for Image Registration Methods
 *
 *
 * This class is templated over the type of the image and the type
 * of the SpatialObject to be registered.
 * A generic Transform is used by this class. That allows
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
 * The terms : Fixed image and Moving SpatialObject are used in this class
 * to indicate that the SpatialObject is being mapped by the transform.
 *
 * This class uses the coordinate system of the Fixed image and searchs
 * for a transform that will map the Moving SpatialObject on top of 
 * the Fixed image. For doing so, a Metric will be continously applied to 
 * compare the Fixed image with the Transformed Moving SpatialObject.
 *
 * \ingroup RegistrationFilters
 */
template <typename TFixedImage, typename TMovingSpatialObject>
class ITK_EXPORT ImageToSpatialObjectRegistrationMethod : public ProcessObject 
{
public:
  /** Standard class typedefs. */
  typedef ImageToSpatialObjectRegistrationMethod  Self;
  typedef ProcessObject  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToSpatialObjectRegistrationMethod, ProcessObject);

  /**  Type of the Fixed image. */
  typedef          TFixedImage                     FixedImageType;
  typedef typename FixedImageType::ConstPointer    FixedImageConstPointer;

  /**  Type of the Moving image. */
  typedef TMovingSpatialObject       MovingSpatialObjectType;
  typedef typename MovingSpatialObjectType::ConstPointer
                                          MovingSpatialObjectConstPointer;

  /**  Type of the metric. */
  typedef ImageToSpatialObjectMetric< FixedImageType,
                        MovingSpatialObjectType >     MetricType;
  typedef typename MetricType::Pointer             MetricPointer;

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

  /** Set/Get the Moving Spatial Object. */
  itkSetConstObjectMacro( MovingSpatialObject, MovingSpatialObjectType );
  itkGetConstObjectMacro( MovingSpatialObject, MovingSpatialObjectType );

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
  itkSetMacro( InitialTransformParameters, ParametersType );
  itkGetConstReferenceMacro( InitialTransformParameters, ParametersType );

  /** Get the last transformation parameters visited by 
   * the optimizer. */
  itkGetConstReferenceMacro( LastTransformParameters, ParametersType );

protected:
  ImageToSpatialObjectRegistrationMethod();
  virtual ~ImageToSpatialObjectRegistrationMethod() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Initialize by setting the interconnects between the components. */
  void Initialize() throw (ExceptionObject);

  ParametersType                   m_InitialTransformParameters;
  ParametersType                   m_LastTransformParameters;

private:
  ImageToSpatialObjectRegistrationMethod(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  MetricPointer                    m_Metric;
  OptimizerType::Pointer           m_Optimizer;

  MovingSpatialObjectConstPointer  m_MovingSpatialObject;
  FixedImageConstPointer           m_FixedImage;

  TransformPointer                 m_Transform;
  InterpolatorPointer              m_Interpolator;
  
};


} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkImageToSpatialObjectRegistrationMethod.txx"
#endif

#endif

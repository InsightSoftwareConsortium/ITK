/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionImageRegistrationMethod.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMultiResolutionImageRegistrationMethod_h
#define __itkMultiResolutionImageRegistrationMethod_h

#include "itkProcessObject.h"
#include "itkImageToImageMetric.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkMultiResolutionPyramidImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{

/** \class MultiResolutionImageRegistrationMethod
 * \brief Base class for multi-resolution image registration methods
 *
 * This class provides a generic interface for multi-resolution 
 * registration using components of the registration framework.
 *
 * At each resolution level, the user specified registration components
 * are used to register two images by computing the transform parameters
 * that will map a moving image onto the fixed image.
 *
 * See documentation for ImageRegistrationMethod for description
 * of the registration framework components.
 *
 * The input fixed and moving images are set via methods SetFixedImage
 * SetMovingImage respectively. Initial transform parameters are set via
 * SetInitialTransformParameters. MultiResolutionPyramidImageFilters are used to 
 * downsample the fixed and moving images.
 *
 * Before each resolution level an IterationEvent is invoked providing an
 * opportunity for a user interface to change any of the components or
 * component parameters.
 *
 * This class is templated over the fixed image type and the moving image
 * type.
 *
 * \sa ImageRegistrationMethod
 * \ingroup RegistrationFilters
 */
template <typename TFixedImage, typename TMovingImage>
class ITK_EXPORT MultiResolutionImageRegistrationMethod : public ProcessObject 
{
public:
  /** Standard class typedefs. */
  typedef MultiResolutionImageRegistrationMethod  Self;
  typedef ProcessObject  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiResolutionImageRegistrationMethod, ProcessObject);

  /**  Type of the Fixed image. */
  typedef          TFixedImage                     FixedImageType;
  typedef typename FixedImageType::Pointer         FixedImageConstPointer;

  /**  Type of the Moving image. */
  typedef          TMovingImage                    MovingImageType;
  typedef typename MovingImageType::Pointer        MovingImageConstPointer;

  /**  Type of the metric. */
  typedef ImageToImageMetric< FixedImageType,
                              MovingImageType >    MetricType;
  typedef typename MetricType::Pointer             MetricPointer;

  /**  Type of the Transform . */
  typedef typename MetricType::TransformType       TransformType;
  typedef typename TransformType::Pointer          TransformPointer;

  /**  Type of the Interpolator. */
  typedef typename MetricType::InterpolatorType    InterpolatorType;
  typedef typename InterpolatorType::Pointer       InterpolatorPointer;

  /**  Type of the optimizer. */
  typedef SingleValuedNonLinearOptimizer           OptimizerType;

  /** Type of the Fixed image multiresolution pyramid. */
  typedef MultiResolutionPyramidImageFilter< FixedImageType,
                                             FixedImageType >
                                                   FixedImagePyramidType;
  typedef typename FixedImagePyramidType::Pointer  FixedImagePyramidPointer;

  /** Type of the moving image multiresolution pyramid. */
  typedef MultiResolutionPyramidImageFilter< MovingImageType,
                                             MovingImageType >
                                                   MovingImagePyramidType;
  typedef typename MovingImagePyramidType::Pointer MovingImagePyramidPointer;

  /** Type of the Transformation parameters This is the same type used to
   *  represent the search space of the optimization algorithm */
  typedef  typename MetricType::TransformParametersType    ParametersType;

  /** Method that initiates the registration. */
  void StartRegistration(void);

  /** Set/Get the Fixed image. */
  itkSetObjectMacro( FixedImage, FixedImageType );
  itkGetObjectMacro( FixedImage, FixedImageType ); 

  /** Set/Get the Moving image. */
  itkSetObjectMacro( MovingImage, MovingImageType );
  itkGetObjectMacro( MovingImage, MovingImageType );

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

  /** Set/Get the Fixed image pyramid. */
  itkSetObjectMacro( FixedImagePyramid, FixedImagePyramidType );
  itkGetObjectMacro( FixedImagePyramid, FixedImagePyramidType ); 

  /** Set/Get the Moving image pyramid. */
  itkSetObjectMacro( MovingImagePyramid, MovingImagePyramidType );
  itkGetObjectMacro( MovingImagePyramid, MovingImagePyramidType );

  /** Set/Get the number of multi-resolution levels. */
  itkSetClampMacro( NumberOfLevels, unsigned long, 1,
    NumericTraits<unsigned long>::max() );

  /** Get the current resolution level being processed. */
  itkGetMacro( CurrentLevel, unsigned long );

  /** Set/Get the initial transformation parameters. */
  itkSetMacro( InitialTransformParameters, ParametersType );
  itkGetConstReferenceMacro( InitialTransformParameters, ParametersType );

  /** Set/Get the initial transformation parameters of the next resolution
   level to be processed. The default is the last set of parameters of
   the last resolution level. */
  itkSetMacro( InitialTransformParametersOfNextLevel, ParametersType );
  itkGetConstReferenceMacro( InitialTransformParametersOfNextLevel, ParametersType );

  /** Get the last transformation parameters visited by 
   * the optimizer. */
  itkGetConstReferenceMacro( LastTransformParameters, ParametersType );  

protected:
  MultiResolutionImageRegistrationMethod();
  virtual ~MultiResolutionImageRegistrationMethod() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Initialize by setting the interconnects between the components. */
  void Initialize() throw (ExceptionObject);

private:
  MultiResolutionImageRegistrationMethod(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  MetricPointer                    m_Metric;
  OptimizerType::Pointer           m_Optimizer;

  MovingImageConstPointer          m_MovingImage;
  FixedImageConstPointer           m_FixedImage;

  TransformPointer                 m_Transform;
  InterpolatorPointer              m_Interpolator;

  MovingImagePyramidPointer        m_MovingImagePyramid;
  FixedImagePyramidPointer         m_FixedImagePyramid;

  ParametersType                   m_InitialTransformParameters;
  ParametersType                   m_InitialTransformParametersOfNextLevel;
  ParametersType                   m_LastTransformParameters;

  unsigned long                    m_NumberOfLevels;
  unsigned long                    m_CurrentLevel;
  
};


} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiResolutionImageRegistrationMethod.txx"
#endif

#endif




/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkImageRegistrationMethodv4_h
#define __itkImageRegistrationMethodv4_h

#include "itkProcessObject.h"

#include "itkAffineTransform.h"
#include "itkCompositeTransform.h"
#include "itkDataObjectDecorator.h"
#include "itkObjectToObjectOptimizerBase.h"
#include "itkImageToImageMetricv4.h"
#include "itkInterpolateImageFunction.h"
#include "itkTransform.h"
#include "itkTransformParametersAdaptor.h"

#include <vector>

namespace itk
{

/** \class ImageRegistrationMethodv4
 * \brief Interface method for the current registration framework.
 *
 * This interface method class encapsulates typical registration
 * usage by incorporating all the necessary elements for performing a
 * simple image registration between two images.  This method also
 * allows for multistage registration whereby each stage is
 * characterize by possibly different transforms of and different
 * image metrics.  For example, many users will want to perform
 * a linear registration followed by deformable registration where
 * both stages are peformed in multiple levels.  Each level can be
 * characterized by:
 *
 *   \li the resolution of the virtual domain image (see below)
 *   \li smoothing of the fixed and moving images
 *   \li the coarseness of the current transform via transform adaptors
 *       (see below)
 *
 * Multiple stages are handled by linking multiple instantiations of
 * this class where the output transform is added to the optional
 * composite transform input.
 *
 * Transform adaptors:  To accommodate new changes to the current ITK
 * registration framework, we introduced the concept of transform adaptors.
 * Whereas each stage is associated with a moving and, possibly, fixed
 * transform, each level of each stage is defined by a transform adaptor
 * which describes how to adapt the transform to the current level.  For
 * example, if one were to use the B-spline transform during a deformable
 * registration stage, common practice is to increase the resolution of
 * the B-spline mesh (or, analogously, the control point grid size) at
 * each level.  At each level, one would define the parameters of the
 * B-spline transform adaptor at that level which increases the resolution
 * from the previous level.  For many transforms, such as affine, this
 * concept of an adaptor may be nonsensical.  For this reason, the base
 * transform adaptor class does not do anything to the transform but merely
 * passes it through.  Each level of each stage must define a transform
 * adaptor but, by default, the base adaptor class is assigned which, again,
 * does not do anything to the transform.  A special mention should be made
 * of the transform adaptor at level 0 of any stage.  Most likely, the user
 * will not want to do anything to the transform as it enters into the
 * given stage so typical use will be to assign the base adaptor class to
 * level 0 of all stages but we leave that open to the user.
 *
 * Output: The output is the updated transform which has been added to the
 * composite transform.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKRegistrationMethodsv4
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform =
  AffineTransform<double, GetImageDimension<TFixedImage>::ImageDimension> >
class ITK_EXPORT ImageRegistrationMethodv4
:public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageRegistrationMethodv4                 Self;
  typedef ProcessObject                             Superclass;
  typedef SmartPointer<Self>                        Pointer;
  typedef SmartPointer<const Self>                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** ImageDimension constants */
  itkStaticConstMacro( ImageDimension, unsigned int, TFixedImage::ImageDimension );

  /** Run-time type information (and related methods). */
  itkTypeMacro( ImageRegistrationMethodv4, ProcessObject );

  /** Input typedefs for the images and transforms. */
  typedef TFixedImage                                                 FixedImageType;
  typedef typename FixedImageType::Pointer                            FixedImagePointer;
  typedef TMovingImage                                                MovingImageType;
  typedef typename MovingImageType::Pointer                           MovingImagePointer;

  /** Metric and transform typedefs */
  typedef ImageToImageMetricv4<FixedImageType, MovingImageType>       MetricType;
  typedef typename MetricType::Pointer                                MetricPointer;

  typedef TTransform                                                  TransformType;
  typedef typename TransformType::Pointer                             TransformPointer;
  typedef typename TransformType::ScalarType                          RealType;
  typedef typename TransformType::DerivativeType                      DerivativeType;
  typedef typename DerivativeType::ValueType                          DerivativeValueType;

  typedef CompositeTransform<RealType, ImageDimension>                CompositeTransformType;
  typedef typename CompositeTransformType::Pointer                    CompositeTransformPointer;

  /**
   * Type for the output: Using Decorator pattern for enabling the transform to be
   * passed in the data pipeline
   */
  typedef DataObjectDecorator<TransformType>                          TransformOutputType;
  typedef typename TransformOutputType::Pointer                       TransformOutputPointer;

  /** array typedef **/
  typedef Array<SizeValueType>                                        ShrinkFactorsArrayType;
  typedef Array<RealType>                                             SmoothingSigmasArrayType;

  /** Interpolator typedefs */
  typedef InterpolateImageFunction<FixedImageType, RealType>          FixedInterpolatorType;
  typedef typename FixedInterpolatorType::Pointer                     FixedInterpolatorPointer;
  typedef InterpolateImageFunction<MovingImageType, RealType>         MovingInterpolatorType;
  typedef typename MovingInterpolatorType::Pointer                    MovingInterpolatorPointer;

  /** Transform adaptor typedefs */
  typedef TransformParametersAdaptor<TransformType>                   TransformParametersAdaptorType;
  typedef typename TransformParametersAdaptorType::Pointer            TransformParametersAdaptorPointer;
  typedef std::vector<TransformParametersAdaptorPointer>              TransformParametersAdaptorsContainerType;

  /**  Type of the optimizer. */
  typedef ObjectToObjectOptimizerBase                                 OptimizerType;
  typedef typename OptimizerType::Pointer                             OptimizerPointer;

  /** enum type for metric sampling strategy */
  enum MetricSamplingStrategyType { NONE, REGULAR, RANDOM };

  typedef typename MetricType::FixedSampledPointSetType               MetricSamplePointSetType;


  /** Set/Get the fixed image. */
  itkSetInputMacro( FixedImage, FixedImageType );
  itkGetInputMacro( FixedImage, FixedImageType );

  /** Set/Get the moving image. */
  itkSetInputMacro( MovingImage, MovingImageType );
  itkGetInputMacro( MovingImage, MovingImageType );

  /** Set/Get the fixed interpolator. */
  itkSetObjectMacro( FixedInterpolator, FixedInterpolatorType );
  itkGetConstObjectMacro( FixedInterpolator, FixedInterpolatorType );

  /** Set/Get the moving interpolator. */
  itkSetObjectMacro( MovingInterpolator, MovingInterpolatorType );
  itkGetConstObjectMacro( MovingInterpolator, MovingInterpolatorType );

  /** Set/Get the metric. */
  itkSetObjectMacro( Metric, MetricType );
  itkGetObjectMacro( Metric, MetricType );

  /** Set/Get the metric sampling strategy. */
  itkSetMacro( MetricSamplingStrategy, MetricSamplingStrategyType );
  itkGetConstMacro( MetricSamplingStrategy, MetricSamplingStrategyType );

  /** Set/Get the metric sampling percentage. */
  itkSetClampMacro( MetricSamplingPercentage, RealType, 0.0, 1.0 );
  itkGetConstMacro( MetricSamplingPercentage, RealType );

  /** Set/Get the transform. */
  itkSetObjectMacro( Transform, TransformType );
  itkGetObjectMacro( Transform, TransformType );

  /** Set/Get the optimizer. */
  itkSetObjectMacro( Optimizer, OptimizerType );
  itkGetObjectMacro( Optimizer, OptimizerType );

  /** Set/Get the composite transform (optional---useful for multi-stage registration). */
  itkSetObjectMacro( CompositeTransform, CompositeTransformType );
  itkGetObjectMacro( CompositeTransform, CompositeTransformType );

  /** Set/Get the transform adaptors. */
  void SetTransformParametersAdaptorsPerLevel( TransformParametersAdaptorsContainerType & );
  const TransformParametersAdaptorsContainerType & GetTransformParametersAdaptorsPerLevel() const;

  /**
   * Set/Get the number of multi-resolution levels.  In setting the number of
   * levels we need to set the following for each level:
   *   \li shrink factors for the virtual domain
   *   \li sigma smoothing parameter
   *   \li transform adaptor with specific parameters for the specified level
   */
  void SetNumberOfLevels( const SizeValueType );
  itkGetConstMacro( NumberOfLevels, SizeValueType );

  /**
   * Set/Get the shrink factors for each level. At each resolution level, the
   * \c itkShrinkImageFilter is called.  For a 256x256 image, a shrink factor
   * of 2 reduces the image to 128x128.
   */
  itkSetMacro( ShrinkFactorsPerLevel, ShrinkFactorsArrayType );
  itkGetConstMacro( ShrinkFactorsPerLevel, ShrinkFactorsArrayType );

  /**
   * Set/Get the smoothing sigmas for each level.  At each resolution level, a gaussian smoothing
   * filter (specifically, the \c itkDiscreteGaussianImageFilter) is applied.  Sigma values are
   * specified in physical units.
   */
  itkSetMacro( SmoothingSigmasPerLevel, SmoothingSigmasArrayType );
  itkGetConstMacro( SmoothingSigmasPerLevel, SmoothingSigmasArrayType );

  /** Method that initiates the registration */
  void StartRegistration() { this->GenerateData(); }

  /** Make a DataObject of the correct type to be used as the specified output. */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput( DataObjectPointerArraySizeType );

  /** Returns the transform resulting from the registration process  */
  virtual const TransformOutputType * GetOutput() const;

  /** Get the current level.  This is a helper function for reporting observations. */
  itkGetConstMacro( CurrentLevel, SizeValueType );

protected:
  ImageRegistrationMethodv4();
  virtual ~ImageRegistrationMethodv4();
  virtual void PrintSelf( std::ostream & os, Indent indent ) const;

  /** Perform the registration. */
  virtual void  GenerateData();

  /** Initialize by setting the interconnects between the components. */
  virtual void InitializeRegistrationAtEachLevel( const SizeValueType );

  /** Get metric samples. */
  virtual void SetMetricSamplePoints();

  SizeValueType                                                   m_CurrentLevel;
  SizeValueType                                                   m_NumberOfLevels;

  MovingImagePointer                                              m_MovingSmoothImage;
  FixedImagePointer                                               m_FixedSmoothImage;

  FixedInterpolatorPointer                                        m_FixedInterpolator;
  MovingInterpolatorPointer                                       m_MovingInterpolator;

  OptimizerPointer                                                m_Optimizer;

  MetricPointer                                                   m_Metric;
  MetricSamplingStrategyType                                      m_MetricSamplingStrategy;
  RealType                                                        m_MetricSamplingPercentage;

  ShrinkFactorsArrayType                                          m_ShrinkFactorsPerLevel;
  SmoothingSigmasArrayType                                        m_SmoothingSigmasPerLevel;

  TransformParametersAdaptorsContainerType                        m_TransformParametersAdaptorsPerLevel;

  CompositeTransformPointer                                       m_CompositeTransform;

  TransformPointer                                                m_Transform;

private:
  ImageRegistrationMethodv4( const Self & );   //purposely not implemented
  void operator=( const Self & );                  //purposely not implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegistrationMethodv4.hxx"
#endif

#endif

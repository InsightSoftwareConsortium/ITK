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

#include "itkCompositeTransform.h"
#include "itkDataObjectDecorator.h"
#include "itkObjectToObjectMetricBase.h"
#include "itkObjectToObjectMultiMetricv4.h"
#include "itkObjectToObjectOptimizerBase.h"
#include "itkImageToImageMetricv4.h"
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
 * both stages are performed in multiple levels.  Each level can be
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
 * Output: The output is the updated transform.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKRegistrationMethodsv4
 */
template<typename TFixedImage, typename TMovingImage, typename TOutputTransform>
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
  typedef std::vector<FixedImagePointer>                              FixedImagesContainerType;
  typedef TMovingImage                                                MovingImageType;
  typedef typename MovingImageType::Pointer                           MovingImagePointer;
  typedef std::vector<MovingImagePointer>                             MovingImagesContainerType;

  /** Metric and transform typedefs */
  typedef ObjectToObjectMetricBase                                    MetricType;
  typedef typename MetricType::Pointer                                MetricPointer;

  typedef ObjectToObjectMultiMetricv4<ImageDimension, ImageDimension> MultiMetricType;
  typedef ImageToImageMetricv4<FixedImageType, MovingImageType>       ImageMetricType;
  typedef typename ImageMetricType::VirtualImageType                  VirtualImageType;

  typedef TOutputTransform                                            OutputTransformType;
  typedef typename OutputTransformType::Pointer                       OutputTransformPointer;
  typedef typename OutputTransformType::ScalarType                    RealType;
  typedef typename OutputTransformType::DerivativeType                DerivativeType;
  typedef typename DerivativeType::ValueType                          DerivativeValueType;

  typedef Transform<RealType, ImageDimension, ImageDimension>         InitialTransformType;
  typedef typename InitialTransformType::Pointer                      InitialTransformPointer;

  typedef CompositeTransform<RealType, ImageDimension>                CompositeTransformType;
  typedef typename CompositeTransformType::Pointer                    CompositeTransformPointer;

  /**
   * Type for the output: Using Decorator pattern for enabling the transform to be
   * passed in the data pipeline
   */
  typedef DataObjectDecorator<OutputTransformType>                    DecoratedOutputTransformType;
  typedef typename DecoratedOutputTransformType::Pointer              DecoratedOutputTransformPointer;

  /** array typedef **/
  typedef Array<SizeValueType>                                        ShrinkFactorsArrayType;
  typedef Array<RealType>                                             SmoothingSigmasArrayType;
  typedef Array<RealType>                                             MetricSamplingPercentageArrayType;

  /** Transform adaptor typedefs */
  typedef TransformParametersAdaptor<OutputTransformType>             TransformParametersAdaptorType;
  typedef typename TransformParametersAdaptorType::Pointer            TransformParametersAdaptorPointer;
  typedef std::vector<TransformParametersAdaptorPointer>              TransformParametersAdaptorsContainerType;

  /**  Type of the optimizer. */
  typedef ObjectToObjectOptimizerBase                                 OptimizerType;
  typedef typename OptimizerType::Pointer                             OptimizerPointer;

  /** enum type for metric sampling strategy */
  enum MetricSamplingStrategyType { NONE, REGULAR, RANDOM };

  typedef typename ImageMetricType::FixedSampledPointSetType          MetricSamplePointSetType;

  /** Set/get the fixed images. */
  virtual void SetFixedImage( const FixedImageType *image )
    {
    this->SetFixedImage( 0, image );
    }
  virtual const FixedImageType * GetFixedImage() const
    {
    return this->GetFixedImage( 0 );
    }
  virtual void SetFixedImage( SizeValueType, const FixedImageType * );
  virtual const FixedImageType * GetFixedImage( SizeValueType ) const;

  /** Set the moving images. */
  virtual void SetMovingImage( const MovingImageType *image )
    {
    this->SetMovingImage( 0, image );
    }
  virtual const MovingImageType * GetMovingImage() const
    {
    return this->GetMovingImage( 0 );
    }
  virtual void SetMovingImage( SizeValueType, const MovingImageType * );
  virtual const MovingImageType * GetMovingImage( SizeValueType ) const;

  /** Set/Get the optimizer. */
  itkSetObjectMacro( Optimizer, OptimizerType );
  itkGetModifiableObjectMacro(Optimizer, OptimizerType );

  /** Set/Get the metric. */
  itkSetObjectMacro( Metric, MetricType );
  itkGetModifiableObjectMacro(Metric, MetricType );

  /** Set/Get the metric sampling strategy. */
  itkSetMacro( MetricSamplingStrategy, MetricSamplingStrategyType );
  itkGetConstMacro( MetricSamplingStrategy, MetricSamplingStrategyType );

  /** Set the metric sampling percentage. */
  void SetMetricSamplingPercentage( const RealType );

  /** Set the metric sampling percentage. */
  itkSetMacro( MetricSamplingPercentagePerLevel, MetricSamplingPercentageArrayType );
  itkGetConstMacro( MetricSamplingPercentagePerLevel, MetricSamplingPercentageArrayType );

  /** Set/Get the initial fixed transform. */
  itkSetObjectMacro( FixedInitialTransform, InitialTransformType );
  itkGetModifiableObjectMacro(FixedInitialTransform, InitialTransformType );

  /** Set/Get the initial moving transform. */
  itkSetObjectMacro( MovingInitialTransform, InitialTransformType );
  itkGetModifiableObjectMacro(MovingInitialTransform, InitialTransformType );

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
   * specified according to the option \c m_SmoothingSigmasAreSpecifiedInPhysicalUnits.
   */
  itkSetMacro( SmoothingSigmasPerLevel, SmoothingSigmasArrayType );
  itkGetConstMacro( SmoothingSigmasPerLevel, SmoothingSigmasArrayType );

  /**
   * Set/Get whether to specify the smoothing sigmas for each level in physical units
   * (default) or in terms of voxels.
   */
  itkSetMacro( SmoothingSigmasAreSpecifiedInPhysicalUnits, bool );
  itkGetConstMacro( SmoothingSigmasAreSpecifiedInPhysicalUnits, bool );
  itkBooleanMacro( SmoothingSigmasAreSpecifiedInPhysicalUnits );

  /** Make a DataObject of the correct type to be used as the specified output. */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput( DataObjectPointerArraySizeType );

  /** Returns the transform resulting from the registration process  */
  virtual const DecoratedOutputTransformType * GetOutput() const;

  /** Get the current level.  This is a helper function for reporting observations. */
  itkGetConstMacro( CurrentLevel, SizeValueType );

   /** Get the current iteration.  This is a helper function for reporting observations. */
   itkGetConstReferenceMacro( CurrentIteration, SizeValueType );

   /* Get the current metric value.  This is a helper function for reporting observations. */
   itkGetConstReferenceMacro( CurrentMetricValue, RealType );

   /** Get the current convergence value.  This is a helper function for reporting observations. */
   itkGetConstReferenceMacro( CurrentConvergenceValue, RealType );

   /** Get the current convergence state per level.  This is a helper function for reporting observations. */
   itkGetConstReferenceMacro( IsConverged, bool );

#ifdef ITKV3_COMPATIBILITY
  /** Method that initiates the registration. This will Initialize and ensure
   * that all inputs the registration needs are in place, via a call to
   * Initialize() will then start the optimization process via a call to
   * StartOptimization()
   * StartRegistration is an old API from before
   * ImageRegistrationMethod was a subclass of ProcessObject.
   * Historically, one could call StartRegistration() instead of
   * calling Update().  However, when called directly by the user, the
   * inputs to ImageRegistrationMethod may not be up to date.  This
   * may cause an unexpected behavior.
   *
   * Since we cannot eliminate StartRegistration for backward
   * compatibility reasons, we check whether StartRegistration was
   * called directly or whether Update() (which in turn called
   * StartRegistration()). */
  void StartRegistration(void) { this->Update(); }
#endif

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
  SizeValueType                                                   m_CurrentIteration;
  RealType                                                        m_CurrentMetricValue;
  RealType                                                        m_CurrentConvergenceValue;
  bool                                                            m_IsConverged;

  FixedImagesContainerType                                        m_FixedSmoothImages;
  MovingImagesContainerType                                       m_MovingSmoothImages;
  SizeValueType                                                   m_NumberOfFixedImages;
  SizeValueType                                                   m_NumberOfMovingImages;

  OptimizerPointer                                                m_Optimizer;

  MetricPointer                                                   m_Metric;
  MetricSamplingStrategyType                                      m_MetricSamplingStrategy;
  MetricSamplingPercentageArrayType                               m_MetricSamplingPercentagePerLevel;

  ShrinkFactorsArrayType                                          m_ShrinkFactorsPerLevel;
  SmoothingSigmasArrayType                                        m_SmoothingSigmasPerLevel;
  bool                                                            m_SmoothingSigmasAreSpecifiedInPhysicalUnits;

  InitialTransformPointer                                         m_MovingInitialTransform;
  InitialTransformPointer                                         m_FixedInitialTransform;

  TransformParametersAdaptorsContainerType                        m_TransformParametersAdaptorsPerLevel;

  CompositeTransformPointer                                       m_CompositeTransform;

  OutputTransformPointer                                          m_OutputTransform;

private:
  ImageRegistrationMethodv4( const Self & );   //purposely not implemented
  void operator=( const Self & );                  //purposely not implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegistrationMethodv4.hxx"
#endif

#endif

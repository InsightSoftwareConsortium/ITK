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
#ifndef itkImageToImageMetricv4_h
#define itkImageToImageMetricv4_h

#include "itkCovariantVector.h"
#include "itkImageFunction.h"
#include "itkObjectToObjectMetric.h"
#include "itkInterpolateImageFunction.h"
#include "itkSpatialObject.h"
#include "itkResampleImageFilter.h"
#include "itkThreadedIndexedContainerPartitioner.h"
#include "itkThreadedImageRegionPartitioner.h"
#include "itkImageToImageFilter.h"
#include "itkImageToImageMetricv4GetValueAndDerivativeThreader.h"
#include "itkPointSet.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkDefaultImageToImageMetricTraitsv4.h"

namespace itk
{
/** \class ImageToImageMetricv4
 *
 * Computes similarity between regions of two images, using two
 * user-supplied transforms, a 'fixed' transform and a 'moving' transform.
 *
 * \warning Integer-type images are not yet supported. See concept-checking
 * in DefaultImageToImageMetricTraitsv4.
 *
 * Templated over the fixed and moving image types, as well as an optional
 * VirtualImage type to define the virtual domain. The VirtualImage type
 * defaults to TFixedImage.
 * \note If TFixedImage is type VectorImage, then TVirtualImage must be set
 * separately to a non-VectorImage type, e.g. Image<unsigned char, dimension>.
 *
 * If the user does not set the virtual domain explicitly,
 * then it is created during the call to \c Initialize from
 * the fixed image by copying its information.
 * See ObjectToObjectMetric for more discussion on the virtual domain.
 *
 * At a minimum, the user must:
 *  1) Set images using SetFixedImage and SetMovingImage.
 *  2) Call Initialize.
 *
 * Image gradient calculations
 *
 * Image gradients can be calculated in one of two ways:
 * 1) Using a gradient image filter, by setting
 *  \c Use[Fixed|Moving]ImageGradientFilter to true. By default this is set
 *  as an itkGradientRecursiveGaussianImageFilter, a
 *  smoothed gradient filter. A filter uses more memory, because it
 *  calculates all gradients at once and stores them in an image. The advantage
 *  of pre-calculation is for the fixed image gradients, since they only need be
 *  calculated once, and for metrics that need to access image gradients more
 *  than once for a particular point. The fixed image gradients are only
 *  calculated once when this option is set, during \c Initialize.
 * 2) Otherwise, an image gradient calculator based on ImageFunction is used.
 *  By default the CentralDifferenceImageFunction is used. This calculation
 *  is not smoothed and gives different results than
 *  GradientRecursiveGaussianImageFilter. The advantage is that less memory is
 *  used. However for the fixed image, it means needlessly computing the image
 *  gradients at each iteration of a registration instead of just computing
 *  once at the beginning. The user can supply a different function by calling
 *  SetFixedImageGradientCalculator and/or SetMovingImageGradientCalculator.
 *
 * Both image gradient calculation methods are threaded.
 * Generally it is not recommended to use different image gradient methods for
 * the fixed and moving images because the methods return different results.
 *
 * Image Masks
 *
 * Image masks are supported using SetMovingImageMask or SetFixedImageMask.
 * If the image mask is sparse, see the comments for use of sparse point sets.
 *
 * Sparse Sampling
 *
 * Sparse sampling is performed by supplying an arbitrary point list over
 * which to evaluate the
 * metric. It's presumed that the user will be working in terms of the fixed
 * image domain, and thus the point list is expected to be in the fixed domain.
 * Internally, the points are transformed into the virtual domain as needed.
 * \note The attributes/data of each point in the set are not used, but rather
 * the point's geometric coordinates.
 * Point sets are set via SetFixedSampledPointSet, and the point set is enabled
 * for use by calling SetUseFixedSampledPointSet.
 * \note If the point set is sparse, the option SetUse[Fixed|Moving]ImageGradientFilter
 * typically should be disabled to avoid excessive computation. However,
 * the gradient values of the fixed image are not cached
 * when using a point set (there are plans for this in the future), so
 * depending on the number of iterations (when used during optimization)
 * and the level of sparsity, it may be more efficient to
 * use a gradient image filter for it because it will only be
 * calculated once.
 *
 * Vector Images
 *
 * To support vector images, the class must be declared using the
 * VectorImageToImageMetricTraitsv4 class in the template declaration,
 * as described above.
 * Derived classes must provide special handling for vector pixel
 * types. MeanSquaresImageToImageMetricv4 can be used as an example.
 *
 * Threading
 *
 * This class is threaded. Threading is handled by friend classes
 * ImageToImageMetricv4GetValueAndDerivativeThreaderBase and
 * ImageToImageMetricv4GetValueAndDerivativeThreader. Dense and sparse
 * evaluation are handled by template specialization of the
 * ImageToImageMetricv4GetValueAndDerivativeThreader::ThreadedExecution
 * method, in order to iterate over either all points in the virtual space in
 * the case of dense evaluation, or a list of points in the sparse case.
 *
 * Methods and members of ImageToImageMetricv4 are accessed by
 * the threading class using its m_Associate member, which points
 * to the containing instance of ImageToImageMetricv4.
 *
 * Pre- and post-processing for threaded operation is handled in
 *  ImageToImageMetricv4GetValueAndDerivativeThreaderBase::BeforeThreadedExecution, and
 * ImageToImageMetricv4GetValueAndDerivativeThreaderBase::AfterThreadedExecution,
 * respectively.
 *
 * Derived classes:
 *
 *  The GetValue method may be overridden to provide better-optimized or
 *  otherwise different behavior as needed. Otherwise, the m_ComputeDerivative
 *  member should be checked to avoid computing derivatives when the caller
 *  has called GetValue(). See GetComputeDerivative() in this class and in
 *  this metric's threader class.
 *
 *  Derived classes must derive a threader class from
 *  ImageToImageMetricv4GetValueAndDerivativeThreader, from which
 *  a DenseGetValueAndDerivativeThreader and SparseGetValueAndDerivativeThreader
 *  must be defined. Then,
 *  \code
 *    this->m_DenseGetValueAndDerivativeThreader   = DenseDerivedClassGetValueAndDerivativeThreader::New();
 *    this->m_SparseGetValueAndDerivativeThreader  = SparseDerivedClassGetValueAndDerivativeThreader::New();
 *  \endcode
 *  must be called in the constructor.
 *
 *  The ProcessPoint method of the derived threader must be overriden to
 *  provide the metric-specific evaluation.
 *
 *  To access methods and members within the derived metric class from the
 *  derived threader class, the user must cast m_Associate to the type of the
 *  derived metric class.
 *
 *  See \c ImageToImageMetricv4Test for a clear example of what a
 *  derived class must implement and do.
 *
 * \ingroup ITKMetricsv4
 */
template<typename TFixedImage,typename TMovingImage,typename TVirtualImage = TFixedImage,
         typename TInternalComputationValueType = double,
         typename TMetricTraits = DefaultImageToImageMetricTraitsv4< TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType >
         >
class ITK_TEMPLATE_EXPORT ImageToImageMetricv4
  : public ObjectToObjectMetric<TFixedImage::ImageDimension, TMovingImage::ImageDimension, TVirtualImage, TInternalComputationValueType>
{
public:

  /** Standard class typedefs. */
  typedef ImageToImageMetricv4                                                                                                     Self;
  typedef ObjectToObjectMetric<TFixedImage::ImageDimension, TMovingImage::ImageDimension, TVirtualImage, TInternalComputationValueType>  Superclass;
  typedef SmartPointer<Self>                                                                                                       Pointer;
  typedef SmartPointer<const Self>                                                                                                 ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToImageMetricv4, ObjectToObjectMetric);

  /** Type used internally for computations */
  /** It should be possible to derive the internal computation type from the class object. */
  typedef TInternalComputationValueType                     InternalComputationValueType;

  /** Type used for representing parameter values  */
  typedef typename Superclass::CoordinateRepresentationType CoordinateRepresentationType;

  /**  Type of the parameters. */
  typedef typename Superclass::ParametersType       ParametersType;
  typedef typename Superclass::ParametersValueType  ParametersValueType;

  /** Gradient source type */
  typedef typename Superclass::GradientSourceType GradientSourceType;

  /** Dimension type */
  typedef typename Superclass::DimensionType      DimensionType;
  typedef typename Superclass::DimensionType      ImageDimensionType;

  /** Transform types from Superclass*/
  typedef typename Superclass::FixedTransformType            FixedTransformType;
  typedef typename Superclass::FixedTransformPointer         FixedTransformPointer;
  typedef typename Superclass::FixedInputPointType           FixedInputPointType;
  typedef typename Superclass::FixedOutputPointType          FixedOutputPointType;
  typedef typename Superclass::FixedTransformParametersType  FixedTransformParametersType;

  typedef typename Superclass::MovingTransformType            MovingTransformType;
  typedef typename Superclass::MovingTransformPointer         MovingTransformPointer;
  typedef typename Superclass::MovingInputPointType           MovingInputPointType;
  typedef typename Superclass::MovingOutputPointType          MovingOutputPointType;
  typedef typename Superclass::MovingTransformParametersType  MovingTransformParametersType;

  typedef typename Superclass::JacobianType                   JacobianType;
  typedef typename Superclass::FixedTransformJacobianType     FixedTransformJacobianType;
  typedef typename Superclass::MovingTransformJacobianType    MovingTransformJacobianType;

  typedef typename Superclass::ObjectType                     ObjectType;

  /** Image-accessor typedefs */
  typedef TFixedImage                             FixedImageType;
  typedef typename FixedImageType::PixelType      FixedImagePixelType;
  typedef FixedImagePixelType                     FixedPixelType;
  typedef typename FixedImageType::Pointer        FixedImagePointer;
  typedef typename FixedImageType::ConstPointer   FixedImageConstPointer;
  typedef typename FixedImageType::PointType      FixedImagePointType;
  typedef typename FixedImageType::IndexType      FixedImageIndexType;
  typedef TMovingImage                            MovingImageType;
  typedef typename MovingImageType::PixelType     MovingImagePixelType;
  typedef MovingImagePixelType                    MovingPixelType;
  typedef typename MovingImageType::Pointer       MovingImagePointer;
  typedef typename MovingImageType::ConstPointer  MovingImageConstPointer;
  typedef typename MovingImageType::PointType     MovingImagePointType;
  typedef typename MovingImageType::RegionType    MovingImageRegionType;
  typedef typename MovingImageType::IndexType     MovingImageIndexType;

  /** Types for the virtual domain */
  typedef typename Superclass::VirtualImageType       VirtualImageType;
  typedef typename Superclass::VirtualImagePointer    VirtualImagePointer;
  typedef typename Superclass::VirtualPixelType       VirtualPixelType;
  typedef typename Superclass::VirtualRegionType      VirtualRegionType;
  typedef typename Superclass::VirtualSizeType        VirtualSizeType;
  typedef typename Superclass::VirtualSpacingType     VirtualSpacingType;
  typedef typename Superclass::VirtualPointType       VirtualOriginType;
  typedef typename Superclass::VirtualPointType       VirtualPointType;
  typedef typename Superclass::VirtualDirectionType   VirtualDirectionType;
  typedef typename Superclass::VirtualSizeType        VirtualRadiusType;
  typedef typename Superclass::VirtualIndexType       VirtualIndexType;
  typedef typename Superclass::VirtualPointSetType    VirtualPointSetType;
  typedef typename Superclass::VirtualPointSetPointer VirtualPointSetPointer;

  /** Typedef for traits class */
  typedef TMetricTraits MetricTraits;

  /* Image dimension accessors */
  itkStaticConstMacro(FixedImageDimension, DimensionType, Superclass::FixedDimension);
  itkStaticConstMacro(MovingImageDimension, DimensionType, Superclass::MovingDimension);
  itkStaticConstMacro(VirtualImageDimension, DimensionType, Superclass::VirtualDimension);

  /**  Type for the mask of the fixed image. Only pixels that are "inside"
       this mask will be considered for the computation of the metric */
  typedef SpatialObject< itkGetStaticConstMacro(FixedImageDimension) >  FixedImageMaskType;
  typedef typename FixedImageMaskType::Pointer                          FixedImageMaskPointer;
  typedef typename FixedImageMaskType::ConstPointer                     FixedImageMaskConstPointer;

  /**  Type for the mask of the moving image. Only pixels that are "inside"
       this mask will be considered for the computation of the metric */
  typedef SpatialObject< itkGetStaticConstMacro(MovingImageDimension) > MovingImageMaskType;
  typedef typename MovingImageMaskType::Pointer                         MovingImageMaskPointer;
  typedef typename MovingImageMaskType::ConstPointer                    MovingImageMaskConstPointer;

  /** Type of the point set used for sparse sampling. */
  typedef PointSet<typename FixedImageType::PixelType, itkGetStaticConstMacro(FixedImageDimension)>
                                                                        FixedSampledPointSetType;
  typedef typename FixedSampledPointSetType::Pointer                    FixedSampledPointSetPointer;
  typedef typename FixedSampledPointSetType::ConstPointer               FixedSampledPointSetConstPointer;

  /**  Type of the Interpolator Base class */
  typedef InterpolateImageFunction< FixedImageType,
                                    CoordinateRepresentationType >
                                                      FixedInterpolatorType;
  typedef InterpolateImageFunction< MovingImageType,
                                    CoordinateRepresentationType >
                                                      MovingInterpolatorType;
  typedef typename FixedInterpolatorType::Pointer     FixedInterpolatorPointer;
  typedef typename MovingInterpolatorType::Pointer    MovingInterpolatorPointer;

  /** Image derivatives types */
  typedef typename MetricTraits::FixedImageGradientType    FixedImageGradientType;
  typedef typename MetricTraits::MovingImageGradientType   MovingImageGradientType;
  typedef typename MetricTraits::VirtualImageGradientType  VirtualImageGradientType;

  typedef CovariantVector<typename FixedImageGradientType::ValueType, FixedImageDimension>
    FixedImageComponentGradientType;

  typedef CovariantVector<typename MovingImageGradientType::ValueType, MovingImageDimension>
    MovingImageComponentGradientType;

  typedef CovariantVector<typename VirtualImageGradientType::ValueType, VirtualImageDimension>
    VirtualImageComponentGradientType;

  /** Type of the filter used to calculate the gradients.
   * Note that RealType is always double (or long double for
   * long double pixel-type).*/
  typedef typename MetricTraits::FixedRealType  FixedRealType;
  typedef typename MetricTraits::MovingRealType MovingRealType;

  typedef typename NumericTraits<FixedRealType>::ScalarRealType  FixedScalarRealType;
  typedef typename NumericTraits<MovingRealType>::ScalarRealType MovingScalarRealType;

  typedef typename MetricTraits::FixedGradientPixelType  FixedGradientPixelType;
  typedef typename MetricTraits::MovingGradientPixelType MovingGradientPixelType;

  typedef typename MetricTraits::FixedImageGradientImageType  FixedImageGradientImageType;
  typedef typename MetricTraits::MovingImageGradientImageType MovingImageGradientImageType;

  typedef typename FixedImageGradientImageType::Pointer  FixedImageGradientImagePointer;
  typedef typename MovingImageGradientImageType::Pointer MovingImageGradientImagePointer;

  typedef typename MetricTraits::FixedImageGradientFilterType  FixedImageGradientFilterType;
  typedef typename MetricTraits::MovingImageGradientFilterType MovingImageGradientFilterType;

  typedef typename FixedImageGradientFilterType::Pointer
                                              FixedImageGradientFilterPointer;
  typedef typename MovingImageGradientFilterType::Pointer
                                              MovingImageGradientFilterPointer;


  /** Default image gradient filter types */
  typedef typename MetricTraits::DefaultFixedImageGradientFilter  DefaultFixedImageGradientFilter;
  typedef typename MetricTraits::DefaultMovingImageGradientFilter DefaultMovingImageGradientFilter;

  /** Image gradient calculator types. The TOutput template parameter
   * is chosen to match that of CentralDiffererenceImageFunction. */
  typedef typename MetricTraits::FixedImageGradientCalculatorType
                                            FixedImageGradientCalculatorType;
  typedef typename MetricTraits::MovingImageGradientCalculatorType
                                            MovingImageGradientCalculatorType;

  typedef typename FixedImageGradientCalculatorType::Pointer
                                            FixedImageGradientCalculatorPointer;
  typedef typename MovingImageGradientCalculatorType::Pointer
                                            MovingImageGradientCalculatorPointer;

  /** Default image gradient calculator types */
  typedef typename MetricTraits::DefaultFixedImageGradientCalculator  DefaultFixedImageGradientCalculator;
  typedef typename MetricTraits::DefaultMovingImageGradientCalculator DefaultMovingImageGradientCalculator;

  /**  Type of the measure. */
  typedef typename Superclass::MeasureType    MeasureType;

  /**  Type of the metric derivative. */
  typedef typename Superclass::DerivativeType DerivativeType;
  typedef typename DerivativeType::ValueType  DerivativeValueType;

  /** Type to represent the number of parameters that are being optimized at
   * any given iteration of the optimizer. */
  typedef typename Superclass::NumberOfParametersType   NumberOfParametersType;

  /** Set fixed image*/
  virtual void SetFixedObject( const ObjectType *object ) ITK_OVERRIDE
    {
    FixedImageType *image = dynamic_cast<FixedImageType *>( const_cast<ObjectType *>( object ) );
    if( image != ITK_NULLPTR )
      {
      this->SetFixedImage( image );
      }
    else
      {
      itkExceptionMacro( "Incorrect object type.  Should be an image." )
      }
    }

  /** Set moving image*/
  virtual void SetMovingObject( const ObjectType *object ) ITK_OVERRIDE
    {
    MovingImageType *image = dynamic_cast<MovingImageType *>( const_cast<ObjectType *>( object ) );
    if( image != ITK_NULLPTR )
      {
      this->SetMovingImage( image );
      }
    else
      {
      itkExceptionMacro( "Incorrect object type.  Should be an image." )
      }
    }

  /* Get/Set the Fixed Image.  */
  itkSetConstObjectMacro(FixedImage, FixedImageType);
  itkGetConstObjectMacro(FixedImage, FixedImageType);

  /** Get/Set the Moving Image.  */
  itkSetConstObjectMacro(MovingImage, MovingImageType);
  itkGetConstObjectMacro(MovingImage, MovingImageType);

  /** Connect the fixed interpolator. */
  itkSetObjectMacro(FixedInterpolator, FixedInterpolatorType);
  /** Get a pointer to the fixed interpolator.  */
  itkGetModifiableObjectMacro(FixedInterpolator, FixedInterpolatorType);

  /** Connect the Moving interpolator. */
  itkSetObjectMacro(MovingInterpolator, MovingInterpolatorType);
  /** Get a pointer to the Moving interpolator.  */
  itkGetModifiableObjectMacro(MovingInterpolator, MovingInterpolatorType);

  /** Set/Get the moving image mask. */
  itkSetObjectMacro(MovingImageMask, MovingImageMaskType);
  itkSetConstObjectMacro(MovingImageMask, MovingImageMaskType);
  itkGetConstObjectMacro(MovingImageMask, MovingImageMaskType);

  /** Set/Get the fixed image mask. */
  itkSetObjectMacro(FixedImageMask, FixedImageMaskType);
  itkSetConstObjectMacro(FixedImageMask, FixedImageMaskType);
  itkGetConstObjectMacro(FixedImageMask, FixedImageMaskType);

  /** Set/Get the fixed image domain sampling point set
   * See main documentation regarding using fixed vs virtual domain
   * for the point set. */
  itkSetObjectMacro(FixedSampledPointSet, FixedSampledPointSetType);
  itkSetConstObjectMacro(FixedSampledPointSet, FixedSampledPointSetType);
  itkGetConstObjectMacro(FixedSampledPointSet, FixedSampledPointSetType);

  /** Set/Get flag to use fixed image domain sampling point set */
  itkSetMacro(UseFixedSampledPointSet, bool);
  itkGetConstReferenceMacro(UseFixedSampledPointSet, bool);
  itkBooleanMacro(UseFixedSampledPointSet);

  /** Get the virtual domain sampling point set */
  itkGetModifiableObjectMacro(VirtualSampledPointSet, VirtualPointSetType);

  /** Set/Get the gradient filter */
  itkSetObjectMacro( FixedImageGradientFilter, FixedImageGradientFilterType );
  itkGetModifiableObjectMacro(FixedImageGradientFilter, FixedImageGradientFilterType );
  itkSetObjectMacro( MovingImageGradientFilter, MovingImageGradientFilterType );
  itkGetModifiableObjectMacro(MovingImageGradientFilter, MovingImageGradientFilterType );

  /** Set/Get gradient calculators */
  itkSetObjectMacro( FixedImageGradientCalculator, FixedImageGradientCalculatorType);
  itkGetModifiableObjectMacro(FixedImageGradientCalculator, FixedImageGradientCalculatorType);
  itkSetObjectMacro( MovingImageGradientCalculator, MovingImageGradientCalculatorType);
  itkGetModifiableObjectMacro(MovingImageGradientCalculator, MovingImageGradientCalculatorType);

  /** Set/Get gradient computation via an image filter,
   * for fixed image. */
  itkSetMacro(UseFixedImageGradientFilter, bool);
  itkGetConstReferenceMacro(UseFixedImageGradientFilter, bool);
  itkBooleanMacro(UseFixedImageGradientFilter);

  /** Set/Get gradient computation via an image filter. */
  itkSetMacro(UseMovingImageGradientFilter, bool);
  itkGetConstReferenceMacro(UseMovingImageGradientFilter, bool);
  itkBooleanMacro(UseMovingImageGradientFilter);

  /** Get number of threads to used in the the most recent
   * evaluation.  Only valid after GetValueAndDerivative() or
   * GetValue() has been called. */
  virtual ThreadIdType GetNumberOfThreadsUsed() const;

  /** Set number of threads to use. This the maximum number of threads to use
   * when multithreaded.  The actual number of threads used (may be less than
   * this value) can be obtained with \c GetNumberOfThreadsUsed. */
  virtual void SetMaximumNumberOfThreads( const ThreadIdType threads );
  virtual ThreadIdType GetMaximumNumberOfThreads() const;

  /**
    * Finalize the per-thread components for computing
    * metric.  Some threads can accumulate their data
    * as the thread finishes rather than waiting
    * for all threads to finish before the accumulation
    * occurs.
    */
  virtual void FinalizeThread( const ThreadIdType /*threadId*/ ) { /*Do nothing by default */ }

  /** Get Fixed Gradient Image. */
  itkGetModifiableObjectMacro(FixedImageGradientImage, FixedImageGradientImageType);

  /** Get Moving Gradient Image. */
  itkGetModifiableObjectMacro(MovingImageGradientImage, MovingImageGradientImageType);

  /** Get number of valid points from most recent update */
  virtual SizeValueType GetNumberOfValidPoints() const ITK_OVERRIDE
    {
    return this->m_NumberOfValidPoints;
    }

  /** Get the number of points in the domain used to evaluate
   * the metric. This will differ depending on whether a sampled
   * point set or dense sampling is used, and will be greater than
   * or equal to GetNumberOfValidPoints(). */
  SizeValueType GetNumberOfDomainPoints() const;

  /** Set/Get the option for applying floating point resolution truncation
   * to derivative calculations in global support cases. False by default. It is only
   * applied in global support cases (i.e. with global-support transforms) because
   * in these cases, the per-point derivative values are added cumulatively,
   * which can lead to loss of precision when the sum becomes much larger than
   * the values being added.
   * The goal is more consistent results across the number of threads used for an evaluation.
   * The resolution can be changed using SetFloatingPointCorrectionResolution().
   * \note The metric always sums derivative values using a CompensatedSummation object,
   * but empirically this provides only a slight improvement in precision across number
   * of threads during registration.
   * \warning The metric does not perform any normalization so the results
   * of this truncation are highly dependent on the derivative magnitudes. */
  itkSetMacro(UseFloatingPointCorrection, bool);
  itkGetConstReferenceMacro(UseFloatingPointCorrection, bool);
  itkBooleanMacro(UseFloatingPointCorrection);

  /** Set/Get the floating point resolution used optionally by the derivatives.
   * If this is set, for example to 1e5, then the derivative will have precision up to 5
   * points beyond the decimal point. And precision beyond that will be
   * truncated. */
  itkSetMacro( FloatingPointCorrectionResolution, DerivativeValueType );
  itkGetConstMacro( FloatingPointCorrectionResolution, DerivativeValueType );

  /* Initialize the metric before calling GetValue or GetDerivative.
   * Derived classes must call this Superclass version if they override
   * this to perform their own initialization.
   * \note This is meant to be called once for a particular metric setup.
   * That is, when used in registration, this method would be called once
   * before entering the registration loop, during which GetValue or
   * GetDerivative will be called repeatedly. It must be called again if
   * metric settings are changed before beginning a new registration. */
  virtual void Initialize(void) ITK_OVERRIDE;

  virtual MeasureType GetValue() const ITK_OVERRIDE;

  virtual void GetDerivative( DerivativeType & ) const ITK_OVERRIDE;

  /** Calculate and return both the value for the metric and its derivative.
   * This calls the SparseGetValueAndDerivativeThreader if \c UsedFixedSampledPointSet
   * is true, and DenseGetValueAndDerivativeThreader otherwise.  The threaders
   * in turn call \c ProcessPoint on each point in the
   * domain to be examined. */
  virtual void GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const ITK_OVERRIDE;

  /** Get the number of sampled fixed sampled points that are
   * deemed invalid during conversion to virtual domain in Initialize().
   * For informational purposes. */
  itkGetConstReferenceMacro(NumberOfSkippedFixedSampledPoints, SizeValueType);

  virtual bool SupportsArbitraryVirtualDomainSamples( void ) const ITK_OVERRIDE
  {
    return true;
  }

  typedef typename Superclass::MetricCategoryType   MetricCategoryType;

  /** Get metric category */
  virtual MetricCategoryType GetMetricCategory() const ITK_OVERRIDE
    {
    return Superclass::IMAGE_METRIC;
    }

protected:
  /* Interpolators for image gradient filters. */
  typedef LinearInterpolateImageFunction< FixedImageGradientImageType,
                                          CoordinateRepresentationType >
                                                  FixedImageGradientInterpolatorType;
  typedef LinearInterpolateImageFunction< MovingImageGradientImageType,
                                          CoordinateRepresentationType >
                                                  MovingImageGradientInterpolatorType;

  friend class ImageToImageMetricv4GetValueAndDerivativeThreaderBase< ThreadedImageRegionPartitioner< VirtualImageDimension >, Self >;
  friend class ImageToImageMetricv4GetValueAndDerivativeThreaderBase< ThreadedIndexedContainerPartitioner, Self >;
  friend class ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< VirtualImageDimension >, Self >;
  friend class ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Self >;

  /* A DenseGetValueAndDerivativeThreader
   * Derived classes must define this class and assign it in their constructor
   * if threaded processing in GetValueAndDerivative is performed. */
  typename ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< VirtualImageDimension >, Self >::Pointer m_DenseGetValueAndDerivativeThreader;
  /* A SparseGetValueAndDerivativeThreader
   * Derived classes must define this class and assign it in their constructor
   * if threaded processing in GetValueAndDerivative is performed. */
  typename ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Self >::Pointer m_SparseGetValueAndDerivativeThreader;

  /** Perform any initialization required before each evaluation of
   * \c GetValueAndDerivative. This is distinct from Initialize, which
   * is called only once before a number of iterations, e.g. before
   * a registration loop. */
  virtual void InitializeForIteration() const;

  /**
   * Transform a point from VirtualImage domain to FixedImage domain and evaluate.
   * This function also checks if mapped point is within the mask if
   * one is set, and that is within the fixed image buffer, in which
   * case the return value will be true.
   * Parameters \c mappedFixedPoint and \c mappedFixedPixelValue are  returned.
   */
  bool TransformAndEvaluateFixedPoint(
                         const VirtualPointType & virtualPoint,
                         FixedImagePointType & mappedFixedPoint,
                         FixedImagePixelType & mappedFixedPixelValue ) const;

  /** Transform and evaluate a point from VirtualImage domain to MovingImage domain. */
  bool TransformAndEvaluateMovingPoint(
                         const VirtualPointType & virtualPoint,
                         MovingImagePointType & mappedMovingPoint,
                         MovingImagePixelType & mappedMovingPixelValue ) const;

  /** Compute image derivatives for a Fixed point. */
  virtual void ComputeFixedImageGradientAtPoint( const FixedImagePointType & mappedPoint, FixedImageGradientType & gradient ) const;

  /** Compute image derivatives for a moving point. */
  virtual void ComputeMovingImageGradientAtPoint( const MovingImagePointType & mappedPoint, MovingImageGradientType & gradient ) const;

  /** Computes the gradients of the fixed image, using the
   * GradientFilter, assigning the output to
   * to m_FixedImageGradientImage. */
  virtual void ComputeFixedImageGradientFilterImage();

  /** Computes the gradients of the moving image, using the
   * GradientFilter, assigning the output to
   * to m_MovingImageGradientImage. */
  virtual void ComputeMovingImageGradientFilterImage() const;

  /** Perform the actual threaded processing, using the appropriate
   * GetValueAndDerivativeThreader. Results get written to
   * member vars. This is available as a separate method so it
   * can be used by dervied classes that implement their own
   * GetValueAndDerivative, and/or need to run the processing loop
   * more than once.*/
  virtual void GetValueAndDerivativeExecute() const;

  /** Initialize the default image gradient filters. This must only
   * be called once the fixed and moving images have been set. */
  virtual void InitializeDefaultFixedImageGradientFilter();
  virtual void InitializeDefaultMovingImageGradientFilter();

  /** Get accessor for flag to calculate derivative. */
  itkGetConstMacro( ComputeDerivative, bool );

  FixedImageConstPointer  m_FixedImage;
  MovingImageConstPointer m_MovingImage;

  /** Pointers to interpolators */
  FixedInterpolatorPointer                                m_FixedInterpolator;
  MovingInterpolatorPointer                               m_MovingInterpolator;
  typename FixedImageGradientInterpolatorType::Pointer    m_FixedImageGradientInterpolator;
  typename MovingImageGradientInterpolatorType::Pointer   m_MovingImageGradientInterpolator;

  /** Flag to control use of precomputed gradient filter image or gradient
   * calculator for image gradient calculations. */
  bool                          m_UseFixedImageGradientFilter;
  bool                          m_UseMovingImageGradientFilter;

  /** Gradient filters */
  FixedImageGradientFilterPointer   m_FixedImageGradientFilter;
  MovingImageGradientFilterPointer  m_MovingImageGradientFilter;

  /** Pointer to default gradient filter. Used for easier
   * initialization of the default filter. */
  typename DefaultFixedImageGradientFilter::Pointer
                                             m_DefaultFixedImageGradientFilter;
  typename DefaultMovingImageGradientFilter::Pointer
                                             m_DefaultMovingImageGradientFilter;

  /** Pointer to default gradient calculators. Used for easier
   * initialization of the default filter. */
  typename DefaultFixedImageGradientCalculator::Pointer
                                             m_DefaultFixedImageGradientCalculator;
  typename DefaultMovingImageGradientCalculator::Pointer
                                             m_DefaultMovingImageGradientCalculator;

  /** Gradient images to store gradient filter output. */
  mutable FixedImageGradientImagePointer    m_FixedImageGradientImage;
  mutable MovingImageGradientImagePointer   m_MovingImageGradientImage;

  /** Image gradient calculators */
  FixedImageGradientCalculatorPointer   m_FixedImageGradientCalculator;
  MovingImageGradientCalculatorPointer  m_MovingImageGradientCalculator;

  /** Derivative results holder. User a raw pointer so we can point it
   * to a user-provided object. This is used in internal methods so
   * the user-provided variable does not have to be passed around. It also enables
   * safely sharing a derivative object between metrics during multi-variate
   * analsys, for memory efficiency.
   * Will be ITK_NULLPTR if not set. */
  mutable DerivativeType *                m_DerivativeResult;

  /** Masks */
  FixedImageMaskConstPointer              m_FixedImageMask;
  MovingImageMaskConstPointer             m_MovingImageMask;

  /** Sampled point sets */
  FixedSampledPointSetConstPointer        m_FixedSampledPointSet;
  VirtualPointSetPointer                  m_VirtualSampledPointSet;

  /** Flag to use FixedSampledPointSet, i.e. Sparse sampling. */
  bool                                    m_UseFixedSampledPointSet;

  ImageToImageMetricv4();
  virtual ~ImageToImageMetricv4() ITK_OVERRIDE;

  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

private:
  /** Map the fixed point set samples to the virtual domain */
  void MapFixedSampledPointSetToVirtual();

  /** Transform a point. Avoid cast if possible */
  void LocalTransformPoint(const typename FixedTransformType::OutputPointType &virtualPoint,
                           typename FixedTransformType::OutputPointType &mappedFixedPoint) const
    {
      mappedFixedPoint = this->m_FixedTransform->TransformPoint(virtualPoint);
    }
  // cast the virtual point
  template <typename TVirtualPoint>
  void LocalTransformPoint(const TVirtualPoint &virtualPoint,
                           typename FixedTransformType::OutputPointType  &mappedFixedPoint) const
    {
      typename FixedTransformType::OutputPointType localVirtualPoint;

      localVirtualPoint.CastFrom(virtualPoint);

      mappedFixedPoint = this->m_FixedTransform->TransformPoint( localVirtualPoint );
    }
  // cast the mapped Fixed Point
  template <typename TFixedImagePoint>
  void LocalTransformPoint(const typename FixedTransformType::OutputPointType &virtualPoint,
                           TFixedImagePoint &mappedFixedPoint) const
    {
      typename FixedTransformType::OutputPointType localMappedFixedPoint;
      localMappedFixedPoint.CastFrom(mappedFixedPoint);
      localMappedFixedPoint = this->m_FixedTransform->TransformPoint( virtualPoint );
      mappedFixedPoint.CastFrom(localMappedFixedPoint);
    }
  // cast both mapped and fixed point.
  template <typename TVirtualPoint,typename TFixedImagePoint>
  void LocalTransformPoint(const TVirtualPoint &virtualPoint,
                           TFixedImagePoint &mappedFixedPoint) const
    {
      typename FixedTransformType::OutputPointType localVirtualPoint;
      typename FixedTransformType::OutputPointType localMappedFixedPoint;

      localVirtualPoint.CastFrom(virtualPoint);
      localMappedFixedPoint.CastFrom(mappedFixedPoint);

      localMappedFixedPoint = this->m_FixedTransform->TransformPoint( localVirtualPoint );
      mappedFixedPoint.CastFrom(localMappedFixedPoint);
    }

  /** Flag for warning about use of GetValue. Will be removed when
   *  GetValue implementation is improved. */
  mutable bool m_HaveMadeGetValueWarning;

  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToImageMetricv4);

  /** Keep track of the number of sampled fixed points that are
   * deemed invalid during conversion to virtual domain.
   * For informational purposes. */
  SizeValueType m_NumberOfSkippedFixedSampledPoints;

  bool                m_UseFloatingPointCorrection;
  DerivativeValueType m_FloatingPointCorrectionResolution;

  MetricTraits m_MetricTraits;

  /** Flag to know if derivative should be calculated */
  mutable bool        m_ComputeDerivative;

  /** Only floating-point images are currently supported. To support integer images,
   * several small changes must be made */
  #ifdef ITK_USE_CONCEPT_CHECKING
  typedef typename PixelTraits<FixedImagePixelType>::ValueType  FixedImagePixelValueType;
  typedef typename PixelTraits<MovingImagePixelType>::ValueType MovingImagePixelValueType;
  itkConceptMacro( OnlyDefinedForFloatingPointTypes0, ( itk::Concept::IsFloatingPoint<FixedImagePixelValueType> ) );
  itkConceptMacro( OnlyDefinedForFloatingPointTypes1, ( itk::Concept::IsFloatingPoint<MovingImagePixelValueType> ) );
  #endif // ITK_USE_CONCEPT_CHECKING

};
}//namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageMetricv4.hxx"
#endif

#endif

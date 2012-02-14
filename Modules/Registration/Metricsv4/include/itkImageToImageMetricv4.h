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
#ifndef __itkImageToImageMetricv4_h
#define __itkImageToImageMetricv4_h

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
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkPointSet.h"

namespace itk
{
/** \class ImageToImageMetricv4
 *
 * Computes similarity between regions of two images, using two
 * user-supplied transforms, a 'fixed' transform and a 'moving' transform.
 *
 * \warning Integer-type images are not yet supported. See concept-checking
 * in class declaration for details.
 *
 * \note Currently GetValue is implemented in this base simply by calling
 * GetValueAndDerivative and discarding the derivative. This is inefficient
 * but sufficient to match API requirements. Derived classes may
 * override and implement differently.
 *
 * Templated over the fixed and moving image types, as well as an optional
 * VirtualImage type to define the virtual domain. The VirtualImage type
 * defaults to TFixedImage.
 *
 * This class uses a virtual reference space. This space defines the resolution
 * at which the registration is performed, as well as the physical coordinate
 * system. This is useful for unbiased registration. The region over which
 * registration is performed is taken from the virtual image buffered region.
 * The user can define a virtual domain by calling either
 * \c CreateVirtualDomainImage or \c SetVirtualDomainImage. See these
 * methods for details.
 * If the user does not set the virtual image explicitly,
 * then it is created during the call to \c Initialize by allocating a new
 * image of type TVirtualImage and copying the information from
 * the fixed image using \c CopyInformation.
 * \note When the virtual image is created by this class, memory is conserved by
 * not allocating space for data within the image. Therefore, derived classes must
 * not access pixel data within the virtual image, it can only be used for
 * iterating over points and performing transformation to and from index and
 * physical coordinates.
 * \note If TFixedImage is type VectorImage, then TVirtualImage must be set
 * separately to a non-VectorImage type, e.g. Image<unsigned char, dim>.
 *
 * Both transforms are initialized to an IdentityTransform, and can be
 * set by the user using SetFixedTranform and SetMovingTransform.
 *
 * At a minimum, the user must:
 *  1) Set images using SetFixedImage and SetMovingImage.
 *  2) Call Initialize.
 *
 * Pre-warping
 *
 * The \c SetDoFixedImagePreWarp and \c SetDoMovingImagePreWarp options can be set
 * for better speed. When set, these create a warped version for each image at
 * the beginning of each iteration, warping each image into the virtual domain.
 * However the cost is more memory usage (VirtualDomain size for each image).
 * The fixed image is only pre-warped once, during the call to \c Initialize,
 * because it is assumed the fixed transform is not changing. The moving image
 * is pre-warped at the beginning of every iteration, because it is assumed the
 * moving transform is changing (e.g. during registration).
 * By default, pre-warping is enabled for both fixed and moving images.
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
 * \note If the point set is sparse, the options
 * SetDo[Fixed|Moving]ImagePreWarp and SetUse[Fixed|Moving]ImageGradientFilter
 * typically should be disabled to avoid excessive computation. However,
 * the warped position and gradient values of the fixed image are not cached
 * when using a point set (there are plans for this in the future), so
 * depending on the number of iterations (when used during optimization)
 * and the level of sparsity, it may be more efficient to pre-warp the fixed
 * image and use a gradient image filter for it because they will only be
 * calculated once.
 *
 * Threading
 *
 * This class is threaded. Threading is handled by friend classes
 * ImageToImageMetricv4GetValueAndDerivativeThreaderBase and
 * ImageToImageMetricv4GetValueAndDerivativeThreader. Dense and sparse
 * evaluation are handled by template specialization of the
 * ImageToImageMetricv4GetValueAndDerivativeThreader::ThreadedExecution
 * method, in order to iterate over either all points in the virutal space in
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
 *  otherwise different behavior as needed.
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
template<class TFixedImage,class TMovingImage,class TVirtualImage = TFixedImage>
class ITK_EXPORT ImageToImageMetricv4 : public ObjectToObjectMetric
{
public:

  /** Standard class typedefs. */
  typedef ImageToImageMetricv4       Self;
  typedef ObjectToObjectMetric       Superclass;
  typedef SmartPointer<Self>         Pointer;
  typedef SmartPointer<const Self>   ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToImageMetricv4, ObjectToObjectMetric);

  /** Type used internally for computations */
  typedef typename Superclass::InternalComputationValueType
                                                  InternalComputationValueType;

  /** Type used for representing parameter values  */
  typedef typename Superclass::CoordinateRepresentationType
                                                  CoordinateRepresentationType;

  /**  Type of the parameters. */
  typedef typename Superclass::ParametersType       ParametersType;
  typedef typename Superclass::ParametersValueType  ParametersValueType;

  /** Graident source type */
  typedef typename Superclass::GradientSourceType GradientSourceType;

  /** Image-accessor typedefs */
  typedef TFixedImage                             FixedImageType;
  typedef typename FixedImageType::PixelType      FixedImagePixelType;
  typedef typename FixedImageType::Pointer        FixedImagePointer;
  typedef typename FixedImageType::ConstPointer   FixedImageConstPointer;
  typedef typename FixedImageType::PointType      FixedImagePointType;
  typedef typename FixedImageType::IndexType      FixedImageIndexType;
  typedef TMovingImage                            MovingImageType;
  typedef typename MovingImageType::PixelType     MovingImagePixelType;
  typedef typename MovingImageType::Pointer       MovingImagePointer;
  typedef typename MovingImageType::ConstPointer  MovingImageConstPointer;
  typedef typename MovingImageType::PointType     MovingImagePointType;
  typedef typename MovingImageType::RegionType    MovingImageRegionType;
  typedef typename MovingImageType::IndexType     MovingImageIndexType;
  /** Types for the virtual domain */
  typedef TVirtualImage                             VirtualImageType;
  typedef typename VirtualImageType::PixelType      VirtualImagePixelType;
  typedef typename VirtualImageType::Pointer        VirtualImagePointer;
  typedef typename VirtualImageType::RegionType     VirtualRegionType;
  typedef typename VirtualRegionType::SizeType      VirtualSizeType;
  typedef typename VirtualImageType::SpacingType    VirtualSpacingType;
  typedef typename VirtualImageType::PointType      VirtualOriginType;
  typedef typename VirtualImageType::PointType      VirtualPointType;
  typedef typename VirtualImageType::DirectionType  VirtualDirectionType;
  typedef typename VirtualImageType::SizeType       VirtualRadiusType;
  typedef typename VirtualImageType::IndexType      VirtualIndexType;

  /* Image dimension accessors */
  typedef unsigned int   ImageDimensionType;
  itkStaticConstMacro(FixedImageDimension, ImageDimensionType,
      ::itk::GetImageDimension<FixedImageType>::ImageDimension);
  itkStaticConstMacro(MovingImageDimension, ImageDimensionType,
      ::itk::GetImageDimension<MovingImageType>::ImageDimension);
  itkStaticConstMacro(VirtualImageDimension, ImageDimensionType,
      ::itk::GetImageDimension<VirtualImageType>::ImageDimension);

  /**  Type of the Transform Base classes */
  typedef Transform<CoordinateRepresentationType,
    itkGetStaticConstMacro( MovingImageDimension ),
    itkGetStaticConstMacro( VirtualImageDimension )> MovingTransformType;

  typedef Transform<CoordinateRepresentationType,
    itkGetStaticConstMacro( FixedImageDimension ),
    itkGetStaticConstMacro( VirtualImageDimension )> FixedTransformType;

  typedef typename FixedTransformType::Pointer         FixedTransformPointer;
  typedef typename FixedTransformType::InputPointType  FixedInputPointType;
  typedef typename FixedTransformType::OutputPointType FixedOutputPointType;
  typedef typename FixedTransformType::ParametersType
                                                FixedTransformParametersType;

  typedef typename MovingTransformType::Pointer         MovingTransformPointer;
  typedef typename MovingTransformType::InputPointType  MovingInputPointType;
  typedef typename MovingTransformType::OutputPointType MovingOutputPointType;
  typedef typename MovingTransformType::ParametersType
                                                MovingTransformParametersType;

  /** Jacobian type. This is the same for all transforms */
  typedef typename FixedTransformType::JacobianType     JacobianType;
  typedef typename FixedTransformType::JacobianType     FixedTransformJacobianType;
  typedef typename MovingTransformType::JacobianType    MovingTransformJacobianType;

  /**  Type for the mask of the fixed image. Only pixels that are "inside"
       this mask will be considered for the computation of the metric */
  typedef SpatialObject< itkGetStaticConstMacro(FixedImageDimension) >
                                                       FixedImageMaskType;
  typedef typename FixedImageMaskType::Pointer         FixedImageMaskPointer;
  typedef typename FixedImageMaskType::ConstPointer
                                                  FixedImageMaskConstPointer;

  /**  Type for the mask of the moving image. Only pixels that are "inside"
       this mask will be considered for the computation of the metric */
  typedef SpatialObject< itkGetStaticConstMacro(MovingImageDimension) >
                                                        MovingImageMaskType;
  typedef typename MovingImageMaskType::Pointer         MovingImageMaskPointer;
  typedef typename MovingImageMaskType::ConstPointer
                                                   MovingImageMaskConstPointer;

  /** Type of the point set used for sparse sampling. */
  typedef PointSet<typename FixedImageType::PixelType,
                   itkGetStaticConstMacro(FixedImageDimension)>
                                                     FixedSampledPointSetType;
  typedef typename FixedSampledPointSetType::Pointer FixedSampledPointSetPointer;
  typedef typename FixedSampledPointSetType::ConstPointer FixedSampledPointSetConstPointer;

  typedef PointSet<typename VirtualImageType::PixelType,
                   itkGetStaticConstMacro(VirtualImageDimension)>
                                                     VirtualSampledPointSetType;

  typedef typename VirtualSampledPointSetType::Pointer VirtualSampledPointSetPointer;

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
  typedef   CovariantVector< CoordinateRepresentationType,
                             itkGetStaticConstMacro(FixedImageDimension) >
                                                      FixedImageGradientType;
  typedef   CovariantVector< CoordinateRepresentationType,
                             itkGetStaticConstMacro(MovingImageDimension) >
                                                      MovingImageGradientType;

  typedef   CovariantVector< CoordinateRepresentationType,
                             itkGetStaticConstMacro(VirtualImageDimension) >
                                                      VirtualImageGradientType;

  /** Type of the filter used to calculate the gradients.
   * Note that RealType is always double (or long double for
   * long double pixel-type).*/
  typedef typename NumericTraits< FixedImagePixelType >::RealType
                                                    FixedRealType;
  typedef CovariantVector< FixedRealType,
                           itkGetStaticConstMacro(FixedImageDimension) >
                                                    FixedGradientPixelType;
  typedef Image< FixedGradientPixelType,
                 itkGetStaticConstMacro(FixedImageDimension) >
                                                FixedImageGradientImageType;
  typedef typename FixedImageGradientImageType::Pointer
                                                FixedImageGradientImagePointer;

  typedef ImageToImageFilter< FixedImageType, FixedImageGradientImageType >
                                                 FixedImageGradientFilterType;

  typedef typename NumericTraits< MovingImagePixelType >::RealType
                                                 MovingRealType;
  typedef CovariantVector< MovingRealType,
                           itkGetStaticConstMacro(MovingImageDimension) >
                                                 MovingGradientPixelType;
  typedef Image< MovingGradientPixelType,
                 itkGetStaticConstMacro(MovingImageDimension) >
                                                    MovingImageGradientImageType;
  typedef typename MovingImageGradientImageType::Pointer MovingImageGradientImagePointer;

  typedef ImageToImageFilter< MovingImageType, MovingImageGradientImageType >
                                                 MovingImageGradientFilterType;
  typedef typename FixedImageGradientFilterType::Pointer
                                              FixedImageGradientFilterPointer;
  typedef typename MovingImageGradientFilterType::Pointer
                                              MovingImageGradientFilterPointer;

  /** Default image gradient filter types */
  typedef GradientRecursiveGaussianImageFilter< FixedImageType,
                                                FixedImageGradientImageType >
                                                  DefaultFixedImageGradientFilter;
  typedef GradientRecursiveGaussianImageFilter< MovingImageType,
                                                MovingImageGradientImageType >
                                                  DefaultMovingImageGradientFilter;

  /** Image gradient calculator types. The TOutput template parameter
   * is chosen to match that of CentralDiffererenceImageFunction. */
  typedef ImageFunction<FixedImageType,
                        CovariantVector<double,
                                  itkGetStaticConstMacro( FixedImageDimension )>,
                        CoordinateRepresentationType>
                                            FixedImageGradientCalculatorType;
  typedef ImageFunction<MovingImageType,
                        CovariantVector<double,
                                  itkGetStaticConstMacro( MovingImageDimension )>,
                        CoordinateRepresentationType>
                                            MovingImageGradientCalculatorType;

  typedef typename FixedImageGradientCalculatorType::Pointer
                                            FixedImageGradientCalculatorPointer;
  typedef typename MovingImageGradientCalculatorType::Pointer
                                            MovingImageGradientCalculatorPointer;

  /** ResampleImageFilter types for image pre-warping */
  typedef ResampleImageFilter< MovingImageType,
                               VirtualImageType,
                               MovingRealType >
                                             MovingWarpResampleImageFilterType;
  typedef typename MovingWarpResampleImageFilterType::Pointer
                                          MovingWarpResampleImageFilterPointer;
  typedef ResampleImageFilter< FixedImageType,
                               VirtualImageType,
                               FixedRealType >
                                             FixedWarpResampleImageFilterType;
  typedef typename FixedWarpResampleImageFilterType::Pointer
                                          FixedWarpResampleImageFilterPointer;

  /**  Type of the measure. */
  typedef typename Superclass::MeasureType    MeasureType;

  /**  Type of the metric derivative. */
  typedef typename Superclass::DerivativeType DerivativeType;
  typedef typename DerivativeType::ValueType  DerivativeValueType;

  /** Type to represent the number of parameters that are being optimized at
   * any given iteration of the optimizer. */
  typedef typename Superclass::NumberOfParametersType   NumberOfParametersType;

  /* Set/get images */
  /** Connect the Fixed Image.  */
  itkSetConstObjectMacro(FixedImage, FixedImageType);
  /** Get the Fixed Image. */
  itkGetConstObjectMacro(FixedImage, FixedImageType);
  /** Connect the Moving Image.  */
  itkSetConstObjectMacro(MovingImage, MovingImageType);
  /** Get the Moving Image. */
  itkGetConstObjectMacro(MovingImage, MovingImageType);

  /** Define the virtual reference space. This space defines the resolution
   * at which the registration is performed as well as the physical coordinate
   * system.  Useful for unbiased registration.
   * This method will allocate \c m_VirtualDomainImage with the passed
   * information. Metric evaluation will be performed over
   * the image's buffered region.
   * \param spacing   spacing
   * \param origin    origin
   * \param direction direction
   * \param region    region is used to set all image regions.
   * If the user does not set this explicitly then it is taken from the fixed
   * image in \c Initialize method.
   * To define the virtual domain from an existing image,
   * use \c SetVirtualDomainImage.
   */
  void CreateVirtualDomainImage( VirtualSpacingType & spacing,
                                    VirtualOriginType & origin,
                                    VirtualDirectionType & direction,
                                    VirtualRegionType & region );

  /** Set a virtual domain image to define the virtual reference space.
   * Metric evaluation will be performed over the image's buffered region.
   * The image is expected to be allocated.
   * If the user does not set this explicitly then it is taken from the fixed
   * image in \c Initialize method. */
  void SetVirtualDomainImage( VirtualImageType * virtualImage);

  /** Get the virtual domain image */
  itkGetConstObjectMacro(VirtualDomainImage, VirtualImageType);

  /** Convenience get-accessors for the virtual domain information.
   * These are returned from the VirtualDomainImage */
  const VirtualSpacingType    GetVirtualDomainSpacing( void ) const;
  const VirtualOriginType     GetVirtualDomainOrigin( void ) const;
  const VirtualDirectionType  GetVirtualDomainDirection( void ) const;
  const VirtualRegionType     GetVirtualDomainRegion( void ) const;

  /** Connect the fixed transform. */
  itkSetObjectMacro(FixedTransform, FixedTransformType);

  /** Get a pointer to the fixed transform.  */
  itkGetConstObjectMacro(FixedTransform, FixedTransformType);

  /** Connect the moving transform. */
  itkSetObjectMacro(MovingTransform, MovingTransformType);

  /** Get a pointer to the moving transform.  */
  itkGetConstObjectMacro(MovingTransform, MovingTransformType);

  /** Connect the moving transform using a backwards-compatible name.
   * This assigns the input transform to the moving transform. */
  void SetTransform( MovingTransformType* transform );

  /** Get the moving transform using a backwards-compatible name */
  const MovingTransformType * GetTransform();

  /** Connect the fixed interpolator. */
  itkSetObjectMacro(FixedInterpolator, FixedInterpolatorType);
  /** Get a pointer to the fixed interpolator.  */
  itkGetConstObjectMacro(FixedInterpolator, FixedInterpolatorType);

  /** Connect the Moving interpolator. */
  itkSetObjectMacro(MovingInterpolator, MovingInterpolatorType);
  /** Get a pointer to the Moving interpolator.  */
  itkGetConstObjectMacro(MovingInterpolator, MovingInterpolatorType);

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
  itkGetConstObjectMacro(VirtualSampledPointSet, VirtualSampledPointSetType);

  /** Set/Get the gradient filter */
  itkSetObjectMacro( FixedImageGradientFilter, FixedImageGradientFilterType );
  itkGetObjectMacro( FixedImageGradientFilter, FixedImageGradientFilterType );
  itkSetObjectMacro( MovingImageGradientFilter, MovingImageGradientFilterType );
  itkGetObjectMacro( MovingImageGradientFilter, MovingImageGradientFilterType );

  /** Set/Get gradient calculators */
  itkSetObjectMacro( FixedImageGradientCalculator, FixedImageGradientCalculatorType);
  itkGetObjectMacro( FixedImageGradientCalculator, FixedImageGradientCalculatorType);
  itkSetObjectMacro( MovingImageGradientCalculator, MovingImageGradientCalculatorType);
  itkGetObjectMacro( MovingImageGradientCalculator, MovingImageGradientCalculatorType);

  /** Set/Get gradient computation via an image filter,
   * for fixed image. */
  itkSetMacro(UseFixedImageGradientFilter, bool);
  itkGetConstReferenceMacro(UseFixedImageGradientFilter, bool);
  itkBooleanMacro(UseFixedImageGradientFilter);

  /** Set/Get gradient computation via an image filter. */
  itkSetMacro(UseMovingImageGradientFilter, bool);
  itkGetConstReferenceMacro(UseMovingImageGradientFilter, bool);
  itkBooleanMacro(UseMovingImageGradientFilter);

  /** Set/Get pre-warping of fixed image option. */
  itkSetMacro(DoFixedImagePreWarp, bool);
  itkGetConstReferenceMacro(DoFixedImagePreWarp, bool);
  itkBooleanMacro(DoFixedImagePreWarp);

  /** Set/Get pre-warping of Moving image option. */
  itkSetMacro(DoMovingImagePreWarp, bool);
  itkGetConstReferenceMacro(DoMovingImagePreWarp, bool);
  itkBooleanMacro(DoMovingImagePreWarp);

  /** Get pre-warped images */
  itkGetConstObjectMacro( MovingWarpedImage, MovingImageType );
  itkGetConstObjectMacro( FixedWarpedImage, FixedImageType );

  /** Get number of threads to used in the the \c GetValueAndDerivative
   * calculation.  Only valid after \c GetValueAndDerivative has been called. */
  ThreadIdType GetNumberOfThreadsUsed() const;

  /** Set number of threads to use. This the maximum number of threads to use
   * when multithreaded.  The actual number of threads used (may be less than
   * this value) can be obtained with \c GetNumberOfThreadsUsed. */
  void SetMaximumNumberOfThreads( const ThreadIdType threads );
  ThreadIdType GetMaximumNumberOfThreads() const;

  /** Get Fixed Gradient Image. */
  itkGetConstObjectMacro(FixedImageGradientImage, FixedImageGradientImageType);
  /** Get Moving Gradient Image. */
  itkGetConstObjectMacro(MovingImageGradientImage, MovingImageGradientImageType);

  /** Get number of valid points from most recent update */
  itkGetConstMacro( NumberOfValidPoints, SizeValueType );

  /** Get the number of points in the domain used to evaluate
   * the metric. This will differ depending on whether a sampled
   * point set or dense sampling is used, and will be greater than
   * or equal to GetNumberOfValidPoints(). */
  SizeValueType GetNumberOfDomainPoints() const;

  /** Set/Get the floating point resolution used by the derivatives.  This
   * If this is set to 1e5, then the derivative will have precision up to 5
   * points beyond the decimal point. And precision beyond that will be
   * truncated. */
  itkSetMacro( FloatingPointCorrectionResolution, DerivativeValueType );
  itkGetConstMacro( FloatingPointCorrectionResolution, DerivativeValueType );

  /** Return the number of parameters.
   * \note Currently we're always optimizing the moving image transform,
   * so return its number of parameters. The class will eventually allow
   * either the fixed transform to be optimized, or both. This and related
   * methods make it so the user or calling-class doesn't need to know which
   * of the transforms are being optimized.
   */
  virtual NumberOfParametersType GetNumberOfParameters() const;

  /** Set the active (moving) transform's parameters. */
  virtual void SetParameters( ParametersType & params );

  /** Get a const reference to the active (moving) transform's parameters. */
  virtual const ParametersType & GetParameters() const;

  /** Update the active (moving) transform's parameters.
   * This call is passed through directly to the transform.
   * \c factor is a scalar multiplier for each value in update, and
   * defaults to 1.0 .
   * \c derivative must be the proper size, as retrieved
   * from GetNumberOfParameters. */
  virtual void UpdateTransformParameters( DerivativeType & derivative,
                                          ParametersValueType factor = NumericTraits< ParametersValueType >::One );

  /** Get the number of local parameters from the moving transform. */
  virtual NumberOfParametersType GetNumberOfLocalParameters() const;

  /** Get if the moving transform has local support. */
  virtual bool HasLocalSupport() const;

  /* Initialize the metric before calling GetValue or GetDerivative.
   * Derived classes must call this Superclass version if they override
   * this to perform their own initialization.
   * \note This is meant to be called once for a particular metric setup.
   * That is, when used in registration, this method would be called once
   * before entering the registration loop, during which GetValue or
   * GetDerivative will be called repeatedly. It must be called again if
   * metric settings are changed before beginning a new registration. */
  virtual void Initialize(void) throw ( itk::ExceptionObject );

  /* Computes an offset for accessing parameter data from a virtual domain
   * index. Relevant for metrics with local-support transforms, to access
   * parameter or derivative memory that is stored linearly in a 1D array.
   * The result is the offset (1D array index) to the first of N parameters
   * corresponding to the given virtual index, where N is the number of
   * local parameters. */
  OffsetValueType ComputeParameterOffsetFromVirtualDomainIndex( const VirtualIndexType & index, const NumberOfParametersType numberOfLocalParameters ) const;

  /** Calculate and return the value for the metric based on the current
   * transformation(s). */
  virtual MeasureType GetValue() const;

  /** Calculate and return both the value for the metric and its derivative.
   * This calls the SparseGetValueAndDerivativeThreader if \c UsedFixedSampledPointSet
   * is true, and DenseGetValueAndDerivativeThreader otherwise.  The threaders
   * in turn call \c ProcessPoint on each point in the
   * domain to be examined. */
  virtual void GetValueAndDerivative( MeasureType & value,
                                      DerivativeType & derivative ) const;

  /** Get the number of sampled fixed sampled points that are
   * deemed invalid during conversion to virtual domain in Initialize().
   * For informational purposes. */
  itkGetConstReferenceMacro(NumberOfSkippedFixedSampledPoints, SizeValueType);

  /** Get the current metric value stored in m_Value. This is only
   * meaningful after a call to GetValue() or GetValueAndDerivative().
   * Note that this would normally be called GetValue, but that name is
   * used for historical reasons by GetValue() to compute the current
   * metric value and store it in m_Value. */
  MeasureType GetCurrentValue();

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
   * Transform a point from VirtualImage domain to FixedImage domain.
   * This function also checks if mapped point is within the mask if
   * one is set, and that is within the fixed image buffer, in which
   * case the return value will be true.
   * \c mappedFixedPoint and \c mappedFixedPixelValue are  returned, and
   * \c mappedFixedImageGradient is returned if \c computeImageGradient is set.
   * All return values are in the virtual domain.
   * \note It would be better for maintainence to have a single method
   * that could work for either fixed or moving domains. However setting
   * that up is complicated because dimensionality and pixel type may
   * be different between the two. */
  bool TransformAndEvaluateFixedPoint(
                           const VirtualIndexType & index,
                           const VirtualPointType & point,
                           const bool computeImageGradient,
                           FixedImagePointType & mappedFixedPoint,
                           FixedImagePixelType & mappedFixedPixelValue,
                           FixedImageGradientType & mappedFixedImageGradient ) const;
  /** Transform a point from VirtualImage domain to MovingImage domain. */
  bool TransformAndEvaluateMovingPoint(
                           const VirtualIndexType & index,
                           const VirtualPointType & point,
                           const bool computeImageGradient,
                           MovingImagePointType & mappedMovingPoint,
                           MovingImagePixelType & mappedMovingPixelValue,
                           MovingImageGradientType & mappedMovingImageGradient ) const;

  /** Compute image derivatives for a Fixed point.
   * \warning This doesn't transform result into virtual space. For that,
   * see TransformAndEvaluateFixedPoint
   */
  virtual void ComputeFixedImageGradientAtPoint(
                                    const FixedImagePointType & mappedPoint,
                                    FixedImageGradientType & gradient ) const;
  /** Compute image derivatives for a moving point. */
  virtual void ComputeMovingImageGradientAtPoint(
                                    const MovingImagePointType & mappedPoint,
                                    MovingImageGradientType & gradient ) const;

  /**
   * Compute fixed warped image derivatives for an index at virtual domain.
   * \warning This doesn't transform result into virtual space. For that,
   * see TransformAndEvaluateFixedPoint
   */
  virtual void ComputeFixedImageGradientAtIndex(
                                    const VirtualIndexType & index,
                                    FixedImageGradientType & gradient ) const;
  /** Compute image derivatives for a moving point. */
  virtual void ComputeMovingImageGradientAtIndex(
                                    const VirtualIndexType & index,
                                    MovingImageGradientType & gradient ) const;

  /** Computes the gradients of the fixed image, using the
   * GradientFilter, assigning the output to
   * to m_FixedImageGradientImage. It will use either the original
   * fixed image, or the pre-warped version, depending on the setting
   * of DoFixedImagePreWarp. */
  virtual void ComputeFixedImageGradientFilterImage();

  /** Computes the gradients of the moving image, using the
   * GradientFilter, assigning the output to
   * to m_MovingImageGradientImage. It will use either the original
   * moving image, or the pre-warped version, depending on the setting
   * of DoMovingImagePreWarp. */
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
  virtual void InitializeDefaultFixedImageGradientFilter(void);
  virtual void InitializeDefaultMovingImageGradientFilter(void);

  FixedImageConstPointer  m_FixedImage;
  FixedTransformPointer   m_FixedTransform;
  MovingImageConstPointer m_MovingImage;
  MovingTransformPointer  m_MovingTransform;
  VirtualImagePointer     m_VirtualDomainImage;

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

  /** Gradient images to store gradient filter output. */
  mutable FixedImageGradientImagePointer    m_FixedImageGradientImage;
  mutable MovingImageGradientImagePointer   m_MovingImageGradientImage;

  /** Image gradient calculators */
  FixedImageGradientCalculatorPointer   m_FixedImageGradientCalculator;
  MovingImageGradientCalculatorPointer  m_MovingImageGradientCalculator;

  /** Flag to control pre-warping of fixed image. */
  bool                               m_DoFixedImagePreWarp;

  /** Flag to control pre-warping of moving image. */
  bool                               m_DoMovingImagePreWarp;

  /** Pre-warped images. */
  mutable FixedImagePointer   m_FixedWarpedImage;
  mutable MovingImagePointer  m_MovingWarpedImage;

  /** Resample image filters for pre-warping images */
  MovingWarpResampleImageFilterPointer    m_MovingWarpResampleImageFilter;
  FixedWarpResampleImageFilterPointer     m_FixedWarpResampleImageFilter;

  /** Derivative results holder. User a raw pointer so we can point it
   * to a user-provided object. This enables
   * safely sharing a derivative object between metrics during multi-variate
   * analsys, for memory efficiency. */
  mutable DerivativeType *                m_DerivativeResult;

  /** Store the number of points used during most recent value and derivative
   * calculation. */
  mutable SizeValueType                   m_NumberOfValidPoints;

  /** Flag that is set when user provides a virtual domain, either via
   * CreateVirtualDomainImage or SetVirtualDomainImage. */
  bool                                    m_UserHasProvidedVirtualDomainImage;

  /** Masks */
  FixedImageMaskConstPointer              m_FixedImageMask;
  MovingImageMaskConstPointer             m_MovingImageMask;

  /** Sampled point sets */
  FixedSampledPointSetConstPointer        m_FixedSampledPointSet;
  VirtualSampledPointSetPointer           m_VirtualSampledPointSet;

  /** Flag to use FixedSampledPointSet, i.e. Sparse sampling. */
  bool                                    m_UseFixedSampledPointSet;

  /** Metric value, stored after evaluating */
  mutable MeasureType                     m_Value;

  ImageToImageMetricv4();
  virtual ~ImageToImageMetricv4();

  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Verify that virtual domain and displacement field are the same size
   * and in the same physical space. */
  virtual void VerifyDisplacementFieldSizeAndPhysicalSpace();

  /** Check that the number of valid points is above a default
   * minimum (zero). If not, then return false, and assign to 'value' a value
   * indicating insufficient valid points were found during evaluation, and set
   * the derivative to zero. A warning is also output.
   * This functionality is provided as a separate method so derived classes
   * can use it without hardcoding the details. */
  bool VerifyNumberOfValidPoints( MeasureType & value, DerivativeType & derivative ) const;

private:
  /** Map the fixed point set samples to the virtual domain */
  void MapFixedSampledPointSetToVirtual( void );

  /** Pre-warp the images for efficiency and computational stability.
   * See main class documentation for important considerations. */
  void DoFixedImagePreWarp( void ) const;
  void DoMovingImagePreWarp( void ) const;

  /** Flag for warning about use of GetValue. Will be removed when
   *  GetValue implementation is improved. */
  mutable bool m_HaveMadeGetValueWarning;

  ImageToImageMetricv4(const Self &); //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  //Sample point coordinates from the virtual image domain
  std::vector<VirtualPointType> m_VirtualImageCornerPoints;

  /* Keep track of the number of sampled fixed points that are
   * deemed invalid during conversion to virtual domain.
   * For informational purposes. */
  SizeValueType m_NumberOfSkippedFixedSampledPoints;

  DerivativeValueType m_FloatingPointCorrectionResolution;

  /** Only floating-point images are currently supported. To support integer images,
   * several small changes must be made, but a larger complication is handling
   * pre-warping cleanly. */
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

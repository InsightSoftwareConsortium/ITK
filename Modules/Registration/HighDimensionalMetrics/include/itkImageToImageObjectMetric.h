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
#ifndef __itkImageToImageObjectMetric_h
#define __itkImageToImageObjectMetric_h

#include "itkCovariantVector.h"
#include "itkImageFunction.h"
#include "itkObjectToObjectMetric.h"
#include "itkInterpolateImageFunction.h"
#include "itkSpatialObject.h"
#include "itkResampleImageFilter.h"
#include "itkImageToImageFilter.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkPointSet.h"

namespace itk
{

//Forward-declare these because of module dependency conflict.
//They will soon be moved to a different module, at which
// time this can be removed.
template <unsigned int VDimension, class TDataHolder>
class ImageToData;
template <class TDataHolder>
class Array1DToData;

/** \class ImageToImageObjectMetric
 *
 * Computes similarity between regions of two images, using two
 * user-supplied transforms, a 'fixed' transform and a 'moving' transform.
 *
 * \note Currently support for using only \c GetValueAndDerivative is
 * implemented. \c GetValue will follow after final implementation details
 * are worked out.
 *
 * Templated over the fixed and moving image types, as well as an optional
 * VirtualImage type to define the virtual domain. The VirtualImage type
 * defaults to TFixedImage.
 *
 * This class uses a virtual reference space. This space defines the resolution
 * at which the registration is performed, as well as the physical coordinate
 * system.  Useful for unbiased registration.  If the user does not set this
 * explicitly then it is taken from the fixed image in \c Initialize method.
 * The user can define a virtual domain by calling either
 * \c CreateVirtualDomainImage or \c SetVirtualDomainImage. The virtual
 * domain region can be changed via \c SetVirtualDomainRegion. See these
 * methods for details.
 *
 * Both transforms are initialized to an IdentityTransform, and can be
 * set by the user using \c SetFixedTranform and \c SetMovingTransform.
 *
 * At a minimum, the user must:
 *  1) Set images using \c SetFixedImage and \c SetMovingImage.
 *  2) Call \c Initialize.
 *
 * Pre-warping:
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
 * Image gradient calculations:
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
 *TODO: Update:
 * 2) Otherwise, the CentralDifferenceImageFunction is used. This calculation
 *  is not smoothed and gives different results than
 *  GradientRecursiveGaussianImageFilter. The advantage is that less memory is
 *  used. However for the fixed image, it means needlessly computing the image
 *  gradients at each iteration of a registration instead of just computing
 *  once at the beginning.
 *
 * Both image gradient calculation methods are threaded.
 * Generally it is not recommended to use different image gradient methods for
 * the fixed and moving images because the methods return different results.
 *
 * Image masks are supported using SetMovingImageMask or SetFixedImageMask.
 *
 * Random sampling or user-supplied point lists are not yet supported, except
 * via an image mask. If the mask is sparse, the
 * SetPreWarp[Fixed|Moving]Image and Use[Fixed|Moving]ImageGradientFilter
 * options typically should be disabled to avoid excessive computation. However,
 * depending on the number of iterations and the level of sparsity, it may
 * be more efficient to pre-warp the fixed image and use a gradient image filter
 * for it because they will only be calculated once.
 *
 * This class is threaded.
 *
 * Derived classes:
 *
 *  Derived classes need to override at least:
 *  \c GetValueAndDerivative
 *  Pure virtual methods declared in the base class.
 *
 *  \c GetValueAndDerivativeProcessPoint must be overridden by derived
 *  classes that use \c GetValueAndDerivativeThreadedExecute.
 *
 *  See \c ImageToImageObjectMetricTest for a clear example of what a
 *  derived class must implement and do. Pre- and Post-processing are
 *  handled by the derived class in its \c GetValueAndDerivative method, as
 *  described in \c ImageToImageObjectMetricTest.
 *
 * \ingroup ITKHighDimensionalMetrics
 */
template<class TFixedImage,class TMovingImage,class TVirtualImage = TFixedImage>
class ITK_EXPORT ImageToImageObjectMetric :
  public ObjectToObjectMetric
{
public:

  /** Standard class typedefs. */
  typedef ImageToImageObjectMetric                          Self;
  typedef ObjectToObjectMetric                              Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToImageObjectMetric, ObjectToObjectMetric);

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

  /** Type of the point set used for sparse sampling. The user can pass
   * an arbitrary point set to designate point for sampling. It's presumed that
   * the user will be working in terms of the fixed image domain, and thus the
   * sampling domain is the same. Internally, the points are transformed
   * into the virtual domain as needed. */
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

  /** Type of the filter used to calculate the gradients. */
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
   * \param region is used to set all image regions.
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

  /** Set/Get the fixed image domain sampling point set */
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

  /** Get number of threads to use.
   * \warning This value can change during Initialize, if the threader
   * determines that fewer threads would be more efficient. The default
   * is the default value returned by the
   * assigned threader object, and will never be changed in Initialize to
   * a larger value. This method will throw an exception if it is used
   * before \c Initialize() has been called.
   */
  ThreadIdType GetNumberOfThreads( void ) const;

  /** Set number of threads to use. The default is the number of threads
   * available as reported by MultiThreader.
   * \warning See discussion for GetNumberOfThreads. */
  void SetNumberOfThreads( ThreadIdType );

  /** Get Fixed Gradient Image. */
  itkGetConstObjectMacro(FixedImageGradientImage, FixedImageGradientImageType);
  /** Get Moving Gradient Image. */
  itkGetConstObjectMacro(MovingImageGradientImage, MovingImageGradientImageType);

  /** Get number of valid points from most recent update */
  itkGetConstMacro( NumberOfValidPoints, SizeValueType );

  /** Get the measure value in m_Value, as computed by most recent evaluation.
   * Need to differentiate it from GetValue method in base class. */
  MeasureType GetValueResult() const;

  /** Type to represent the number of parameters that are being optimized at
   * any given iteration of the optimizer. */
  typedef typename Superclass::NumberOfParametersType   NumberOfParametersType;

  /** Return the number of parameters.
   * \note Currently we're always optimizing the moving image transform,
   * so return its number of parameters. The class will eventually allow
   * either the fixed transform to be optimized, or both. This and related
   * methods make it so the user or calling-class doesn't need to know which
   * of the transforms are being optimized.
   */
  virtual NumberOfParametersType GetNumberOfParameters() const;

  /** Get a const reference to the moving transform's parameters */
  virtual const ParametersType & GetParameters() const;

  /** Update the moving transform's parameters.
   * This call is passed through directly to the transform.
   * \c factor is a scalar multiplier for each value in update, and
   * defaults to 1.0 .
   * \c derivative must be the proper size, as retrieved
   * from GetNumberOfParameters. */
  virtual void UpdateTransformParameters( DerivativeType & derivative,
                                          ParametersValueType factor = 1.0);

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

protected:

  /** \class SamplingIteratorHelper class
   * \brief Simple helper class for working with both dense sampling via
   * an image iterator, and sparse sampling for a point set.
   * Fully-declared in defined in .hxx file.
   *
   * \ingroup ITKHighDimensionalMetrics
   */
  class SamplingIteratorHelper;

  /* Worker method to iterate over an image sub region or list of sample
   * points. It calculates fixed and moving point values and image derivatives
   * and calls the derived class' user worker method to calculate
   * value and derivative. */
  void GetValueAndDerivativeProcessPointRange(
                                      SamplingIteratorHelper & samplingIterator,
                                      ThreadIdType threadID,
                                      Self * self);

  /** Method to calculate the metric value and derivative
   * given a point, value and image derivative for both fixed and moving
   * spaces. The provided values have been calculated from \c virtualPoint,
   * which is provided in case it's needed.
   * Derived classes that use \c GetValueAndDerivativeThreadedExecute
   * to initiate process must override this method, otherwise an exception
   * is thrown.
   * \note This method is not pure virtual because some derived classes
   * do not use \c GetValueAndDerivativeThreadedExecute, and instead
   * provide their own processing control.
   * \param virtualPoint is the point within the virtual domain from which
   * the passed parameters have been calculated.
   * \param mappedFixedPoint is a valid point within the moving image space
   *  that has passed bounds checking, and lies within any mask that may
   *  be assigned.
   * \param mappedFixedPixelValue holds the pixel value at the mapped fixed
   *  point.
   * \param mappedFixedImageGradient holds the image gradient at the fixed point,
   *  but only when \c m_GradientSource is set to calculate fixed image gradients
   *  (either when it's set to calculate only fixed gradients, or both fixed and
   *  moving). Otherwise, the value is meaningless and should be ignored.
   * \param mappedMovingPoint
   * \param mappedMovingPixelValue
   * \param mappedMovingImageGradient
   *  These three parameters hold the point, pixel value and image gradient for
   *  the moving image space, as described above for the fixed image space.
   * Results must be returned by derived classes in:
   *   \param metricValueReturn, and
   *   \param localDerivativeReturn
   * \param threadID may be used as needed, for example to access any per-thread
   * data cached during pre-processing by the derived class.
   * \warning The derived class should use \c GetNumberOfThreads from this base
   * class only after ImageToImageObjectMetric:: Initialize has been called, to
   * assure that the same number of threads are used.
   * \warning  This is called from the threader, and thus must be thread-safe.
   */
  virtual bool GetValueAndDerivativeProcessPoint(
        const VirtualPointType &          virtualPoint,
        const FixedImagePointType &       mappedFixedPoint,
        const FixedImagePixelType &       mappedFixedPixelValue,
        const FixedImageGradientType &    mappedFixedImageGradient,
        const MovingImagePointType &      mappedMovingPoint,
        const MovingImagePixelType &      mappedMovingPixelValue,
        const MovingImageGradientType &   mappedMovingImageGradient,
        MeasureType &                     metricValueReturn,
        DerivativeType &                  localDerivativeReturn,
        const ThreadIdType                threadID ) const;

  /** Perform any initialization required before each evaluation of
   * value and derivative. This is distinct from Initialize, which
   * is called only once before a number of iterations, e.g. before
   * a registration loop.
   * Called from \c GetValueAndDerivativeThreadedExecute before
   * threading starts. */     //NOTE: make this private or protected?
  virtual void InitializeForIteration(void) const;

  /** Transform and evaluate a point into the virtual domain.
   * This function also checks if mapped point is within the mask if
   * one is set, and that is within the fixed image buffer, in which
   * case \c pointIsValid will be true on return.
   * \c mappedFixedPoint and \c mappedFixedPixelValue are  returned, and
   * \c mappedFixedImageGradient is returned if \c computeImageGradient is set.
   * All return values are in the virtual domain.
   * \note It would be better for maintainence to have a single method
   * that could work for either fixed or moving domains. However setting
   * that up is complicated because dimensionality and pixel type may
   * be different between the two. */
  virtual void TransformAndEvaluateFixedPoint(
                           const VirtualIndexType & index,
                           const VirtualPointType & point,
                           const bool computeImageGradient,
                           FixedImagePointType & mappedFixedPoint,
                           FixedImagePixelType & mappedFixedPixelValue,
                           FixedImageGradientType & mappedFixedImageGradient,
                           bool & pointIsValid ) const;

  /** See TransformAndEvaluateFixedPoint. TODO. */
  virtual void TransformAndEvaluateMovingPoint(
                           const VirtualIndexType & index,
                           const VirtualPointType & point,
                           const bool computeImageGradient,
                           MovingImagePointType & mappedMovingPoint,
                           MovingImagePixelType & mappedMovingPixelValue,
                           MovingImageGradientType & mappedMovingImageGradient,
                           bool & pointIsValid ) const;

  /** Compute image derivatives at a point. */
  virtual void ComputeFixedImageGradientAtPoint(
                                    const FixedImagePointType & mappedPoint,
                                    FixedImageGradientType & gradient ) const;
  virtual void ComputeMovingImageGradientAtPoint(
                                    const MovingImagePointType & mappedPoint,
                                    MovingImageGradientType & gradient ) const;

  /** Compute image derivatives at an index. */
  virtual void ComputeFixedImageGradientAtIndex(
                                    const VirtualIndexType & index,
                                    FixedImageGradientType & gradient ) const;
  virtual void ComputeMovingImageGradientAtIndex(
                                    const VirtualIndexType & index,
                                    MovingImageGradientType & gradient ) const;

  /** Computes the gradients of the fixed image, using the
   * GradientFilter, assigning the output to
   * to m_FixedImageGradientImage. It will use either the original
   * fixed image, or the pre-warped version, depending on the setting
   * of DoFixedImagePreWarp. */
  virtual void ComputeFixedImageGradientFilterImage(void);

  /** Computes the gradients of the moving image, using the
   * GradientFilter, assigning the output to
   * to m_MovingImageGradientImage. It will use either the original
   * moving image, or the pre-warped version, depending on the setting
   * of DoMovingImagePreWarp. */
  virtual void ComputeMovingImageGradientFilterImage(void) const;

  /** Initialize the default image gradient filters. This must only
   * be called once the fixed and moving images have been set. */
  virtual void InitializeDefaultFixedImageGradientFilter(void);
  virtual void InitializeDefaultMovingImageGradientFilter(void);

  /** Store derivative result from a single point calculation.
   * \warning If this method is overridden or otherwise not used
   * in a derived class, be sure to *accumulate* results in
   * \c derivative, and not assign them. */
  virtual void StoreDerivativeResult(  DerivativeType & derivative,
                                        const VirtualIndexType & virtualIndex,
                                        ThreadIdType threadID );

  /** Called from \c GetValueAndDerivativeThreadedExecute after
   * threading is complete, to count the total number of valid points
   * used during calculations, storing it in \c m_NumberOfValidPoints */
  virtual void CollectNumberOfValidPoints(void) const;

  FixedImageConstPointer  m_FixedImage;
  FixedTransformPointer   m_FixedTransform;
  MovingImageConstPointer m_MovingImage;
  MovingTransformPointer  m_MovingTransform;
  VirtualImagePointer     m_VirtualDomainImage;

  /** Pointers to interpolators */
  FixedInterpolatorPointer                        m_FixedInterpolator;
  MovingInterpolatorPointer                       m_MovingInterpolator;

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
  mutable DerivativeType *                    m_DerivativeResult;

  /** Store the number of points used during most recent value and derivative
   * calculation. */
  mutable SizeValueType                       m_NumberOfValidPoints;

  /** Flag that is set when user provides a virtual domain, either via
   * CreateVirtualDomainImage or SetVirtualDomainImage. */
  bool                                        m_UserHasProvidedVirtualDomainImage;

  /** Masks */
  FixedImageMaskConstPointer                  m_FixedImageMask;
  MovingImageMaskConstPointer                 m_MovingImageMask;

  /** Sampled point sets */
  FixedSampledPointSetConstPointer            m_FixedSampledPointSet;
  VirtualSampledPointSetPointer               m_VirtualSampledPointSet;

  /** Flag to use FixedSampledPointSet */
  bool                                        m_UseFixedSampledPointSet;

  /** Metric value, stored after evaluating */
  mutable MeasureType             m_Value;

  /*
   * Multi-threading variables and methods
   */

  /** Initialize memory for threading.
   * \c derivativeReturn will be used to store the derivative results.
   * Typically this will be the user-supplied object from a call to
   * GetValueAndDerivative.
   */ //NOTE: make this private, or will a derived class want to override?
  virtual void InitializeThreadingMemory( DerivativeType & derivativeReturn ) const;

  /** Initiates multi-threading to evaluate the current metric value
   * and derivatives.
   * Derived classes should call this in their GetValueAndDerivative method.
   * This will end up calling the derived class'
   * GetValueAndDerivativeProcessPoint for each valid point in
   * VirtualDomainRegion.
   * Pass in \c derivativeReturn from user. Results are written directly
   * into this parameter.
   * \sa GetValueAndDerivativeThreadedPostProcess
   */
  virtual void GetValueAndDerivativeThreadedExecute( DerivativeType & derivativeReturn ) const;

  /** Default post-processing after multi-threaded calculation of
   * value and derivative. Typically called by derived classes after
   * GetValueAndDerivativeThreadedExecute. Collects the results
   * from each thread and sums them.
   * Results are stored in \c m_Value and \c m_DerivativeResult.
   * \c m_DerivativeResult is set during initialization to point to the
   * user-supplied derivative parameter in GetValueAndDerivative. Thus,
   * the derivative results are written directly to this parameter.
   * Pass true for \c doAverage to use the number of valid points, \c
   * m_NumberOfValidPoints, to average the value sum, and to average derivative
   * sums for global transforms only (i.e. transforms without local support).
   * Derived classes need not call this if they require special handling.
   */
  virtual void GetValueAndDerivativeThreadedPostProcess( bool doAverage ) const;

  /** Type of the default threader used for dense (full-image) evaulation
   * in GetValue and GetDerivative.
   * This splits an image region in per-thread sub-regions over the outermost
   * image dimension. */
  typedef ImageToData<VirtualImageDimension, Self>
                                             DenseValueAndDerivativeThreaderType;
  typedef typename DenseValueAndDerivativeThreaderType::InputObjectType
                                             DenseThreaderInputObjectType;

  /** Type of the default threader used for sampled evaulation
   * in GetValue and GetDerivative.
   * This splits the list of sample points into equal blocks. */
  typedef Array1DToData<Self> SampledValueAndDerivativeThreaderType;
  typedef typename SampledValueAndDerivativeThreaderType::InputObjectType
                                             SampledThreaderInputObjectType;
  typedef typename SampledValueAndDerivativeThreaderType::InputObjectValueType
                                             SampledThreaderInputObjectValueType;

  /* Optinally set the threader type to use. This performs the splitting of the
   * virtual region over threads, and user may wish to provide a different
   * one that does a different split. The default is ImageToData. */
  itkSetObjectMacro(DenseValueAndDerivativeThreader,DenseValueAndDerivativeThreaderType);

  /** Threader used for dense evaluation of value and deriviative. */
  typename DenseValueAndDerivativeThreaderType::Pointer
                                              m_DenseValueAndDerivativeThreader;

  /** Threader used for sampled evaluation of value and deriviative. */
  typename SampledValueAndDerivativeThreaderType::Pointer
                                              m_SampledValueAndDerivativeThreader;

  /** Intermediary threaded metric value storage. */
  mutable std::vector<InternalComputationValueType>  m_MeasurePerThread;
  mutable std::vector< DerivativeType >              m_DerivativesPerThread;
  mutable std::vector< DerivativeType >              m_LocalDerivativesPerThread;
  mutable std::vector< SizeValueType >               m_NumberOfValidPointsPerThread;

  /** Pre-allocated transform jacobian objects, for use as needed by dervied
   * classes for efficiency. */
  mutable std::vector<JacobianType>           m_MovingTransformJacobianPerThread;

  ImageToImageObjectMetric();
  virtual ~ImageToImageObjectMetric();

  void PrintSelf(std::ostream& os, Indent indent) const;

  /* Verify that virtual domain and displacement field are the same size
   * and in the same physical space. */
  virtual void VerifyDisplacementFieldSizeAndPhysicalSpace();

private:

  /** Multi-threader callback used to initiate processing over images.
   * If a derived class needs to implement its own callback to replace this,
   * define a static method with a different name, and assign it to the
   * threader in the class' constructor by calling
   * \c m_ValueAndDerivativeThreader->SetThreadedGenerateData( mycallback ) */
  static void DenseGetValueAndDerivativeThreadedCallback(
                        const DenseThreaderInputObjectType& virtualImageSubRegion,
                        ThreadIdType threadId,
                        Self * dataHolder);
  static void SampledGetValueAndDerivativeThreadedCallback(
                        const SampledThreaderInputObjectType& sampledRange,
                        ThreadIdType threadId,
                        Self * dataHolder);

  /** Map the fixed point set samples to the virtual domain */
  void MapFixedSampledPointSetToVirtual( void );

  /** Pre-warp the images for efficiency and computational stability.
   * See main class documentation for important considerations. */
  void DoFixedImagePreWarp( void ) const;
  void DoMovingImagePreWarp( void ) const;

  /** Flag to track if threading memory has been initialized since last
   * call to Initialize. */
  mutable bool            m_ThreadingMemoryHasBeenInitialized;

  /** Flag to track if the number of threads has been initialized in
   * the Initialize routine. We don't want GetNumberOfThreads to be
   * called until this has been initialized. */
  mutable bool            m_NumberOfThreadsHasBeenInitialized;

  //purposely not implemented
  ImageToImageObjectMetric(const Self &);
  //purposely not implemented
  void operator=(const Self &);

  //Sample point coordinates from the virtual image domain
  std::vector<VirtualPointType> m_VirtualImageCornerPoints;
};
}//namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageObjectMetric.hxx"
#endif

#endif

/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkImageToImageMetric_h
#define itkImageToImageMetric_h

#include "itkBSplineBaseTransform.h"
#include "itkBSplineInterpolateImageFunction.h"
#include "itkSingleValuedCostFunction.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkSpatialObject.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkMultiThreaderBase.h"

namespace itk
{
/** \class ImageToImageMetric
 * \brief Computes similarity between regions of two images.
 *
 * This Class is templated over the type of the two input images.
 * It expects a Transform and an Interpolator to be plugged in.
 * This particular class is the base class for a hierarchy of
 * similarity metrics.
 *
 * This class computes a value that measures the similarity
 * between the Fixed image and the transformed Moving image.
 * The Interpolator is used to compute intensity values on
 * non-grid positions resulting from mapping points through
 * the Transform.
 *
 *
 * \ingroup RegistrationMetrics
 *
 * \ingroup ITKRegistrationCommon
 */

template <typename TFixedImage, typename TMovingImage>
class ITK_TEMPLATE_EXPORT ImageToImageMetric : public SingleValuedCostFunction
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageToImageMetric);

  /** Standard class type aliases. */
  using Self = ImageToImageMetric;
  using Superclass = SingleValuedCostFunction;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Type used for representing point components  */
  using CoordinateRepresentationType = typename Superclass::ParametersValueType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToImageMetric, SingleValuedCostFunction);

  /**  Type of the moving Image. */
  using MovingImageType = TMovingImage;
  using MovingImagePixelType = typename TMovingImage::PixelType;
  using MovingImageConstPointer = typename MovingImageType::ConstPointer;

  /**  Type of the fixed Image. */
  using FixedImageType = TFixedImage;
  using FixedImagePixelType = typename TFixedImage::PixelType;
  using FixedImageConstPointer = typename FixedImageType::ConstPointer;
  using FixedImageRegionType = typename FixedImageType::RegionType;

  /** Constants for the image dimensions */
  static constexpr unsigned int MovingImageDimension = TMovingImage::ImageDimension;
  static constexpr unsigned int FixedImageDimension = TFixedImage::ImageDimension;

  /**  Type of the Transform Base class */
  using TransformType = Transform<CoordinateRepresentationType, Self::MovingImageDimension, Self::FixedImageDimension>;

  using TransformPointer = typename TransformType::Pointer;
  using InputPointType = typename TransformType::InputPointType;
  using OutputPointType = typename TransformType::OutputPointType;
  using TransformParametersType = typename TransformType::ParametersType;
  using TransformJacobianType = typename TransformType::JacobianType;

  /** Index and Point type alias support */
  using FixedImageIndexType = typename FixedImageType::IndexType;
  using FixedImageIndexValueType = typename FixedImageIndexType::IndexValueType;
  using MovingImageIndexType = typename MovingImageType::IndexType;
  using FixedImagePointType = typename TransformType::InputPointType;
  using MovingImagePointType = typename TransformType::OutputPointType;

  using FixedImageIndexContainer = std::vector<FixedImageIndexType>;

  /**  Type of the Interpolator Base class */
  using InterpolatorType = InterpolateImageFunction<MovingImageType, CoordinateRepresentationType>;

  /** Gaussian filter to compute the gradient of the Moving Image */
  using RealType = typename NumericTraits<MovingImagePixelType>::RealType;
  using GradientPixelType = CovariantVector<RealType, Self::MovingImageDimension>;
  using GradientImageType = Image<GradientPixelType, Self::MovingImageDimension>;
  using GradientImagePointer = SmartPointer<GradientImageType>;
  using GradientImageFilterType = GradientRecursiveGaussianImageFilter<MovingImageType, GradientImageType>;
  using GradientImageFilterPointer = typename GradientImageFilterType::Pointer;

  using InterpolatorPointer = typename InterpolatorType::Pointer;

  /**  Type for the mask of the fixed image. Only pixels that are "inside"
       this mask will be considered for the computation of the metric */
  using FixedImageMaskType = SpatialObject<Self::FixedImageDimension>;
  using FixedImageMaskPointer = typename FixedImageMaskType::Pointer;
  using FixedImageMaskConstPointer = typename FixedImageMaskType::ConstPointer;

  /**  Type for the mask of the moving image. Only pixels that are "inside"
       this mask will be considered for the computation of the metric */
  using MovingImageMaskType = SpatialObject<Self::MovingImageDimension>;
  using MovingImageMaskPointer = typename MovingImageMaskType::Pointer;
  using MovingImageMaskConstPointer = typename MovingImageMaskType::ConstPointer;

  /**  Type of the measure. */
  using MeasureType = typename Superclass::MeasureType;

  /**  Type of the derivative. */
  using DerivativeType = typename Superclass::DerivativeType;

  /**  Type of the parameters. */
  using ParametersType = typename Superclass::ParametersType;

  /** Get/Set the Fixed Image.  */
  itkSetConstObjectMacro(FixedImage, FixedImageType);
  itkGetConstObjectMacro(FixedImage, FixedImageType);

  /** Get/Set the Moving Image.  */
  itkSetConstObjectMacro(MovingImage, MovingImageType);
  itkGetConstObjectMacro(MovingImage, MovingImageType);

  /** Connect the Transform. */
  itkSetObjectMacro(Transform, TransformType);

  /** Get a pointer to the Transform.  */
  itkGetModifiableObjectMacro(Transform, TransformType);

  /** Connect the Interpolator. */
  itkSetObjectMacro(Interpolator, InterpolatorType);

  /** Get a pointer to the Interpolator.  */
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Get the number of pixels considered in the computation. */
  SizeValueType
  GetNumberOfMovingImageSamples()
  {
    return this->GetNumberOfPixelsCounted();
  }

  itkGetConstReferenceMacro(NumberOfPixelsCounted, SizeValueType);

  /** Set the region over which the metric will be computed */
  virtual void
  SetFixedImageRegion(const FixedImageRegionType reg);

  /** Get the region over which the metric will be computed */
  itkGetConstReferenceMacro(FixedImageRegion, FixedImageRegionType);

  /** Set/Get the moving image mask. */
  itkSetObjectMacro(MovingImageMask, MovingImageMaskType);
  itkSetConstObjectMacro(MovingImageMask, MovingImageMaskType);
  itkGetConstObjectMacro(MovingImageMask, MovingImageMaskType);

  /** Set/Get the fixed image mask. */
  itkSetObjectMacro(FixedImageMask, FixedImageMaskType);
  itkSetConstObjectMacro(FixedImageMask, FixedImageMaskType);
  itkGetConstObjectMacro(FixedImageMask, FixedImageMaskType);

  /** Set the fixed image indexes to be used as the samples when
   *   computing the match metric */
  void
  SetFixedImageIndexes(const FixedImageIndexContainer & indexes);

  void
  SetUseFixedImageIndexes(bool useIndexes);

  itkGetConstReferenceMacro(UseFixedImageIndexes, bool);

  /** Set/Get number of work units to use for computations. */
  void
  SetNumberOfWorkUnits(ThreadIdType numberOfThreads);
  itkGetConstReferenceMacro(NumberOfWorkUnits, ThreadIdType);

  /** Set/Get gradient computation. */
  itkSetMacro(ComputeGradient, bool);
  itkGetConstReferenceMacro(ComputeGradient, bool);
  itkBooleanMacro(ComputeGradient);

  /** Computes the gradient image and assigns it to m_GradientImage */
  virtual void
  ComputeGradient();

  /** Get Gradient Image. */
  itkGetModifiableObjectMacro(GradientImage, GradientImageType);

  /** Set the parameters defining the Transform. */
  void
  SetTransformParameters(const ParametersType & parameters) const;

  /** Return the number of parameters required by the Transform */
  unsigned int
  GetNumberOfParameters() const override
  {
    return m_Transform->GetNumberOfParameters();
  }

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly     */
  virtual void
  Initialize();

  /** Initialize the components related to supporting multiple threads */
  virtual void
  MultiThreadingInitialize();

  /** Number of spatial samples to used to compute metric
   *   This sets the number of samples.  */
  virtual void
  SetNumberOfFixedImageSamples(SizeValueType numSamples);
  itkGetConstReferenceMacro(NumberOfFixedImageSamples, SizeValueType);

  /** Number of spatial samples to used to compute metric
   *   This sets the number of samples.  */
  void
  SetNumberOfSpatialSamples(SizeValueType num)
  {
    this->SetNumberOfFixedImageSamples(num);
  }

  SizeValueType
  GetNumberOfSpatialSamples()
  {
    return this->GetNumberOfFixedImageSamples();
  }

  /** Minimum fixed-image intensity needed for a sample to be used in the
   *  metric computation */
  void
  SetFixedImageSamplesIntensityThreshold(const FixedImagePixelType & thresh);

  itkGetConstReferenceMacro(FixedImageSamplesIntensityThreshold, FixedImagePixelType);

  void
  SetUseFixedImageSamplesIntensityThreshold(bool useThresh);

  itkGetConstReferenceMacro(UseFixedImageSamplesIntensityThreshold, bool);

  /** Select whether the metric will be computed using all the pixels on the
   * fixed image region, or only using a set of randomly selected pixels.
   * This value override IntensityThreshold, Masks, and SequentialSampling. */
  void
  SetUseAllPixels(bool useAllPixels);

  void
  UseAllPixelsOn()
  {
    this->SetUseAllPixels(true);
  }

  void
  UseAllPixelsOff()
  {
    this->SetUseAllPixels(false);
  }

  itkGetConstReferenceMacro(UseAllPixels, bool);

  /** If set to true, then every pixel in the fixed image will be scanned to
   * determine if it should be used in registration metric computation.  A
   * pixel will be chosen if it meets any mask or threshold limits set.  If
   * set to false, then UseAllPixels will be set to false. */
  void
  SetUseSequentialSampling(bool useSequential);

  itkGetConstReferenceMacro(UseSequentialSampling, bool);

  /** Reinitialize the seed of the random number generator that selects the
   * sample of pixels used for estimating the image histograms and the joint
   * histogram. By nature, this metric is not deterministic, since at each run
   * it may select a different set of pixels. By initializing the random number
   * generator seed to the same value you can restore determinism. On the other
   * hand, calling the method ReinitializeSeed() without arguments will use the
   * clock from your machine in order to have a very random initialization of
   * the seed. This will indeed increase the non-deterministic behavior of the
   * metric. */
  void
  ReinitializeSeed();
  void
  ReinitializeSeed(int seed);

  /** This boolean flag is only relevant when this metric is used along
   * with a BSplineBaseTransform. The flag enables/disables the
   * caching of values computed when a physical point is mapped through
   * the BSplineBaseTransform. In particular it will cache the
   * values of the BSpline weights for that points, and the indexes
   * indicating what BSpline-grid nodes are relevant for that specific
   * point. This caching is made optional due to the fact that the
   * memory arrays used for the caching can reach large sizes even for
   * moderate image size problems. For example, for a 3D image of
   * 256^3, using 20% of pixels, these arrays will take about 1
   * Gigabyte of RAM for storage. The ratio of computing time between
   * using the cache or not using the cache can reach 1:5, meaning that
   * using the caching can provide a five times speed up. It is
   * therefore, interesting to enable the caching, if enough memory is
   * available for it. The caching is enabled by default, in order to
   * preserve backward compatibility with previous versions of ITK. */
  itkSetMacro(UseCachingOfBSplineWeights, bool);
  itkGetConstReferenceMacro(UseCachingOfBSplineWeights, bool);
  itkBooleanMacro(UseCachingOfBSplineWeights);

  using MultiThreaderType = MultiThreaderBase;
  /** Get the Threader. */
  itkGetModifiableObjectMacro(Threader, MultiThreaderType);
  const TransformPointer *
  GetThreaderTransform()
  {
    return m_ThreaderTransform;
  }

protected:
  ImageToImageMetric();
  ~ImageToImageMetric() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** \class FixedImageSamplePoint
   * A fixed image spatial sample consists of the fixed domain point
   * and the fixed image value at that point.
   * \ingroup ITKRegistrationCommon
   */
  class FixedImageSamplePoint
  {
  public:
    FixedImageSamplePoint()
    {
      point.Fill(0.0);
      value = 0;
      valueIndex = 0;
    }

    ~FixedImageSamplePoint() = default;

  public:
    FixedImagePointType point;
    double              value;
    unsigned int        valueIndex;
  };

  bool                     m_UseFixedImageIndexes{ false };
  FixedImageIndexContainer m_FixedImageIndexes;

  bool                m_UseFixedImageSamplesIntensityThreshold{ false };
  FixedImagePixelType m_FixedImageSamplesIntensityThreshold;

  /** FixedImageSamplePoint type alias support */
  using FixedImageSampleContainer = std::vector<FixedImageSamplePoint>;

  /** Uniformly select a sample set from the fixed image domain. */
  virtual void
  SampleFixedImageRegion(FixedImageSampleContainer & samples) const;

  virtual void
  SampleFixedImageIndexes(FixedImageSampleContainer & samples) const;

  /** Gather all the pixels from the fixed image domain. */
  virtual void
  SampleFullFixedImageRegion(FixedImageSampleContainer & samples) const;

  /** Container to store a set of points and fixed image values. */
  FixedImageSampleContainer m_FixedImageSamples;

  SizeValueType m_NumberOfParameters{ 0 };

  SizeValueType m_NumberOfFixedImageSamples{ 50000 };
  // m_NumberOfPixelsCounted must be mutable because the const
  // thread consolidation functions merge each work unit's values
  // onto this accumulator variable.
  mutable SizeValueType m_NumberOfPixelsCounted{ 0 };

  FixedImageConstPointer  m_FixedImage;
  MovingImageConstPointer m_MovingImage;

  /** Main transform to be used in thread = 0 */
  TransformPointer m_Transform;
  /** Copies of Transform helpers per thread (N-1 of them, since m_Transform
   * will do the work for thread=0. */
  TransformPointer * m_ThreaderTransform;

  InterpolatorPointer m_Interpolator;

  bool                 m_ComputeGradient{ true };
  GradientImagePointer m_GradientImage;

  FixedImageMaskConstPointer  m_FixedImageMask;
  MovingImageMaskConstPointer m_MovingImageMask;

  ThreadIdType m_NumberOfWorkUnits{ 1 };

  bool m_UseAllPixels{ false };
  bool m_UseSequentialSampling{ false };

  bool m_ReseedIterator{ false };

  mutable int m_RandomSeed;

  /** Types and variables related to BSpline deformable transforms.
   * If the transform is of type third order BSplineBaseTransform,
   * then we can speed up the metric derivative calculation by
   * only inspecting the parameters within the support region
   * of a mapped point.  */

  /** Boolean to indicate if the transform is BSpline deformable. */
  bool m_TransformIsBSpline{ false };

  /** The number of BSpline transform weights is the number of
   * of parameter in the support region (per dimension ). */
  SizeValueType m_NumBSplineWeights{ 0 };

  static constexpr unsigned int DeformationSplineOrder = 3;

  using BSplineTransformType =
    BSplineBaseTransform<CoordinateRepresentationType, FixedImageType::ImageDimension, Self::DeformationSplineOrder>;

  using BSplineTransformWeightsType = typename BSplineTransformType::WeightsType;
  using WeightsValueType = typename BSplineTransformWeightsType::ValueType;
  using BSplineTransformWeightsArrayType = Array2D<WeightsValueType>;

  using BSplineTransformIndexArrayType = typename BSplineTransformType::ParameterIndexArrayType;
  using IndexValueType = typename BSplineTransformIndexArrayType::ValueType;
  using BSplineTransformIndicesArrayType = Array2D<IndexValueType>;

  using MovingImagePointArrayType = std::vector<MovingImagePointType>;
  using BooleanArrayType = std::vector<bool>;
  using BSplineParametersOffsetType = FixedArray<SizeValueType, FixedImageType::ImageDimension>;
  /**
   * If a BSplineInterpolationFunction is used, this class obtain
   * image derivatives from the BSpline interpolator. Otherwise,
   * image derivatives are computed using central differencing.
   */
  using BSplineInterpolatorType = BSplineInterpolateImageFunction<MovingImageType, CoordinateRepresentationType>;
  /** Typedefs for using central difference calculator. */
  using DerivativeFunctionType = CentralDifferenceImageFunction<MovingImageType, CoordinateRepresentationType>;
  using ImageDerivativesType = CovariantVector<double, Self::MovingImageDimension>;

  typename BSplineTransformType::Pointer m_BSplineTransform;

  BSplineTransformWeightsArrayType m_BSplineTransformWeightsArray;
  BSplineTransformIndicesArrayType m_BSplineTransformIndicesArray;
  MovingImagePointArrayType        m_BSplinePreTransformPointsArray;
  BooleanArrayType                 m_WithinBSplineSupportRegionArray;

  BSplineParametersOffsetType m_BSplineParametersOffset;

  // Variables needed for optionally caching values when using a BSpline
  // transform.
  bool                                   m_UseCachingOfBSplineWeights{ true };
  mutable BSplineTransformWeightsType    m_BSplineTransformWeights;
  mutable BSplineTransformIndexArrayType m_BSplineTransformIndices;

  mutable BSplineTransformWeightsType *    m_ThreaderBSplineTransformWeights;
  mutable BSplineTransformIndexArrayType * m_ThreaderBSplineTransformIndices;

  virtual void
  PreComputeTransformValues();

  /** Transform a point from FixedImage domain to MovingImage domain.
   * This function also checks if mapped point is within support region. */
  virtual void
  TransformPoint(unsigned int           sampleNumber,
                 MovingImagePointType & mappedPoint,
                 bool &                 sampleOk,
                 double &               movingImageValue,
                 ThreadIdType           threadId) const;

  virtual void
  TransformPointWithDerivatives(unsigned int           sampleNumber,
                                MovingImagePointType & mappedPoint,
                                bool &                 sampleOk,
                                double &               movingImageValue,
                                ImageDerivativesType & movingImageGradient,
                                ThreadIdType           threadId) const;

  /** Boolean to indicate if the interpolator BSpline. */
  bool m_InterpolatorIsBSpline{ false };
  /** Pointer to BSplineInterpolator. */
  typename BSplineInterpolatorType::Pointer m_BSplineInterpolator;

  /** Pointer to central difference calculator. */
  typename DerivativeFunctionType::Pointer m_DerivativeCalculator;

  /** Compute image derivatives at a point. */
  virtual void
  ComputeImageDerivatives(const MovingImagePointType & mappedPoint,
                          ImageDerivativesType &       gradient,
                          ThreadIdType                 threadId) const;

  /**
   * Types and variables related to multi-threading
   */

  /**
   * \class  ConstantPointerWrapper
   * A class to wrap around a const pointer that can be passed
   * as a non-const object to the SetSingleMethod function
   * as a non-const void *.
   * Do not allow inheritance for objects that are intended for static_cast<void *>
   * \ingroup ITKRegistrationCommon
   */
  class ConstantPointerWrapper final
  {
  public:
    ConstantPointerWrapper(ImageToImageMetric * i2i_metricPointer)
      : m_ConstMetricPointer{ i2i_metricPointer }
    {}
    const ImageToImageMetric *
    GetConstMetricPointer() const
    {
      return m_ConstMetricPointer;
    }

  private:
    const ImageToImageMetric * m_ConstMetricPointer;
  };

  /**
   * \class MultiThreaderWorkUnitInfoImageToImageMetricWrapper
   * This helper local class is used to extract information from the
   * MultiThreaderType::WorkUnitInfo info type
   * Do not allow inheritance for objects that are intended for static_cast<void *>
   * \ingroup ITKRegistrationCommon
   */
  class MultiThreaderWorkUnitInfoImageToImageMetricWrapper final
  {
  public:
    MultiThreaderWorkUnitInfoImageToImageMetricWrapper(const void * workunitInfoAsVoid)
      : m_WorkUnitInfo(static_cast<const typename MultiThreaderType::WorkUnitInfo *>(workunitInfoAsVoid))
    {}
    ThreadIdType
    GetThreadId() const
    {
      return m_WorkUnitInfo->WorkUnitID;
    }
    const ImageToImageMetric *
    GetConstImageToImageMetricPointer() const
    {
      return (static_cast<ConstantPointerWrapper *>(m_WorkUnitInfo->UserData))->GetConstMetricPointer();
    }

  private:
    const typename MultiThreaderType::WorkUnitInfo * m_WorkUnitInfo;
  };

  MultiThreaderType::Pointer m_Threader;
  ConstantPointerWrapper *   m_ConstSelfWrapper;
  mutable unsigned int *     m_ThreaderNumberOfMovingImageSamples{ nullptr };
  bool                       m_WithinThreadPreProcess{ false };
  bool                       m_WithinThreadPostProcess{ false };

  void
  GetValueMultiThreadedInitiate() const;

  void
  GetValueMultiThreadedPostProcessInitiate() const;

  static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
  GetValueMultiThreaded(void * workunitInfoAsVoid);

  static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
  GetValueMultiThreadedPostProcess(void * workunitInfoAsVoid);

  virtual inline void
  GetValueThread(ThreadIdType threadId) const;

  virtual inline void
  GetValueThreadPreProcess(ThreadIdType itkNotUsed(threadId), bool itkNotUsed(withinSampleThread)) const
  {}
  virtual inline bool
  GetValueThreadProcessSample(ThreadIdType                 itkNotUsed(threadId),
                              SizeValueType                itkNotUsed(fixedImageSample),
                              const MovingImagePointType & itkNotUsed(mappedPoint),
                              double                       itkNotUsed(movingImageValue)) const
  {
    return false;
  }
  virtual inline void
  GetValueThreadPostProcess(ThreadIdType itkNotUsed(threadId), bool itkNotUsed(withinSampleThread)) const
  {}

  void
  GetValueAndDerivativeMultiThreadedInitiate() const;

  void
  GetValueAndDerivativeMultiThreadedPostProcessInitiate() const;

  static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
  GetValueAndDerivativeMultiThreaded(void * workunitInfoAsVoid);

  static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
  GetValueAndDerivativeMultiThreadedPostProcess(void * workunitInfoAsVoid);

  virtual inline void
  GetValueAndDerivativeThread(ThreadIdType threadId) const;

  virtual inline void
  GetValueAndDerivativeThreadPreProcess(ThreadIdType itkNotUsed(threadId), bool itkNotUsed(withinSampleThread)) const
  {}
  virtual inline bool
  GetValueAndDerivativeThreadProcessSample(ThreadIdType                 itkNotUsed(threadId),
                                           SizeValueType                itkNotUsed(fixedImageSample),
                                           const MovingImagePointType & itkNotUsed(mappedPoint),
                                           double                       itkNotUsed(movingImageValue),
                                           const ImageDerivativesType & itkNotUsed(movingImageGradientValue)) const
  {
    return false;
  }
  virtual inline void
  GetValueAndDerivativeThreadPostProcess(ThreadIdType itkNotUsed(threadId), bool itkNotUsed(withinSampleThread)) const
  {}

  /** Synchronizes the threader transforms with the transform
   *   member variable.
   */
  virtual void
  SynchronizeTransforms() const;

private:
  FixedImageRegionType m_FixedImageRegion;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageToImageMetric.hxx"
#endif

#endif

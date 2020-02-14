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
#ifndef itkImageRegistrationMethodv4_h
#define itkImageRegistrationMethodv4_h

#include "itkProcessObject.h"

#include "itkCompositeTransform.h"
#include "itkDataObjectDecorator.h"
#include "itkObjectToObjectMetricBase.h"
#include "itkObjectToObjectMultiMetricv4.h"
#include "itkObjectToObjectOptimizerBase.h"
#include "itkImageToImageMetricv4.h"
#include "itkPointSetToPointSetMetricv4.h"
#include "itkShrinkImageFilter.h"
#include "itkIdentityTransform.h"
#include "itkTransformParametersAdaptorBase.h"
#include "ITKRegistrationMethodsv4Export.h"

#include <vector>

namespace itk
{
/**\class ImageRegistrationMethodv4Enums
 *\brief Contains all enum classes for ImageRegistrationMethodv4 class.
 * \ingroup ITKRegistrationMethodsv4
 */
class ImageRegistrationMethodv4Enums
{
public:
  /**
   * \class MetricSamplingStrategy
   * \ingroup ITKRegistrationMethodsv4
   * \brief enum type for metric sampling strategy
   */
  enum class MetricSamplingStrategy : uint8_t
  {
    NONE,
    REGULAR,
    RANDOM
  };
};
// Define how to print enumeration
extern ITKRegistrationMethodsv4_EXPORT std::ostream &
                                       operator<<(std::ostream & out, const ImageRegistrationMethodv4Enums::MetricSamplingStrategy value);

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
template <typename TFixedImage,
          typename TMovingImage,
          typename TOutputTransform = Transform<double, TFixedImage::ImageDimension, TFixedImage::ImageDimension>,
          typename TVirtualImage = TFixedImage,
          typename TPointSet = PointSet<unsigned int, TFixedImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT ImageRegistrationMethodv4 : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageRegistrationMethodv4);

  /** Standard class type aliases. */
  using Self = ImageRegistrationMethodv4;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TFixedImage::ImageDimension;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageRegistrationMethodv4, ProcessObject);

  /** Input type alias for the images and transforms. */
  using FixedImageType = TFixedImage;
  using FixedImagePointer = typename FixedImageType::Pointer;
  using FixedImageConstPointer = typename FixedImageType::ConstPointer;
  using FixedImagesContainerType = std::vector<FixedImageConstPointer>;
  using MovingImageType = TMovingImage;
  using MovingImagePointer = typename MovingImageType::Pointer;
  using MovingImageConstPointer = typename MovingImageType::ConstPointer;
  using MovingImagesContainerType = std::vector<MovingImageConstPointer>;

  using PointSetType = TPointSet;
  using PointSetConstPointer = typename PointSetType::ConstPointer;
  using PointSetsContainerType = std::vector<PointSetConstPointer>;

  /** Metric and transform type alias */
  using OutputTransformType = TOutputTransform;
  using OutputTransformPointer = typename OutputTransformType::Pointer;
  using RealType = typename OutputTransformType::ScalarType;
  using DerivativeType = typename OutputTransformType::DerivativeType;
  using DerivativeValueType = typename DerivativeType::ValueType;

  using InitialTransformType = Transform<RealType, ImageDimension, ImageDimension>;
  using InitialTransformPointer = typename InitialTransformType::Pointer;

  using CompositeTransformType = CompositeTransform<RealType, ImageDimension>;
  using CompositeTransformPointer = typename CompositeTransformType::Pointer;

  using MetricType = ObjectToObjectMetricBaseTemplate<RealType>;
  using MetricPointer = typename MetricType::Pointer;

  using VectorType = Vector<RealType, ImageDimension>;

  using VirtualImageType = TVirtualImage;
  using VirtualImagePointer = typename VirtualImageType::Pointer;
  using VirtualImageBaseType = ImageBase<ImageDimension>;
  using VirtualImageBaseConstPointer = typename VirtualImageBaseType::ConstPointer;

  using MultiMetricType = ObjectToObjectMultiMetricv4<ImageDimension, ImageDimension, VirtualImageType, RealType>;
  using ImageMetricType = ImageToImageMetricv4<FixedImageType, MovingImageType, VirtualImageType, RealType>;
  using PointSetMetricType = PointSetToPointSetMetricv4<PointSetType, PointSetType, RealType>;

  using FixedImageMaskType = typename ImageMetricType::FixedImageMaskType;
  using FixedImageMaskConstPointer = typename FixedImageMaskType::ConstPointer;
  using FixedImageMasksContainerType = std::vector<FixedImageMaskConstPointer>;
  using MovingImageMaskType = typename ImageMetricType::MovingImageMaskType;
  using MovingImageMaskConstPointer = typename MovingImageMaskType::ConstPointer;
  using MovingImageMasksContainerType = std::vector<MovingImageMaskConstPointer>;

  /**
   * Type for the output: Using Decorator pattern for enabling the transform to be
   * passed in the data pipeline
   */
  using DecoratedOutputTransformType = DataObjectDecorator<OutputTransformType>;
  using DecoratedOutputTransformPointer = typename DecoratedOutputTransformType::Pointer;
  using DecoratedInitialTransformType = DataObjectDecorator<InitialTransformType>;
  using DecoratedInitialTransformPointer = typename DecoratedInitialTransformType::Pointer;

  using ShrinkFilterType = ShrinkImageFilter<FixedImageType, VirtualImageType>;
  using ShrinkFactorsPerDimensionContainerType = typename ShrinkFilterType::ShrinkFactorsType;

  using ShrinkFactorsArrayType = Array<SizeValueType>;

  using SmoothingSigmasArrayType = Array<RealType>;
  using MetricSamplingPercentageArrayType = Array<RealType>;

  /** Transform adaptor type alias */
  using TransformParametersAdaptorType = TransformParametersAdaptorBase<InitialTransformType>;
  using TransformParametersAdaptorPointer = typename TransformParametersAdaptorType::Pointer;
  using TransformParametersAdaptorsContainerType = std::vector<TransformParametersAdaptorPointer>;

  /**  Type of the optimizer. */
  using OptimizerType = ObjectToObjectOptimizerBaseTemplate<RealType>;
  using OptimizerPointer = typename OptimizerType::Pointer;

  /** Weights type for the optimizer. */
  using OptimizerWeightsType = typename OptimizerType::ScalesType;

  using MetricSamplingStrategyEnum = ImageRegistrationMethodv4Enums::MetricSamplingStrategy;
#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  using MetricSamplingStrategyType = MetricSamplingStrategyEnum;
  static constexpr MetricSamplingStrategyEnum NONE = MetricSamplingStrategyEnum::NONE;
  static constexpr MetricSamplingStrategyEnum REGULAR = MetricSamplingStrategyEnum::REGULAR;
  static constexpr MetricSamplingStrategyEnum RANDOM = MetricSamplingStrategyEnum::RANDOM;
#endif


  using MetricSamplePointSetType = typename ImageMetricType::FixedSampledPointSetType;

  /** Set/get the fixed images. */
  virtual void
  SetFixedImage(const FixedImageType * image)
  {
    this->SetFixedImage(0, image);
  }
  virtual const FixedImageType *
  GetFixedImage() const
  {
    return this->GetFixedImage(0);
  }
  virtual void
                                 SetFixedImage(SizeValueType, const FixedImageType *);
  virtual const FixedImageType * GetFixedImage(SizeValueType) const;

  /** Set the moving images. */
  virtual void
  SetMovingImage(const MovingImageType * image)
  {
    this->SetMovingImage(0, image);
  }
  virtual const MovingImageType *
  GetMovingImage() const
  {
    return this->GetMovingImage(0);
  }
  virtual void
                                  SetMovingImage(SizeValueType, const MovingImageType *);
  virtual const MovingImageType * GetMovingImage(SizeValueType) const;

  /** Set/get the fixed point sets. */
  virtual void
  SetFixedPointSet(const PointSetType * pointSet)
  {
    this->SetFixedPointSet(0, pointSet);
  }
  virtual const PointSetType *
  GetFixedPointSet() const
  {
    return this->GetFixedPointSet(0);
  }
  virtual void
                               SetFixedPointSet(SizeValueType, const PointSetType *);
  virtual const PointSetType * GetFixedPointSet(SizeValueType) const;

  /** Set the moving point sets. */
  virtual void
  SetMovingPointSet(const PointSetType * pointSet)
  {
    this->SetMovingPointSet(0, pointSet);
  }
  virtual const PointSetType *
  GetMovingPointSet() const
  {
    return this->GetMovingPointSet(0);
  }
  virtual void
                               SetMovingPointSet(SizeValueType, const PointSetType *);
  virtual const PointSetType * GetMovingPointSet(SizeValueType) const;

  /** Set/Get the optimizer. */
  itkSetObjectMacro(Optimizer, OptimizerType);
  itkGetModifiableObjectMacro(Optimizer, OptimizerType);

  /**
   * Set/Get the optimizer weights.  Allows setting of a per-local-parameter
   * weighting array. If unset, the weights are treated as identity. Weights
   * are used to mask out a particular parameter during optimization to hold
   * it constant. Or they may be used to apply another kind of prior knowledge.
   * The size of the weights must be equal to the number of the local transformation
   * parameters.
   */
  void
  SetOptimizerWeights(OptimizerWeightsType &);
  itkGetConstMacro(OptimizerWeights, OptimizerWeightsType);

  /** Set/Get the metric. */
  itkSetObjectMacro(Metric, MetricType);
  itkGetModifiableObjectMacro(Metric, MetricType);

  /** Set/Get the metric sampling strategy. */
  itkSetEnumMacro(MetricSamplingStrategy, MetricSamplingStrategyEnum);
  itkGetEnumMacro(MetricSamplingStrategy, MetricSamplingStrategyEnum);

  /** Reinitialize the seed for the random number generators that
   * select the samples for some metric sampling strategies.
   *
   * By initializing the random number generator seed to a value the
   * same deterministic sampling will be used each Update
   * execution. On the other hand, calling the method
   * ReinitializeSeed() without arguments will use the wall clock in
   * order to have psuedo-random initialization of the seeds. This
   * will indeed increase the non-deterministic behavior of the
   * metric.
   */
  void
  MetricSamplingReinitializeSeed();
  void
  MetricSamplingReinitializeSeed(int seed);

  /** Set the metric sampling percentage. Valid values are in (0.0, 1.0] */
  void
  SetMetricSamplingPercentage(const RealType);

  /** Set the metric sampling percentage. Valid values are in (0.0,1.0]. */
  virtual void
  SetMetricSamplingPercentagePerLevel(const MetricSamplingPercentageArrayType & samplingPercentages);
  itkGetConstMacro(MetricSamplingPercentagePerLevel, MetricSamplingPercentageArrayType);

  /** Set/Get the initial fixed transform. */
  itkSetGetDecoratedObjectInputMacro(FixedInitialTransform, InitialTransformType);

  /** Set/Get the initial moving transform. */
  itkSetGetDecoratedObjectInputMacro(MovingInitialTransform, InitialTransformType);

  /** Set/Get the initial transform to be optimized
   *
   * This transform is composed with the MovingInitialTransform to
   * specify the initial transformation from the moving image to
   * the virtual image. It is used for the default parameters, and can
   * be use to specify the transform type.
   *
   * If the filter has "InPlace" set then this transform will be the
   * output transform object or "grafted" to the output. Otherwise,
   * this InitialTransform will be deep copied or "cloned" to the
   * output.
   *
   * If this parameter is not set then a default constructed output
   * transform is used.
   */
  itkSetGetDecoratedObjectInputMacro(InitialTransform, InitialTransformType);

  /** Set/Get the transform adaptors. */
  void
  SetTransformParametersAdaptorsPerLevel(TransformParametersAdaptorsContainerType &);
  const TransformParametersAdaptorsContainerType &
  GetTransformParametersAdaptorsPerLevel() const;

  /**
   * Set/Get the number of multi-resolution levels.  In setting the number of
   * levels we need to set the following for each level:
   *   \li shrink factors for the virtual domain
   *   \li sigma smoothing parameter
   *   \li transform adaptor with specific parameters for the specified level
   */
  void
  SetNumberOfLevels(const SizeValueType);
  itkGetConstMacro(NumberOfLevels, SizeValueType);

  /**
   * Set the shrink factors for each level where each level has a constant
   * shrink factor for each dimension.  For example, input to the function
   * of factors = [4,2,1] will shrink the image in every dimension by 4
   * the first level, then by 2 at the second level, then the original resolution
   * for the final level (uses the \c itkShrinkImageFilter).
   */
  void
  SetShrinkFactorsPerLevel(ShrinkFactorsArrayType factors)
  {
    for (unsigned int level = 0; level < factors.Size(); ++level)
    {
      ShrinkFactorsPerDimensionContainerType shrinkFactors;
      shrinkFactors.Fill(factors[level]);
      this->SetShrinkFactorsPerDimension(level, shrinkFactors);
    }
  }

  /**
   * Get the shrink factors for a specific level.
   */
  ShrinkFactorsPerDimensionContainerType
  GetShrinkFactorsPerDimension(const unsigned int level) const
  {
    if (level >= this->m_ShrinkFactorsPerLevel.size())
    {
      itkExceptionMacro("Requesting level greater than the number of levels.");
    }
    return this->m_ShrinkFactorsPerLevel[level];
  }

  /**
   * Set the shrink factors for a specific level for each dimension.
   */
  void
  SetShrinkFactorsPerDimension(unsigned int level, ShrinkFactorsPerDimensionContainerType factors)
  {
    if (level >= this->m_ShrinkFactorsPerLevel.size())
    {
      this->m_ShrinkFactorsPerLevel.resize(level + 1);
    }
    this->m_ShrinkFactorsPerLevel[level] = factors;
    this->Modified();
  }

  /**
   * Set/Get the smoothing sigmas for each level.  At each resolution level, a gaussian smoothing
   * filter (specifically, the \c itkDiscreteGaussianImageFilter) is applied.  Sigma values are
   * specified according to the option \c m_SmoothingSigmasAreSpecifiedInPhysicalUnits.
   */
  itkSetMacro(SmoothingSigmasPerLevel, SmoothingSigmasArrayType);
  itkGetConstMacro(SmoothingSigmasPerLevel, SmoothingSigmasArrayType);

  /**
   * Set/Get whether to specify the smoothing sigmas for each level in physical units
   * (default) or in terms of voxels.
   */
  itkSetMacro(SmoothingSigmasAreSpecifiedInPhysicalUnits, bool);
  itkGetConstMacro(SmoothingSigmasAreSpecifiedInPhysicalUnits, bool);
  itkBooleanMacro(SmoothingSigmasAreSpecifiedInPhysicalUnits);

  /** Make a DataObject of the correct type to be used as the specified output. */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer MakeOutput(DataObjectPointerArraySizeType) override;

  /** Returns the transform resulting from the registration process  */
  virtual DecoratedOutputTransformType *
  GetOutput();
  virtual const DecoratedOutputTransformType *
  GetOutput() const;

  virtual DecoratedOutputTransformType *
  GetTransformOutput()
  {
    return this->GetOutput();
  }
  virtual const DecoratedOutputTransformType *
  GetTransformOutput() const
  {
    return this->GetOutput();
  }

  virtual OutputTransformType *
  GetModifiableTransform();
  virtual const OutputTransformType *
  GetTransform() const;

  /** Get the current level.  This is a helper function for reporting observations. */
  itkGetConstMacro(CurrentLevel, SizeValueType);

  /** Get the current iteration.  This is a helper function for reporting observations. */
  itkGetConstReferenceMacro(CurrentIteration, SizeValueType);

  /* Get the current metric value.  This is a helper function for reporting observations. */
  itkGetConstReferenceMacro(CurrentMetricValue, RealType);

  /** Get the current convergence value.  This is a helper function for reporting observations. */
  itkGetConstReferenceMacro(CurrentConvergenceValue, RealType);

  /** Get the current convergence state per level.  This is a helper function for reporting observations. */
  itkGetConstReferenceMacro(IsConverged, bool);

  /** Request that the InitialTransform be grafted onto the output,
   * there by not creating a copy.
   */
  itkSetMacro(InPlace, bool);
  itkGetConstMacro(InPlace, bool);
  itkBooleanMacro(InPlace);

  /**
   * Initialize the current linear transform to be optimized with the center of the
   * previous transform in the queue.  This provides a much better initialization than
   * the default origin.
   */
  itkBooleanMacro(InitializeCenterOfLinearOutputTransform);
  itkSetMacro(InitializeCenterOfLinearOutputTransform, bool);
  itkGetConstMacro(InitializeCenterOfLinearOutputTransform, bool);

  /**
   * We try to initialize the center of a linear transform (specifically those
   * derived from itk::MatrixOffsetTransformBase).  There are a number of
   * checks that we need to make to account for all possible scenarios:
   *   1)  we check to make sure the m_OutputTransform is of the appropriate type
   *       such that it makes sense to try to center the transform.  Local transforms
   *       such as SyN and B-spline do not need to be "centered",
   *   2)  we check to make sure the composite transform (to which we'll add the
   *       m_OutputTransform) is not empty,
   *   3)  we look for the first previous transform which has a center parameter,
   *       (which, presumably, been optimized beforehand), and
   */
  void
  InitializeCenterOfLinearOutputTransform();

protected:
  ImageRegistrationMethodv4();
  ~ImageRegistrationMethodv4() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Perform the registration. */
  void
  GenerateData() override;

  virtual void
  AllocateOutputs();

  /** Initialize by setting the interconnects between the components. */
  virtual void
  InitializeRegistrationAtEachLevel(const SizeValueType);

  /** Get the virtual domain image from the metric(s) */
  virtual VirtualImageBaseConstPointer
  GetCurrentLevelVirtualDomainImage();

  /** Get metric samples. */
  virtual void
  SetMetricSamplePoints();

  SizeValueType m_CurrentLevel;
  SizeValueType m_NumberOfLevels;
  SizeValueType m_CurrentIteration;
  RealType      m_CurrentMetricValue;
  RealType      m_CurrentConvergenceValue;
  bool          m_IsConverged;

  FixedImagesContainerType      m_FixedSmoothImages;
  MovingImagesContainerType     m_MovingSmoothImages;
  FixedImageMasksContainerType  m_FixedImageMasks;
  MovingImageMasksContainerType m_MovingImageMasks;
  VirtualImagePointer           m_VirtualDomainImage;
  PointSetsContainerType        m_FixedPointSets;
  PointSetsContainerType        m_MovingPointSets;
  SizeValueType                 m_NumberOfFixedObjects;
  SizeValueType                 m_NumberOfMovingObjects;

  OptimizerPointer     m_Optimizer;
  OptimizerWeightsType m_OptimizerWeights;
  bool                 m_OptimizerWeightsAreIdentity;

  MetricPointer                                       m_Metric;
  MetricSamplingStrategyEnum                          m_MetricSamplingStrategy;
  MetricSamplingPercentageArrayType                   m_MetricSamplingPercentagePerLevel;
  SizeValueType                                       m_NumberOfMetrics;
  int                                                 m_FirstImageMetricIndex;
  std::vector<ShrinkFactorsPerDimensionContainerType> m_ShrinkFactorsPerLevel;
  SmoothingSigmasArrayType                            m_SmoothingSigmasPerLevel;
  bool                                                m_SmoothingSigmasAreSpecifiedInPhysicalUnits;

  bool m_ReseedIterator;
  int  m_RandomSeed;
  int  m_CurrentRandomSeed;


  TransformParametersAdaptorsContainerType m_TransformParametersAdaptorsPerLevel;

  CompositeTransformPointer m_CompositeTransform;

  // TODO: m_OutputTransform should be removed and replaced with a named input parameter for
  //      the pipeline
  OutputTransformPointer m_OutputTransform;


private:
  bool m_InPlace;

  bool m_InitializeCenterOfLinearOutputTransform;

  // helper function to create the right kind of concrete transform
  template <typename TTransform>
  static void
  MakeOutputTransform(SmartPointer<TTransform> & ptr)
  {
    ptr = TTransform::New();
  }

  static void
  MakeOutputTransform(SmartPointer<InitialTransformType> & ptr)
  {
    ptr = IdentityTransform<RealType, ImageDimension>::New().GetPointer();
  }
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageRegistrationMethodv4.hxx"
#endif

#endif

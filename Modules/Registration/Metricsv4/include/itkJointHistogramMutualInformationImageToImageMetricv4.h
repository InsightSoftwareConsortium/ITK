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

#ifndef itkJointHistogramMutualInformationImageToImageMetricv4_h
#define itkJointHistogramMutualInformationImageToImageMetricv4_h

#include "itkImageToImageMetricv4.h"
#include "itkImage.h"
#include "itkBSplineDerivativeKernelFunction.h"

#include "itkJointHistogramMutualInformationComputeJointPDFThreader.h"
#include "itkJointHistogramMutualInformationGetValueAndDerivativeThreader.h"

namespace itk
{
/** \class JointHistogramMutualInformationImageToImageMetricv4
 * \brief Computes the mutual information between two images to be
 * registered using the method referenced below.
 *
 * References:
 * [1] "Optimization of Mutual Information for MultiResolution Image
 *      Registration"
 *      P. Thevenaz and M. Unser
 *      IEEE Transactions in Image Processing, 9(12) December 2000.
 *
 * \ingroup ITKMetricsv4
 */
template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage = TFixedImage,
          typename TInternalComputationValueType = double,
          typename TMetricTraits =
            DefaultImageToImageMetricTraitsv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType>>
class ITK_TEMPLATE_EXPORT JointHistogramMutualInformationImageToImageMetricv4
  : public ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(JointHistogramMutualInformationImageToImageMetricv4);

  /** Standard class type aliases. */
  using Self = JointHistogramMutualInformationImageToImageMetricv4;
  using Superclass =
    ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(JointHistogramMutualInformationImageToImageMetricv4, ImageToImageMetricv4);

  /** Type used for representing parameter values  */
  using CoordinateRepresentationType = typename Superclass::CoordinateRepresentationType;
  /** Type used internally for computations */
  /** It should be possible to derive the internal computation type from the class object. */
  using InternalComputationValueType = TInternalComputationValueType;
  /**  Type of the parameters. */
  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = typename Superclass::ParametersValueType;
  using NumberOfParametersType = typename Superclass::NumberOfParametersType;

  /** Superclass type alias */
  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using FixedImagePointType = typename Superclass::FixedImagePointType;
  using FixedImagePixelType = typename Superclass::FixedImagePixelType;
  using FixedImageGradientType = typename Superclass::FixedGradientPixelType;
  using MovingImagePointType = typename Superclass::MovingImagePointType;
  using MovingImagePixelType = typename Superclass::MovingImagePixelType;
  using MovingImageGradientType = typename Superclass::MovingGradientPixelType;

  using FixedTransformJacobianType = typename Superclass::FixedTransformType::JacobianType;
  using MovingTransformJacobianType = typename Superclass::MovingTransformType::JacobianType;

  using VirtualImageType = typename Superclass::VirtualImageType;
  using VirtualIndexType = typename Superclass::VirtualIndexType;
  using VirtualPointType = typename Superclass::VirtualPointType;
  using VirtualPointSetType = typename Superclass::VirtualPointSetType;

  /* Image dimension accessors */
  static constexpr typename TVirtualImage::ImageDimensionType VirtualImageDimension = TVirtualImage::ImageDimension;
  static constexpr typename TMovingImage::ImageDimensionType  MovingImageDimension = TMovingImage::ImageDimension;

  /** Value type of the PDF */
  using PDFValueType = TInternalComputationValueType;

  /** Typedef for the joint PDF and marginal PDF are stored as ITK Images. */
  using MarginalPDFType = Image<PDFValueType, 1>;
  using MarginalPDFIndexType = typename MarginalPDFType::IndexType;
  using MarginalPDFPointType = typename MarginalPDFType::PointType;
  using JointPDFType = Image<PDFValueType, 2>;
  using JointPDFIndexType = typename JointPDFType::IndexType;
  using JointPDFPointType = typename JointPDFType::PointType;
  using JointPDFIndexValueType = typename JointPDFType::IndexValueType;

  /** Get the JointPDF.  Valid after GetValueAndDerivative has been called. */
  itkGetModifiableObjectMacro(JointPDF, JointPDFType);

  // Declare the type for the derivative calculation
  using JPDFGradientFilterType = itk::GradientRecursiveGaussianImageFilter<JointPDFType>;
  using JPDFGradientImageType = typename JPDFGradientFilterType::OutputImageType;
  using JPDFGradientImagePointer = typename JPDFGradientImageType::Pointer;

  using MarginalGradientFilterType = itk::GradientRecursiveGaussianImageFilter<MarginalPDFType>;
  using MarginalGradientImageType = typename MarginalGradientFilterType::OutputImageType;
  using MarginalGradientImagePointer = typename MarginalGradientImageType::Pointer;

  /** pdf interpolator */
  using JointPDFInterpolatorType = LinearInterpolateImageFunction<JointPDFType, double>;
  using JointPDFInterpolatorPointer = typename JointPDFInterpolatorType::Pointer;
  using MarginalPDFInterpolatorType = LinearInterpolateImageFunction<MarginalPDFType, double>;
  using MarginalPDFInterpolatorPointer = typename MarginalPDFInterpolatorType::Pointer;

  /** Joint PDF types */
  using JointPDFValueType = typename JointPDFType::PixelType;
  using JointPDFRegionType = typename JointPDFType::RegionType;
  using JointPDFSizeType = typename JointPDFType::SizeType;
  using JointPDFSpacingType = typename JointPDFType::SpacingType;


  /** Get/Set the number of histogram bins */
  itkSetClampMacro(NumberOfHistogramBins, SizeValueType, 5, NumericTraits<SizeValueType>::max());
  itkGetConstReferenceMacro(NumberOfHistogramBins, SizeValueType);

  /** Get/Set option to smooth the joint pdf after it's updated */
  itkSetMacro(VarianceForJointPDFSmoothing, TInternalComputationValueType);
  itkGetMacro(VarianceForJointPDFSmoothing, TInternalComputationValueType);

  /** Initialize the metric. Make sure all essential inputs are plugged in. */
  void
  Initialize() override;

  MeasureType
  GetValue() const override;

protected:
  JointHistogramMutualInformationImageToImageMetricv4();
  ~JointHistogramMutualInformationImageToImageMetricv4() override = default;

  /** Update the histograms for use in GetValueAndDerivative
   *  Results are returned in \c value and \c derivative.
   */
  void
  InitializeForIteration() const override;

  /** Compute the metric value. For internal use. */
  MeasureType
  ComputeValue() const;

  /** Compute the point location with the JointPDF image.  Returns false if the
   * point is not inside the image. */
  inline void
  ComputeJointPDFPoint(const FixedImagePixelType  fixedImageValue,
                       const MovingImagePixelType movingImageValue,
                       JointPDFPointType &        jointPDFpoint) const;

  friend class JointHistogramMutualInformationComputeJointPDFThreaderBase<
    ThreadedImageRegionPartitioner<Self::VirtualImageDimension>,
    Self>;
  friend class JointHistogramMutualInformationComputeJointPDFThreaderBase<ThreadedIndexedContainerPartitioner, Self>;
  friend class JointHistogramMutualInformationComputeJointPDFThreader<
    ThreadedImageRegionPartitioner<Self::VirtualImageDimension>,
    Self>;
  friend class JointHistogramMutualInformationComputeJointPDFThreader<ThreadedIndexedContainerPartitioner, Self>;

  using JointHistogramMutualInformationDenseComputeJointPDFThreaderType =
    JointHistogramMutualInformationComputeJointPDFThreader<ThreadedImageRegionPartitioner<Self::VirtualImageDimension>,
                                                           Self>;
  using JointHistogramMutualInformationSparseComputeJointPDFThreaderType =
    JointHistogramMutualInformationComputeJointPDFThreader<ThreadedIndexedContainerPartitioner, Self>;

  typename JointHistogramMutualInformationDenseComputeJointPDFThreaderType::Pointer
    m_JointHistogramMutualInformationDenseComputeJointPDFThreader;
  typename JointHistogramMutualInformationSparseComputeJointPDFThreaderType::Pointer
    m_JointHistogramMutualInformationSparseComputeJointPDFThreader;

  friend class JointHistogramMutualInformationGetValueAndDerivativeThreader<
    ThreadedImageRegionPartitioner<Superclass::VirtualImageDimension>,
    Superclass,
    Self>;
  friend class JointHistogramMutualInformationGetValueAndDerivativeThreader<ThreadedIndexedContainerPartitioner,
                                                                            Superclass,
                                                                            Self>;

  using JointHistogramMutualInformationDenseGetValueAndDerivativeThreaderType =
    JointHistogramMutualInformationGetValueAndDerivativeThreader<
      ThreadedImageRegionPartitioner<Superclass::VirtualImageDimension>,
      Superclass,
      Self>;
  using JointHistogramMutualInformationSparseGetValueAndDerivativeThreaderType =
    JointHistogramMutualInformationGetValueAndDerivativeThreader<ThreadedIndexedContainerPartitioner, Superclass, Self>;

  /** Standard PrintSelf method. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Count of the number of valid histogram points. */
  SizeValueType m_JointHistogramTotalCount{ 0 };

private:
  /** The fixed image marginal PDF */
  typename MarginalPDFType::Pointer m_FixedImageMarginalPDF;

  /** The moving image marginal PDF. */
  typename MarginalPDFType::Pointer m_MovingImageMarginalPDF;

  /** The joint PDF and PDF derivatives. */
  mutable typename JointPDFType::Pointer m_JointPDF;

  /** Flag to control smoothing of joint pdf */
  TInternalComputationValueType m_VarianceForJointPDFSmoothing;

  /** Variables to define the marginal and joint histograms. */
  SizeValueType                 m_NumberOfHistogramBins;
  TInternalComputationValueType m_FixedImageTrueMin;
  TInternalComputationValueType m_FixedImageTrueMax;
  TInternalComputationValueType m_MovingImageTrueMin;
  TInternalComputationValueType m_MovingImageTrueMax;
  TInternalComputationValueType m_FixedImageBinSize;
  TInternalComputationValueType m_MovingImageBinSize;

  TInternalComputationValueType m_JointPDFSum;
  JointPDFSpacingType           m_JointPDFSpacing;

  TInternalComputationValueType m_Log2;
  JointPDFIndexValueType        m_Padding;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkJointHistogramMutualInformationImageToImageMetricv4.hxx"
#endif

#endif

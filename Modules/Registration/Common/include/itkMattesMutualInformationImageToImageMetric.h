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
#ifndef itkMattesMutualInformationImageToImageMetric_h
#define itkMattesMutualInformationImageToImageMetric_h

#include "itkImageToImageMetric.h"
#include "itkPoint.h"
#include "itkIndex.h"
#include "itkBSplineDerivativeKernelFunction.h"
#include "itkArray2D.h"

#include <mutex>


namespace itk
{
/** \class MattesMutualInformationImageToImageMetric
 * \brief Computes the mutual information between two images to be
 * registered using the method of Mattes et al.
 *
 * MattesMutualInformationImageToImageMetric computes the mutual
 * information between a fixed and moving image to be registered.
 *
 * This class is templated over the FixedImage type and the MovingImage
 * type.
 *
 * The fixed and moving images are set via methods SetFixedImage() and
 * SetMovingImage(). This metric makes use of user specified Transform and
 * Interpolator. The Transform is used to map points from the fixed image to
 * the moving image domain. The Interpolator is used to evaluate the image
 * intensity at user specified geometric points in the moving image.
 * The Transform and Interpolator are set via methods SetTransform() and
 * SetInterpolator().
 *
 * If a BSplineInterpolationFunction is used, this class obtain
 * image derivatives from the BSpline interpolator. Otherwise,
 * image derivatives are computed using central differencing.
 *
 * \warning This metric assumes that the moving image has already been
 * connected to the interpolator outside of this class.
 *
 * The method GetValue() computes of the mutual information
 * while method GetValueAndDerivative() computes
 * both the mutual information and its derivatives with respect to the
 * transform parameters.
 *
 * The calculations are based on the method of Mattes et al [1,2]
 * where the probability density distribution are estimated using
 * Parzen histograms. Since the fixed image PDF does not contribute
 * to the derivatives, it does not need to be smooth. Hence,
 * a zero order (box car) BSpline kernel is used
 * for the fixed image intensity PDF. On the other hand, to ensure
 * smoothness a third order BSpline kernel is used for the
 * moving image intensity PDF.
 *
 * On Initialize(), the FixedImage is uniformly sampled within
 * the FixedImageRegion. The number of samples used can be set
 * via SetNumberOfSpatialSamples(). Typically, the number of
 * spatial samples used should increase with the image size.
 *
 * The option UseAllPixelOn() disables the random sampling and uses
 * all the pixels of the FixedImageRegion in order to estimate the
 * joint intensity PDF.
 *
 * During each call of GetValue(), GetDerivatives(),
 * GetValueAndDerivatives(), marginal and joint intensity PDF's
 * values are estimated at discrete position or bins.
 * The number of bins used can be set via SetNumberOfHistogramBins().
 * To handle data with arbitrary magnitude and dynamic range,
 * the image intensity is scale such that any contribution to the
 * histogram will fall into a valid bin.
 *
 * One the PDF's have been constructed, the mutual information
 * is obtained by doubling summing over the discrete PDF values.
 *
 *
 * Notes:
 * 1. This class returns the negative mutual information value.
 *
 * References:
 * [1] "Nonrigid multimodality image registration"
 *      D. Mattes, D. R. Haynor, H. Vesselle, T. Lewellen and W. Eubank
 *      Medical Imaging 2001: Image Processing, 2001, pp. 1609-1620.
 * [2] "PET-CT Image Registration in the Chest Using Free-form Deformations"
 *      D. Mattes, D. R. Haynor, H. Vesselle, T. Lewellen and W. Eubank
 *      IEEE Transactions in Medical Imaging. Vol.22, No.1,
        January 2003. pp.120-128.
 * [3] "Optimization of Mutual Information for MultiResolution Image
 *      Registration"
 *      P. Thevenaz and M. Unser
 *      IEEE Transactions in Image Processing, 9(12) December 2000.
 *
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 *
 * \sphinx
 * \sphinxexample{Registration/Common/WatchRegistration,Watch Registration}
 * \endsphinx
 */
template <typename TFixedImage, typename TMovingImage>
class ITK_TEMPLATE_EXPORT MattesMutualInformationImageToImageMetric
  : public ImageToImageMetric<TFixedImage, TMovingImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MattesMutualInformationImageToImageMetric);

  /** Standard class type aliases. */
  using Self = MattesMutualInformationImageToImageMetric;
  using Superclass = ImageToImageMetric<TFixedImage, TMovingImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MattesMutualInformationImageToImageMetric, ImageToImageMetric);

  /** Types inherited from Superclass. */
  using TransformType = typename Superclass::TransformType;
  using TransformPointer = typename Superclass::TransformPointer;
  using TransformJacobianType = typename Superclass::TransformJacobianType;
  using InterpolatorType = typename Superclass::InterpolatorType;
  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using ParametersType = typename Superclass::ParametersType;
  using FixedImageType = typename Superclass::FixedImageType;
  using MovingImageType = typename Superclass::MovingImageType;
  using MovingImagePointType = typename Superclass::MovingImagePointType;
  using FixedImageConstPointer = typename Superclass::FixedImageConstPointer;
  using MovingImageConstPointer = typename Superclass::MovingImageConstPointer;
  using BSplineTransformWeightsType = typename Superclass::BSplineTransformWeightsType;
  using BSplineTransformIndexArrayType = typename Superclass::BSplineTransformIndexArrayType;

  using CoordinateRepresentationType = typename Superclass::CoordinateRepresentationType;
  using FixedImageSampleContainer = typename Superclass::FixedImageSampleContainer;
  using ImageDerivativesType = typename Superclass::ImageDerivativesType;
  using WeightsValueType = typename Superclass::WeightsValueType;
  using IndexValueType = typename Superclass::IndexValueType;

  using OffsetValueType = typename FixedImageType::OffsetValueType;

  /** The moving image dimension. */
  static constexpr unsigned int MovingImageDimension = MovingImageType::ImageDimension;

  /**
   *  Initialize the Metric by
   *  (1) making sure that all the components are present and plugged
   *      together correctly,
   *  (2) uniformly select NumberOfSpatialSamples within
   *      the FixedImageRegion, and
   *  (3) allocate memory for pdf data structures. */
  void
  Initialize() override;

  /**  Get the value. */
  MeasureType
  GetValue(const ParametersType & parameters) const override;

  /** Get the derivatives of the match measure. */
  void
  GetDerivative(const ParametersType & parameters, DerivativeType & derivative) const override;

  /**  Get the value and derivatives for single valued optimizers. */
  void
  GetValueAndDerivative(const ParametersType & parameters,
                        MeasureType &          value,
                        DerivativeType &       derivative) const override;

  /** Number of bins to used in the histogram.
   * According to Mattes et al the optimum value is 50.
   * The minimum value is 5 due to the padding required by the Parzen
   * windowing with a cubic-BSpline kernel. Note that even if the metric
   * is used on binary images, the number of bins should at least be
   * equal to five. */
  itkSetClampMacro(NumberOfHistogramBins, SizeValueType, 5, NumericTraits<SizeValueType>::max());
  itkGetConstReferenceMacro(NumberOfHistogramBins, SizeValueType);

  /** This variable selects the method to be used for computing the Metric
   * derivatives with respect to the Transform parameters. Two modes of
   * computation are available. The choice between one and the other is a
   * trade-off between computation speed and memory allocations. The two modes
   * are described in detail below:
   *
   * UseExplicitPDFDerivatives = True
   * will compute the Metric derivative by first calculating the derivatives of
   * each one of the Joint PDF bins with respect to each one of the Transform
   * parameters and then accumulating these contributions in the final metric
   * derivative array by using a bin-specific weight.  The memory required for
   * storing the intermediate derivatives is a 3D array of floating point values with size
   * equals to the product of (number of histogram bins)^2 times number of
   * transform parameters. This method is well suited for Transform with a small
   * number of parameters.
   *
   * UseExplicitPDFDerivatives = False will compute the Metric derivative by
   * first computing the weights for each one of the Joint PDF bins and caching
   * them into an array. Then it will revisit each one of the PDF bins for
   * computing its weighted contribution to the full derivative array. In this
   * method an extra 2D array is used for storing the weights of each one of
   * the PDF bins. This is an array of floating point values with size equals to (number of
   * histogram bins)^2. This method is well suited for Transforms with a large
   * number of parameters, such as, BSplineTransforms. */
  itkSetMacro(UseExplicitPDFDerivatives, bool);
  itkGetConstReferenceMacro(UseExplicitPDFDerivatives, bool);
  itkBooleanMacro(UseExplicitPDFDerivatives);

  /** The marginal PDFs are stored as std::vector. */
  using PDFValueType = double; // NOTE:  floating point precision is not as stable.  Double precision proves faster and
                               // more robust in real-world testing.

  /** Typedef for the joint PDF and PDF derivatives are stored as ITK Images. */
  using JointPDFType = Image<PDFValueType, 2>;
  using JointPDFDerivativesType = Image<PDFValueType, 3>;

  /**
   * Get the internal JointPDF image that was used in
   * creating the metric value.
   */
  const typename JointPDFType::Pointer
  GetJointPDF() const
  {
    if (this->m_MMIMetricPerThreadVariables == nullptr)
    {
      return JointPDFType::Pointer(nullptr);
    }
    return this->m_MMIMetricPerThreadVariables[0].JointPDF;
  }

  /**
   * Get the internal JointPDFDeriviative image that was used in
   * creating the metric derivative value.
   * This is only created when UseExplicitPDFDerivatives is ON, and
   * derivatives are requested.
   */
  const typename JointPDFDerivativesType::Pointer
  GetJointPDFDerivatives() const
  {
    if (this->m_MMIMetricPerThreadVariables == nullptr)
    {
      return JointPDFDerivativesType::Pointer(nullptr);
    }
    return this->m_MMIMetricPerThreadVariables[0].JointPDFDerivatives;
  }

protected:
  MattesMutualInformationImageToImageMetric();
  ~MattesMutualInformationImageToImageMetric() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  using JointPDFIndexType = JointPDFType::IndexType;
  using JointPDFValueType = JointPDFType::PixelType;
  using JointPDFRegionType = JointPDFType::RegionType;
  using JointPDFSizeType = JointPDFType::SizeType;
  using JointPDFDerivativesIndexType = JointPDFDerivativesType::IndexType;
  using JointPDFDerivativesValueType = JointPDFDerivativesType::PixelType;
  using JointPDFDerivativesRegionType = JointPDFDerivativesType::RegionType;
  using JointPDFDerivativesSizeType = JointPDFDerivativesType::SizeType;

  /** Typedefs for BSpline kernel and derivative functions. */
  using CubicBSplineFunctionType = BSplineKernelFunction<3, PDFValueType>;
  using CubicBSplineDerivativeFunctionType = BSplineDerivativeKernelFunction<3, PDFValueType>;

  /** Precompute fixed image parzen window indices. */
  void
  ComputeFixedImageParzenWindowIndices(FixedImageSampleContainer & samples);

  /** Compute PDF derivative contribution for each parameter. */
  void
  ComputePDFDerivatives(ThreadIdType                 threadId,
                        unsigned int                 sampleNumber,
                        int                          pdfMovingIndex,
                        const ImageDerivativesType & movingImageGradientValue,
                        PDFValueType                 cubicBSplineDerivativeValue) const;

  void
  GetValueThreadPreProcess(ThreadIdType threadId, bool withinSampleThread) const override;
  void
  GetValueThreadPostProcess(ThreadIdType threadId, bool withinSampleThread) const override;
  // NOTE:  The signature in base class requires that movingImageValue is of type double
  bool
  GetValueThreadProcessSample(ThreadIdType                 threadId,
                              SizeValueType                fixedImageSample,
                              const MovingImagePointType & mappedPoint,
                              double                       movingImageValue) const override;

  void
  GetValueAndDerivativeThreadPreProcess(ThreadIdType threadId, bool withinSampleThread) const override;
  void
  GetValueAndDerivativeThreadPostProcess(ThreadIdType threadId, bool withinSampleThread) const override;
  // NOTE:  The signature in base class requires that movingImageValue is of type double
  bool
  GetValueAndDerivativeThreadProcessSample(ThreadIdType                 threadId,
                                           SizeValueType                fixedImageSample,
                                           const MovingImagePointType & mappedPoint,
                                           double                       movingImageValue,
                                           const ImageDerivativesType & movingImageGradientValue) const override;

  /** Variables to define the marginal and joint histograms. */
  SizeValueType m_NumberOfHistogramBins{ 50 };
  PDFValueType  m_MovingImageNormalizedMin{ 0.0 };
  PDFValueType  m_FixedImageNormalizedMin{ 0.0 };
  PDFValueType  m_FixedImageTrueMin{ 0.0 };
  PDFValueType  m_FixedImageTrueMax{ 0.0 };
  PDFValueType  m_MovingImageTrueMin{ 0.0 };
  PDFValueType  m_MovingImageTrueMax{ 0.0 };
  PDFValueType  m_FixedImageBinSize{ 0.0 };
  PDFValueType  m_MovingImageBinSize{ 0.0 };

  /** Cubic BSpline kernel for computing Parzen histograms. */
  typename CubicBSplineFunctionType::Pointer           m_CubicBSplineKernel;
  typename CubicBSplineDerivativeFunctionType::Pointer m_CubicBSplineDerivativeKernel;

  /** Helper array for storing the values of the JointPDF ratios. */
  using PRatioType = PDFValueType;
  using PRatioArrayType = Array2D<PRatioType>;

  mutable PRatioArrayType m_PRatioArray;

  /** The moving image marginal PDF. */
  using MarginalPDFType = std::vector<PDFValueType>;
  mutable MarginalPDFType m_MovingImageMarginalPDF;

  struct MMIMetricPerThreadStruct
  {
    int JointPDFStartBin;
    int JointPDFEndBin;

    PDFValueType JointPDFSum;

    /** Helper variable for accumulating the derivative of the metric. */
    DerivativeType MetricDerivative;

    /** The joint PDF and PDF derivatives. */
    typename JointPDFType::Pointer            JointPDF;
    typename JointPDFDerivativesType::Pointer JointPDFDerivatives;

    typename TransformType::JacobianType Jacobian;

    MarginalPDFType FixedImageMarginalPDF;
  };

#if !defined(ITK_WRAPPING_PARSER)
  itkPadStruct(ITK_CACHE_LINE_ALIGNMENT, MMIMetricPerThreadStruct, PaddedMMIMetricPerThreadStruct);
  itkAlignedTypedef(ITK_CACHE_LINE_ALIGNMENT, PaddedMMIMetricPerThreadStruct, AlignedMMIMetricPerThreadStruct);
  // Due to a bug in older version of Visual Studio where std::vector resize
  // uses a value instead of a const reference, this must be a pointer.
  // See
  //   http://thetweaker.wordpress.com/2010/05/05/stdvector-of-aligned-elements/
  //   http://connect.microsoft.com/VisualStudio/feedback/details/692988
  mutable AlignedMMIMetricPerThreadStruct * m_MMIMetricPerThreadVariables;
#endif

  bool         m_UseExplicitPDFDerivatives{ true };
  mutable bool m_ImplicitDerivativesSecondPass{ false };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMattesMutualInformationImageToImageMetric.hxx"
#endif

#endif

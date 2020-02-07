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
#ifndef itkMattesMutualInformationImageToImageMetricv4_h
#define itkMattesMutualInformationImageToImageMetricv4_h

#include "itkImageToImageMetricv4.h"
#include "itkMattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader.h"
#include "itkPoint.h"
#include "itkIndex.h"
#include "itkBSplineDerivativeKernelFunction.h"
#include "itkArray2D.h"
#include "itkThreadedIndexedContainerPartitioner.h"
#include <mutex>

namespace itk
{

/** \class MattesMutualInformationImageToImageMetricv4
 *
 * \brief Computes the mutual information between two images to be
 * registered using the method of Mattes et al.
 *
 * MattesMutualInformationImageToImageMetric computes the mutual
 * information between a fixed and moving image to be registered.
 *
 * This class is templated over the FixedImage type and the MovingImage
 * type.
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
 * \warning Local-support transforms are not yet supported. If used,
 * an exception is thrown during Initialize().
 *
 * \note The per-iteration post-processing code is not multi-threaded, but could be
 * readily be made so for a small performance gain.
 * See GetValueCommonAfterThreadedExecution(), GetValueAndDerivative()
 * and threader::AfterThreadedExecution().
 *
 * The algorithm and much of the code was copied from the previous
 * Mattes MI metric, i.e. itkMattesMutualInformationImageToImageMetric.
 *
 * See
 *  MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader::ProcessPoint
 *  for portions of the algorithm implementation.
 *
 * See ImageToImageMetricv4 for details of common metric operation and options.
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
 * \sa itkImageToImageMetricv4
 * \ingroup ITKMetricsv4
 */
template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage = TFixedImage,
          typename TInternalComputationValueType = double,
          typename TMetricTraits =
            DefaultImageToImageMetricTraitsv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType>>
class ITK_TEMPLATE_EXPORT MattesMutualInformationImageToImageMetricv4
  : public ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MattesMutualInformationImageToImageMetricv4);

  /** Standard class type aliases. */
  using Self = MattesMutualInformationImageToImageMetricv4;
  using Superclass =
    ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MattesMutualInformationImageToImageMetricv4, ImageToImageMetricv4);

  /** Superclass types */
  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using DerivativeValueType = typename DerivativeType::ValueType;

  using FixedImageType = typename Superclass::FixedImageType;
  using FixedImagePointType = typename Superclass::FixedImagePointType;
  using FixedImageIndexType = typename Superclass::FixedImageIndexType;
  using FixedImagePixelType = typename Superclass::FixedImagePixelType;
  using FixedImageGradientType = typename Superclass::FixedImageGradientType;

  using MovingImagePointType = typename Superclass::MovingImagePointType;
  using MovingImagePixelType = typename Superclass::MovingImagePixelType;
  using MovingImageGradientType = typename Superclass::MovingImageGradientType;

  using MovingTransformType = typename Superclass::MovingTransformType;
  using JacobianType = typename Superclass::JacobianType;
  using VirtualImageType = typename Superclass::VirtualImageType;
  using VirtualIndexType = typename Superclass::VirtualIndexType;
  using VirtualPointType = typename Superclass::VirtualPointType;
  using VirtualPointSetType = typename Superclass::VirtualPointSetType;

  /** Types inherited from Superclass. */
  using FixedSampledPointSetPointer = typename Superclass::FixedSampledPointSetPointer;

  /* Image dimension accessors */
  static constexpr typename TVirtualImage::ImageDimensionType VirtualImageDimension = TVirtualImage::ImageDimension;
  static constexpr typename TFixedImage::ImageDimensionType   FixedImageDimension = TFixedImage::ImageDimension;
  static constexpr typename TMovingImage::ImageDimensionType  MovingImageDimension = TMovingImage::ImageDimension;

  /** Number of bins to used in the histogram. Typical value is
   * 50. The minimum value is 5 due to the padding required by the Parzen
   * windowing with a cubic-BSpline kernel. Note that even if the metric
   * is used on binary images, the number of bins should at least be
   * equal to five. */
  itkSetClampMacro(NumberOfHistogramBins, SizeValueType, 5, NumericTraits<SizeValueType>::max());
  itkGetConstReferenceMacro(NumberOfHistogramBins, SizeValueType);

  void
  Initialize() override;

  /** The marginal PDFs are stored as std::vector. */
  // NOTE:  floating point precision is not as stable.
  // Double precision proves faster and more robust in real-world testing.
  using PDFValueType = TInternalComputationValueType;

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
    if (this->m_ThreaderJointPDF.empty())
    {
      return typename JointPDFType::Pointer(nullptr);
    }
    return this->m_ThreaderJointPDF[0];
  }

  /**
   * Get the internal JointPDFDeriviative image that was used in
   * creating the metric derivative value.
   * This is only created when a global support transform is used, and
   * derivatives are requested.
   */
  const typename JointPDFDerivativesType::Pointer
  GetJointPDFDerivatives() const
  {
    return this->m_JointPDFDerivatives;
  }

  void
  FinalizeThread(const ThreadIdType threadId) override;

protected:
  MattesMutualInformationImageToImageMetricv4();
  ~MattesMutualInformationImageToImageMetricv4() override = default;

  friend class MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader<
    ThreadedImageRegionPartitioner<Superclass::VirtualImageDimension>,
    Superclass,
    Self>;
  friend class MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader<
    ThreadedIndexedContainerPartitioner,
    Superclass,
    Self>;
  using MattesMutualInformationDenseGetValueAndDerivativeThreaderType =
    MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader<
      ThreadedImageRegionPartitioner<Superclass::VirtualImageDimension>,
      Superclass,
      Self>;
  using MattesMutualInformationSparseGetValueAndDerivativeThreaderType =
    MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader<ThreadedIndexedContainerPartitioner,
                                                                             Superclass,
                                                                             Self>;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  using JointPDFIndexType = typename JointPDFType::IndexType;
  using JointPDFValueType = typename JointPDFType::PixelType;
  using JointPDFRegionType = typename JointPDFType::RegionType;
  using JointPDFSizeType = typename JointPDFType::SizeType;
  using JointPDFDerivativesIndexType = typename JointPDFDerivativesType::IndexType;
  using JointPDFDerivativesValueType = typename JointPDFDerivativesType::PixelType;
  using JointPDFDerivativesRegionType = typename JointPDFDerivativesType::RegionType;
  using JointPDFDerivativesSizeType = typename JointPDFDerivativesType::SizeType;

  /** Typedefs for BSpline kernel and derivative functions. */
  using CubicBSplineFunctionType = BSplineKernelFunction<3, PDFValueType>;
  using CubicBSplineDerivativeFunctionType = BSplineDerivativeKernelFunction<3, PDFValueType>;

  /** Post-processing code common to both GetValue
   * and GetValueAndDerivative. */
  virtual void
  GetValueCommonAfterThreadedExecution();

  OffsetValueType
  ComputeSingleFixedImageParzenWindowIndex(const FixedImagePixelType & value) const;

  /** Variables to define the marginal and joint histograms. */
  SizeValueType m_NumberOfHistogramBins{ 50 };
  PDFValueType  m_MovingImageNormalizedMin;
  PDFValueType  m_FixedImageNormalizedMin;
  PDFValueType  m_FixedImageTrueMin;
  PDFValueType  m_FixedImageTrueMax;
  PDFValueType  m_MovingImageTrueMin;
  PDFValueType  m_MovingImageTrueMax;
  PDFValueType  m_FixedImageBinSize;
  PDFValueType  m_MovingImageBinSize;

  /** Cubic BSpline kernel for computing Parzen histograms. */
  typename CubicBSplineFunctionType::Pointer           m_CubicBSplineKernel;
  typename CubicBSplineDerivativeFunctionType::Pointer m_CubicBSplineDerivativeKernel;

  /** Helper array for storing the values of the JointPDF ratios. */
  using PRatioType = PDFValueType;
  using PRatioArrayType = std::vector<PRatioType>;

  mutable PRatioArrayType m_PRatioArray;

  /** Helper array for storing per-parameter linearized index to
   * retrieve the pRatio during evaluation with local-support transform. */
  mutable std::vector<OffsetValueType> m_JointPdfIndex1DArray;

  /** The moving image marginal PDF. */
  mutable std::vector<PDFValueType>              m_MovingImageMarginalPDF;
  mutable std::vector<std::vector<PDFValueType>> m_ThreaderFixedImageMarginalPDF;

  /** The joint PDF and PDF derivatives. */
  typename std::vector<typename JointPDFType::Pointer> m_ThreaderJointPDF;

  /* \class DerivativeBufferManager
   * A helper class to manage complexities of minimizing memory
   * needs for mattes mutual information derivative computations
   * per thread.
   *
   * Thread safety note:
   * A separate object is used locally per each thread. Only the members
   * m_ParentJointPDFDerivativesLockPtr and m_ParentJointPDFDerivatives
   * are shared between threads and access to m_ParentJointPDFDerivatives
   * is controlled with the m_ParentJointPDFDerivativesLockPtr mutex lock.
   * \ingroup ITKMetricsv4
   */
  class DerivativeBufferManager
  {
    using Self = DerivativeBufferManager;

  public:
    /* All these methods are thread safe except ReduceBuffer */

    void
    Initialize(size_t                                    maxBufferLength,
               const size_t                              cachedNumberOfLocalParameters,
               std::mutex *                              parentDerivativeLockPtr,
               typename JointPDFDerivativesType::Pointer parentJointPDFDerivatives);

    void
    DoubleBufferSize();

    DerivativeBufferManager()
      : m_MemoryBlock(0)
    {}

    ~DerivativeBufferManager() = default;

    size_t
    GetCachedNumberOfLocalParameters() const
    {
      return this->m_CachedNumberOfLocalParameters;
    }

    /**
     * Attempt to dump the buffer if it is full.
     * If the attempt to acquire the lock fails, double the buffer size and try again.
     */
    void
    CheckAndReduceIfNecessary();

    /**
     * Force the buffer to dump by blocking.
     */
    void
    BlockAndReduce();

    // If offset is same as previous offset, then accumulate with previous
    PDFValueType *
    GetNextElementAndAddOffset(const OffsetValueType & offset)
    {
      m_BufferOffsetContainer[m_CurrentFillSize] = offset;
      PDFValueType * PDFBufferForWriting = m_BufferPDFValuesContainer[m_CurrentFillSize];
      ++m_CurrentFillSize;
      return PDFBufferForWriting;
    }

    /**
     * Apply the operations stored in the buffer.
     * This method is not thread safe and requires a lock while threading.
     */
    void
    ReduceBuffer();

  private:
    // How many AccumlatorElements used
    size_t m_CurrentFillSize{ 0 };
    // Continguous chunk of memory for efficiency
    std::vector<PDFValueType> m_MemoryBlock;
    // The (number of lines in the buffer) * (cells per line)
    size_t                       m_MemoryBlockSize;
    std::vector<PDFValueType *>  m_BufferPDFValuesContainer;
    std::vector<OffsetValueType> m_BufferOffsetContainer;
    size_t                       m_CachedNumberOfLocalParameters;
    size_t                       m_MaxBufferSize;
    // Pointer handle to parent version
    std::mutex * m_ParentJointPDFDerivativesLockPtr;
    // Smart pointer handle to parent version
    typename JointPDFDerivativesType::Pointer m_ParentJointPDFDerivatives;
  };

  std::vector<DerivativeBufferManager>      m_ThreaderDerivativeManager;
  std::mutex                                m_JointPDFDerivativesLock;
  typename JointPDFDerivativesType::Pointer m_JointPDFDerivatives;

  PDFValueType m_JointPDFSum;

  /** Store the per-point local derivative result by parzen window bin.
   * For local-support transforms only. */
  mutable std::vector<DerivativeType> m_LocalDerivativeByParzenBin;

private:
  /** Perform the final step in computing results */
  virtual void
  ComputeResults() const;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMattesMutualInformationImageToImageMetricv4.hxx"
#endif

#endif

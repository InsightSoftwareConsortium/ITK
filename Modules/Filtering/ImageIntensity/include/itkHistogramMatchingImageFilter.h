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
#ifndef itkHistogramMatchingImageFilter_h
#define itkHistogramMatchingImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkHistogram.h"
#include "vnl/vnl_matrix.h"

namespace itk
{
/** \class HistogramMatchingImageFilter
 * \brief Normalize the grayscale values between two images by histogram
 * matching.
 *
 * HistogramMatchingImageFilter normalizes the grayscale values of a source
 * image based on the grayscale values of a reference image.
 * This filter uses a histogram matching technique where the histograms of the
 * two images are matched only at a specified number of quantile values.
 *
 * This filter was originally designed to normalize MR images of the same
 * MR protocol and same body part. The algorithm works best if background
 * pixels are excluded from both the source and reference histograms.
 * A simple background exclusion method is to exclude all pixels whose
 * grayscale values are smaller than the mean grayscale value.
 * ThresholdAtMeanIntensityOn() switches on this simple background
 * exclusion method.
 *
 * The source image can be set via either SetInput() or SetSourceImage().
 * The reference image can be set via SetReferenceImage().
 *
 * SetNumberOfHistogramLevels() sets the number of bins used when
 * creating histograms of the source and reference images.
 * SetNumberOfMatchPoints() governs the number of quantile values to be
 * matched.
 *
 * This filter assumes that both the source and reference are of the same
 * type and that the input and output image type have the same number of
 * dimension and have scalar pixel types.
 *
 * \par REFERENCE
 * Laszlo G. Nyul, Jayaram K. Udupa, and Xuan Zhang, "New Variants of a Method
 * of MRI Scale Standardization", IEEE Transactions on Medical Imaging,
 * 19(2):143-150, 2000.
 *
 * \ingroup IntensityImageFilters MultiThreaded
 *
 * \ingroup ITKImageIntensity
 */
/* THistogramMeasurement -- The precision level for which to do
  HistogramMeasurmenets */
template< typename TInputImage, typename TOutputImage, typename THistogramMeasurement = typename TInputImage::PixelType >
class ITK_TEMPLATE_EXPORT HistogramMatchingImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(HistogramMatchingImageFilter);

  /** Standard class type aliases. */
  using Self = HistogramMatchingImageFilter;
  using Superclass = ImageToImageFilter< TInputImage, TOutputImage >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(HistogramMatchingImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Inherited type alias. */
  using InputImageType = typename Superclass::InputImageType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputImageConstPointer = typename Superclass::InputImageConstPointer;
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImagePointer = typename Superclass::OutputImagePointer;

  /** Pixel related type alias. */
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;

  /** Histogram related type alias. */
  using HistogramType = Statistics::Histogram< THistogramMeasurement >;
  using HistogramPointer = typename HistogramType::Pointer;

  /** Set/Get the source image. */
  void SetSourceImage(const InputImageType *source)
  { this->SetInput(source); }
  const InputImageType * GetSourceImage()
  { return this->GetInput(); }

  /** Set/Get the reference image. */
  void SetReferenceImage(const InputImageType *reference);

  const InputImageType * GetReferenceImage();

  /** Set/Get the number of histogram levels used. */
  itkSetMacro(NumberOfHistogramLevels, SizeValueType);
  itkGetConstMacro(NumberOfHistogramLevels, SizeValueType);

  /** Set/Get the number of match points used. */
  itkSetMacro(NumberOfMatchPoints, SizeValueType);
  itkGetConstMacro(NumberOfMatchPoints, SizeValueType);

  /** Set/Get the threshold at mean intensity flag.
   * If true, only source (reference) pixels which are greater
   * than the mean source (reference) intensity is used in
   * the histogram matching. If false, all pixels are
   * used. */
  itkSetMacro(ThresholdAtMeanIntensity, bool);
  itkGetConstMacro(ThresholdAtMeanIntensity, bool);
  itkBooleanMacro(ThresholdAtMeanIntensity);

  /** This filter requires all of the input to be in the buffer. */
  void GenerateInputRequestedRegion() override;

  /** Methods to get the histograms of the source, reference, and
   * output. Objects are only valid after Update() has been called
   * on this filter. */
  itkGetModifiableObjectMacro(SourceHistogram, HistogramType);
  itkGetModifiableObjectMacro(ReferenceHistogram, HistogramType);
  itkGetModifiableObjectMacro(OutputHistogram, HistogramType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( IntConvertibleToInputCheck,
                   ( Concept::Convertible< int, InputPixelType > ) );
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< ImageDimension, OutputImageDimension > ) );
  itkConceptMacro( DoubleConvertibleToInputCheck,
                   ( Concept::Convertible< double, InputPixelType > ) );
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, OutputPixelType > ) );
  itkConceptMacro( InputConvertibleToDoubleCheck,
                   ( Concept::Convertible< InputPixelType, double > ) );
  itkConceptMacro( OutputConvertibleToDoubleCheck,
                   ( Concept::Convertible< OutputPixelType, double > ) );
  itkConceptMacro( SameTypeCheck,
                   ( Concept::SameType< InputPixelType, OutputPixelType > ) );
  // End concept checking
#endif

protected:
  HistogramMatchingImageFilter();
  ~HistogramMatchingImageFilter() override = default;
  void PrintSelf(std::ostream & os, Indent indent) const override;

  void BeforeThreadedGenerateData() override;

  void AfterThreadedGenerateData() override;

  void DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  /** Override VeriyInputInformation() since this filter does not expect
   * the input images to occupy the same physical space.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  void VerifyInputInformation() ITKv5_CONST override {}

  /** Compute min, max and mean of an image. */
  void ComputeMinMaxMean(const InputImageType *image,
                         THistogramMeasurement & minValue,
                         THistogramMeasurement & maxValue,
                         THistogramMeasurement & meanValue);

  /** Construct a histogram from an image. */
  void ConstructHistogram(const InputImageType *image,
                          HistogramType *histogram, const THistogramMeasurement minValue,
                          const THistogramMeasurement maxValue);

private:
  SizeValueType m_NumberOfHistogramLevels{256};
  SizeValueType m_NumberOfMatchPoints{1};
  bool          m_ThresholdAtMeanIntensity{true};

  InputPixelType  m_SourceIntensityThreshold;
  InputPixelType  m_ReferenceIntensityThreshold;
  OutputPixelType m_OutputIntensityThreshold;

  THistogramMeasurement m_SourceMinValue;
  THistogramMeasurement m_SourceMaxValue;
  THistogramMeasurement m_SourceMeanValue;
  THistogramMeasurement m_ReferenceMinValue;
  THistogramMeasurement m_ReferenceMaxValue;
  THistogramMeasurement m_ReferenceMeanValue;
  THistogramMeasurement m_OutputMinValue;
  THistogramMeasurement m_OutputMaxValue;
  THistogramMeasurement m_OutputMeanValue;

  HistogramPointer m_SourceHistogram;
  HistogramPointer m_ReferenceHistogram;
  HistogramPointer m_OutputHistogram;

  using TableType = vnl_matrix< double >;
  TableType m_QuantileTable;

  using GradientArrayType = vnl_vector< double >;
  GradientArrayType m_Gradients;
  double            m_LowerGradient{0.0};
  double            m_UpperGradient{0.0};
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHistogramMatchingImageFilter.hxx"
#endif

#endif

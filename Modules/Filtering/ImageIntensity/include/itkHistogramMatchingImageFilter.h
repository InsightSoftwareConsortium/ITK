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
#ifndef itkHistogramMatchingImageFilter_h
#define itkHistogramMatchingImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkHistogram.h"
#include "vnl/vnl_matrix.h"

namespace itk
{
/** \class HistogramMatchingImageFilter
 * \brief Normalize the grayscale values for a source image by
 * matching the shape of the source image histogram to a
 * reference histogram.
 *
 * HistogramMatchingImageFilter normalizes the grayscale values of a source
 * image based on the grayscale values of either a reference image or a
 * reference histogram.
 * This filter uses a histogram matching technique where the histograms of the
 * are matched only at a specified number of quantile values.
 *
 * This filter was originally designed to normalize MR images of the same
 * MR protocol and same body part. The algorithm works best if background
 * pixels are excluded from both the source and reference histograms.
 * A simple background exclusion method is to exclude all pixels whose
 * grayscale values are smaller than the mean grayscale value.
 * ThresholdAtMeanIntensityOn() switches on this simple background
 * exclusion method.  With ThresholdAtMeanIntensityOn(), The reference
 * histogram returned from this filter will expand the first and last
 * bin bounds to include the minimum and maximum intensity values of
 * the entire reference image, but only intensity values greater than
 * the mean will be used to populate the histogram.
 *
 * The source image can be set via either SetInput() or SetSourceImage().
 * The reference object used is selected with
 * can be set via SetReferenceImage() or SetReferenceHistogram().
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
template <typename TInputImage, typename TOutputImage, typename THistogramMeasurement = typename TInputImage::PixelType>
class ITK_TEMPLATE_EXPORT HistogramMatchingImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HistogramMatchingImageFilter);

  /** Standard class type aliases. */
  using Self = HistogramMatchingImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

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
  using HistogramType = Statistics::Histogram<THistogramMeasurement>;
  using HistogramPointer = typename HistogramType::Pointer;

  /** Set and Get the source image */
  itkSetInputMacro(SourceImage, InputImageType);
  itkGetInputMacro(SourceImage, InputImageType);

  /** Set/Get the reference image. */
  itkSetInputMacro(ReferenceImage, InputImageType);
  itkGetInputMacro(ReferenceImage, InputImageType);

  /** Set the reference histogram.  The reference histogram must
   * have the first bin minimum be the smallest intensity value for the
   * reference image space and the last bin maximum must contain the largest
   * intensity value for the reference image space.
   * (Note that the ThresholdAtMeanIntensity may restrict the voxels
   * that are used to populate the histogram to a smaller intensity range
   * than is represented by the smallest and largest intensity values.)
   */
  itkSetInputMacro(ReferenceHistogram, HistogramType);
  itkGetInputMacro(ReferenceHistogram, HistogramType);

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

  /** Set/Get if the reference histogram is regenerated from
   * the supplied ReferenceImage (true) or supplied directly
   * as in input argument (false).  If SetReferenceHistogram(myhistogram)
   * is used, then GenerateReferenceHistogramFromImageOff() should almost
   * certainly be used.  If both SetReferenceHistogram(myhistogram) and
   * SetReferenceImage(myreferenceimage) are set, only the input object
   * indicated by GenerateReferenceHistogramFromImage choice will be used
   * and the other object will be ignored.
   */
  itkSetMacro(GenerateReferenceHistogramFromImage, bool);
  itkGetConstMacro(GenerateReferenceHistogramFromImage, bool);
  itkBooleanMacro(GenerateReferenceHistogramFromImage);

  /** This filter requires all of the input to be in the buffer. */
  void
  GenerateInputRequestedRegion() override;

  /** Methods to get the histograms of the source, reference, and
   * output. Objects are only valid after Update() has been called
   * on this filter. */
  itkGetModifiableObjectMacro(SourceHistogram, HistogramType);
  itkGetModifiableObjectMacro(OutputHistogram, HistogramType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(IntConvertibleToInputCheck, (Concept::Convertible<int, InputPixelType>));
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(DoubleConvertibleToInputCheck, (Concept::Convertible<double, InputPixelType>));
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, OutputPixelType>));
  itkConceptMacro(InputConvertibleToDoubleCheck, (Concept::Convertible<InputPixelType, double>));
  itkConceptMacro(OutputConvertibleToDoubleCheck, (Concept::Convertible<OutputPixelType, double>));
  itkConceptMacro(SameTypeCheck, (Concept::SameType<InputPixelType, OutputPixelType>));
  // End concept checking
#endif

protected:
  HistogramMatchingImageFilter();
  ~HistogramMatchingImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  BeforeThreadedGenerateData() override;

  void
  AfterThreadedGenerateData() override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  /** Override VerifyInputInformation() since this filter does not expect
   * the input images to occupy the same physical space.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  void
  VerifyInputInformation() ITKv5_CONST override
  {}

  void
  VerifyPreconditions() ITKv5_CONST override;

  /** Compute min, max and mean of an image. */
  void
  ComputeMinMaxMean(const InputImageType *  image,
                    THistogramMeasurement & minValue,
                    THistogramMeasurement & maxValue,
                    THistogramMeasurement & meanValue);

  /**
   * Construct a histogram from an image using only values in range of [minValue, maxValue].
   * Values outside that range are ignored.
   */
  void
  ConstructHistogramFromIntensityRange(const InputImageType *      image,
                                       HistogramType *             histogram,
                                       const THistogramMeasurement minHistogramValidValue,
                                       const THistogramMeasurement maxHistogramValidValue,
                                       const THistogramMeasurement imageTrueMinValue,
                                       const THistogramMeasurement imageTrueMaxValue);

private:
  SizeValueType m_NumberOfHistogramLevels{ 256 };
  SizeValueType m_NumberOfMatchPoints{ 1 };
  bool          m_ThresholdAtMeanIntensity{ true };

  THistogramMeasurement m_SourceMinValue;
  THistogramMeasurement m_SourceMaxValue;

  THistogramMeasurement m_ReferenceMinValue;
  THistogramMeasurement m_ReferenceMaxValue;

  HistogramPointer m_SourceHistogram;
  HistogramPointer m_OutputHistogram;

  using TableType = vnl_matrix<double>;
  TableType m_QuantileTable;

  using GradientArrayType = vnl_vector<double>;
  GradientArrayType m_Gradients;
  double            m_LowerGradient{ 0.0 };
  double            m_UpperGradient{ 0.0 };
  bool              m_GenerateReferenceHistogramFromImage{ true };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHistogramMatchingImageFilter.hxx"
#endif

#endif

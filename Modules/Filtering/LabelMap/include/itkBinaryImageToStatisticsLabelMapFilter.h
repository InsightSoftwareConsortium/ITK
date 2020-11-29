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
#ifndef itkBinaryImageToStatisticsLabelMapFilter_h
#define itkBinaryImageToStatisticsLabelMapFilter_h

#include "itkStatisticsLabelObject.h"
#include "itkBinaryImageToLabelMapFilter.h"
#include "itkStatisticsLabelMapFilter.h"

namespace itk
{
/**
 *\class BinaryImageToStatisticsLabelMapFilter
 * \brief a convenient class to convert a binary image to a label map and valuate the statistics attributes at once
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \sa StatisticsLabelObject, LabelStatisticsOpeningImageFilter, BinaryStatisticsOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TInputImage,
          typename TFeatureImage,
          typename TOutputImage = LabelMap<StatisticsLabelObject<SizeValueType, TInputImage::ImageDimension>>>
class ITK_TEMPLATE_EXPORT BinaryImageToStatisticsLabelMapFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryImageToStatisticsLabelMapFilter);

  /** Standard class type aliases. */
  using Self = BinaryImageToStatisticsLabelMapFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using LabelObjectType = typename OutputImageType::LabelObjectType;

  using FeatureImageType = TFeatureImage;
  using FeatureImagePointer = typename FeatureImageType::Pointer;
  using FeatureImageConstPointer = typename FeatureImageType::ConstPointer;
  using FeatureImagePixelType = typename FeatureImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using LabelizerType = BinaryImageToLabelMapFilter<InputImageType, OutputImageType>;
  using LabelObjectValuatorType = StatisticsLabelMapFilter<OutputImageType, FeatureImageType>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryImageToStatisticsLabelMapFilter, ImageToImageFilter);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputEqualityComparableCheck, (Concept::EqualityComparable<InputImagePixelType>));
  itkConceptMacro(IntConvertibleToInputCheck, (Concept::Convertible<int, InputImagePixelType>));
  itkConceptMacro(InputOStreamWritableCheck, (Concept::OStreamWritable<InputImagePixelType>));
  // End concept checking
#endif

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<PixelType>::NonpositiveMin().
   */
  itkSetMacro(OutputBackgroundValue, OutputImagePixelType);
  itkGetConstMacro(OutputBackgroundValue, OutputImagePixelType);

  /**
   * Set/Get the value used as "foreground" in the output image.
   * Defaults to NumericTraits<PixelType>::max().
   */
  itkSetMacro(InputForegroundValue, InputImagePixelType);
  itkGetConstMacro(InputForegroundValue, InputImagePixelType);

  /**
   * Set/Get whether the maximum Feret diameter should be computed or not. The
   * defaut value is false, because of the high computation time required.
   */
  itkSetMacro(ComputeFeretDiameter, bool);
  itkGetConstReferenceMacro(ComputeFeretDiameter, bool);
  itkBooleanMacro(ComputeFeretDiameter);

  /**
   * Set/Get whether the perimeter should be computed or not. The defaut value
   * is false, because of the high computation time required.
   */
  itkSetMacro(ComputePerimeter, bool);
  itkGetConstReferenceMacro(ComputePerimeter, bool);
  itkBooleanMacro(ComputePerimeter);

  /** Set the feature image */
  void
  SetFeatureImage(const TFeatureImage * input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput(1, const_cast<TFeatureImage *>(input));
  }

  /** Get the feature image */
  const FeatureImageType *
  GetFeatureImage()
  {
    return static_cast<const FeatureImageType *>(this->ProcessObject::GetInput(1));
  }

  /** Set the input image */
  void
  SetInput1(const InputImageType * input)
  {
    this->SetInput(input);
  }

  /** Set the feature image */
  void
  SetInput2(const FeatureImageType * input)
  {
    this->SetFeatureImage(input);
  }

  /**
   * Set/Get whether the histogram should be attached to the label object or not.
   * This option defaults to `true`, but because the histogram may take a lot of memory
   * compared to the other attributes, this option is useful to reduce the memory usage
   * when the histogram is not required.
   */
  itkSetMacro(ComputeHistogram, bool);
  itkGetConstReferenceMacro(ComputeHistogram, bool);
  itkBooleanMacro(ComputeHistogram);

  /**
   * Set/Get the number of bins in the histogram. Note that the histogram is used
   * to compute the median value, and that this option may have an effect on the
   * value of the median.
   */
  itkSetMacro(NumberOfBins, unsigned int);
  itkGetConstReferenceMacro(NumberOfBins, unsigned int);

protected:
  BinaryImageToStatisticsLabelMapFilter();
  ~BinaryImageToStatisticsLabelMapFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** BinaryImageToStatisticsLabelMapFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** BinaryImageToStatisticsLabelMapFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicErodeImageFilter. */
  void
  GenerateData() override;

private:
  bool                 m_FullyConnected;
  OutputImagePixelType m_OutputBackgroundValue;
  InputImagePixelType  m_InputForegroundValue;
  bool                 m_ComputeFeretDiameter;
  bool                 m_ComputePerimeter;
  unsigned int         m_NumberOfBins;
  bool                 m_ComputeHistogram;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryImageToStatisticsLabelMapFilter.hxx"
#endif

#endif

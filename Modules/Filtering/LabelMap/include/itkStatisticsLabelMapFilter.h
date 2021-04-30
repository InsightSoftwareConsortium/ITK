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
#ifndef itkStatisticsLabelMapFilter_h
#define itkStatisticsLabelMapFilter_h

#include "itkShapeLabelMapFilter.h"

namespace itk
{
/**
 *\class StatisticsLabelMapFilter
 * \brief The valuator class for the StatisticsLabelObject
 *
 * StatisticsLabelMapFilter can be used to set the attributes values
 * of the StatisticsLabelObject in a LabelMap.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TImage, typename TFeatureImage>
class ITK_TEMPLATE_EXPORT StatisticsLabelMapFilter
  : public ShapeLabelMapFilter<TImage, Image<typename TImage::PixelType, TImage::ImageDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(StatisticsLabelMapFilter);

  /** Standard class type aliases. */
  using Self = StatisticsLabelMapFilter;
  using Superclass = ShapeLabelMapFilter<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using ImageType = TImage;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using PixelType = typename ImageType::PixelType;
  using IndexType = typename ImageType::IndexType;
  using PointType = typename ImageType::PointType;
  using LabelObjectType = typename ImageType::LabelObjectType;
  using MatrixType = typename LabelObjectType::MatrixType;
  using VectorType = typename LabelObjectType::VectorType;

  using FeatureImageType = TFeatureImage;
  using FeatureImagePointer = typename FeatureImageType::Pointer;
  using FeatureImageConstPointer = typename FeatureImageType::ConstPointer;
  using FeatureImagePixelType = typename FeatureImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(StatisticsLabelMapFilter, ShapeLabelMapFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
/*  itkConceptMacro(InputEqualityComparableCheck,
    (Concept::EqualityComparable<InputImagePixelType>));
  itkConceptMacro(IntConvertibleToInputCheck,
    (Concept::Convertible<int, InputImagePixelType>));
  itkConceptMacro(InputOStreamWritableCheck,
    (Concept::OStreamWritable<InputImagePixelType>));*/
// End concept checking
#endif

  /** Set the feature image */
  void
  SetFeatureImage(const TFeatureImage * input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput(1, const_cast<TFeatureImage *>(input));
  }

  /** Get the feature image */
  FeatureImageType *
  GetFeatureImage()
  {
    return static_cast<FeatureImageType *>(const_cast<DataObject *>(this->ProcessObject::GetInput(1)));
  }

  /** Set the input image */
  void
  SetInput1(TImage * input)
  {
    this->SetInput(input);
  }

  /** Set the feature image */
  void
  SetInput2(const TFeatureImage * input)
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

  // Set the default number of bins to match the number of values for 8 or 16-bit integers; otherwise 128
  static constexpr unsigned int
  GetDefaultNumberOfBins()
  {
    return NumericTraits<FeatureImagePixelType>::IsInteger && sizeof(FeatureImagePixelType) <= 2
             ? 1 << (8 * sizeof(FeatureImagePixelType))
             : 128;
  }

protected:
  StatisticsLabelMapFilter();
  ~StatisticsLabelMapFilter() override = default;

  void
  ThreadedProcessLabelObject(LabelObjectType * labelObject) override;

  void
  BeforeThreadedGenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  FeatureImagePixelType m_Minimum{ NumericTraits<FeatureImagePixelType>::ZeroValue() };
  FeatureImagePixelType m_Maximum{ NumericTraits<FeatureImagePixelType>::ZeroValue() };
  unsigned int          m_NumberOfBins{ GetDefaultNumberOfBins() };
  bool                  m_ComputeHistogram{ true };
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStatisticsLabelMapFilter.hxx"
#endif

#endif

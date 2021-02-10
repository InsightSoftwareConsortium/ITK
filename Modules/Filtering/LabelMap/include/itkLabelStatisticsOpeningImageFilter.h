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
#ifndef itkLabelStatisticsOpeningImageFilter_h
#define itkLabelStatisticsOpeningImageFilter_h

#include "itkLabelImageToLabelMapFilter.h"
#include "itkStatisticsLabelMapFilter.h"
#include "itkStatisticsOpeningLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

namespace itk
{
/**
 *\class LabelStatisticsOpeningImageFilter
 * \brief remove the objects according to the value of their statistics attribute
 *
 * LabelStatisticsOpeningImageFilter removes the objects in a labeled image
 * with an attribute value smaller or greater than a threshold called Lambda.
 * The attributes are the ones of the StatisticsLabelObject.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \sa StatisticsLabelObject, BinaryStatisticsOpeningImageFilter, LabelShapeOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TInputImage, typename TFeatureImage>
class ITK_TEMPLATE_EXPORT LabelStatisticsOpeningImageFilter : public ImageToImageFilter<TInputImage, TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelStatisticsOpeningImageFilter);

  /** Standard class type aliases. */
  using Self = LabelStatisticsOpeningImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  using FeatureImageType = TFeatureImage;
  using FeatureImagePointer = typename FeatureImageType::Pointer;
  using FeatureImageConstPointer = typename FeatureImageType::ConstPointer;
  using FeatureImagePixelType = typename FeatureImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using LabelObjectType = StatisticsLabelObject<InputImagePixelType, Self::ImageDimension>;
  using LabelMapType = LabelMap<LabelObjectType>;
  using LabelizerType = LabelImageToLabelMapFilter<InputImageType, LabelMapType>;
  using LabelObjectValuatorType = StatisticsLabelMapFilter<LabelMapType, TFeatureImage>;
  using AttributeType = typename LabelObjectType::AttributeType;
  using OpeningType = StatisticsOpeningLabelMapFilter<LabelMapType>;
  using BinarizerType = LabelMapToLabelImageFilter<LabelMapType, OutputImageType>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelStatisticsOpeningImageFilter, ImageToImageFilter);

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
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

  /**
   * Set/Get the threshold used to keep or remove the objects.
   */
  itkGetConstMacro(Lambda, double);
  itkSetMacro(Lambda, double);

  /**
   * Set/Get the ordering of the objects. By default, the objects with
   * an attribute value smaller than Lamba are removed. Turning ReverseOrdering
   * to true make this filter remove the object with an attribute value greater
   * than Lambda instead.
   */
  itkGetConstMacro(ReverseOrdering, bool);
  itkSetMacro(ReverseOrdering, bool);
  itkBooleanMacro(ReverseOrdering);

  /**
   * Set/Get the attribute to use to select the object to remove. The default
   * is "Mean".
   */
  itkGetConstMacro(Attribute, AttributeType);
  itkSetMacro(Attribute, AttributeType);
  void
  SetAttribute(const std::string & s)
  {
    this->SetAttribute(LabelObjectType::GetAttributeFromName(s));
  }

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

protected:
  LabelStatisticsOpeningImageFilter();
  ~LabelStatisticsOpeningImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** LabelStatisticsOpeningImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** LabelStatisticsOpeningImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicErodeImageFilter. */
  void
  GenerateData() override;

private:
  OutputImagePixelType m_BackgroundValue;
  double               m_Lambda;
  bool                 m_ReverseOrdering;
  AttributeType        m_Attribute;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelStatisticsOpeningImageFilter.hxx"
#endif

#endif

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
#ifndef itkLabelShapeKeepNObjectsImageFilter_h
#define itkLabelShapeKeepNObjectsImageFilter_h

#include "itkShapeLabelObject.h"
#include "itkLabelImageToLabelMapFilter.h"
#include "itkShapeLabelMapFilter.h"
#include "itkShapeKeepNObjectsLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

namespace itk
{
/**
 *\class LabelShapeKeepNObjectsImageFilter
 * \brief keep N objects according to their shape attributes
 *
 * LabelShapeKeepNObjectsImageFilter keep the N objects in a labeled image
 * with the highest (or lowest) attribute value. The attributes are the ones
 * of the ShapeLabelObject.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \sa ShapeLabelObject, BinaryShapeKeepNObjectsImageFilter, LabelStatisticsKeepNObjectsImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 *
 * \sphinx
 * \sphinxexample{Filtering/LabelMap/KeepRegionsAboveLevel,Keep Regions Above Certain Level}
 * \endsphinx
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT LabelShapeKeepNObjectsImageFilter : public ImageToImageFilter<TInputImage, TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelShapeKeepNObjectsImageFilter);

  /** Standard class type aliases. */
  using Self = LabelShapeKeepNObjectsImageFilter;
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

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using LabelObjectType = ShapeLabelObject<InputImagePixelType, Self::ImageDimension>;
  using LabelMapType = LabelMap<LabelObjectType>;
  using LabelizerType = LabelImageToLabelMapFilter<InputImageType, LabelMapType>;
  using ShapeLabelFilterOutput = Image<typename OutputImageType::PixelType, Self::OutputImageDimension>;
  using LabelObjectValuatorType = ShapeLabelMapFilter<LabelMapType, ShapeLabelFilterOutput>;
  using AttributeType = typename LabelObjectType::AttributeType;
  using KeepNObjectsType = ShapeKeepNObjectsLabelMapFilter<LabelMapType>;
  using BinarizerType = LabelMapToLabelImageFilter<LabelMapType, OutputImageType>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelShapeKeepNObjectsImageFilter, ImageToImageFilter);

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
   * Set/Get the number of objects to keep
   */
  itkGetConstMacro(NumberOfObjects, SizeValueType);
  itkSetMacro(NumberOfObjects, SizeValueType);

  /**
   * Set/Get the ordering of the objects. By default, the ones with the
   * highest value are kept. Turming ReverseOrdering to true make this filter
   * keep the objects with the smallest values
   */
  itkGetConstMacro(ReverseOrdering, bool);
  itkSetMacro(ReverseOrdering, bool);
  itkBooleanMacro(ReverseOrdering);

  /**
   * Set/Get the attribute to use to select the object to keep. The default
   * is "Size".
   */
  itkGetConstMacro(Attribute, AttributeType);
  itkSetMacro(Attribute, AttributeType);
  void
  SetAttribute(const std::string & s)
  {
    this->SetAttribute(LabelObjectType::GetAttributeFromName(s));
  }

protected:
  LabelShapeKeepNObjectsImageFilter();
  ~LabelShapeKeepNObjectsImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** LabelShapeKeepNObjectsImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** LabelShapeKeepNObjectsImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicErodeImageFilter. */
  void
  GenerateData() override;

private:
  OutputImagePixelType m_BackgroundValue;
  SizeValueType        m_NumberOfObjects;
  bool                 m_ReverseOrdering;
  AttributeType        m_Attribute;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelShapeKeepNObjectsImageFilter.hxx"
#endif

#endif

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
#ifndef itkMergeLabelMapFilter_h
#define itkMergeLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "ITKLabelMapExport.h"

namespace itk
{
// See https://docs.microsoft.com/en-us/cpp/error-messages/compiler-errors-1/compiler-error-c2059?view=vs-2019
#ifdef STRICT
#  define TEMPINPLACELABELMAPSTRICT STRICT
#  undef STRICT
#endif
/**\class MergeLabelMapFilterEnums
 * \brief Contains all enum classes used by MergeLabelMapFilter class.
 * \ingroup ITKLabelMap
 */
class MergeLabelMapFilterEnums
{
public:
  /**\class ChoiceMethod
   * \ingroup ITKLabelMap
   */
  enum class ChoiceMethod : uint8_t
  {
    KEEP = 0,
    AGGREGATE = 1,
    PACK = 2,
    STRICT = 3
  };
};
// Define how to print enumeration
extern ITKLabelMap_EXPORT std::ostream &
                          operator<<(std::ostream & out, const MergeLabelMapFilterEnums::ChoiceMethod value);

using ChoiceMethodEnum = MergeLabelMapFilterEnums::ChoiceMethod;
#if !defined(ITK_LEGACY_REMOVE)
/** Enables backwards compatibility for enum values */
using ChoiceMethod = ChoiceMethodEnum;
// We need to expose the enum values at the class level
// for backwards compatibility
static constexpr ChoiceMethodEnum KEEP = ChoiceMethodEnum::KEEP;
static constexpr ChoiceMethodEnum AGGREGATE = ChoiceMethodEnum::AGGREGATE;
static constexpr ChoiceMethodEnum PACK = ChoiceMethodEnum::PACK;
static constexpr ChoiceMethodEnum STRICT = ChoiceMethodEnum::STRICT;
#endif

#ifdef TEMPINPLACELABELMAPSTRICT
#  define STRICT TEMPINPLACELABELMAPSTRICT
#  undef TEMPINPLACELABELMAPSTRICT
#endif
/**
 *\class MergeLabelMapFilter
 * \brief Merges several Label Maps
 *
 * This filter takes one or more input Label Map and merges them.
 *
 * SetMethod() can be used to change how the filter manage the
 * labels from the different label maps.
 *   KEEP (0): MergeLabelMapFilter do its best to keep the label unchanged,
 *             but if a label is already used in a previous label map, a new
 *             label is assigned.
 *   AGGREGATE (1): If the same label is found several times in the label maps,
 *                  the label objects with the same label are merged.
 *   PACK (2): MergeLabelMapFilter relabel all the label objects by order of
 *             processing. No conflict can occur.
 *   STRICT (3): MergeLabelMapFilter keeps the labels unchanged and raises an
 *               exception if the same label is found in several images.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ShapeLabelObject, RelabelComponentImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 *
 * \sphinx
 * \sphinxexample{Filtering/LabelMap/MergeLabelMaps,Merge LabelMaps}
 * \endsphinx
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT MergeLabelMapFilter : public InPlaceLabelMapFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MergeLabelMapFilter);

  /** Standard class type aliases. */
  using Self = MergeLabelMapFilter;
  using Superclass = InPlaceLabelMapFilter<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using ImageType = TImage;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using PixelType = typename ImageType::PixelType;
  using IndexType = typename ImageType::IndexType;
  using LabelObjectType = typename ImageType::LabelObjectType;
  using LabelObjectPointer = typename LabelObjectType::Pointer;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MergeLabelMapFilter, InPlaceLabelMapFilter);

#if !defined(ITK_LEGACY_REMOVE)
  /** Enables backwards compatibility for enum values */
  using MethodChoice = ChoiceMethodEnum;
#endif

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

#ifdef STRICT
#  undef STRICT
#endif

  /** Set/Get the method used to merge the label maps */
  itkSetMacro(Method, ChoiceMethodEnum);
  itkGetConstReferenceMacro(Method, ChoiceMethodEnum);

protected:
  MergeLabelMapFilter();
  ~MergeLabelMapFilter() override = default;

  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  ChoiceMethodEnum m_Method;

private:
  void
  MergeWithKeep();

  void
  MergeWithAggregate();

  void
  MergeWithPack();

  void
  MergeWithStrict();
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMergeLabelMapFilter.hxx"
#endif

#endif

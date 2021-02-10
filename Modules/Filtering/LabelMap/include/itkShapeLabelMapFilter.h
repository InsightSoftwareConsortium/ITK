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
#ifndef itkShapeLabelMapFilter_h
#define itkShapeLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkLexicographicCompare.h"

namespace itk
{
/**
 *\class ShapeLabelMapFilter
 * \brief The valuator class for the ShapeLabelObject
 *
 * ShapeLabelMapFilter can be used to set the attributes values of the
 * ShapeLabelObject in a LabelMap.
 *
 * ShapeLabelMapFilter takes an optional parameter, used only to
 * optimize the computation time and the memory usage when the
 * perimeter or the feret diameter is used: the exact copy of the
 * input LabelMap is stored in an Image.
 * It can be set with SetLabelImage(). It is cleared at the end of the
 * computation, so must be reset before running Update() again. It is
 * not part of the pipeline management design, to let the subclasses
 * of ShapeLabelMapFilter use the pipeline design to specify truly
 * required inputs.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */

template <typename TImage, typename TLabelImage = Image<typename TImage::PixelType, TImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT ShapeLabelMapFilter : public InPlaceLabelMapFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ShapeLabelMapFilter);

  /** Standard class type aliases. */
  using Self = ShapeLabelMapFilter;
  using Superclass = InPlaceLabelMapFilter<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using ImageType = TImage;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using PixelType = typename ImageType::PixelType;
  using IndexType = typename ImageType::IndexType;
  using SizeType = typename ImageType::SizeType;
  using RegionType = typename ImageType::RegionType;
  using OffsetType = typename ImageType::OffsetType;
  using LabelObjectType = typename ImageType::LabelObjectType;
  using MatrixType = typename LabelObjectType::MatrixType;
  using VectorType = typename LabelObjectType::VectorType;

  using LabelImageType = TLabelImage;
  using LabelImagePointer = typename LabelImageType::Pointer;
  using LabelImageConstPointer = typename LabelImageType::ConstPointer;
  using LabelPixelType = typename LabelImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ShapeLabelMapFilter, InPlaceLabelMapFilter);

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

  /**
   * Set/Get whether the maximum Feret diameter should be computed or not.
   * Default value is false because of the high computation time required.
   */
  itkSetMacro(ComputeFeretDiameter, bool);
  itkGetConstReferenceMacro(ComputeFeretDiameter, bool);
  itkBooleanMacro(ComputeFeretDiameter);

  /**
   * Set/Get whether the perimeter should be computed or not.
   * Default value is true;
   */
  itkSetMacro(ComputePerimeter, bool);
  itkGetConstReferenceMacro(ComputePerimeter, bool);
  itkBooleanMacro(ComputePerimeter);

  /**
   * Set/Get whether the oriented bounding box should be
   * computed or not. Default value is false because of potential
   * memory consumption issues with sparse labels.
   */
  itkSetMacro(ComputeOrientedBoundingBox, bool);
  itkGetConstReferenceMacro(ComputeOrientedBoundingBox, bool);
  itkBooleanMacro(ComputeOrientedBoundingBox);

  /** Set the label image */
  void
  SetLabelImage(const TLabelImage * input)
  {
    m_LabelImage = input;
  }

protected:
  ShapeLabelMapFilter();
  ~ShapeLabelMapFilter() override = default;

  void
  ThreadedProcessLabelObject(LabelObjectType * labelObject) override;

  void
  BeforeThreadedGenerateData() override;

  void
  AfterThreadedGenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  bool                   m_ComputeFeretDiameter;
  bool                   m_ComputePerimeter;
  bool                   m_ComputeOrientedBoundingBox;
  LabelImageConstPointer m_LabelImage;

  void
  ComputeFeretDiameter(LabelObjectType * labelObject);
  void
  ComputePerimeter(LabelObjectType * labelObject);
  void
  ComputeOrientedBoundingBox(LabelObjectType * labelObject);

  using Offset2Type = itk::Offset<2>;
  using Offset3Type = itk::Offset<3>;
  using Spacing2Type = itk::Vector<double, 2>;
  using Spacing3Type = itk::Vector<double, 3>;
  using MapIntercept2Type = std::map<Offset2Type, SizeValueType, Functor::LexicographicCompare>;
  using MapIntercept3Type = std::map<Offset3Type, SizeValueType, Functor::LexicographicCompare>;

  // it seems impossible to specialize a method without specializing the whole class, but we
  // can use simple overloading
  template <typename TMapIntercept, typename TSpacing>
  double
  PerimeterFromInterceptCount(TMapIntercept & intercepts, const TSpacing & spacing);
#if !defined(ITK_DO_NOT_USE_PERIMETER_SPECIALIZATION)
  double
  PerimeterFromInterceptCount(MapIntercept2Type & intercepts, const Spacing2Type spacing);
  double
  PerimeterFromInterceptCount(MapIntercept3Type & intercepts, const Spacing3Type spacing);
#endif
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShapeLabelMapFilter.hxx"
#endif

#endif

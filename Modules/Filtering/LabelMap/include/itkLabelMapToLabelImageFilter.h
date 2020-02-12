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
#ifndef itkLabelMapToLabelImageFilter_h
#define itkLabelMapToLabelImageFilter_h

#include "itkLabelMapFilter.h"

namespace itk
{
/**
 *\class LabelMapToLabelImageFilter
 * \brief Converts a LabelMap to a labeled image.
 *
 * LabelMapToBinaryImageFilter to a label image.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelMapToBinaryImageFilter, LabelMapMaskImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup LabeledImageFilters
 * \ingroup ITKLabelMap
 *
 * \sphinx
 * \sphinxexample{Filtering/LabelMap/ConvertLabelMapToImage,Convert Label Map To Normal Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT LabelMapToLabelImageFilter : public LabelMapFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelMapToLabelImageFilter);

  /** Standard class type aliases. */
  using Self = LabelMapToLabelImageFilter;
  using Superclass = LabelMapFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = typename Superclass::InputImageType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputImageConstPointer = typename Superclass::InputImageConstPointer;
  using InputImageRegionType = typename Superclass::InputImageRegionType;
  using InputImagePixelType = typename Superclass::InputImagePixelType;
  using LabelObjectType = typename Superclass::LabelObjectType;

  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  using OutputImageConstPointer = typename Superclass::OutputImageConstPointer;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using OutputImagePixelType = typename Superclass::OutputImagePixelType;
  using IndexType = typename OutputImageType::IndexType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelMapToLabelImageFilter, LabelMapFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
#endif

protected:
  LabelMapToLabelImageFilter();
  ~LabelMapToLabelImageFilter() override = default;

  void
  BeforeThreadedGenerateData() override;

  void
  ThreadedProcessLabelObject(LabelObjectType * labelObject) override;

private:
  OutputImageType * m_OutputImage;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelMapToLabelImageFilter.hxx"
#endif

#endif

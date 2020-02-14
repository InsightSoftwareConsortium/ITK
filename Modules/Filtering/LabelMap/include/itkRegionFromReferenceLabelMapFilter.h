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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkRegionFromReferenceLabelMapFilter_h
#define itkRegionFromReferenceLabelMapFilter_h

#include "itkChangeRegionLabelMapFilter.h"

namespace itk
{
/**
 *\class RegionFromReferenceLabelMapFilter
 * \brief Set the region from a reference image
 *
 * Change the region of a label map to be the same as one of a reference image.
 * This filter implements the same feature as its superclass, but with the input region
 * well integrated in the pipeline architecture.
 * If the output cannot contain some of the objects' lines, they are truncated or removed.
 * All objects fully outside the output region are removed.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT RegionFromReferenceLabelMapFilter : public ChangeRegionLabelMapFilter<TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegionFromReferenceLabelMapFilter);

  /** Standard class type aliases. */
  using Self = RegionFromReferenceLabelMapFilter;
  using Superclass = ChangeRegionLabelMapFilter<TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionFromReferenceLabelMapFilter, ChangeRegionImageFilter);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Superclass type alias. */
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using OutputImagePixelType = typename Superclass::OutputImagePixelType;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using LabelObjectType = typename InputImageType::LabelObjectType;

  using PixelType = typename InputImageType::PixelType;
  using IndexType = typename InputImageType::IndexType;
  using SizeType = typename InputImageType::SizeType;
  using RegionType = typename InputImageType::RegionType;

  using TOutputImage = TInputImage;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  using ReferenceImageType = ImageBase<Self::ImageDimension>;

  /** Copy the output information from another Image. */
  void
  SetReferenceImage(const ReferenceImageType * image);

  const ReferenceImageType *
  GetReferenceImage() const;

  /** Set the input image */
  void
  SetInput1(const TInputImage * input)
  {
    this->SetInput(input);
  }

  /** Set the reference image */
  void
  SetInput2(const ReferenceImageType * input)
  {
    this->SetReferenceImage(input);
  }

protected:
  RegionFromReferenceLabelMapFilter() { this->SetNumberOfRequiredInputs(2); }

  ~RegionFromReferenceLabelMapFilter() override = default;

  void
  GenerateOutputInformation() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRegionFromReferenceLabelMapFilter.hxx"
#endif

#endif

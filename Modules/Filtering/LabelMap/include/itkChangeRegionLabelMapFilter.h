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
#ifndef itkChangeRegionLabelMapFilter_h
#define itkChangeRegionLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"

namespace itk
{
/**
 *\class ChangeRegionLabelMapFilter
 * \brief Change the region of a label map.
 *
 * Change the region of a label map. If the output can't contain some of the objects' lines
 * they are truncated or removed. All objects fully outside the output region are removed.
 *
 *
 * This code was contributed in the Insight Journal paper:
 * "Label object representation and manipulation with ITK"
 * by Lehmann G.
 * https://www.insight-journal.org/browse/publication/176
 *
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa LabelMapMaskImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT ChangeRegionLabelMapFilter : public InPlaceLabelMapFilter<TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ChangeRegionLabelMapFilter);

  /** Standard class type aliases. */
  using Self = ChangeRegionLabelMapFilter;
  using Superclass = InPlaceLabelMapFilter<TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ChangeRegionLabelMapFilter, InPlaceImageFilter);

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
  using IndexValueType = typename InputImageType::IndexValueType;
  using SizeType = typename InputImageType::SizeType;
  using RegionType = typename InputImageType::RegionType;

  using TOutputImage = TInputImage;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** The output region to use */
  itkSetMacro(Region, OutputImageRegionType);
  itkGetConstReferenceMacro(Region, OutputImageRegionType);

protected:
  ChangeRegionLabelMapFilter() = default;
  ~ChangeRegionLabelMapFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  ThreadedProcessLabelObject(LabelObjectType * labelObject) override;

  void
  GenerateInputRequestedRegion() override;

  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  void
  GenerateOutputInformation() override;

  void
  GenerateData() override;

private:
  OutputImageRegionType m_Region;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkChangeRegionLabelMapFilter.hxx"
#endif

#endif

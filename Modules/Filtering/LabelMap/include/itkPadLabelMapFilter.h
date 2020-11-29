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
#ifndef itkPadLabelMapFilter_h
#define itkPadLabelMapFilter_h

#include "itkChangeRegionLabelMapFilter.h"

namespace itk
{
/**
 *\class PadLabelMapFilter
 * \brief Pad a LabelMap image
 *
 * This filter pads a label map.
 *
 * The SetPadSize() method can be used to set the pad size of the lower and
 * the upper boundaries in a single call.
 * By default, the filter doesn't pad anything.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa CropLabelMapFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT PadLabelMapFilter : public ChangeRegionLabelMapFilter<TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PadLabelMapFilter);

  /** Standard class type aliases. */
  using Self = PadLabelMapFilter;
  using Superclass = ChangeRegionLabelMapFilter<TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PadLabelMapFilter, ChangeRegionLabelMapFilter);

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

  /** Set/Get the cropping sizes for the upper and lower boundaries. */
  itkSetMacro(UpperBoundaryPadSize, SizeType);
  itkGetMacro(UpperBoundaryPadSize, SizeType);
  itkSetMacro(LowerBoundaryPadSize, SizeType);
  itkGetMacro(LowerBoundaryPadSize, SizeType);

  void
  SetPadSize(const SizeType & size)
  {
    this->SetUpperBoundaryPadSize(size);
    this->SetLowerBoundaryPadSize(size);
  }

protected:
  PadLabelMapFilter()
  {
    m_UpperBoundaryPadSize.Fill(0);
    m_LowerBoundaryPadSize.Fill(0);
  }

  ~PadLabelMapFilter() override = default;

  void
  GenerateOutputInformation() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  SizeType m_UpperBoundaryPadSize;
  SizeType m_LowerBoundaryPadSize;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPadLabelMapFilter.hxx"
#endif

#endif

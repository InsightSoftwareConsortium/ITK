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
#ifndef itkCropLabelMapFilter_h
#define itkCropLabelMapFilter_h

#include "itkChangeRegionLabelMapFilter.h"

namespace itk
{
/**
 *\class CropLabelMapFilter
 * \brief Crop a LabelMap image
 *
 * Crop a label map. If the output cannot contain some lines of the objects, they are truncated
 * or removed. All objects fully outside the output region are removed.
 *
 * The SetCropSize() method can be used to set the crop size of the lower and the upper
 * boundaries in a single call. By default, the filter does not crop anything.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa PadLabelMapFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT CropLabelMapFilter : public ChangeRegionLabelMapFilter<TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(CropLabelMapFilter);

  /** Standard class type aliases. */
  using Self = CropLabelMapFilter;
  using Superclass = ChangeRegionLabelMapFilter<TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CropLabelMapFilter, ChangeRegionImageFilter);

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
  itkSetMacro(UpperBoundaryCropSize, SizeType);
  itkGetMacro(UpperBoundaryCropSize, SizeType);
  itkSetMacro(LowerBoundaryCropSize, SizeType);
  itkGetMacro(LowerBoundaryCropSize, SizeType);

  void
  SetCropSize(const SizeType & size)
  {
    this->SetUpperBoundaryCropSize(size);
    this->SetLowerBoundaryCropSize(size);
  }

protected:
  CropLabelMapFilter()
  {
    m_UpperBoundaryCropSize.Fill(0);
    m_LowerBoundaryCropSize.Fill(0);
  }

  ~CropLabelMapFilter() override = default;

  void
  GenerateOutputInformation() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  SizeType m_UpperBoundaryCropSize;
  SizeType m_LowerBoundaryCropSize;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCropLabelMapFilter.hxx"
#endif

#endif

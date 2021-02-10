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
#ifndef itkAutoCropLabelMapFilter_h
#define itkAutoCropLabelMapFilter_h

#include "itkChangeRegionLabelMapFilter.h"

namespace itk
{
/**
 *\class AutoCropLabelMapFilter
 * \brief Crop a LabelMap image to fit exactly the objects in the LabelMap.
 *
 * The CropBorder can be used to add a border which will never be larger than
 * the input image. To add a border of size independent of the input image,
 * PadLabelMapFilter can be used.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa PadLabelMapFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT AutoCropLabelMapFilter : public ChangeRegionLabelMapFilter<TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AutoCropLabelMapFilter);

  /** Standard class type aliases. */
  using Self = AutoCropLabelMapFilter;
  using Superclass = ChangeRegionLabelMapFilter<TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AutoCropLabelMapFilter, ChangeRegionImageFilter);

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

  /**
   * Set/Get the border added to the mask before the crop. The default is 0 on * all the axis.
   */
  itkSetMacro(CropBorder, SizeType);
  itkGetConstReferenceMacro(CropBorder, SizeType);

protected:
  AutoCropLabelMapFilter();
  ~AutoCropLabelMapFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateOutputInformation() override;

private:
  SizeType m_CropBorder;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAutoCropLabelMapFilter.hxx"
#endif

#endif

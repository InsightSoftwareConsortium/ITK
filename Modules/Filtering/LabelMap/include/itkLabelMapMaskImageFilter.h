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
#ifndef itkLabelMapMaskImageFilter_h
#define itkLabelMapMaskImageFilter_h

#include "itkLabelMapFilter.h"

namespace itk
{

/**
 *\class LabelMapMaskImageFilter
 * \brief Mask and image with a LabelMap
 *
 * LabelMapMaskImageFilter mask the content of an input image according
 * to the content of the input LabelMap. The masked pixel of the input image
 * are set to the BackgroundValue.
 * LabelMapMaskImageFilter can keep the input image for one label only, with
 * Negated = false (the default) or it can mask the input image for a single label, when
 * Negated equals true. In Both cases, the label is set with SetLabel().
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelMapToBinaryImageFilter, LabelMapToLabelImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT LabelMapMaskImageFilter : public LabelMapFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelMapMaskImageFilter);

  /** Standard class type aliases. */
  using Self = LabelMapMaskImageFilter;
  using Superclass = LabelMapFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using LabelObjectType = typename InputImageType::LabelObjectType;
  using LabelType = typename LabelObjectType::LabelType;
  using LengthType = typename LabelObjectType::LengthType;

  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using IndexType = typename OutputImageType::IndexType;
  using SizeType = typename OutputImageType::SizeType;
  using RegionType = typename OutputImageType::RegionType;


  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelMapMaskImageFilter, LabelMapFilter);

  /** Set the feature image */
  void
  SetFeatureImage(const TOutputImage * input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput(1, const_cast<TOutputImage *>(input));
  }

  /** Get the feature image */
  const OutputImageType *
  GetFeatureImage()
  {
    return static_cast<OutputImageType *>(const_cast<DataObject *>(this->ProcessObject::GetInput(1)));
  }

  /** Set the input image */
  void
  SetInput1(const TInputImage * input)
  {
    this->SetInput(input);
  }

  /** Set the feature image */
  void
  SetInput2(const TOutputImage * input)
  {
    this->SetFeatureImage(input);
  }

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<PixelType>::ZeroValue().
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

  /**
   * The label to mask or to not mask, depending on the value of the Negated ivar.
   */
  itkSetMacro(Label, InputImagePixelType);
  itkGetConstMacro(Label, InputImagePixelType);

  /**
   * Set/Get whether the Label should be masked or not.
   */
  itkSetMacro(Negated, bool);
  itkGetConstReferenceMacro(Negated, bool);
  itkBooleanMacro(Negated);

  /**
   * Set/Get whether the image size should be adjusted to the masked image or not.
   */
  itkSetMacro(Crop, bool);
  itkGetConstReferenceMacro(Crop, bool);
  itkBooleanMacro(Crop);

  /**
   * Set/Get the boder added to the mask before the crop. The default is 0 on
   * all the axes.
   */
  itkSetMacro(CropBorder, SizeType);
  itkGetConstReferenceMacro(CropBorder, SizeType);

protected:
  LabelMapMaskImageFilter();
  ~LabelMapMaskImageFilter() override = default;

  /** LabelMapMaskImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** LabelMapMaskImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  void
  GenerateOutputInformation() override;

  void
  GenerateData() override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

  // part of a compile error workaround for GCC 4.8.5-28 (Red Hat) from 20150623
  void
  SuperclassDynamicTGD(const OutputImageRegionType & outputRegion)
  {
    Superclass::DynamicThreadedGenerateData(outputRegion);
  }

  void
  ThreadedProcessLabelObject(LabelObjectType * labelObject) override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  InputImagePixelType  m_Label;
  OutputImagePixelType m_BackgroundValue;
  bool                 m_Negated{ false };
  bool                 m_Crop{ false };
  SizeType             m_CropBorder;

  TimeStamp m_CropTimeStamp;
}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelMapMaskImageFilter.hxx"
#endif

#endif

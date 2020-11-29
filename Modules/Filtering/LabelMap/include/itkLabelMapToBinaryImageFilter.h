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
#ifndef itkLabelMapToBinaryImageFilter_h
#define itkLabelMapToBinaryImageFilter_h

#include "itkLabelMapFilter.h"

namespace itk
{
/**
 *\class LabelMapToBinaryImageFilter
 * \brief Convert a LabelMap to a binary image.
 *
 * LabelMapToBinaryImageFilter to a binary image. All the objects in the image
 * are used as foreground.  The background values of the original binary image
 * can be restored by passing this image to the filter with the
 * SetBackgroundImage() method.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * \sa LabelMapToLabelImageFilter, LabelMapMaskImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT LabelMapToBinaryImageFilter : public LabelMapFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelMapToBinaryImageFilter);

  /** Standard class type aliases. */
  using Self = LabelMapToBinaryImageFilter;
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

  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using IndexType = typename OutputImageType::IndexType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelMapToBinaryImageFilter, ImageToImageFilter);

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<PixelType>::NonpositiveMin().
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

  /**
   * Set/Get the value used as "foreground" in the output image.
   * Defaults to NumericTraits<PixelType>::max().
   */
  itkSetMacro(ForegroundValue, OutputImagePixelType);
  itkGetConstMacro(ForegroundValue, OutputImagePixelType);

  /** Set/Get the background image top be used to restore the background values
   */
  void
  SetBackgroundImage(const OutputImageType * input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput(1, const_cast<OutputImageType *>(input));
  }

  OutputImageType *
  GetBackgroundImage()
  {
    return static_cast<OutputImageType *>(const_cast<DataObject *>(this->ProcessObject::GetInput(1)));
  }

  /** Set the input image */
  void
  SetInput1(const InputImageType * input)
  {
    this->SetInput(input);
  }

  /** Set the marker image */
  void
  SetInput2(const OutputImageType * input)
  {
    this->SetBackgroundImage(input);
  }

protected:
  LabelMapToBinaryImageFilter();
  ~LabelMapToBinaryImageFilter() override = default;

  /** LabelMapToBinaryImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** LabelMapToBinaryImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

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
  OutputImagePixelType m_BackgroundValue;
  OutputImagePixelType m_ForegroundValue;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelMapToBinaryImageFilter.hxx"
#endif

#endif

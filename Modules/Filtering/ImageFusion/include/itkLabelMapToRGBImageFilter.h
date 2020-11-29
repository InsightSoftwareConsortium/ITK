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
#ifndef itkLabelMapToRGBImageFilter_h
#define itkLabelMapToRGBImageFilter_h

#include "itkLabelMapFilter.h"
#include "itkLabelToRGBFunctor.h"
#include "itkImage.h"
#include "itkRGBPixel.h"


namespace itk
{

/**
 *\class LabelMapToRGBImageFilter
 * \brief Convert a LabelMap to a colored image
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelToRGBImageFilter, LabelToRGBFunctor
 * \sa LabelMapOverlayImageFilter, LabelMapToBinaryImageFilter, LabelMapMaskImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKImageFusion
 */
template <typename TInputImage, typename TOutputImage = Image<RGBPixel<unsigned char>, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT LabelMapToRGBImageFilter : public LabelMapFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelMapToRGBImageFilter);

  /** Standard class type aliases. */
  using Self = LabelMapToRGBImageFilter;
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

  using FunctorType = typename Functor::LabelToRGBFunctor<InputImagePixelType, OutputImagePixelType>;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelMapToRGBImageFilter, ImageToImageFilter);

  /** Set/Get the rgb functor - defaults to a reasonable set of colors.
   * This can be used to apply a different colormap.
   */
  virtual void
  SetFunctor(const FunctorType & functor)
  {
    if (m_Functor != functor)
    {
      m_Functor = functor;
      this->Modified();
    }
  }
  FunctorType &
  GetFunctor()
  {
    return m_Functor;
  }
  const FunctorType &
  GetFunctor() const
  {
    return m_Functor;
  }

protected:
  LabelMapToRGBImageFilter() = default;
  ~LabelMapToRGBImageFilter() override = default;

  void
  BeforeThreadedGenerateData() override;

  void
  ThreadedProcessLabelObject(LabelObjectType * labelObject) override;

  void
  GenerateOutputInformation() override;

private:
  FunctorType m_Functor;
}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelMapToRGBImageFilter.hxx"
#endif

#endif

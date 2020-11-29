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
#ifndef itkLabelToRGBImageFilter_h
#define itkLabelToRGBImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkLabelToRGBFunctor.h"

namespace itk
{
/**
 *\class LabelToRGBImageFilter
 * \brief Apply a colormap to a label image
 *
 * Apply a colormap to a label image. The set of colors
 * is a good selection of distinct colors. The user can choose to use a background
 * value. In that case, a gray pixel with the same intensity than the background
 * label is produced.
 *
 * This code was contributed in the Insight Journal paper:
 * "The watershed transform in ITK - discussion and new developments"
 * by Beare R., Lehmann G.
 * https://www.insight-journal.org/browse/publication/92
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 * \author Richard Beare. Department of Medicine, Monash University, Melbourne, Australia.
 *
 * \sa  LabelOverlayImageFilter
 * \sa  LabelMapToRGBImageFilter, LabelToRGBFunctor, ScalarToRGBPixelFunctor
 * \ingroup MultiThreaded
 *
 * \ingroup ITKImageFusion
 */
template <typename TLabelImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT LabelToRGBImageFilter
  : public UnaryFunctorImageFilter<
      TLabelImage,
      TOutputImage,
      Functor::LabelToRGBFunctor<typename TLabelImage::PixelType, typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelToRGBImageFilter);

  /** Standard class type aliases. */
  using Self = LabelToRGBImageFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using Superclass = UnaryFunctorImageFilter<
    TLabelImage,
    TOutputImage,
    Functor::LabelToRGBFunctor<typename TLabelImage::PixelType, typename TOutputImage::PixelType>>;

  using OutputImageType = TOutputImage;
  using LabelImageType = TLabelImage;

  using OutputPixelType = typename TOutputImage::PixelType;
  using LabelPixelType = typename TLabelImage::PixelType;
  using OutputPixelValueType = typename NumericTraits<OutputPixelType>::ValueType;

  /** Runtime information support. */
  itkTypeMacro(LabelToRGBImageFilter, UnaryFunctorImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set/Get the background value */
  itkSetMacro(BackgroundValue, LabelPixelType);
  itkGetConstReferenceMacro(BackgroundValue, LabelPixelType);

  /** Set/Get the background color in the output image */
  itkSetMacro(BackgroundColor, OutputPixelType);
  itkGetConstReferenceMacro(BackgroundColor, OutputPixelType);

  /** Empty the color LUT container */
  void
  ResetColors();

  /** Get number of colors in the LUT container */
  unsigned int
  GetNumberOfColors() const;

  /** Type of the color component */
  using ComponentType = typename OutputPixelType::ComponentType;

  /** Add color to the LUT container */
  void
  AddColor(ComponentType r, ComponentType g, ComponentType b);

protected:
  LabelToRGBImageFilter();
  ~LabelToRGBImageFilter() override = default;

  /** Process to execute before entering the multithreaded section */
  void
  BeforeThreadedGenerateData() override;

  /** Print internal ivars */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateOutputInformation() override;

private:
  OutputPixelType m_BackgroundColor;
  LabelPixelType  m_BackgroundValue;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelToRGBImageFilter.hxx"
#endif

#endif

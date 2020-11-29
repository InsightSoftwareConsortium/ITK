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
#ifndef itkLabelOverlayImageFilter_h
#define itkLabelOverlayImageFilter_h

#include "itkLabelOverlayFunctor.h"
#include "itkBinaryGeneratorImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/**
 *\class LabelOverlayImageFilter
 * \brief Apply a colormap to a label image and put it on top of the
 *  input image
 *
 * Apply a colormap to a label image and put it on top of the input image.
 * The set of colors is a good selection of distinct colors. The opacity of
 * the label image can be defined by the user. The user can also choose if
 * the want to use a background and which label value is the background.
 * A background label produce a gray pixel with the same intensity
 * than the input one.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * This class was contributed to the Insight Journal
 * https://www.insight-journal.org/browse/publication/79
 *
 * \sa LabelToRGBImageFilter
 * \sa LabelMapOverlayImageFilter, LabelOverlayFunctor
 * \ingroup MultiThreaded
 *
 * \ingroup ITKImageFusion
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageFusion/OverlayLabelMapOnImage,Overlay Label Map On Image}
 * \endsphinx
 */
template <typename TInputImage, typename TLabelImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT LabelOverlayImageFilter
  : public BinaryGeneratorImageFilter<TInputImage, TLabelImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelOverlayImageFilter);

  /** Standard class type aliases. */
  using Self = LabelOverlayImageFilter;

  using Superclass = BinaryGeneratorImageFilter<TInputImage, TLabelImage, TOutputImage>;

  using FunctorType = Functor::LabelOverlayFunctor<typename TInputImage::PixelType,
                                                   typename TLabelImage::PixelType,
                                                   typename TOutputImage::PixelType>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using OutputImageType = TOutputImage;
  using LabelImageType = TLabelImage;
  using InputImageType = TInputImage;

  using OutputPixelType = typename TOutputImage::PixelType;
  using LabelPixelType = typename TLabelImage::PixelType;
  using InputPixelType = typename TInputImage::PixelType;

  /** Runtime information support. */
  itkTypeMacro(LabelOverlayImageFilter, BinaryGeneratorImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set the label image */
  void
  SetLabelImage(const TLabelImage * input);

  /** Get the label image */
  const LabelImageType *
  GetLabelImage() const;

  /** Set/Get the opacity of the colored label image. The value must be
   * between 0 and 1
   */
  itkSetMacro(Opacity, double);
  itkGetConstReferenceMacro(Opacity, double);

  /** Set/Get the background value */
  itkSetMacro(BackgroundValue, LabelPixelType);
  itkGetConstReferenceMacro(BackgroundValue, LabelPixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputPixelShouldHaveValueType, (Concept::HasValueType<OutputPixelType>));
  itkConceptMacro(OutputPixelShouldHaveBracketOperator,
                  (Concept::BracketOperator<OutputPixelType, unsigned int, typename OutputPixelType::ValueType>));
  // End concept checking
#endif

  /** Empty the color LUT container */
  void
  ResetColors();

  /** Get number of colors in the LUT container */
  unsigned int
  GetNumberOfColors() const;

  /** type of the color component */
  using ComponentType = typename OutputPixelType::ComponentType;

  /** Add color to the LUT container */
  void
  AddColor(ComponentType r, ComponentType g, ComponentType b);


  itkGetConstReferenceMacro(Functor, FunctorType);
  FunctorType &
  GetFunctor()
  {
    return m_Functor;
  }

protected:
  LabelOverlayImageFilter();
  ~LabelOverlayImageFilter() override = default;

  /** Process to execute before entering the multithreaded section */
  void
  BeforeThreadedGenerateData() override;

  /** Print internal ivars */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateOutputInformation() override;

private:
  FunctorType    m_Functor;
  double         m_Opacity;
  LabelPixelType m_BackgroundValue;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelOverlayImageFilter.hxx"
#endif

#endif

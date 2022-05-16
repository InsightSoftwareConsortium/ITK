/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkStochasticFractalDimensionImageFilter_h
#define itkStochasticFractalDimensionImageFilter_h

#include "itkImageToImageFilter.h"

#include "itkConstNeighborhoodIterator.h"

namespace itk
{
/** \class StochasticFractalDimensionImageFilter
 *  \brief This filter computes the stochastic fractal dimension of the input image.
 *
 * The methodology is based on Madelbrot's fractal theory and the concept of
 * fractional Brownian motion and yields images which have been used for
 * classification and edge enhancement.
 *
 * This class which is templated over the input and output images as well as a
 * mask image type. The input is a scalar image, an optional neighborhood
 * radius (default = 2), and an optional mask. The mask can be specified to
 * decrease computation time since, as the authors point out, calculation is
 * time-consuming.
 *
 * This filter was contributed by Nick Tustison and James Gee
 * from the PICSL lab, at the University of Pennsylvania
 * as an paper to the Insight Journal:
 *
 *  "Stochastic Fractal Dimension Image"
 *  https://hdl.handle.net/1926/1525
 *  https://www.insight-journal.org/browse/publication/318
 *
 * \author Nick Tustison
 *
 * \ingroup ITKReview
 */
template <typename TInputImage,
          typename TMaskImage = Image<unsigned char, TInputImage::ImageDimension>,
          class TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT StochasticFractalDimensionImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(StochasticFractalDimensionImageFilter);

  /** Standard class type aliases. */
  using Self = StochasticFractalDimensionImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Some convenient type alias. */
  using RealType = float;
  using InputImageType = TInputImage;
  using MaskImageType = TMaskImage;
  using OutputImageType = TOutputImage;

  /** Runtime information support. */
  itkTypeMacro(StochasticFractalDimensionImageFilter, ImageToImageFilter);

  /** Set/Get the input mask image that will constraint the computation of the
   * fractal dimension to pixels that are on in the mask. This is intended to
   * reduce the computation time. */
  void
  SetMaskImage(const MaskImageType * mask);

  const MaskImageType *
  GetMaskImage() const;

  /** Type of the neighborhood iterator used to evaluate similarity between the
   * image pixels. */
  using ConstNeighborhoodIteratorType = ConstNeighborhoodIterator<InputImageType>;
  using RadiusType = typename ConstNeighborhoodIteratorType::RadiusType;

  /** Manhattan radius used for evaluating the fractal dimension. */
  itkSetMacro(NeighborhoodRadius, RadiusType);
  itkGetConstMacro(NeighborhoodRadius, RadiusType);

protected:
  StochasticFractalDimensionImageFilter();
  ~StochasticFractalDimensionImageFilter() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

private:
  RadiusType m_NeighborhoodRadius;

  typename MaskImageType::Pointer m_MaskImage;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStochasticFractalDimensionImageFilter.hxx"
#endif

#endif

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
#ifndef itkFirstOrderTextureFeaturesImageFilter_h
#define itkFirstOrderTextureFeaturesImageFilter_h

#include "itkMovingHistogramImageFilter.h"
#include "itkFirstOrderTextureHistogram.h"

namespace itk
{
/**
 * \class FirstOrderTextureFeaturesImageFilter
 * \brief Compute first order statistics in a neighborhood for each
 * pixel.
 *
 * The output of this filter is a multi-component image where each
 * pixel is:
 *   -# mean
 *   -# minimum
 *   -# maximum
 *   -# variance
 *   -# standard deviation (sigma)
 *   -# skewness
 *   -# kurtosis
 *   -# entropy.
 *  These first order statistics are computed based on a define
 * neighborhood or kernel such as FlatStructuringElement::Box.
 *
 * The boundary is handle by only considering the pixel in the image,
 * so that the boundary pixel have lets data to compute the
 * statistics.
 *
 * \ingroup ITKTextureFeatures
 */

template <class TInputImage, class TOutputImage, class TKernel>
class ITK_TEMPLATE_EXPORT FirstOrderTextureFeaturesImageFilter
  : public MovingHistogramImageFilter<
      TInputImage,
      TOutputImage,
      TKernel,
      typename Function::FirstOrderTextureHistogram<typename TInputImage::PixelType, typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FirstOrderTextureFeaturesImageFilter);

  /** Standard class type alias. */
  using Self = FirstOrderTextureFeaturesImageFilter;
  using Superclass = MovingHistogramImageFilter<
    TInputImage,
    TOutputImage,
    TKernel,
    typename Function::FirstOrderTextureHistogram<typename TInputImage::PixelType, typename TOutputImage::PixelType>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(FirstOrderTextureFeaturesImageFilter, MovingHistogramMorphologyImageFilter);

  /** Image related type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename TInputImage::SizeType;
  using IndexType = typename TInputImage::IndexType;
  using PixelType = typename TInputImage::PixelType;
  using OffsetType = typename TInputImage::OffsetType;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using OutputPixelType = typename TOutputImage::PixelType;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

protected:
  unsigned int
  GetNumberOfOutputComponents()
  {
    return 8;
  }

  FirstOrderTextureFeaturesImageFilter() = default;

  void
  GenerateOutputInformation() override
  {
    // this methods is overloaded so that if the output image is a
    // VectorImage then the correct number of components are set.

    Superclass::GenerateOutputInformation();
    OutputImageType * output = this->GetOutput();

    if (!output)
    {
      return;
    }
    if (output->GetNumberOfComponentsPerPixel() != this->GetNumberOfOutputComponents())
    {
      output->SetNumberOfComponentsPerPixel(this->GetNumberOfOutputComponents());
    }
  }


  ~FirstOrderTextureFeaturesImageFilter() override = default;
};
} // end namespace itk

#endif

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
#ifndef itkBinaryMorphologicalOpeningImageFilter_h
#define itkBinaryMorphologicalOpeningImageFilter_h

#include "itkKernelImageFilter.h"

namespace itk
{
/**
 * \class BinaryMorphologicalOpeningImageFilter
 * \brief binary morphological opening of an image.
 *
 * This filter removes small (i.e., smaller than the structuring
 * element) structures in the interior or at the
 * boundaries of the image. The morphological opening of an image
 * "f" is defined as:
 * Opening(f) = Dilatation(Erosion(f)).
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 *
 * This code was contributed in the Insight Journal paper:
 * "Binary morphological closing and opening image filters"
 * by Lehmann G.
 * https://www.insight-journal.org/browse/publication/58
 *
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, GrayscaleErodeImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKBinaryMathematicalMorphology
 *
 * \sphinx
 * \sphinxexample{Filtering/BinaryMathematicalMorphology/OpeningBinaryImage,Opening A Binary Image}
 * \endsphinx
 */

template <typename TInputImage, typename TOutputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT BinaryMorphologicalOpeningImageFilter
  : public KernelImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryMorphologicalOpeningImageFilter);

  /** Standard class type aliases. */
  using Self = BinaryMorphologicalOpeningImageFilter;
  using Superclass = KernelImageFilter<TInputImage, TOutputImage, TKernel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryMorphologicalOpeningImageFilter, KernelImageFilter);

  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /** Declaration of pixel type. */
  using PixelType = typename TInputImage::PixelType;

  /** Kernel type alias. */
  using KernelType = TKernel;

  /** Set the value in the image to consider as "foreground". Defaults to
   * maximum value of PixelType. */
  itkSetMacro(ForegroundValue, PixelType);

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType. */
  itkGetConstMacro(ForegroundValue, PixelType);

  /** Set the value in eroded part of the image. Defaults to zero */
  itkSetMacro(BackgroundValue, PixelType);

  /** Set the value in eroded part of the image. Defaults to zero */
  itkGetConstMacro(BackgroundValue, PixelType);

protected:
  BinaryMorphologicalOpeningImageFilter();
  ~BinaryMorphologicalOpeningImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleDilateImageFilter GrayscaleErodeImageFilter. */
  void
  GenerateData() override;

private:
  PixelType m_ForegroundValue;

  PixelType m_BackgroundValue;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryMorphologicalOpeningImageFilter.hxx"
#endif

#endif

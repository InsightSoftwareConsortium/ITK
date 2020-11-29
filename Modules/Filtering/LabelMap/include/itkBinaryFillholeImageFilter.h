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
#ifndef itkBinaryFillholeImageFilter_h
#define itkBinaryFillholeImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{

/**
 *\class BinaryFillholeImageFilter
 * \brief Remove holes not connected to the boundary of the image.
 *
 * BinaryFillholeImageFilter fills holes in a binary image.
 *
 * Geodesic morphology and the Fillhole algorithm is described in
 * Chapter 6 of Pierre Soille's book "Morphological Image Analysis:
 * Principles and Applications", Second Edition, Springer, 2003.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \sa GrayscaleFillholeImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT BinaryFillholeImageFilter : public ImageToImageFilter<TInputImage, TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryFillholeImageFilter);

  /** Standard class type aliases. */
  using Self = BinaryFillholeImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = OutputImageType::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryFillholeImageFilter, ImageToImageFilter);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputOStreamWritableCheck, (Concept::OStreamWritable<InputImagePixelType>));
  // End concept checking
#endif

  /** Set the value in the image to consider as "foreground". Defaults to
   * maximum value of InputPixelType. */
  itkSetMacro(ForegroundValue, InputImagePixelType);

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of InputPixelType. */
  itkGetMacro(ForegroundValue, InputImagePixelType);

protected:
  BinaryFillholeImageFilter();
  ~BinaryFillholeImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** BinaryFillholeImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** BinaryFillholeImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicErodeImageFilter. */
  void
  GenerateData() override;

private:
  InputImagePixelType m_ForegroundValue;

  bool m_FullyConnected;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryFillholeImageFilter.hxx"
#endif

#endif

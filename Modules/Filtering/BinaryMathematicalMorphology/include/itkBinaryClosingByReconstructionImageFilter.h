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
#ifndef itkBinaryClosingByReconstructionImageFilter_h
#define itkBinaryClosingByReconstructionImageFilter_h

#include "itkKernelImageFilter.h"

namespace itk
{

/**
 * \class BinaryClosingByReconstructionImageFilter
 * \brief binary closing by reconstruction of an image.
 *
 * This filter removes small (i.e., smaller than the structuring
 * element) holes in the image. It is defined as:
 * Closing(f) = ReconstructionByErosion(Dilation(f)).
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \sa MorphologyImageFilter, ClosingByReconstructionImageFilter, BinaryOpeningByReconstructionImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKBinaryMathematicalMorphology
 */

template <typename TInputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT BinaryClosingByReconstructionImageFilter
  : public KernelImageFilter<TInputImage, TInputImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryClosingByReconstructionImageFilter);

  /** Standard class type aliases. */
  using Self = BinaryClosingByReconstructionImageFilter;
  using Superclass = KernelImageFilter<TInputImage, TInputImage, TKernel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryClosingByReconstructionImageFilter, KernelImageFilter);

  using InputImageType = TInputImage;
  using OutputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /** Declaration of pixel type. */
  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TInputImage::PixelType;

  /** Kernel type alias. */
  using KernelType = TKernel;

  /** Set the value in the image to consider as "foreground". Defaults to
   * maximum value of InputPixelType. */
  itkSetMacro(ForegroundValue, InputPixelType);

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of InputPixelType. */
  itkGetMacro(ForegroundValue, InputPixelType);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

protected:
  BinaryClosingByReconstructionImageFilter();
  ~BinaryClosingByReconstructionImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** BinaryClosingByReconstructionImageFilter need to make sure they request enough of an
   * input image to account for the structuring element size.  The input
   * requested region is expanded by the radius of the structuring element.
   * If the request extends past the LargestPossibleRegion for the input,
   * the request is cropped by the LargestPossibleRegion. */
  void
  GenerateInputRequestedRegion() override;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleDilateImageFilter GrayscaleErodeImageFilter. */
  void
  GenerateData() override;

private:
  InputPixelType m_ForegroundValue;

  bool m_FullyConnected;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryClosingByReconstructionImageFilter.hxx"
#endif

#endif

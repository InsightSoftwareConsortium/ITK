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
#ifndef itkClosingByReconstructionImageFilter_h
#define itkClosingByReconstructionImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class ClosingByReconstructionImageFilter
 * \brief Closing by reconstruction of an image
 *
 * This filter is similar to the morphological closing, but contrary
 * to the morphological closing, the closing by reconstruction
 * preserves the shape of the components.  The closing by
 * reconstruction of an image "f" is defined as:
 *
 * ClosingByReconstruction(f) = ErosionByReconstruction(f, Dilation(f)).
 *
 * Closing by reconstruction not only preserves structures preserved by
 * the dilation, but also levels raises the contrast of the darkest
 * regions. If PreserveIntensities is on, a subsequent reconstruction
 * by dilation using a marker image that is the original image for all
 * unaffected pixels.
 *
 * Closing by reconstruction is described in Chapter 6.3.9 of Pierre
 * Soille's book "Morphological Image Analysis: Principles and
 * Applications", Second Edition, Springer, 2003.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa GrayscaleMorphologicalClosingImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */
template <typename TInputImage, typename TOutputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT ClosingByReconstructionImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ClosingByReconstructionImageFilter);

  /** Standard class type aliases. */
  using Self = ClosingByReconstructionImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** Kernel type alias. */
  using KernelType = TKernel;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ClosingByReconstructionImageFilter, ImageToImageFilter);

  /** Set kernel (structuring element). */
  itkSetMacro(Kernel, KernelType);

  /** Get the kernel (structuring element). */
  itkGetConstReferenceMacro(Kernel, KernelType);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

  /**
   * Set/Get whether the original intensities of the image retained for
   * those pixels unaffected by the opening by reconstruction. If Off,
   * the output pixel contrast will be reduced. */
  itkSetMacro(PreserveIntensities, bool);
  itkGetConstReferenceMacro(PreserveIntensities, bool);
  itkBooleanMacro(PreserveIntensities);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
  // End concept checking
#endif

protected:
  ClosingByReconstructionImageFilter();
  ~ClosingByReconstructionImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** ClosingByReconstructionImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** ClosingByReconstructionImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  void
  GenerateData() override;

private:
  /** kernel or structuring element to use. */
  KernelType m_Kernel;
  bool       m_FullyConnected;
  bool       m_PreserveIntensities;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkClosingByReconstructionImageFilter.hxx"
#endif

#endif

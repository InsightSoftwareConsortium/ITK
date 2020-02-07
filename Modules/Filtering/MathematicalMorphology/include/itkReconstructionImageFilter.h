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
#ifndef itkReconstructionImageFilter_h
#define itkReconstructionImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"
#include <queue>

//#define BASIC
#define COPY

#ifdef COPY
#  include "itkNeighborhoodAlgorithm.h"
#endif

namespace itk
{
/** \class ReconstructionImageFilter
 * \brief Performs a grayscale geodesic reconstruction -- for
 * performance comparison with GrayscaleGeodesicDilateImageFilter.
 *
 * This filter uses Luc Vincent's algorithm, which employs raster and
 * antiraster propagation steps followed by a FIFO based propagation
 * step. "Morphological grayscale reconstruction in image analysis -
 * applications and efficient algorithms" -- IEEE Transactions on
 * Image processing, Vol 2, No 2, pp 176-201, April 1993
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 *
 * \sa MorphologicalReonstructionErosionImageFilter MorphologicalReonstructionDilationImageFilter
 * \ingroup MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */

template <typename TInputImage, typename TOutputImage, typename TCompare>
class ITK_TEMPLATE_EXPORT ReconstructionImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ReconstructionImageFilter);

  /** Standard class type aliases. */
  using Self = ReconstructionImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using ISizeType = typename InputImageType::SizeType;
  using MarkerImageType = TInputImage;
  using MarkerImagePointer = typename MarkerImageType::Pointer;
  using MarkerImageConstPointer = typename MarkerImageType::ConstPointer;
  using MarkerImageRegionType = typename MarkerImageType::RegionType;
  using MarkerImagePixelType = typename MarkerImageType::PixelType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using InputImageIndexType = typename InputImageType::IndexType;
  using MaskImageType = TInputImage;
  using MaskImagePointer = typename MaskImageType::Pointer;
  using MaskImageConstPointer = typename MaskImageType::ConstPointer;
  using MaskImageRegionType = typename MaskImageType::RegionType;
  using MaskImagePixelType = typename MaskImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using OutputImageIndexType = typename OutputImageType::IndexType;

  /** ImageDimension constants */
  /** ImageDimension constants */
  static constexpr unsigned int MarkerImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int MaskImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ReconstructionImageFilter, ImageToImageFilter);

  /** Set/Get the marker image. Traditionally, the marker image must
   * be pixelwise less than or equal to the mask image (for dilation),
   * however this filter implicitly applies a mask to force the
   * constraint to hold. The marker image the
   * image that is dilated by this filter. */
  void
  SetMarkerImage(const MarkerImageType *);

  const MarkerImageType *
  GetMarkerImage();

  /** Set/Get the mask image. The mask image is used to "mask" the
   * dilated marker image. The mask operation is a pixelwise
   * minimum. */
  void
  SetMaskImage(const MaskImageType *);

  const MaskImageType *
  GetMaskImage();

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
   * Perform a padding of the image internally to increase the performance
   * of the filter. UseInternalCopy can be set to false to reduce the memory
   * usage.
   */
  itkSetMacro(UseInternalCopy, bool);
  itkGetConstReferenceMacro(UseInternalCopy, bool);
  itkBooleanMacro(UseInternalCopy);

protected:
  ReconstructionImageFilter();
  ~ReconstructionImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** ValuedRegionalExtremaImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** ValuedRegionalExtremaImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  void
  GenerateData() override;

  /**
   * the value of the border - used in boundary condition.
   */
  typename TInputImage::PixelType m_MarkerValue;

private:
  bool m_FullyConnected;
  bool m_UseInternalCopy;

  using FaceCalculatorType = typename itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<OutputImageType>;

  using FaceListType = typename FaceCalculatorType::FaceListType;
  using FaceListTypeIt = typename FaceCalculatorType::FaceListType::iterator;

  using InputIteratorType = ImageRegionConstIterator<InputImageType>;
  using OutputIteratorType = ImageRegionIterator<OutputImageType>;

  using OutIndexType = typename OutputImageType::IndexType;
  using InIndexType = typename InputImageType::IndexType;
  using CNInputIterator = ConstShapedNeighborhoodIterator<InputImageType>;
  using NOutputIterator = ShapedNeighborhoodIterator<OutputImageType>;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkReconstructionImageFilter.hxx"
#endif

#endif

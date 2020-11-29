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
#ifndef itkBinaryReconstructionByDilationImageFilter_h
#define itkBinaryReconstructionByDilationImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkAttributeLabelObject.h"
#include "itkLabelMap.h"
#include "itkBinaryImageToLabelMapFilter.h"
#include "itkBinaryReconstructionLabelMapFilter.h"
#include "itkAttributeOpeningLabelMapFilter.h"
#include "itkLabelMapToBinaryImageFilter.h"


namespace itk
{

/**
 *\class BinaryReconstructionByDilationImageFilter
 *
 * \brief binary reconstruction by dilation of an image
 *
 * Reconstruction by dilation operates on a "marker" image and a "mask"
 * image, and is defined as the dilation of the marker image with
 * respect to the mask image iterated until stability.
 *
 * Geodesic morphology is described in Chapter 6.2 of Pierre Soille's
 * book "Morphological Image Analysis: Principles and Applications",
 * Second Edition, Springer, 2003.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \sa MorphologyImageFilter, ReconstructionByDilationImageFilter, BinaryReconstructionByErosionImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT BinaryReconstructionByDilationImageFilter
  : public ImageToImageFilter<TInputImage, TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryReconstructionByDilationImageFilter);

  /** Standard class type aliases. */
  using Self = BinaryReconstructionByDilationImageFilter;
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
  static constexpr unsigned int OutputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using LabelObjectType = AttributeLabelObject<SizeValueType, ImageDimension, bool>;
  using LabelMapType = LabelMap<LabelObjectType>;
  using LabelizerType = BinaryImageToLabelMapFilter<InputImageType, LabelMapType>;
  using ReconstructionType = BinaryReconstructionLabelMapFilter<LabelMapType, InputImageType>;
  using OpeningType = AttributeOpeningLabelMapFilter<LabelMapType>;
  using BinarizerType = LabelMapToBinaryImageFilter<LabelMapType, OutputImageType>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryReconstructionByDilationImageFilter, ImageToImageFilter);

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
  itkConceptMacro(InputEqualityComparableCheck, (Concept::EqualityComparable<InputImagePixelType>));
  itkConceptMacro(IntConvertibleToInputCheck, (Concept::Convertible<int, InputImagePixelType>));
  itkConceptMacro(InputOStreamWritableCheck, (Concept::OStreamWritable<InputImagePixelType>));
  // End concept checking
#endif

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<PixelType>::NonpositiveMin().
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

  /**
   * Set/Get the value used as "foreground" in the output image.
   * Defaults to NumericTraits<PixelType>::max().
   */
  itkSetMacro(ForegroundValue, OutputImagePixelType);
  itkGetConstMacro(ForegroundValue, OutputImagePixelType);

  /** Set/Get the marker image */
  itkSetInputMacro(MarkerImage, InputImageType);
  itkGetInputMacro(MarkerImage, InputImageType);

  /** Set/Get the mask image */
  itkSetInputMacro(MaskImage, InputImageType);
  itkGetInputMacro(MaskImage, InputImageType);

protected:
  BinaryReconstructionByDilationImageFilter();
  ~BinaryReconstructionByDilationImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** BinaryReconstructionByDilationImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** BinaryReconstructionByDilationImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicErodeImageFilter. */
  void
  GenerateData() override;

private:
  bool                 m_FullyConnected;
  OutputImagePixelType m_BackgroundValue;
  OutputImagePixelType m_ForegroundValue;
}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryReconstructionByDilationImageFilter.hxx"
#endif

#endif

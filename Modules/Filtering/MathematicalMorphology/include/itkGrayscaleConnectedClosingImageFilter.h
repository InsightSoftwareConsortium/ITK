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
#ifndef itkGrayscaleConnectedClosingImageFilter_h
#define itkGrayscaleConnectedClosingImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class GrayscaleConnectedClosingImageFilter
 * \brief Enhance pixels associated with a dark object (identified by
 * a seed pixel) where the dark object is surrounded by a brighter object.
 *
 * GrayscaleConnectedClosingImagefilter is useful for enhancing dark
 * objects that are surrounded by bright borders. This filter makes it
 * easier to threshold the image and extract just the object of
 * interest.
 *
 * Geodesic morphology and the connected closing algorithm are
 * described in Chapter 6 of Pierre Soille's book "Morphological Image
 * Analysis: Principles and Applications", Second Edition, Springer,
 * 2003.
 *
 * \sa GrayscaleGeodesicDilateImageFilter
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, GrayscaleFunctionDilateImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT GrayscaleConnectedClosingImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GrayscaleConnectedClosingImageFilter);

  /** Standard class type aliases. */
  using Self = GrayscaleConnectedClosingImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using InputImageIndexType = typename InputImageRegionType::IndexType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GrayscaleConnectedClosingImageFilter, ImageToImageFilter);

  /** Set/Get the seed pixel for the segmentation */
  itkSetMacro(Seed, InputImageIndexType);
  itkGetConstMacro(Seed, InputImageIndexType);

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
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
  itkConceptMacro(InputOStreamWritableCheck, (Concept::OStreamWritable<InputImagePixelType>));
  // End concept checking
#endif

protected:
  GrayscaleConnectedClosingImageFilter();
  ~GrayscaleConnectedClosingImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** GrayscaleConnectedClosingImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** GrayscaleConnectedClosingImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicDilateImageFilter. */
  void
  GenerateData() override;

private:
  unsigned long       m_NumberOfIterationsUsed{ 1 };
  InputImageIndexType m_Seed;

  bool m_FullyConnected;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGrayscaleConnectedClosingImageFilter.hxx"
#endif

#endif

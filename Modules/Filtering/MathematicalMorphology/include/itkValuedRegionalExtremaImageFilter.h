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
#ifndef itkValuedRegionalExtremaImageFilter_h
#define itkValuedRegionalExtremaImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkConstantBoundaryCondition.h"
#include <stack>

namespace itk
{
/** \class ValuedRegionalExtremaImageFilter
 *
 * \brief Uses a flooding algorithm to set all voxels that are not a
 * regional extrema to the max or min of the pixel type.
 *
 * This is the class used by ValuedRegionalMinimaImageFilter and
 * ValuedRegionalMaximaImageFilter. There is no suppression of regional
 * minima based on dynamics, as available in HMinimaImageFilter. This
 * flooding algorithm is a very simple one, but I'm not sure where it
 * came from - I certainly didn't invent it.
 *
 * Let's consider the case of regional minima.
 * The basic algorithm is:
 *    Boundary conditions are such that the image is logically
 *    surrounded by a border that is either maximal or minimal for the
 *    pixel type. An optimized version could explicitly set the border
 *    to avoid the need for boundary checks. For regional minima the
 *    boundary is set to the maximal value for the pixel type.
 *
 *    Pixels are visited in raster order. The neighbors of each pixel
 *    are examined. If any neighbor is greater than the centre, then
 *    the centre pixel cannot be a regional minima. The centre pixel
 *    is part of a flat region (consisting of at least one pixel) that
 *    is therefore not a regional minima either. This region is set to
 *    the maximum value for the pixel type using a flooding algorithm.
 *
 *    There are some minor complications that prevent pixels being
 *    examined more than once -- basically check that the output value
 *    is less than the maximum for the pixel type.
 *
 * The implementation uses the functor model from itkMaximumImageFilter.
 *
 *
 * This code was contributed in the Insight Journal paper:
 * "Finding regional extrema - methods and performance"
 * by Beare R., Lehmann G.
 * https://www.insight-journal.org/browse/publication/65
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 *
 * \sa ValuedRegionalMinimaImageFilter, ValuedRegionalMaximaImageFilter,
 * \sa HMinimaImageFilter
 *
 * \ingroup MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */

template <typename TInputImage, typename TOutputImage, typename TFunction1, typename TFunction2>
class ITK_TEMPLATE_EXPORT ValuedRegionalExtremaImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ValuedRegionalExtremaImageFilter);

  /** Standard class type aliases. */
  using Self = ValuedRegionalExtremaImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using ISizeType = typename InputImageType::SizeType;
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
  itkTypeMacro(ValuedRegionalExtremaImageFilter, ImageToImageFilter);

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
   * Set/Get the value used to mark all pixels which are not extrema.
   */
  itkSetMacro(MarkerValue, typename TInputImage::PixelType);
  itkGetConstReferenceMacro(MarkerValue, typename TInputImage::PixelType);

  /**
   * Get whether the image is flat or not.
   */
  itkGetConstMacro(Flat, bool);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasPixelTraitsCheck, (Concept::HasPixelTraits<InputImagePixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputImagePixelType>));
  // End concept checking
#endif

protected:
  ValuedRegionalExtremaImageFilter();
  ~ValuedRegionalExtremaImageFilter() override = default;
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

private:
  typename TInputImage::PixelType m_MarkerValue;

  bool m_FullyConnected{ false };
  bool m_Flat{ false };

  using OutIndexType = typename OutputImageType::IndexType;
  using InIndexType = typename InputImageType::IndexType;
  using ConstInputIterator = ConstShapedNeighborhoodIterator<InputImageType>;
  using NOutputIterator = ShapedNeighborhoodIterator<OutputImageType>;
  using IndexStack = std::stack<OutIndexType>;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkValuedRegionalExtremaImageFilter.hxx"
#endif

#endif

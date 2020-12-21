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
#ifndef itkMaskedRankImageFilter_h
#define itkMaskedRankImageFilter_h

#include "itkMaskedMovingHistogramImageFilter.h"
#include <list>
#include <map>
#include <set>
#include "itkRankHistogram.h"
#include "itkFlatStructuringElement.h"

namespace itk
{
/**
 * \class MaskedRankImageFilter
 * \brief Rank filter of a greyscale image
 *
 * Nonlinear filter in which each output pixel is a user defined
 * rank of input pixels in a user defined neighborhood. The default
 * rank is 0.5 (median). The boundary conditions are different to the
 * standard itkMedianImageFilter. In this filter the neighborhood is
 * cropped at the boundary, and is therefore smaller.
 *
 * This filter uses a recursive implementation - essentially the one
 * by Huang 1979, I believe, to compute the rank,
 * and is therefore usually a lot faster than the direct
 * implementation. The extensions to Huang are support for arbitrary
 * pixel types (using c++ maps) and arbitrary neighborhoods. I presume
 * that these are not new ideas.
 *
 * This filter is based on the sliding window code from the
 * consolidatedMorphology package on InsightJournal.
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://www.insight-journal.org/browse/publication/160
 *
 * \author Richard Beare
 * \ingroup ITKMathematicalMorphology
 */

template <typename TInputImage,
          typename TMaskImage,
          typename TOutputImage,
          typename TKernel = FlatStructuringElement<TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT MaskedRankImageFilter
  : public MaskedMovingHistogramImageFilter<TInputImage,
                                            TMaskImage,
                                            TOutputImage,
                                            TKernel,
                                            Function::RankHistogram<typename TInputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MaskedRankImageFilter);

  /** Standard class type aliases. */
  using Self = MaskedRankImageFilter;
  using Superclass = MaskedMovingHistogramImageFilter<TInputImage,
                                                      TMaskImage,
                                                      TOutputImage,
                                                      TKernel,
                                                      Function::RankHistogram<typename TInputImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MaskedRankImageFilter, MovingHistogramImageFilter);

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
  using InputPixelType = typename TInputImage::PixelType;

  using HistogramType = typename Superclass::HistogramType;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Kernel type alias. */
  using KernelType = TKernel;

  /** Kernel (structuring element) iterator. */
  using KernelIteratorType = typename KernelType::ConstIterator;

  /** n-dimensional Kernel radius. */
  using RadiusType = typename KernelType::SizeType;

  itkSetClampMacro(Rank, float, 0.0, 1.0);
  itkGetConstMacro(Rank, float);

  bool
  GetUseVectorBasedAlgorithm() const
  {
    return HistogramType::UseVectorBasedAlgorithm();
  }

protected:
  MaskedRankImageFilter();
  ~MaskedRankImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  ConfigureHistogram(HistogramType & histogram) override;

private:
  float m_Rank;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMaskedRankImageFilter.hxx"
#endif

#endif

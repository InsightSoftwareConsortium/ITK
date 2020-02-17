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
#ifndef itkFastApproximateRankImageFilter_h
#define itkFastApproximateRankImageFilter_h

#include "itkMiniPipelineSeparableImageFilter.h"
#include "itkRankImageFilter.h"

namespace itk
{
/**
 * \class FastApproximateRankImageFilter
 * \brief A separable rank filter
 *
 * Medians aren't separable, but if you want a large robust smoother
 * to be relatively quick then it is worthwhile pretending that they
 * are.
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://hdl.handle.net/1926/555
 * http://www.insight-journal.org/browse/publication/160
 *
 * \author Richard Beare
 * \ingroup ITKReview
 */

template <typename TInputImage, typename TOutputImage>
class FastApproximateRankImageFilter
  : public MiniPipelineSeparableImageFilter<
      TInputImage,
      TOutputImage,
      RankImageFilter<TInputImage, TInputImage, FlatStructuringElement<TInputImage::ImageDimension>>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FastApproximateRankImageFilter);

  /** Standard class type aliases. */
  using Self = FastApproximateRankImageFilter;
  using Superclass = MiniPipelineSeparableImageFilter<
    TInputImage,
    TOutputImage,
    RankImageFilter<TInputImage, TInputImage, FlatStructuringElement<TInputImage::ImageDimension>>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(FastApproximateRankImageFilter, MiniPipelineSeparableImageFilter);

  /** Image related type alias. */
  using InputImageType = TInputImage;
  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename TInputImage::SizeType;
  using IndexType = typename TInputImage::IndexType;
  using PixelType = typename TInputImage::PixelType;
  using OffsetType = typename TInputImage::OffsetType;
  using FilterType = typename Superclass::FilterType;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  /** n-dimensional Kernel radius. */
  using RadiusType = typename TInputImage::SizeType;

  void
  SetRank(float rank)
  {
    if (m_Rank != rank)
    {
      m_Rank = rank;
      for (unsigned i = 0; i < TInputImage::ImageDimension - 1; i++)
      {
        this->m_Filters[i]->SetRank(m_Rank);
      }
      this->Modified();
    }
  }

  itkGetConstMacro(Rank, float);

protected:
  FastApproximateRankImageFilter()
  {
    // to avoid valgrind warning
    m_Rank = 0.0;
    this->SetRank(0.5);
  }

  ~FastApproximateRankImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "Rank: " << m_Rank << std::endl;
  }

private:
  float m_Rank;
};
} // namespace itk

#endif

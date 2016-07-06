/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

template< typename TInputImage, typename TOutputImage >
class FastApproximateRankImageFilter:
  public MiniPipelineSeparableImageFilter< TInputImage, TOutputImage,
                                           RankImageFilter< TInputImage, TInputImage,
                                                            FlatStructuringElement< TInputImage::ImageDimension > > >
{
public:
  /** Standard class typedefs. */
  typedef FastApproximateRankImageFilter Self;
  typedef MiniPipelineSeparableImageFilter< TInputImage, TOutputImage,
          RankImageFilter< TInputImage, TInputImage,
                      FlatStructuringElement< TInputImage::ImageDimension > > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(FastApproximateRankImageFilter, MiniPipelineSeparableImageFilter);

  /** Image related typedefs. */
  typedef TInputImage                      InputImageType;
  typedef typename TInputImage::RegionType RegionType;
  typedef typename TInputImage::SizeType   SizeType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::PixelType  PixelType;
  typedef typename TInputImage::OffsetType OffsetType;
  typedef typename Superclass::FilterType  FilterType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  /** n-dimensional Kernel radius. */
  typedef typename TInputImage::SizeType RadiusType;

  void SetRank(float rank)
  {
    if ( m_Rank != rank )
      {
      m_Rank = rank;
      for ( unsigned i = 0; i < TInputImage::ImageDimension - 1; i++ )
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

  ~FastApproximateRankImageFilter() {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "Rank: " << m_Rank << std::endl;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FastApproximateRankImageFilter);

  float m_Rank;
};
}

#endif

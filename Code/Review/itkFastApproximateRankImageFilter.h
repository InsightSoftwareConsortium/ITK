/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastApproximateRankImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFastApproximateRankImageFilter_h
#define __itkFastApproximateRankImageFilter_h

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
 * \author Richard Beare
 */

template< class TInputImage, class TOutputImage >
class ITK_EXPORT FastApproximateRankImageFilter:
  public MiniPipelineSeparableImageFilter< TInputImage, TOutputImage,
                                           RankImageFilter< TInputImage, TInputImage,
                                                            FlatStructuringElement< ::itk::GetImageDimension<
                                                                                      TInputImage >::ImageDimension > > >
{
public:
  /** Standard class typedefs. */
  typedef FastApproximateRankImageFilter Self;
  typedef MiniPipelineSeparableImageFilter< TInputImage, TOutputImage, RankImageFilter< TInputImage, TInputImage,
                                                                                        FlatStructuringElement< ::itk::
                                                                                                                GetImageDimension
                                                                                                                <
                                                                                                                  TInputImage >::ImageDimension > > > Superclass;

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

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "Rank: " << m_Rank << std::endl;
  }

private:
  FastApproximateRankImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                 //purposely not implemented

  float m_Rank;
};
}

#endif

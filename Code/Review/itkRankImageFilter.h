/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRankImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRankImageFilter_h
#define __itkRankImageFilter_h

#include "itkMovingHistogramImageFilter.h"
#include <list>
#include <map>
#include <set>
#include "itkRankHistogram.h"
#include "itkFlatStructuringElement.h"

namespace itk
{
/**
 * \class RankImageFilter
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
 * implementation. The extensions to Huang are support for arbitary
 * pixel types (using c++ maps) and arbitary neighborhoods. I presume
 * that these are not new ideas.
 *
 * This filter is based on the sliding window code from the
 * consolidatedMorphology package on InsightJournal.
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * \author Richard Beare
 */

template< class TInputImage, class TOutputImage, class TKernel =
            FlatStructuringElement< ::itk::GetImageDimension< TInputImage >::ImageDimension > >
class ITK_EXPORT RankImageFilter:
  public MovingHistogramImageFilter< TInputImage, TOutputImage, TKernel,
                                     RankHistogram< ITK_TYPENAME TInputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef RankImageFilter            Self;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef MovingHistogramImageFilter< TInputImage, TOutputImage, TKernel,
                                      RankHistogram< typename TInputImage::PixelType > > Superclass;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(RankImageFilter,
               MovingHistogramImageFilter);

  /** Image related typedefs. */
  typedef TInputImage                                InputImageType;
  typedef TOutputImage                               OutputImageType;
  typedef typename TInputImage::RegionType           RegionType;
  typedef typename TInputImage::SizeType             SizeType;
  typedef typename TInputImage::IndexType            IndexType;
  typedef typename TInputImage::PixelType            PixelType;
  typedef typename TInputImage::OffsetType           OffsetType;
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename TOutputImage::PixelType           OutputPixelType;
  typedef typename TInputImage::PixelType            InputPixelType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Kernel typedef. */
  typedef TKernel KernelType;

  /** Kernel (structuring element) iterator. */
  typedef typename KernelType::ConstIterator KernelIteratorType;

  /** n-dimensional Kernel radius. */
  typedef typename KernelType::SizeType RadiusType;

  itkSetMacro(Rank, float)
  itkGetConstMacro(Rank, float)
protected:
  RankImageFilter();
  ~RankImageFilter() {}

  typedef RankHistogram< InputPixelType > HistogramType;

  typedef RankHistogramVec< InputPixelType, std::less< InputPixelType > >  VHistogram;
  typedef RankHistogramMap< InputPixelType, std::less< InputPixelType >  > MHistogram;

  void PrintSelf(std::ostream & os, Indent indent) const;

  bool UseVectorBasedHistogram()
  {
    // bool, short and char are acceptable for vector based algorithm: they do
    // not require
    // too much memory. Other types are not usable with that algorithm
    return typeid( InputPixelType ) == typeid( unsigned char )
           || typeid( InputPixelType ) == typeid( signed char )
//       || typeid(InputPixelType) == typeid(unsigned short)
//       || typeid(InputPixelType) == typeid(signed short)
           || typeid( InputPixelType ) == typeid( bool );
  }

  virtual HistogramType * NewHistogram();

private:
  RankImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);  //purposely not implemented

  float m_Rank;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRankImageFilter.txx"
#endif

#endif

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatDenseHistogram.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkStatDenseHistogram_txx
#define _itkStatDenseHistogram_txx

#include "itkStatDenseHistogram.h"

namespace itk
{

template<class TBin, unsigned int HistogramDimension, class TFeature>
const TBin
DenseHistogram<TBin, HistogramDimension, TFeature>
::GetFrequency(const PointType point)
{
  IndexType index;
  index = this->GetIndex(point);
  return m_Histogram->GetPixel(index);
}

template<class TBin, unsigned int HistogramDimension, class TFeature>
void
DenseHistogram<TBin, HistogramDimension, TFeature>
::SetFrequency(const PointType point, const TBin value) 
{ 
  IndexType index;
  index = this->GetIndex(point);
  m_Histogram->SetPixel(index, value);
}



template<class TBin, unsigned int HistogramDimension, class TFeature>
void
DenseHistogram<TBin, HistogramDimension, TFeature>
::AllocateHistogram()
{
  int i;

  IndexType   start;
  for ( i = 0; i < HistogramDimension; i++)
    {
    start[i] = 0;                   // initial coordinate of the image
    }

  SizeType    size;
  for ( i = 0; i < HistogramDimension; i++)
    {
    size[i]  = GetDimensionSize(i); // the size of the HistogramDimension image
    }
  HistogramType::RegionType  region;
  region.SetSize( size );
  region.SetIndex( start );
  
  m_Histogram = HistogramType::New();

  m_Histogram->SetLargestPossibleRegion( region );
  m_Histogram->SetBufferedRegion( region );
  m_Histogram->SetRequestedRegion( region );

  // Allocate histogram by Allocate() in Image class
  m_Histogram->Allocate();

}


} // end of namespace

#endif

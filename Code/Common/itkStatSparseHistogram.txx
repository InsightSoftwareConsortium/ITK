/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatSparseHistogram.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <map>

#include "itkStatSparseHistogram.h"

namespace itk
{

template<class TBin, unsigned int HistogramDimension>
const TBin &
SparseHistogram<TBin, HistogramDimension>
::GetFrequency(const PointType & point)
{
  IndexType index;
  index = GetIndex(point);
  return m_Histogram[ComputeOffset(index)];
}

template<class TBin, unsigned int HistogramDimension>
void
SparseHistogram<TBin, HistogramDimension>
::SetFrequency(const PointType & point, const TBin& value) 
{ 
  IndexType index;
  index = GetIndex(point);
  m_Histogram[ComputeOffset(index)] = value;
}

template<class TBin, unsigned int HistogramDimension >
void
SparseHistogram<TBin, HistogramDimension>
::ComputeOffsetTable()
{
  unsigned long num = 1;
  m_OffsetTable[0] = num;
  for ( unsigned int i=0; i < HistogramDimension; i++)
    {
    num *= m_Size[i];
    m_OffsetTable[i+1] = num;
    }
}

template<class TBin, unsigned int HistogramDimension >
unsigned long
SparseHistogram<TBin, HistogramDimension>
::ComputeOffset(const IndexType & index)
{
  unsigned long offset = 0;
  unsigned int i;
  for (i=HistogramDimension-1; i > 0; i--)
    {
    offset += index[i]*m_OffsetTable[i];
    }
  offset += index[i];
  return offset;
}

} // end of namespace


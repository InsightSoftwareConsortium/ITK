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
#ifndef _itkStatSparseHistogram_txx
#define _itkStatSparseHistogram_txx

#include "itkStatSparseHistogram.h"

namespace itk
{

template<class TBin, unsigned int HistogramDimension, class TFeature>
const TBin 
SparseHistogram<TBin, HistogramDimension, TFeature>
::GetFrequency(const IndexType index)
{
  unsigned long offset = ComputeOffset(index);
  if ( m_Histogram.find(offset) == m_Histogram.end() )
    return 0;
  else
    return m_Histogram[offset];
}

template<class TBin, unsigned int HistogramDimension, class TFeature>
const TBin
SparseHistogram<TBin, HistogramDimension, TFeature>
::GetFrequency(const PointType point)
{
  IndexType index;
  index = GetIndex(point);
  unsigned long offset = ComputeOffset(index);
  if ( m_Histogram.find(offset) == m_Histogram.end() )
    return 0;
  else
    return m_Histogram[offset];
}

template<class TBin, unsigned int HistogramDimension, class TFeature>
void
SparseHistogram<TBin, HistogramDimension, TFeature>
::SetFrequency(const PointType point, const TBin value) 
{ 
  IndexType index;
  index = GetIndex(point);
  m_Histogram[ComputeOffset(index)] = value;
}

template<class TBin, unsigned int HistogramDimension, class TFeature>
void
SparseHistogram<TBin, HistogramDimension, TFeature>
::Allocate() 
{   
  int dim;
  m_Min.resize(HistogramDimension);
  for ( dim = 0; dim < HistogramDimension; dim++)
    {
    m_Min[dim].resize(m_Size[dim]);
    } 

  m_Max.resize(HistogramDimension);
  for ( dim = 0; dim < HistogramDimension; dim++)
    {
    m_Max[dim].resize(m_Size[dim]);
    } 
  this->ComputeOffsetTable(); 
}


template<class TBin, unsigned int HistogramDimension, class TFeature>
void
SparseHistogram<TBin, HistogramDimension, TFeature>
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

template<class TBin, unsigned int HistogramDimension, class TFeature>
unsigned long
SparseHistogram<TBin, HistogramDimension, TFeature>
::ComputeOffset(const IndexType index)
{
  unsigned long offset = 0;

  for (int i=HistogramDimension-1; i > 0; i--)
    {
    offset += index[i]*m_OffsetTable[i];
    }
  offset += index[0];
  return offset;
}

template<class TBin, unsigned int HistogramDimension, class TFeature>
Index<HistogramDimension>
SparseHistogram<TBin, HistogramDimension, TFeature>
::ComputeIndex(unsigned long offset)
{
  IndexType index;
  for (int i=HistogramDimension-1; i > 0; i--)
    {
    index[i] = offset/m_OffsetTable[i];
    offset -= (index[i] * m_OffsetTable[i]);
    }
  index[0] = offset;
  return index;
}

} // end of namespace

#endif

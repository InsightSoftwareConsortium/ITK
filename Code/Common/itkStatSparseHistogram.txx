/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatSparseHistogram.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

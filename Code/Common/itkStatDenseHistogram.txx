/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatDenseHistogram.txx
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

  int i;

  IndexType   start;
  for ( i = 0; i < HistogramDimension; i++)
    {
    start[i] = 0;                   // initial coordinate of the image
    }

  SizeType    size;
  size  = GetSize(); // the size of the HistogramDimension image
   
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

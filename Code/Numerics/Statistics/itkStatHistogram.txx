/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatHistogram.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkStatHistogram_txx
#define _itkStatHistogram_txx

#include "itkStatHistogram.h"

namespace itk
{

template<class TBin, unsigned int HistogramDimension, class TFeature>
Index<HistogramDimension>
Histogram<TBin, HistogramDimension, TFeature>
::GetIndex(const PointType point)
{
  int dim, nbin, numBin;  // dimension, bin number, number of bins
  IndexType index;

  for ( dim=0; dim< HistogramDimension; dim++)
    {
    numBin = m_Min[dim].size();
    if ( point[dim] < m_Min[dim][0] )
      {
      index[dim] = 0;
      }
    else if ( point[dim] >= m_Max[dim][numBin-1] )
      {
      index[dim] = numBin-1;
      }
    else
      {
      for ( nbin = 0; nbin < numBin; nbin++)
        {
        if (  (m_Min[dim][nbin] <= point[dim]) 
           && (point[dim]  < m_Max[dim][nbin]) )
          {
            index[dim] = nbin;
            break;  // break for(nbin ...) loop and do for (dim ... ) loop
          }
        } // end of for()
      }  // end of if
    } // end of for()

  return index;
    
}

template<class TBin, unsigned int HistogramDimension, class TFeature>
TFeature
Histogram<TBin, HistogramDimension, TFeature>
::GetBinMinFromValue(unsigned int dimension, const TFeature value ) const
{
  // If the value is lower than any of min value in the histogram,
  // it returns the lowest min value
  if ( value <= this->m_Min[dimension][0] )
    {
    return this->m_Min[dimension][0];
    }

  // If the value is higher than any of min value in the histogram,
  // it returns the highest min value
  if ( value >= m_Min[dimension][m_Size[dimension]-1] )
    {
    return m_Min[dimension][this->m_Size[dimension]-1];
    }

  for ( int i=0; i < this->m_Size[dimension]; i++ )
    {
    if (  (value >= this->m_Min[dimension][i])
       && (value <  this->m_Max[dimension][i])  )
      {
      return this->m_Min[dimension][i];
      }
    }
}

template<class TBin, unsigned int HistogramDimension, class TFeature>
TFeature
Histogram<TBin, HistogramDimension, TFeature>
::GetBinMaxFromValue(unsigned int dimension, const TFeature value ) const
{
  // If the value is lower than any of max value in the histogram,
  // it returns the lowest max value
  if ( value <= this->m_Max[dimension][0] )
    {
    return this->m_Max[dimension][0];
    }

  // If the value is higher than any of max value in the histogram,
  // it returns the highest max value
  if ( value >= m_Max[dimension][m_Size[dimension]-1] )
    {
    return m_Max[dimension][this->m_Size[dimension]-1];
    }

  for ( int i=0; i < this->m_Size[dimension]; i++ )
    {
    if (  (value >= this->m_Min[dimension][i])
       && (value <  this->m_Max[dimension][i])  )
      {
      return this->m_Max[dimension][i];
      }
    }
}

template<class TBin, unsigned int HistogramDimension, class TFeature>
Point<TFeature, HistogramDimension>
Histogram<TBin, HistogramDimension, TFeature>
::GetHistogramMinFromValue(const PointType point) 
{
  PointType pnt;
  for ( int i=0; i < HistogramDimension; i++ )
    {
    pnt[i] = this->GetDimensionMinByValue(i,point[i]);
    }
  return pnt;
}

template<class TBin, unsigned int HistogramDimension, class TFeature>
Point<TFeature, HistogramDimension>
Histogram<TBin, HistogramDimension, TFeature>
::GetHistogramMaxFromValue(const PointType point) 
{
  PointType pnt;
  for ( int i=0; i < HistogramDimension; i++ )
    {
    pnt[i] = this->GetDimensionMaxByValue(i,point[i]);
    }
  return pnt;

}

template<class TBin, unsigned int HistogramDimension, class TFeature>
Point<TFeature, HistogramDimension>
Histogram<TBin, HistogramDimension, TFeature>
::GetHistogramMinFromIndex(const IndexType index) 
{
  PointType pnt;
  for ( int i=0; i < HistogramDimension; i++ )
    {
    pnt[i] = this->GetBinMin(i, index[i]);
    }
  return pnt;
}

template<class TBin, unsigned int HistogramDimension, class TFeature>
Point<TFeature, HistogramDimension>
Histogram<TBin, HistogramDimension, TFeature>
::GetHistogramMaxFromIndex(const IndexType index) 
{
  PointType pnt;
  for ( int i=0; i < HistogramDimension; i++ )
    {
    pnt[i] = this->GetBinMax(i, index[i]);
    }
  return pnt;
}



template<class TBin, unsigned int HistogramDimension, class TFeature>
TFeature
Histogram<TBin, HistogramDimension, TFeature>
::GetFeature(const IndexType index, int dimension)
{
  int nbin = index[dimension];
  return (m_Min[dimension][nbin] + m_Max[dimension][nbin])/2;
}

template<class TBin, unsigned int HistogramDimension, class TFeature>
TFeature
Histogram<TBin, HistogramDimension, TFeature>
::GetFeature(const PointType point, int dimension)
{
  IndexType index = GetIndex(point);
  int nbin = index[dimension];
  return (m_Min[dimension][nbin] + m_Max[dimension][nbin])/2;  
}

template<class TBin, unsigned int HistogramDimension, class TFeature>
Point<TFeature, HistogramDimension>
Histogram<TBin, HistogramDimension, TFeature>
::GetFeature(const IndexType index)
{
  int nbin = index[dimension];
  return (m_Min[dimension] + m_Max[dimension])/2;
}

template<class TBin, unsigned int HistogramDimension, class TFeature>
Point<TFeature, HistogramDimension>
Histogram<TBin, HistogramDimension, TFeature>
::GetFeature(const PointType point)
{
  IndexType index = GetIndex(point);
  int nbin = index[dimension];
  return (m_Min[dimension] + m_Max[dimension])/2;  
}

} // end of namespace

#endif

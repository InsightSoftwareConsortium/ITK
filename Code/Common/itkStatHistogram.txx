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

template<class TBin, unsigned int HistogramDimension>
Index<HistogramDimension>
Histogram<TBin, HistogramDimension>
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

template<class TBin, unsigned int HistogramDimension>
double
Histogram<TBin, HistogramDimension>
::GetBinMinFromValue(unsigned int dimension, const double value ) const
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

template<class TBin, unsigned int HistogramDimension>
double
Histogram<TBin, HistogramDimension>
::GetBinMaxFromValue(unsigned int dimension, const double value ) const
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

template<class TBin, unsigned int HistogramDimension>
Point<TBin, HistogramDimension>
Histogram<TBin, HistogramDimension>
::GetHistogramMinFromValue(const PointType point) 
{
  PointType pnt;
  for ( int i=0; i < HistogramDimension; i++ )
    {
    pnt[i] = this->GetDimensionMinByValue(i,point[i]);
    }
  return pnt;
}

template<class TBin, unsigned int HistogramDimension>
Point<TBin, HistogramDimension>
Histogram<TBin, HistogramDimension>
::GetHistogramMaxFromValue(const PointType point) 
{
  PointType pnt;
  for ( int i=0; i < HistogramDimension; i++ )
    {
    pnt[i] = this->GetDimensionMaxByValue(i,point[i]);
    }
  return pnt;

}

template<class TBin, unsigned int HistogramDimension>
Point<TBin, HistogramDimension>
Histogram<TBin, HistogramDimension>
::GetHistogramMin(const IndexType index) 
{
  PointType pnt;
  for ( int i=0; i < HistogramDimension; i++ )
    {
    pnt[i] = this->GetBinMin(i, index[i]);
    }
  return pnt;
}

template<class TBin, unsigned int HistogramDimension>
Point<TBin, HistogramDimension>
Histogram<TBin, HistogramDimension>
::GetHistogramMax(const IndexType index) 
{
  PointType pnt;
  for ( int i=0; i < HistogramDimension; i++ )
    {
    pnt[i] = this->GetBinMax(i, index[i]);
    }
  return pnt;
}

template<class TBin, unsigned int HistogramDimension>
void
Histogram<TBin, HistogramDimension>
::AllocateMins( )
{
  m_Min.resize(HistogramDimension);
  for ( int dim = 0; dim < HistogramDimension; dim++)
    {
    m_Min[dim].resize(m_Size[dim]);
    } 
}

template<class TBin, unsigned int HistogramDimension>
void
Histogram<TBin, HistogramDimension>
::AllocateMaxs( )
{
  m_Max.resize(HistogramDimension);
  for ( int dim = 0; dim < HistogramDimension; dim++)
    {
    m_Max[dim].resize(m_Size[dim]);
    } 
}

} // end of namespace


#endif

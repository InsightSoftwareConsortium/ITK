/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHistogram.txx
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
#ifndef _itkHistogram_txx
#define _itkHistogram_txx

#include "itkHistogram.h"
#include "itkNumericTraits.h"

namespace itk{ 
  namespace Statistics{

template< class TMeasurement, unsigned int VMeasurementVectorSize,
          class TFrequencyContainer>
Histogram<TMeasurement, VMeasurementVectorSize, TFrequencyContainer>
::Histogram()
{
  // all measurement vectors are sored
  SetSortedFlag(true) ;
  // supports GetFrequency method
  SetSupportingFrequencyFlag(true) ;
  // no dupliates
  SetAllowingDuplicatesFlag(false) ;

  m_FrequencyContainer = FrequencyContainerType::New() ;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize,
          class TFrequencyContainer>
void
Histogram<TMeasurement, VMeasurementVectorSize, TFrequencyContainer>
::Initialize(SizeType size)
{
  m_Size = size ;
  
  // creates offset table which will be used for generation of
  // instance identifiers.
  InstanceIdentifier num = 1 ;
  
  m_OffsetTable[0] = num ;
  for (unsigned int i = 0 ; i < MeasurementVectorSize ; i++)
    {
      num *= m_Size[i] ;
      m_OffsetTable[i + 1] = num ;
    }

  // adjust the sizes of min max value containers 
  int dim;
  m_Min.resize(MeasurementVectorSize);
  for ( dim = 0; dim < MeasurementVectorSize; dim++)
    {
    m_Min[dim].resize(m_Size[dim]);
    } 

  m_Max.resize(MeasurementVectorSize);
  for ( dim = 0; dim < MeasurementVectorSize; dim++)
    {
    m_Max[dim].resize(m_Size[dim]);
    } 

  // initialize the frequency container
  m_FrequencyContainer->Initialize(m_OffsetTable[VMeasurementVectorSize]) ;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize, 
  class TFrequencyContainer>
Histogram<TMeasurement, VMeasurementVectorSize,
          TFrequencyContainer>::IndexType
Histogram<TMeasurement, VMeasurementVectorSize, TFrequencyContainer>
::GetIndex(const MeasurementVectorType measurement) 
{

  int dim, nbin, numBin ;  // dimension, bin number, number of bins
  IndexType index ;

  for ( dim=0; dim< MeasurementVectorSize; dim++)
    {
    numBin = m_Min[dim].size();
    if ( measurement[dim] < m_Min[dim][0] )
      {
      index[dim] = 0;
      }
    else if ( measurement[dim] >= m_Max[dim][numBin-1] )
      {
      index[dim] = numBin-1;
      }
    else
      {
      for ( nbin = 0; nbin < numBin; nbin++)
        {
        if (  (m_Min[dim][nbin] <= measurement[dim]) 
           && (measurement[dim]  < m_Max[dim][nbin]) )
          {
            index[dim] = nbin;
            break;  // break for(nbin ...) loop and do for (dim ... ) loop
          }
        } // end of for()
      }  // end of if
    } // end of for()

  return index;
    
}

template< class TMeasurement, unsigned int VMeasurementVectorSize,
          class TFrequencyContainer>
Histogram<TMeasurement, VMeasurementVectorSize, 
          TFrequencyContainer>::IndexType
Histogram<TMeasurement, VMeasurementVectorSize, TFrequencyContainer>
::GetIndex( InstanceIdentifier id)
{
  InstanceIdentifier id2 = id ;
  IndexType index;
  
  for (int i = MeasurementVectorSize - 1 ; i > 0 ; i--)
    {
      index[i] = static_cast<IndexValueType>(id2 / m_OffsetTable[i]);
      id2 -= (index[i] * m_OffsetTable[i]);
    }
  index[0] = static_cast<IndexValueType>(id2);
  
  return index;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize,
          class TFrequencyContainer >
Histogram<TMeasurement, VMeasurementVectorSize,
          TFrequencyContainer>::InstanceIdentifier
Histogram<TMeasurement, VMeasurementVectorSize, TFrequencyContainer>
::GetInstanceIdentifier(const IndexType index)  
{
  InstanceIdentifier id = 0 ;
  for (int i= MeasurementVectorSize - 1 ; i > 0 ; i-- )
    {
      id += index[i] * m_OffsetTable[i];
    }
  
  id += index[0] ;
  
  return id ;
  }


template< class TMeasurement, unsigned int VMeasurementVectorSize,
         class TFrequencyContainer >
Histogram<TMeasurement, VMeasurementVectorSize, 
          TFrequencyContainer>::MeasurementType
Histogram<TMeasurement, VMeasurementVectorSize, TFrequencyContainer>
::GetBinMinFromValue(unsigned int dimension, const float value ) 
{
  // If the value is lower than any of min value in the Histogram,
  // it returns the lowest min value
  if ( value <= this->m_Min[dimension][0] )
    {
    return this->m_Min[dimension][0];
    }

  // If the value is higher than any of min value in the Histogram,
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

template< class TMeasurement, unsigned int VMeasurementVectorSize, 
         class TFrequencyContainer >
Histogram< TMeasurement, VMeasurementVectorSize, 
           TFrequencyContainer >::MeasurementType
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::GetBinMaxFromValue(unsigned int dimension, const float value ) 
{
  // If the value is lower than any of max value in the Histogram,
  // it returns the lowest max value
  if ( value <= this->m_Max[dimension][0] )
    {
    return this->m_Max[dimension][0];
    }

  // If the value is higher than any of max value in the Histogram,
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

template< class TMeasurement, unsigned int VMeasurementVectorSize, 
          class TFrequencyContainer >
Histogram< TMeasurement, VMeasurementVectorSize, 
           TFrequencyContainer >::MeasurementVectorType
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::GetHistogramMinFromValue(const MeasurementVectorType measurement) 
{
  MeasurementVectorType pnt;
  for ( int i=0; i < MeasurementVectorSize; i++ )
    {
    pnt[i] = this->GetDimensionMinByValue(i,measurement[i]);
    }
  return pnt;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize,
          class TFrequencyContainer >
Histogram< TMeasurement, VMeasurementVectorSize,
           TFrequencyContainer >::MeasurementVectorType
Histogram<TMeasurement, VMeasurementVectorSize, TFrequencyContainer>
::GetHistogramMaxFromValue(const MeasurementVectorType measurement) 
{
  MeasurementVectorType pnt;
  for ( int i=0; i < MeasurementVectorSize; i++ )
    {
    pnt[i] = this->GetDimensionMaxByValue(i,measurement[i]);
    }
  return pnt;

}

template< class TMeasurement, unsigned int VMeasurementVectorSize,
          class TFrequencyContainer >
Histogram< TMeasurement, VMeasurementVectorSize,
           TFrequencyContainer >::MeasurementVectorType
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::GetHistogramMinFromIndex(const IndexType index) 
{
  MeasurementVectorType pnt;
  for ( int i=0; i < MeasurementVectorSize; i++ )
    {
    pnt[i] = this->GetBinMin(i, index[i]);
    }
  return pnt;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize,
          class TFrequencyContainer >
Histogram< TMeasurement, VMeasurementVectorSize,
           TFrequencyContainer >::MeasurementVectorType
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::GetHistogramMaxFromIndex(const IndexType index) 
{
  MeasurementVectorType pnt;
  for ( int i=0; i < MeasurementVectorSize; i++ )
    {
    pnt[i] = this->GetBinMax(i, index[i]);
    }
  return pnt;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize, 
          class TFrequencyContainer >
Histogram< TMeasurement, VMeasurementVectorSize, 
           TFrequencyContainer >::MeasurementVectorType
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::GetMeasurementVector(const IndexType index) 
{
  MeasurementVectorType f;
  for ( int i = 0; i < MeasurementVectorSize; i++)
    {
      f[i] =  (m_Min[i][index[i]] + m_Max[i][index[i]])/2;
    }
  return f;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize, 
          class TFrequencyContainer >
Histogram< TMeasurement, VMeasurementVectorSize, 
           TFrequencyContainer >::MeasurementVectorType
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::GetMeasurementVector(const InstanceIdentifier id) 
{
  IndexType index = GetIndex(id) ;
  return GetMeasurementVector(index) ;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize, 
          class TFrequencyContainer >
void
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::SetFrequency(const IndexType index, const FrequencyType value) 
{
  this->SetFrequency(GetInstanceIdentifier(index), value) ;
}
  
template< class TMeasurement, unsigned int VMeasurementVectorSize, 
          class TFrequencyContainer >
void
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::SetFrequency(const MeasurementVectorType measurement, const FrequencyType value) 
{
  this->SetFrequency(GetInstanceIdentifier(GetIndex(measurement)), value) ;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize, 
          class TFrequencyContainer >
void
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::IncreaseFrequency(const IndexType index, const FrequencyType value)
{
  this->IncreaseFrequency(GetInstanceIdentifier(index), value) ;
}
  
template< class TMeasurement, unsigned int VMeasurementVectorSize, 
          class TFrequencyContainer >
void
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::IncreaseFrequency(const MeasurementVectorType measurement, const FrequencyType value) 
{
  this->IncreaseFrequency(GetInstanceIdentifier(GetIndex(measurement)), value) ;
}



template< class TMeasurement, unsigned int VMeasurementVectorSize, 
          class TFrequencyContainer >
Histogram< TMeasurement, VMeasurementVectorSize,
           TFrequencyContainer >::FrequencyType
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::GetFrequency(const IndexType index)  
{
  return ( GetFrequency(GetInstanceIdentifier(index)) ) ;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize, 
          class TFrequencyContainer>
Histogram< TMeasurement, VMeasurementVectorSize, 
           TFrequencyContainer >::MeasurementType
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::GetMeasurement(unsigned int dimension, unsigned long n) 
{
  return static_cast< MeasurementType >((m_Min[dimension][n] + 
                                            m_Max[dimension][n]) / 2) ; 
}

template< class TMeasurement, unsigned int VMeasurementVectorSize, 
          class TFrequencyContainer >
Histogram< TMeasurement, VMeasurementVectorSize, 
           TFrequencyContainer >::FrequencyType
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::GetFrequency(unsigned int dimension, unsigned long n) 
{
  InstanceIdentifier nextOffset = m_OffsetTable[dimension + 1] ;
  InstanceIdentifier current = m_OffsetTable[dimension] * n ;
  InstanceIdentifier includeLength = m_OffsetTable[dimension] ;
  InstanceIdentifier include ;
  InstanceIdentifier includeEnd ;
  InstanceIdentifier last = m_OffsetTable[VMeasurementVectorSize] ;

  FrequencyType frequency = 0 ;
  while (current < last)
    {
      include = current ;
      includeEnd = include + includeLength ;
      while(include < includeEnd)
        {
          frequency += GetFrequency(include) ;
          include++ ;
        }
      current += nextOffset ;
    }
  return frequency ;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize, 
          class TFrequencyContainer >
Histogram< TMeasurement, VMeasurementVectorSize, 
           TFrequencyContainer >::FrequencyType
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::GetTotalFrequency(unsigned int dimension)
{
  FrequencyType frequency = 0 ;
  InstanceIdentifier n = 0 ;
  SizeValueType size = GetSize(dimension) ;
  while( n < size )
    {
      frequency += GetFrequency(dimension, n) ;
      n++ ;
    }
  return frequency ;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize, 
          class TFrequencyContainer >
double
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::Quantile(unsigned int dimension, double p)
{
  InstanceIdentifier n ;
  SizeValueType size = GetSize(dimension) ;
  double p_n_prev ;
  double p_n ;
  double f_n ;
  double cumulated = 0 ;
  double totalFrequency = double(GetTotalFrequency(dimension)) ;
  double binProportion ;
  double min, max, interval ;

  if ( p < 0.5 )
    {
      n = 0 ;
      p_n_prev = NumericTraits< double >::Zero ;
      p_n = NumericTraits< double >::Zero ;
      do 
        {
          f_n = GetFrequency(dimension, n) ;
          cumulated += f_n ;
          p_n_prev = p_n ;
          p_n = cumulated / totalFrequency ;
          n++ ;
        } 
      while( n < size && p_n < p) ;

      binProportion = f_n / totalFrequency ;

      min = double(GetBinMin(dimension, n - 1)) ;
      max = double(GetBinMax(dimension, n - 1)) ;
      interval = max - min ;
      return min + ((p - p_n_prev) / binProportion) * interval ;
    }
  else
    {
      n = size - 1 ;
      p_n_prev = NumericTraits< double >::One ;
      p_n = NumericTraits< double >::One ;
      do 
        {
          f_n = GetFrequency(dimension, n) ;
          cumulated += f_n ;
          p_n_prev = p_n ;
          p_n = NumericTraits< double >::One - cumulated / totalFrequency ;
          n-- ;
        } 
      while( n >= 0 && p_n > p) ;

      binProportion = f_n / totalFrequency ;
      double min = double(GetBinMin(dimension, n + 1)) ;
      double max = double(GetBinMax(dimension, n + 1)) ;
      double interval = max - min ;
      return max - ((p_n_prev - p) / binProportion) * interval ;
    }
}

template< class TMeasurement, unsigned int VMeasurementVectorSize, 
          class TFrequencyContainer >
void 
Histogram< TMeasurement, VMeasurementVectorSize, TFrequencyContainer >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "OffsetTable: " << *m_OffsetTable << std::endl;
  os << indent << "FrequencyContainerPointer: " << m_FrequencyContainer
     << std::endl;
}
  } // end of namespace Statistics 
} // end of namespace itk 

#endif

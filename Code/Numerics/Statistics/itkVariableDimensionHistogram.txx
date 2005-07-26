/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVariableDimensionHistogram.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkHistogram_txx
#define _itkHistogram_txx

#include "itkVariableDimensionHistogram.h"
#include "itkNumericTraits.h"

namespace itk{ 
namespace Statistics{

template< class TMeasurement, 
          class TFrequencyContainer>
VariableDimensionHistogram<TMeasurement,  TFrequencyContainer>
::VariableDimensionHistogram()
{
  m_ClipBinsAtEnds = true;
  m_FrequencyContainer = FrequencyContainerType::New() ;
}

template< class TMeasurement, 
          class TFrequencyContainer>
void VariableDimensionHistogram<TMeasurement,  TFrequencyContainer>
::SetMeasurementVectorSize( const MeasurementVectorSizeType s )
{
  if( s == this->GetMeasurementVectorSize() )
    {
    return;
    }
  
  if( (m_OffsetTable.Size()           != 0) ||
      (m_Size.Size()                  != 0) ||
      (m_TempIndex.Size()             != 0) ||
      (m_TempMeasurementVector.Size() != 0))
    { 
    itkWarningMacro( << "Destructively resizing paramters of the histogram" );
    }
  Superclass::SetMeasurementVectorSize( s );
  m_OffsetTable.SetSize( s + 1 );
  m_OffsetTable.Fill( 0 );
  m_Size.SetSize( s );
  m_TempIndex.SetSize( s );
  m_TempMeasurementVector.SetSize( s );
  this->Modified();
}


template< class TMeasurement, 
          class TFrequencyContainer>
unsigned int
VariableDimensionHistogram<TMeasurement,  TFrequencyContainer>
::Size() const
{
  if( this->GetMeasurementVectorSize() == 0 )
    {
    return 0;
    }
  
  unsigned int size = 1 ;
  for (unsigned int i = 0 ; i < this->GetMeasurementVectorSize() ; i++)
    {
    size *= m_Size[i] ;
    }
  return size ;
}

template< class TMeasurement, 
          class TFrequencyContainer>
void
VariableDimensionHistogram<TMeasurement,  TFrequencyContainer>
::Initialize(const SizeType &size)
{
  MeasurementVectorSizeType s = 
    MeasurementVectorTraits::Assert( size, this->GetMeasurementVectorSize(),
      "Size mismatch in VariableDimensionHistogram::Initialize(const SizeType &size)");
  if( s )
    {
    this->SetMeasurementVectorSize( size.Size() );
    }
  
  m_Size = size ;
  
  // creates offset table which will be used for generation of
  // instance identifiers.
  InstanceIdentifier num = 1 ;
  
  m_OffsetTable[0] = num ;
  for (unsigned int i = 0 ; i < this->GetMeasurementVectorSize() ; i++)
    {
    num *= m_Size[i] ;
    m_OffsetTable[i + 1] = num;
    }

  m_NumberOfInstances = num ;

  // adjust the sizes of min max value containers 
  unsigned int dim;
  m_Min.resize(this->GetMeasurementVectorSize());
  for ( dim = 0; dim < this->GetMeasurementVectorSize(); dim++)
    {
    m_Min[dim].resize(m_Size[dim]);
    } 

  m_Max.resize(this->GetMeasurementVectorSize());
  for ( dim = 0; dim < this->GetMeasurementVectorSize(); dim++)
    {
    m_Max[dim].resize(m_Size[dim]);
    } 

  // initialize the frequency container
  m_FrequencyContainer->Initialize(m_OffsetTable[ this->GetMeasurementVectorSize() ]) ;
  this->SetToZero();
}

template< class TMeasurement, 
          class TFrequencyContainer>
void 
VariableDimensionHistogram<TMeasurement,  TFrequencyContainer>
::SetToZero()
{
  m_FrequencyContainer->SetToZero();
}

template< class TMeasurement, 
          class TFrequencyContainer>
void 
VariableDimensionHistogram<TMeasurement,  TFrequencyContainer>
::Initialize(const SizeType &size, MeasurementVectorType& lowerBound,
             MeasurementVectorType& upperBound)
{
  this->Initialize(size) ;

  // Sanity check to see if size, lowerBound and upperBound are of the
  // same length.
  const MeasurementVectorSizeType measurementVectorSize = 
                                      this->GetMeasurementVectorSize();
  MeasurementVectorTraits::Assert( lowerBound, measurementVectorSize, 
      "Length mismatch: VariableDimensionHistogram::Initialize( , )");
  MeasurementVectorTraits::Assert( upperBound, measurementVectorSize, 
      "Length mismatch: VariableDimensionHistogram::Initialize( , )");
    
  float interval;
  for ( unsigned int i = 0 ; i < measurementVectorSize; i++)
    {
    interval = (float) (upperBound[i] - lowerBound[i]) 
                       / static_cast< MeasurementType >(size[i]) ;

    // Set the min vector and max vector
    for (unsigned int j = 0; j < (size[i] - 1) ; j++)
      {
      this->SetBinMin(i, j, (MeasurementType)(lowerBound[i] +  
                                              ((float)j * interval))) ;
      this->SetBinMax(i, j, (MeasurementType)(lowerBound[i] +  
                                              (((float)j + 1) * interval)));
      }
    this->SetBinMin(i, size[i] - 1, 
                    (MeasurementType)(lowerBound[i] + 
                                      (((float) size[i] - 1) * interval))) ;
    this->SetBinMax(i, size[i] - 1, 
                    (MeasurementType)(upperBound[i])) ;
    }
}


/** */
template< class TMeasurement, 
          class TFrequencyContainer>
bool VariableDimensionHistogram<TMeasurement,  TFrequencyContainer>
::GetIndex(const MeasurementVectorType & measurement,IndexType & index ) const
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  const MeasurementVectorSizeType measurementVectorSize = 
                                      this->GetMeasurementVectorSize();
  MeasurementVectorTraits::Assert( index, measurementVectorSize,
  "Length mismatch: VariableDimensionHistogram::GetIndex(MeasurementVectorType, IndexType)");
  MeasurementVectorTraits::Assert( measurement, measurementVectorSize,
  "Length mismatch: VariableDimensionHistogram::GetIndex(MeasurementVectorType, IndexType)");
  
  
  // now using something similar to binary search to find
  // index.
  unsigned int dim ;
  
  int begin, mid, end ;
  MeasurementType median ;
  MeasurementType tempMeasurement ;

  for (dim = 0 ; dim < measurementVectorSize ; dim++)
    {
    tempMeasurement = measurement[dim] ;
    begin = 0 ;
    if (tempMeasurement < m_Min[dim][begin])
      {
      // one of measurement is below the minimum
      index[dim] = (long) m_Size[dim] ;
      return false;
      }

    end = m_Min[dim].size() - 1 ;
    if (tempMeasurement >= m_Max[dim][end])
      {
      // one of measurement is above the maximum
      index[dim] = (long) m_Size[dim] ;
      return false;
      }

    mid = (end + 1) / 2 ;
    median = m_Min[dim][mid];

    while(true)
      {
      if (tempMeasurement < median )
        {
        end = mid - 1 ;
        } 
      else if (tempMeasurement > median)
        {
        if (tempMeasurement < m_Max[dim][mid])
          {
          index[dim] = mid ;
          break ;
          }
              
        begin = mid + 1 ;
        }
      else
        {
        // measurement[dim] = m_Min[dim][med] 
        index[dim] = mid ;
        break ;
        }
      mid = begin + (end - begin) / 2 ;
      median = m_Min[dim][mid] ;
      } // end of while
    } // end of for()
  return true;
}



template< class TMeasurement, 
          class TFrequencyContainer>
inline const typename VariableDimensionHistogram<TMeasurement,  TFrequencyContainer>::IndexType&
VariableDimensionHistogram<TMeasurement,  TFrequencyContainer>
::GetIndex(const InstanceIdentifier &id)  const
{
  InstanceIdentifier id2 = id ;

  for (int i = this->GetMeasurementVectorSize() - 1 ; i > 0 ; i--)
    {
    m_TempIndex[i] = static_cast<IndexValueType>(id2 / m_OffsetTable[i]);
    id2 -= (m_TempIndex[i] * m_OffsetTable[i]);
    }
  m_TempIndex[0] = static_cast<IndexValueType>(id2);
  
  return m_TempIndex;
}


template< class TMeasurement, 
          class TFrequencyContainer >
inline bool
VariableDimensionHistogram<TMeasurement,  TFrequencyContainer>
::IsIndexOutOfBounds(const IndexType &index) const
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  MeasurementVectorTraits::Assert( index, this->GetMeasurementVectorSize(),
  "Length mismatch: VariableDimensionHistogram::GetIndex(MeasurementVectorType, IndexType)");
  
  for (unsigned int dim = 0 ; dim < this->GetMeasurementVectorSize() ; dim++)
    {
    if (index[dim] < 0 || index[dim] >= static_cast<IndexValueType>(m_Size[dim]))
      {
      return true ;
      }
    }
  return false ;
}

template< class TMeasurement, 
          class TFrequencyContainer >
inline typename VariableDimensionHistogram<TMeasurement, 
                          TFrequencyContainer>::InstanceIdentifier
VariableDimensionHistogram<TMeasurement,  TFrequencyContainer>
::GetInstanceIdentifier(const IndexType &index) const
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  MeasurementVectorTraits::Assert( index, this->GetMeasurementVectorSize(),
  "Length mismatch: VariableDimensionHistogram::GetIndex(MeasurementVectorType, IndexType)");
  
  InstanceIdentifier id = 0 ;
  for (int i= this->GetMeasurementVectorSize() - 1 ; i > 0 ; i-- )
    {
    id += index[i] * m_OffsetTable[i];
    }
  
  id += index[0] ;
  
  return id ;
}


template< class TMeasurement, 
          class TFrequencyContainer >
inline const typename VariableDimensionHistogram<TMeasurement,  
                          TFrequencyContainer>::MeasurementType&
VariableDimensionHistogram<TMeasurement,  TFrequencyContainer>
::GetBinMinFromValue(const unsigned int dimension, const float value ) const
{
  // If the value is lower than any of min value in the VariableDimensionHistogram,
  // it returns the lowest min value
  if ( value <= this->m_Min[dimension][0] )
    {
    return this->m_Min[dimension][0];
    }

  // If the value is higher than any of min value in the VariableDimensionHistogram,
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

template< class TMeasurement,  
          class TFrequencyContainer >
inline const typename VariableDimensionHistogram< TMeasurement,  
                           TFrequencyContainer >::MeasurementType&
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::GetBinMaxFromValue(const unsigned int dimension, const float value ) const
{
  // If the value is lower than any of max value in the VariableDimensionHistogram,
  // it returns the lowest max value
  if ( value <= this->m_Max[dimension][0] )
    {
    return this->m_Max[dimension][0];
    }

  // If the value is higher than any of max value in the VariableDimensionHistogram,
  // it returns the highest max value
  if ( value >= m_Max[dimension][m_Size[dimension]-1] )
    {
    return m_Max[dimension][this->m_Size[dimension]-1];
    }

  for ( int i = 0 ; i < this->m_Size[dimension]; i++ )
    {
    if (  (value >= this->m_Min[dimension][i])
          && (value <  this->m_Max[dimension][i])  )
      {
      return this->m_Max[dimension][i];
      }
    }
}

template< class TMeasurement,  
          class TFrequencyContainer >
inline typename VariableDimensionHistogram< TMeasurement,  
                           TFrequencyContainer >::MeasurementVectorType&
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::GetHistogramMinFromValue(const MeasurementVectorType &measurement) 
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  const MeasurementVectorSizeType measurementVectorSize = 
                                      this->GetMeasurementVectorSize();
  MeasurementVectorTraits::Assert( measurement, measurementVectorSize,
  "Length mismatch: VariableDimensionHistogram::GetIndex(MeasurementVectorType, IndexType)");

  for ( int i = 0; i < measurementVectorSize; i++ )
    {
    m_TempMeasurementVector[i] = this->GetDimensionMinByValue(i,measurement[i]);
    }
  return m_TempMeasurementVector ;
}

template< class TMeasurement, 
          class TFrequencyContainer >
inline typename VariableDimensionHistogram< TMeasurement, 
                           TFrequencyContainer >::MeasurementVectorType&
VariableDimensionHistogram<TMeasurement,  TFrequencyContainer>
::GetHistogramMaxFromValue(const MeasurementVectorType &measurement) 
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  const MeasurementVectorSizeType measurementVectorSize = 
                                      this->GetMeasurementVectorSize();
  MeasurementVectorTraits::Assert( measurement, measurementVectorSize,
  "Length mismatch: VariableDimensionHistogram::GetIndex(MeasurementVectorType, IndexType)");

  for ( int i=0; i < measurementVectorSize; i++ )
    {
    m_TempMeasurementVector[i] = this->GetDimensionMaxByValue(i,measurement[i]);
    }
  return m_TempMeasurementVector ;

}

template< class TMeasurement, 
          class TFrequencyContainer >
inline typename VariableDimensionHistogram< TMeasurement, 
                           TFrequencyContainer >::MeasurementVectorType&
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::GetHistogramMinFromIndex(const IndexType &index) 
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  MeasurementVectorTraits::Assert( index, this->GetMeasurementVectorSize(),
  "Length mismatch: VariableDimensionHistogram::GetIndex(MeasurementVectorType, IndexType)");
  
  for ( int i=0; i < this->GetMeasurementVectorSize(); i++ )
    {
    m_TempMeasurementVector[i] = this->GetBinMin(i, index[i]) ;
    }
  return m_TempMeasurementVector ;
}

template< class TMeasurement, 
          class TFrequencyContainer >
inline typename VariableDimensionHistogram< TMeasurement, 
                           TFrequencyContainer >::MeasurementVectorType&
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::GetHistogramMaxFromIndex(const IndexType &index) 
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  MeasurementVectorTraits::Assert( index, this->GetMeasurementVectorSize(),
  "Length mismatch: VariableDimensionHistogram::GetIndex(MeasurementVectorType, IndexType)");
  
  for ( int i=0; i < this->GetMeasurementVectorSize(); i++ )
    {
    m_TempMeasurementVector[i] = this->GetBinMax(i, index[i]) ;
    }
  return m_TempMeasurementVector ;
}

template< class TMeasurement,  
          class TFrequencyContainer >
inline const typename VariableDimensionHistogram< TMeasurement,  
                           TFrequencyContainer >::MeasurementVectorType &
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::GetMeasurementVector(const IndexType &index) const
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  MeasurementVectorTraits::Assert( index, this->GetMeasurementVectorSize(),
  "Length mismatch: VariableDimensionHistogram::GetIndex(MeasurementVectorType, IndexType)");
  
  for ( unsigned int i = 0; i < this->GetMeasurementVectorSize(); i++)
    {
    MeasurementType value = (m_Min[i][index[i]] + m_Max[i][index[i]]);
    m_TempMeasurementVector[i] =  static_cast< MeasurementType >( value / 2.0 );
    }
  return m_TempMeasurementVector ;
}

template< class TMeasurement,  
          class TFrequencyContainer >
inline const typename VariableDimensionHistogram< TMeasurement,  
                           TFrequencyContainer >::MeasurementVectorType &
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::GetMeasurementVector(const InstanceIdentifier &id) const
{
  return this->GetMeasurementVector( this->GetIndex(id) ) ;
}

template< class TMeasurement,  
          class TFrequencyContainer >
inline void
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::SetFrequency(const FrequencyType value) 
{
  typename Self::Iterator iter = this->Begin() ;
  typename Self::Iterator end = this->End() ;
  
  while ( iter != end )
    {
    iter.SetFrequency(value) ;
    ++iter ;
    }
}

template< class TMeasurement,  
          class TFrequencyContainer >
inline bool
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::SetFrequency(const IndexType &index, const FrequencyType value) 
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  MeasurementVectorTraits::Assert( index, this->GetMeasurementVectorSize(),
  "Length mismatch: VariableDimensionHistogram::GetIndex(MeasurementVectorType, IndexType)");
  
  return this->SetFrequency( this->GetInstanceIdentifier(index), value) ;
}
  
template< class TMeasurement,  
          class TFrequencyContainer >
inline bool
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::SetFrequency(const MeasurementVectorType &measurement, const FrequencyType value) 
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  const MeasurementVectorSizeType measurementVectorSize = 
                                      this->GetMeasurementVectorSize();
  MeasurementVectorTraits::Assert( measurement, this->GetMeasurementVectorSize(),
  "Length mismatch: VariableDimensionHistogram::SetFrequency");

  return this->SetFrequency( this->GetInstanceIdentifier(GetIndex(measurement)), value) ;
}

template< class TMeasurement,  
          class TFrequencyContainer >
inline bool
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::IncreaseFrequency(const IndexType &index, const FrequencyType value)
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  MeasurementVectorTraits::Assert( index, this->GetMeasurementVectorSize(),
  "Length mismatch: VariableDimensionHistogram::GetIndex(MeasurementVectorType, IndexType)");
  
  const bool result = 
      this->IncreaseFrequency( this->GetInstanceIdentifier(index), value) ;
  return result;
}
  
template< class TMeasurement,  
          class TFrequencyContainer >
inline bool
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::IncreaseFrequency(const MeasurementVectorType &measurement, const FrequencyType value) 
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  const MeasurementVectorSizeType measurementVectorSize = 
                                      this->GetMeasurementVectorSize();
  MeasurementVectorTraits::Assert( measurement, this->GetMeasurementVectorSize(),
  "Length mismatch: VariableDimensionHistogram::IncreaseFrequency");

  IndexType index( measurementVectorSize );
  this->GetIndex( measurement, index );
  return this->IncreaseFrequency( this->GetInstanceIdentifier( index ), value );
}



template< class TMeasurement,  
          class TFrequencyContainer >
inline typename VariableDimensionHistogram< TMeasurement, 
                           TFrequencyContainer >::FrequencyType
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::GetFrequency(const IndexType &index) const
{
  MeasurementVectorTraits::Assert( index, this->GetMeasurementVectorSize(),
  "Length mismatch: VariableDimensionHistogram::GetFrequency");
  
  return ( this->GetFrequency( this->GetInstanceIdentifier(index)) ) ;
}

template< class TMeasurement,  
          class TFrequencyContainer>
inline typename VariableDimensionHistogram< TMeasurement,  
                           TFrequencyContainer >::MeasurementType 
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::GetMeasurement(const unsigned long n, const unsigned int dimension) const
{
  return static_cast< MeasurementType >((m_Min[dimension][n] + 
                                         m_Max[dimension][n]) / 2) ; 
}

template< class TMeasurement,  
          class TFrequencyContainer >
inline typename VariableDimensionHistogram< TMeasurement,  
                           TFrequencyContainer >::FrequencyType
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::GetFrequency(const unsigned long n, const unsigned int dimension) const
{
  InstanceIdentifier nextOffset = m_OffsetTable[dimension + 1] ;
  InstanceIdentifier current = m_OffsetTable[dimension] * n ;
  InstanceIdentifier includeLength = m_OffsetTable[dimension] ;
  InstanceIdentifier include ;
  InstanceIdentifier includeEnd ;
  InstanceIdentifier last = m_OffsetTable[this->GetMeasurementVectorSize()] ;

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

template< class TMeasurement,  
          class TFrequencyContainer >
inline typename VariableDimensionHistogram< TMeasurement,  
                           TFrequencyContainer >::FrequencyType
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::GetTotalFrequency() const
{
  return m_FrequencyContainer->GetTotalFrequency() ;
}

template< class TMeasurement,  
          class TFrequencyContainer >
double
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::Quantile(const unsigned int dimension, const double &p) const
{
  InstanceIdentifier n ;
  const unsigned int size = this->GetSize(dimension) ;
  double p_n_prev ;
  double p_n ;
  double f_n ;
  double cumulated = 0 ;
  double totalFrequency = double( this->GetTotalFrequency() ) ;
  double binProportion ;
  double min, max, interval ;

  if ( p < 0.5 )
    {
    n = 0 ;
    p_n = NumericTraits< double >::Zero ;
    do 
      {
      f_n = this->GetFrequency(n, dimension) ;
      cumulated += f_n ;
      p_n_prev = p_n ;
      p_n = cumulated / totalFrequency ;
      n++ ;
      } 
    while( n < size && p_n < p) ;

    binProportion = f_n / totalFrequency ;

    min = double( this->GetBinMin(dimension, n - 1) ) ;
    max = double( this->GetBinMax(dimension, n - 1) ) ;
    interval = max - min ;
    return min + ((p - p_n_prev) / binProportion) * interval ;
    }
  else
    {
    n = size - 1 ;
    InstanceIdentifier m = NumericTraits< InstanceIdentifier >::Zero;
    p_n      = NumericTraits< double >::One ;
    do 
      {
      f_n = this->GetFrequency(n, dimension) ;
      cumulated += f_n ;
      p_n_prev = p_n ;
      p_n = NumericTraits< double >::One - cumulated / totalFrequency ;
      n--;
      m++;
      } 
    while( m < size && p_n > p);

    binProportion = f_n / totalFrequency ;
    double min = double( this->GetBinMin(dimension, n + 1) ) ;
    double max = double( this->GetBinMax(dimension, n + 1) ) ;
    double interval = max - min ;
    return max - ((p_n_prev - p) / binProportion) * interval ;
    }
}

template< class TMeasurement,  
          class TFrequencyContainer >
void 
VariableDimensionHistogram< TMeasurement,  TFrequencyContainer >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  //os << indent << "OffsetTable: " << *m_OffsetTable << std::endl;
  if(m_ClipBinsAtEnds)
    {
    os << indent << "ClipBinsAtEnds: True" << std::endl;
    }
  else
    {
    os << indent << "ClipBinsAtEnds: False" << std::endl;
    }
  os << indent << "FrequencyContainerPointer: " << m_FrequencyContainer
     << std::endl;
}
} // end of namespace Statistics 
} // end of namespace itk 

#endif

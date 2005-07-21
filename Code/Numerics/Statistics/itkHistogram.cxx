/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHistogram.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkHistogram.h"

namespace itk{
namespace Statistics{

// Histogram for Variable length measurement vectors
//
//
Histogram<float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > > 
::Histogram()
{
  m_ClipBinsAtEnds = true;
  m_FrequencyContainer = FrequencyContainerType::New() ;
}


void
Histogram<float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >
::SetMeasurementVectorSize( unsigned int s )
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


unsigned int
Histogram<float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >
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


void
Histogram<float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >
::Initialize(const SizeType &size)
{
  if( size.Size() == 0 )
    {
    itkExceptionMacro( << "Cannot initialize with 0 size." );
    }
  
  // Destructively set all sizes..
  SetMeasurementVectorSize( size.Size() );
  
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


void 
Histogram<float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >
::SetToZero()
{
  m_FrequencyContainer->SetToZero();
}


void 
Histogram<float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >
::Initialize(const SizeType &size, MeasurementVectorType& lowerBound,
             MeasurementVectorType& upperBound)
{
  this->Initialize(size) ;

  // Sanity check to see if size, lowerBound and upperBound are of the
  // same length.
  const MeasurementVectorSizeType measurementVectorSize = 
                                      this->GetMeasurementVectorSize();
  if( (lowerBound.Size() != measurementVectorSize)
   || (upperBound.Size() != measurementVectorSize) )
    {
    itkExceptionMacro( << "bounds passed using the initialize call" 
        << " must have the same length as the dimension as the histogram");
    }
    
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


bool
Histogram<float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >
::GetIndex(const MeasurementVectorType & measurement,IndexType & index ) const
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  const MeasurementVectorSizeType measurementVectorSize = 
                                      this->GetMeasurementVectorSize();
  if( index.Size() != measurementVectorSize )
    {
    itkExceptionMacro( << 
        "Index length does not match number of dimensions in histogram" );
    }
  if( measurement.Size() != measurementVectorSize )
    {
    itkExceptionMacro( << 
        "MeasurementVector length does not match number of dimensions in histogram" );
    }
  
  
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

 
Histogram< float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >::InstanceIdentifier
Histogram<float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >
::GetInstanceIdentifier(const IndexType &index) const
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  if( index.Size() != this->GetMeasurementVectorSize() )
    {
    itkExceptionMacro( << 
        "Index length does not match number of dimensions in histogram" );
    }
  
  InstanceIdentifier id = 0 ;
  for (int i= GetMeasurementVectorSize() - 1 ; i > 0 ; i-- )
    {
    id += index[i] * m_OffsetTable[i];
    }
  
  id += index[0] ;
  
  return id ;
}


inline 
Histogram< float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >::MeasurementVectorType&
Histogram<float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >
::GetHistogramMinFromValue(const MeasurementVectorType &measurement) 
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  const MeasurementVectorSizeType measurementVectorSize = 
                                      this->GetMeasurementVectorSize();
  if( measurement.Size() != measurementVectorSize )
    {
    itkExceptionMacro( << 
        "MeasurementVector length does not match number of dimensions in histogram" );
    }

  for ( int i = 0; i < measurementVectorSize; i++ )
    {
    //m_TempMeasurementVector[i] = this->GetDimensionMinByValue(i,measurement[i]);
    }
  return m_TempMeasurementVector ;
}


inline 
Histogram< float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >::MeasurementVectorType&
Histogram<float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >
::GetHistogramMaxFromValue(const MeasurementVectorType &measurement) 
{
  // Sanity check.. see if index is of the same length as MeasurementVectorSize;
  const MeasurementVectorSizeType measurementVectorSize = 
                                      this->GetMeasurementVectorSize();
  if( measurement.Size() != measurementVectorSize )
    {
    itkExceptionMacro( << 
        "MeasurementVector length does not match number of dimensions in histogram" );
    }

  for ( int i=0; i < measurementVectorSize; i++ )
    {
    //m_TempMeasurementVector[i] = this->GetDimensionMaxByValue(i,measurement[i]);
    }
  return m_TempMeasurementVector ;

}


double
Histogram<float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >
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

 
void
Histogram<float, 
      MeasurementVectorTraits< Array< float > >::MeasurementVectorLength,
      DenseFrequencyContainer< float > >
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

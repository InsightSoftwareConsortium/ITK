/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCovarianceCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCovarianceCalculator_txx
#define __itkCovarianceCalculator_txx

namespace itk{ 
namespace Statistics{

template< class TSample >
CovarianceCalculator< TSample >
::CovarianceCalculator()
{
  m_Mean = 0 ;
  m_InternalMean = 0 ;
}

template< class TSample >
CovarianceCalculator< TSample >
::~CovarianceCalculator()
{
  if ( m_InternalMean != 0 )
    {
    delete m_InternalMean ;
    m_InternalMean = 0 ;
    }
}

template< class TSample >
void
CovarianceCalculator< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Output: " << m_Output << std::endl;

  if ( m_Mean != 0)
    {
    os << indent << "Mean: [" << *m_Mean << "]" << std::endl ;
    }
  else
    {
    os << indent << "Mean: not set" << std::endl ;
    }

  if ( m_InternalMean != 0)
    {
    os << indent << "Internal Mean: [" << *m_InternalMean << "]" << std::endl ;
    }
  else
    {
    os << indent << "Internal Mean: not used" << std::endl ;
    }
}

template< class TSample >
void
CovarianceCalculator< TSample >
::SetMean(MeanType* mean)
{
  if ( m_InternalMean != mean && m_InternalMean != 0 )
    {
    delete m_InternalMean ;
    m_InternalMean = 0 ;
    }
  m_Mean = mean ;
} 

template< class TSample >
typename CovarianceCalculator< TSample >::MeanType*
CovarianceCalculator< TSample >
::GetMean()
{
  if ( m_InternalMean != 0 )
    {
    return m_InternalMean ;
    }
  else
    {
    return m_Mean ;
    }
} 

template< class TSample >
typename CovarianceCalculator< TSample >::OutputType*
CovarianceCalculator< TSample >
::GetOutput()
{
  return &m_Output ;
} 

template< class TSample >
inline void
CovarianceCalculator< TSample >
::ComputeCovarianceWithGivenMean() 
{
  m_Output.Fill(0.0) ;
  double frequency ;
  double totalFrequency = 0.0 ;

  unsigned int row, col ;
  unsigned int i ;
  typename TSample::Iterator iter = this->GetInputSample()->Begin() ;
  typename TSample::Iterator end = this->GetInputSample()->End() ;
  MeanType diff ;
  typename TSample::MeasurementVectorType measurements ;
  // fills the lower triangle and the diagonal cells in the covariance matrix
  while (iter != end)
    {
    frequency = iter.GetFrequency() ;
    totalFrequency += frequency ;
    measurements = iter.GetMeasurementVector() ;
    for (i = 0 ; i < MeasurementVectorSize ; i++)
      {
      diff[i] = measurements[i] - (*m_Mean)[i] ;
      }
    for ( row = 0; row < MeasurementVectorSize ; row++)
      {
      for ( col = 0; col < row + 1 ; col++)
        {
        m_Output.GetVnlMatrix()(row,col) += frequency * diff[row] * diff[col] ;
        }
      }
    ++iter ;
    }

  // fills the upper triangle using the lower triangle  
  for (row = 1 ; row < MeasurementVectorSize ; row++)
    {
    for (col = 0 ; col < row ; col++)
      {
      m_Output.GetVnlMatrix()(col, row) = 
        m_Output.GetVnlMatrix()(row, col) ;
      } 
    }

  m_Output.GetVnlMatrix() /= (totalFrequency - 1.0f);
}

template< class TSample >
inline void
CovarianceCalculator< TSample >
::ComputeCovarianceWithoutGivenMean() 
{
  m_Output.Fill(0.0) ;
  m_InternalMean->Fill(0.0) ;
  double frequency ;
  double totalFrequency = 0.0 ;

  unsigned int row, col ;
  unsigned int i ;
  typename TSample::Iterator iter = this->GetInputSample()->Begin() ;
  typename TSample::Iterator end = this->GetInputSample()->End() ;
  MeanType diff ;
  typename TSample::MeasurementVectorType measurements ;
  // fills the lower triangle and the diagonal cells in the covariance matrix
  while (iter != end)
    {
    frequency = iter.GetFrequency() ;
    totalFrequency += frequency ;
    measurements = iter.GetMeasurementVector() ;
    for ( i = 0 ; i < MeasurementVectorSize ; ++i )
      {
      diff[i] = measurements[i] - (*m_InternalMean)[i] ;
      }

    // updates the mean vector
    double tempWeight = frequency / totalFrequency ;
    for ( i = 0 ; i < MeasurementVectorSize ; ++i )
      {
      (*m_InternalMean)[i] += tempWeight * diff[i] ;
      }

    // updates the covariance matrix
    tempWeight = tempWeight * ( totalFrequency - frequency ) ;
    for ( row = 0; row < MeasurementVectorSize ; row++ )
      {
      for ( col = 0; col < row + 1 ; col++)
        {
        m_Output.GetVnlMatrix()(row,col) += 
          tempWeight * diff[row] * diff[col] ;
        }
      }
    ++iter ;
    }

  // fills the upper triangle using the lower triangle  
  for (row = 1 ; row < MeasurementVectorSize ; row++)
    {
    for (col = 0 ; col < row ; col++)
      {
      m_Output.GetVnlMatrix()(col, row) = 
        m_Output.GetVnlMatrix()(row, col) ;
      } 
    }

  m_Output.GetVnlMatrix() /= ( totalFrequency - 1.0 ) ;
}

template< class TSample >
inline void
CovarianceCalculator< TSample >
::GenerateData() 
{
  if ( m_Mean == 0 )
    {
    m_InternalMean = new MeanType() ;
    this->ComputeCovarianceWithoutGivenMean() ;
    }
  else
    {
    this->ComputeCovarianceWithGivenMean() ;
    }

}


} // end of namespace Statistics 
} // end of namespace itk

#endif


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
}

template< class TSample >
void
CovarianceCalculator< TSample >
::SetSample(SamplePointer sample)
{
  m_Sample = sample ;
} 

template< class TSample >
CovarianceCalculator< TSample >::SamplePointer
CovarianceCalculator< TSample >
::GetSample()
{
  return m_Sample ;
} 

template< class TSample >
void
CovarianceCalculator< TSample >
::SetMean(vnl_vector< double > mean)
{
  m_Mean = mean ;
} 

template< class TSample >
vnl_vector< double >
CovarianceCalculator< TSample >
::GetMean()
{
  return m_Mean ;
} 

template< class TSample >
CovarianceCalculator< TSample >::OutputType
CovarianceCalculator< TSample >
::GetOutput()
{
  return m_Output ;
} 

template< class TSample >
void
CovarianceCalculator< TSample >
::GenerateData() 
{
  enum { Dimension = TSample::MeasurementVectorSize } ;

  m_Output.resize(Dimension, Dimension) ;
  m_Output.fill(0) ;
  double frequency = 0.0 ;
  double totalFrequency = 0.0 ;
  
  unsigned int row, col ;
  unsigned int i ;
  typename TSample::Iterator iter = m_Sample->Begin() ;
  vnl_vector< double > diff ;
  diff.resize(Dimension) ;
  typename TSample::MeasurementVectorType measurements ;
  while (iter != m_Sample->End())
    {
      frequency = iter.GetFrequency() ;
      totalFrequency += frequency ;
      measurements = iter.GetMeasurementVector() ;
      for (i = 0 ; i < Dimension ; i++)
        {
          diff[i] = measurements[i] - m_Mean[i] ;
        }

      for ( row = 0; row < Dimension ; row++)
        {
          for ( col = 0; col < Dimension ; col++)
            {
              m_Output(row,col) += frequency * diff[row] * diff[col] ;
            }
        }
      ++iter ;
    }
  
  m_Output /= totalFrequency ;
}

template< class TSample >
void
CovarianceCalculator< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned int i ;
  Superclass::PrintSelf(os,indent);

  os << indent << "Sample: " << m_Sample << std::endl;
  os << indent << "Output: " << m_Output << std::endl;
  os << indent << "Mean: [" ;
  for (i=0; i < TSample::MeasurementVectorSize - 1; i++)
    {
    os << m_Mean[i] << ", ";
    }
  os << m_Mean[i] << "]" << std::endl;
}

  } // end of namespace Statistics 
} // end of namespace itk

#endif


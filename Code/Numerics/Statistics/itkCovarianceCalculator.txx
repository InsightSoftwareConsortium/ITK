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
::PrintSelf(std::ostream& os, Indent indent) const
{
  int i ;
  Superclass::PrintSelf(os,indent);

  os << indent << "Output: " << m_Output << std::endl;
  os << indent << "Mean: [" ;
  for (i=0; i < MeasurementVectorSize - 1; i++)
    {
      os << m_Mean[i] << ", ";
    }
  os << m_Mean[i] << "]" << std::endl;
}

template< class TSample >
void
CovarianceCalculator< TSample >
::SetMean(MeanType* mean)
{
  m_Mean = mean ;
} 

template< class TSample >
typename CovarianceCalculator< TSample >::MeanType*
CovarianceCalculator< TSample >
::GetMean()
{
  return m_Mean ;
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
::GenerateData() 
{
  m_Output.Fill(0.0) ;
  double frequency = 0.0 ;
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
  
  m_Output.GetVnlMatrix() /= totalFrequency ;
}


} // end of namespace Statistics 
} // end of namespace itk

#endif


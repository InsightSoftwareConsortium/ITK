/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMeanCalculator_txx
#define __itkMeanCalculator_txx

namespace itk{ 
  namespace Statistics{

template< class TSample >
MeanCalculator< TSample >
::MeanCalculator()
{
}

template< class TSample >
void
MeanCalculator< TSample >
::SetSample(SamplePointer sample)
{
  m_Sample = sample ;
} 

template< class TSample >
MeanCalculator< TSample >::SamplePointer
MeanCalculator< TSample >
::GetSample()
{
  return m_Sample ;
} 

template< class TSample >
MeanCalculator< TSample >::OutputType
MeanCalculator< TSample >
::GetOutput()
{
  return m_Output ;
} 

template< class TSample >
inline void
MeanCalculator< TSample >
::GenerateData() 
{
  enum { Dimension = TSample::MeasurementVectorSize } ;
  
  vnl_vector<double> mean(Dimension, 0.0) ;
  
  typename TSample::Iterator iter = m_Sample->Begin() ;
  typename TSample::Iterator end = m_Sample->End() ;
  double totalFrequency = 0.0 ;
  double frequency = 0.0 ;
  unsigned int dim = 0 ;
  
  while (iter != end)
    {
      frequency = iter.GetFrequency() ;
      totalFrequency += frequency ;
      for (dim = 0 ; dim < Dimension ; dim++)
        {
          mean[dim] += iter.GetMeasurement(dim) * frequency ;
        }
      ++iter ;
    }
  
  m_Output = mean / totalFrequency ;
}

template< class TSample >
void
MeanCalculator< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Sample: " << m_Sample << std::endl;
  os << indent << "Output: " << m_Output << std::endl;
}
  } // end of namespace Statistics 
} // end of namespace itk

#endif


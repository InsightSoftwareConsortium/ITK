/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCovarianceCalculator.txx
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

  } // end of namespace Statistics 
} // end of namespace itk

#endif


/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianDensityFunction.txx
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
#ifndef __itkGaussianDensityFunction_txx
#define __itkGaussianDensityFunction_txx

#include "itkGaussianDensityFunction.h"

namespace itk{ 
  namespace Statistics{

template < class TMeasurementVector >
GaussianDensityFunction< TMeasurementVector >
::GaussianDensityFunction()
{
}

template < class TMeasurementVector >
void 
GaussianDensityFunction< TMeasurementVector >
::SetMean(vnl_vector< double > mean)
{
  m_Mean = mean ;
}

template < class TMeasurementVector >
vnl_vector< double > 
GaussianDensityFunction< TMeasurementVector >
::GetMean()
{
  return m_Mean ;
}

template < class TMeasurementVector >
void 
GaussianDensityFunction< TMeasurementVector >
::SetCovariance(vnl_matrix<double> cov)
{
  m_VectorSize = cov.rows() ;
  m_Covariance.resize(m_VectorSize, m_VectorSize) ;
  m_Covariance = cov; 

  // allocate the memory for m_InverseCovariance matrix   
  m_InverseCovariance.resize(m_VectorSize, m_VectorSize) ;
  
  m_InverseCovariance = vnl_matrix_inverse< double >(m_Covariance);
  
  // the determinant of the covaraince matrix
  double det = vnl_determinant(m_Covariance);
 
  // calculate coefficient C of multivariate gaussian
  // p(x) = C exp(-0.5 * (x-u) * inv(covariance) * (x-u)')
  m_PreFactor = double(1.0) / pow( pow(2.0*PI, double(m_VectorSize)), 1/2.0)*sqrt(fabs(det) );
}

template < class TMeasurementVector >
vnl_matrix< double > 
GaussianDensityFunction< TMeasurementVector >
::GetCovariance()
{
  return m_Covariance ;
}

template < class TMeasurementVector >
double 
GaussianDensityFunction< TMeasurementVector >
::Evaluate(MeasurementVectorType measurement)
{ 
  vnl_matrix<double> diff(m_VectorSize,1);
  for ( int i=0; i < m_VectorSize ; i++)
    {
      diff.put(i,0, measurement[i] - m_Mean[i]);
    }

  vnl_matrix< double > exponentMatrix(1,1);
  exponentMatrix = vnl_transpose(diff) * m_InverseCovariance * diff;
  
  double temp = m_PreFactor * exp( double(-0.5) * exponentMatrix.get(0,0) ) ;
//   std::cout << temp << " (" << m_PreFactor << ", " 
//             << -0.5 * exponentMatrix.get(0,0) << ") | " ;
  
  return temp ;
}
  
template < class TMeasurementVector >
void  
GaussianDensityFunction< TMeasurementVector >
::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned int i ;
  Superclass::PrintSelf(os,indent);

  os << indent << "Mean: [" ;
  for (i=0; i < m_VectorSize - 1; i++)
    {
    os << m_Mean[i] << ", ";
    }
  os << m_Mean[i] << "]" << std::endl;

  os << indent << "Covariance: " << m_Covariance << std::endl;
  os << indent << "InverseCovariance" << m_InverseCovariance << std::endl;
  os << indent << "Prefactor" << m_PreFactor << std::endl;
  os << indent << "VectorSize" << m_VectorSize << std::endl;
}
  } // end namespace Statistics
} // end of namespace itk



#endif

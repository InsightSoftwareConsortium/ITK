/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianDensityFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
  m_TempExponential.resize(1, 1) ;
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
  m_TempVector.resize(m_VectorSize) ;
  m_TempVector2.resize(m_VectorSize) ;
//    m_TempDifference.resize(m_VectorSize) ;
//    m_Covariance.resize(m_VectorSize, m_VectorSize) ;
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
inline double
GaussianDensityFunction< TMeasurementVector >
::Evaluate(MeasurementVectorType &measurement)
{ 

  double temp ;
  int i, j ;
  //    for (i = 0 ; i < m_VectorSize ; i++)
  //      {
  //        m_TempDifference[0][i] = measurement[i] - m_Mean[i] ;
  //      }

  // Compute |y - mean | 
  for (i = 0 ; i < m_VectorSize ; i++)
    {
      m_TempVector[i] = measurement[i] - m_Mean[i] ;
    }

  // Compute |y - mean | * inverse(cov) 
  for (i = 0 ; i < m_VectorSize ; i++)
    {
      temp = 0 ;
      for (j = 0 ; j < m_VectorSize ; j++)
        {
          temp += m_TempVector[j] * m_InverseCovariance.get(j, i) ;
        }
      m_TempVector2[i] = temp ;
    }

  // Compute |y - mean | * inverse(cov) * |y - mean|^T 
  temp = 0 ;
  for (i = 0 ; i < m_VectorSize ; i++)
    {
      temp += m_TempVector2[i] * m_TempVector[i] ;
    }

  return  m_PreFactor * exp( double(-0.5) * temp ) ;
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

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianGoodnessOfFitComponent.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianGoodnessOfFitComponent_txx
#define __itkGaussianGoodnessOfFitComponent_txx

#include "itkGaussianGoodnessOfFitComponent.h"

#include "vnl/vnl_math.h"

namespace itk{ 
namespace Statistics{

template< class TInputSample >
GaussianGoodnessOfFitComponent< TInputSample >
::GaussianGoodnessOfFitComponent()
{
  m_Mean.Fill(0.0) ;
  m_Covariance.Fill(0.0) ;
  m_Center.Fill(0.0) ;
  m_StandardDeviation = 0.0 ;
  m_Radius = 0.0 ;

  m_NumberOfParameters = (unsigned int)(MeasurementVectorSize + 1) ;

  m_ProbabilityDensityFunction = ProbabilityDensityFunctionType::New() ;
  m_ProbabilityDensityFunction->SetMean(&m_Mean) ;
  m_ProbabilityDensityFunction->SetCovariance(&m_Covariance) ;
  
  m_CovarianceCalculator = CovarianceCalculatorType::New() ;
  m_CovarianceCalculator->SetMean(&m_Mean) ;
  m_CovarianceCalculator->
    SetWeightFunction(m_ProbabilityDensityFunction.GetPointer()) ;

  m_ProjectionAxisCalculator = ProjectionAxisCalculatorType::New() ;
  m_ProjectionAxisCalculator->SetMatrix(&m_Covariance) ;

}

template< class TInputSample >
GaussianGoodnessOfFitComponent< TInputSample >
::~GaussianGoodnessOfFitComponent()
{
}

template< class TInputSample >
void
GaussianGoodnessOfFitComponent< TInputSample >
::SetParameters(const ParametersType &parameters)
{
  Superclass::SetParameters(parameters) ;

  bool changed = false ;

  int i = 0 ;
  while ( i < MeasurementVectorSize )
    {
      if ( m_Mean[i] != parameters[i] )
        {
          m_Mean[i] = parameters[i] ;
          m_Center[i] = m_Mean[i] ;
          changed = true ;
        }
      i++ ;
    }

  if ( m_StandardDeviation != parameters[i] )
    {
      m_StandardDeviation = parameters[i] ;
      changed = true ;
    }

  if ( changed )
    {
      this->Modified() ;
    }
}

template< class TInputSample >
GaussianGoodnessOfFitComponent< TInputSample >::CenterType*
GaussianGoodnessOfFitComponent< TInputSample >
::GetCenter()
{
  return &m_Center ;
}

template< class TInputSample >
GaussianGoodnessOfFitComponent< TInputSample >::RadiusType*
GaussianGoodnessOfFitComponent< TInputSample >
::GetRadius() 
{
  m_Radius = m_StandardDeviation * this->GetHistogramExtent() ;
  return &m_Radius ;
}
  
template< class TInputSample >
GaussianGoodnessOfFitComponent< TInputSample >::MeanType*
GaussianGoodnessOfFitComponent< TInputSample >
::GetMean()
{
  return &m_Mean ;
}

template< class TInputSample >
GaussianGoodnessOfFitComponent< TInputSample >::StandardDeviationType*
GaussianGoodnessOfFitComponent< TInputSample >
::GetStandardDeviation() 
{
  return &m_StandardDeviation ;
}

template< class TInputSample >
void
GaussianGoodnessOfFitComponent< TInputSample >
::CalculateProjectionAxes()
{
  int i, j ;

  m_CovarianceCalculator->SetInputSample(this->GetResampledSample()) ;
  m_Covariance.Fill(0.0) ;
  m_Covariance.GetVnlMatrix().
    fill_diagonal( m_StandardDeviation * m_StandardDeviation ) ;

  if ( this->GetResampledSample()->GetTotalFrequency(0) > 0 )
    {
      m_ProbabilityDensityFunction->SetCovariance(&m_Covariance) ;
      m_CovarianceCalculator->Update() ;
      
      m_Covariance = (*m_CovarianceCalculator->GetOutput()) ;
    }
      
  m_ProjectionAxisCalculator->Update() ;

  ProjectionAxisArrayType* to = this->GetProjectionAxes() ;
  ProjectionAxisArrayType* from =
    m_ProjectionAxisCalculator->GetEigenVectors() ;

  for ( i = 0 ; i < MeasurementVectorSize ; i++ )
    {
      for (j = 0 ; j < MeasurementVectorSize ; j++)
        {
          (*to)[i][j] = (*from)[i][j] ;
        }
    }

  ProjectionAxisCalculatorType::ArrayType* eigenValues = 
    m_ProjectionAxisCalculator->GetEigenValues() ;

  m_LongestAxisIndex = 0 ;
  m_LargestEigenValue = NumericTraits< double >::min() ;
  for ( i = 0 ; i < MeasurementVectorSize ; i++ )
    {
      if ( (*eigenValues)[i] > m_LargestEigenValue )
        {
          m_LongestAxisIndex = i ;
          m_LargestEigenValue = (*eigenValues)[i] ;
        }
    }

  m_Covariance *= 
    (m_StandardDeviation * m_StandardDeviation / m_LargestEigenValue) ;
}

template< class TInputSample >
double
GaussianGoodnessOfFitComponent< TInputSample >
::GetCumulativeProbability(double x) const
{
  /* |e(x)| < 7.5e-8 : From Handbook, p491 */
  
  double mean = 0.0 ;
  double standardDeviation = 1.0 ;

  double nx = fabs( x - mean ) / standardDeviation ;
  
  double t = 1 / (1 + 0.2316419 * nx);
  double tt = t*t;
  double ttt = tt*t;
  double tttt = ttt*t;
  double ttttt = tttt*t;
  
  double z = exp(-nx*nx/2) / sqrt(2*vnl_math::pi);
  
  if ( x > mean )
    {
      return 1 - z * (0.319381530*t - 0.356563782*tt + 1.781477937*ttt -
                    1.821255978*tttt + 1.330274429*ttttt);
    }
  else
    {
      return z * (0.319381530*t - 0.356563782*tt + 1.781477937*ttt -
                  1.821255978*tttt + 1.330274429*ttttt);
    }
}

template< class TInputSample >
double
GaussianGoodnessOfFitComponent< TInputSample >
::GetProbabilityDensity(MeasurementVectorType &measurements) const
{
  return m_ProbabilityDensityFunction->Evaluate(measurements) ;
}

template< class TInputSample >
void
GaussianGoodnessOfFitComponent< TInputSample >
::PrintParameters(std::ostream &os) const
{
  int i, j ;
  os << m_Mean ;
  for( i = 0 ; i < MeasurementVectorSize ; i++)
    {
      for( j = 0 ; j < MeasurementVectorSize ; j++)
        {
          os << " " << m_Covariance.GetVnlMatrix().get(i,j) ;
        }
    }
  os << std::endl ;
}

template< class TInputSample >
GaussianGoodnessOfFitComponent< TInputSample >::ParametersType
GaussianGoodnessOfFitComponent< TInputSample >
::GetFullParameters() const
{
  ParametersType params(MeasurementVectorSize + 
                        MeasurementVectorSize * MeasurementVectorSize) ;

  int index = 0 ;
  while ( index < MeasurementVectorSize )
    {
      params[index] = m_Mean[index] ;
      ++index ;
    }

  int i, j ;
  for( i = 0 ; i < MeasurementVectorSize ; i++)
    {
      for( j = 0 ; j < MeasurementVectorSize ; j++)
        {
          params[index] = m_Covariance.GetVnlMatrix().get(i,j) ;
          ++index ;
        }
    }

  return params ;
}

} // end of namespace Statistics 
} // end of namespace itk

#endif


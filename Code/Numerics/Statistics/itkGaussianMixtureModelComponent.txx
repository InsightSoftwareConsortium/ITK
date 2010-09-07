/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkGaussianMixtureModelComponent.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkGaussianMixtureModelComponent_txx
#define __itkGaussianMixtureModelComponent_txx

#include <iostream>

#include "itkGaussianMixtureModelComponent.h"

namespace itk
{
namespace Statistics
{
template< class TSample >
GaussianMixtureModelComponent< TSample >
::GaussianMixtureModelComponent()
{
  m_MeanEstimator = MeanEstimatorType::New();
  m_CovarianceEstimator = CovarianceEstimatorType::New();
  m_GaussianMembershipFunction = NativeMembershipFunctionType::New();
  this->SetMembershipFunction( (MembershipFunctionType *)
                               m_GaussianMembershipFunction.GetPointer() );
  m_Mean.Fill(0.0);
  m_Covariance.SetIdentity();
}

template< class TSample >
void
GaussianMixtureModelComponent< TSample >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Mean: " << m_Mean << std::endl;
  os << indent << "Covariance: " << m_Covariance << std::endl;
  os << indent << "Mean Estimator: " << m_MeanEstimator << std::endl;
  os << indent << "Covariance Estimator: " << m_CovarianceEstimator << std::endl;
  os << indent << "GaussianMembershipFunction: " << m_GaussianMembershipFunction << std::endl;
}

template< class TSample >
void
GaussianMixtureModelComponent< TSample >
::SetSample(const TSample *sample)
{
  Superclass::SetSample(sample);

  m_MeanEstimator->SetInput(sample);
  m_CovarianceEstimator->SetInput(sample);

  const MeasurementVectorSizeType measurementVectorLength =
    sample->GetMeasurementVectorSize();
  m_GaussianMembershipFunction->SetMeasurementVectorSize(measurementVectorLength);

  MeasurementVectorTraits::SetLength(m_Mean, measurementVectorLength);
  m_Covariance.SetSize(measurementVectorLength, measurementVectorLength);

  m_Mean.Fill(NumericTraits< double >::Zero);

  m_Covariance.Fill(NumericTraits< double >::Zero);

  typename NativeMembershipFunctionType::MeanType mean;

  MeasurementVectorTraits::SetLength(mean, measurementVectorLength);

  for ( unsigned int i = 0; i < measurementVectorLength; ++i )
    {
    mean[i] = m_Mean[i];
    }

  m_GaussianMembershipFunction->SetMean(mean);
}

template< class TSample >
void
GaussianMixtureModelComponent< TSample >
::SetParameters(const ParametersType & parameters)
{
  Superclass::SetParameters(parameters);

  unsigned int paramIndex = 0;
  unsigned int i, j;

  bool changed = false;

  MeasurementVectorSizeType measurementVectorSize =
    this->GetSample()->GetMeasurementVectorSize();

  for ( i = 0; i < measurementVectorSize; i++ )
    {
    if ( m_Mean[i] != parameters[paramIndex] )
      {
      m_Mean[i] = parameters[paramIndex];
      changed = true;
      }

    ++paramIndex;
    }

  typename NativeMembershipFunctionType::MeanType mean;

  MeasurementVectorTraits::SetLength(mean, measurementVectorSize);

  for ( i = 0; i < measurementVectorSize; ++i )
    {
    mean[i] = m_Mean[i];
    }

  m_GaussianMembershipFunction->SetMean(mean);

  for ( i = 0; i < measurementVectorSize; i++ )
    {
    for ( j = 0; j < measurementVectorSize; j++ )
      {
      if ( m_Covariance.GetVnlMatrix().get(i, j) !=
           parameters[paramIndex] )
        {
        m_Covariance.GetVnlMatrix().put(i, j, parameters[paramIndex]);
        changed = true;
        }
      ++paramIndex;
      }
    }
  m_GaussianMembershipFunction->SetCovariance(m_Covariance);

  this->AreParametersModified(changed);
}

template< class TSample >
double
GaussianMixtureModelComponent< TSample >
::CalculateParametersChange()
{
  unsigned int i, j;

  typename MeanType::MeasurementVectorType meanEstimate =
    m_MeanEstimator->GetMean();

  CovarianceType covEstimateDecoratedObject = m_CovarianceEstimator->GetOutput();
  typename CovarianceType::MeasurementVectorType covEstimate =  covEstimateDecoratedObject->et();

  double                    temp;
  double                    changes = 0.0;
  MeasurementVectorSizeType measurementVectorSize =
    this->GetSample()->GetMeasurementVectorSize();

  for ( i = 0; i < measurementVectorSize; i++ )
    {
    temp = m_Mean[i] - meanEstimate[i];
    changes += temp * temp;
    }

  for ( i = 0; i < measurementVectorSize; i++ )
    {
    for ( j = 0; j < measurementVectorSize; j++ )
      {
      temp = m_Covariance.GetVnlMatrix().get(i, j)
             - covEstimate.GetVnlMatrix().get(i, j);
      changes += temp * temp;
      }
    }

  changes = vcl_sqrt(changes);
  return changes;
}

template< class TSample >
void
GaussianMixtureModelComponent< TSample >
::GenerateData()
{
  MeasurementVectorSizeType measurementVectorSize =
    this->GetSample()->GetMeasurementVectorSize();

  this->AreParametersModified(false);

  const WeightArrayType & weights = this->GetWeights();

  typename TSample::ConstIterator iter = this->GetSample()->Begin();
  typename TSample::ConstIterator end =  this->GetSample()->End();

  typename TSample::MeasurementVectorType measurements;

  while ( iter != end )
    {
    measurements = iter.GetMeasurementVector();
    ++iter;
    }

  m_MeanEstimator->SetWeights(weights);
  m_MeanEstimator->Update();

  unsigned int   i, j;
  double         temp;
  double         changes;
  bool           changed = false;
  ParametersType parameters = this->GetFullParameters();
  int            paramIndex  = 0;

  typename MeanEstimatorType::MeasurementVectorType meanEstimate = m_MeanEstimator->GetMean();
  for ( i = 0; i < measurementVectorSize; i++ )
    {
    temp = m_Mean[i] - meanEstimate[i];
    changes = temp * temp;
    changes = vcl_sqrt(changes);
    if ( changes > this->GetMinimalParametersChange() )
      {
      changed = true;
      }
    }

  if ( changed )
    {
    m_Mean = meanEstimate;
    for ( i = 0; i < measurementVectorSize; i++ )
      {
      parameters[paramIndex] = meanEstimate[i];
      ++paramIndex;
      }
    this->AreParametersModified(true);
    }
  else
    {
    paramIndex = measurementVectorSize;
    }

  m_CovarianceEstimator->SetWeights(weights);
  m_CovarianceEstimator->Update();
  typename CovarianceEstimatorType::MatrixType covEstimate =
    m_CovarianceEstimator->GetCovarianceMatrix();

  changed = false;
  for ( i = 0; i < measurementVectorSize; i++ )
    {
    for ( j = 0; j < measurementVectorSize; j++ )
      {
      temp = m_Covariance.GetVnlMatrix().get(i, j)
             - covEstimate.GetVnlMatrix().get(i, j);
      changes = temp * temp;
      changes = vcl_sqrt(changes);
      if ( changes > this->GetMinimalParametersChange() )
        {
        changed = true;
        }
      }
    }

  if ( changed )
    {
    m_Covariance = covEstimate;
    for ( i = 0; i < measurementVectorSize; i++ )
      {
      for ( j = 0; j < measurementVectorSize; j++ )
        {
        parameters[paramIndex] = covEstimate.GetVnlMatrix().get(i, j);
        ++paramIndex;
        }
      }
    this->AreParametersModified(true);
    }

  //THIS IS NEEDED TO update m_Mean and m_Covariance.SHOULD BE REMOVED
  paramIndex = 0;
  for ( i = 0; i < measurementVectorSize; i++ )
    {
    m_Mean[i] = parameters[paramIndex];
    ++paramIndex;
    }

  typename NativeMembershipFunctionType::MeanType mean;
  MeasurementVectorTraits::SetLength(mean, measurementVectorSize);

  for ( i = 0; i < measurementVectorSize; ++i )
    {
    mean[i] = m_Mean[i];
    }
  m_GaussianMembershipFunction->SetMean(mean);

  for ( i = 0; i < measurementVectorSize; i++ )
    {
    for ( j = 0; j < measurementVectorSize; j++ )
      {
      m_Covariance.GetVnlMatrix().put(i, j, parameters[paramIndex]);
      ++paramIndex;
      }
    }
  m_GaussianMembershipFunction->SetCovariance(m_Covariance);

  Superclass::SetParameters(parameters);
}
} // end of namespace Statistics
} // end of namespace itk

#endif

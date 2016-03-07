/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkGaussianMixtureModelComponent_hxx
#define itkGaussianMixtureModelComponent_hxx

#include <iostream>

#include "itkGaussianMixtureModelComponent.h"
#include "itkMath.h"

namespace itk
{
namespace Statistics
{
template< typename TSample >
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

template< typename TSample >
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

template< typename TSample >
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

  NumericTraits<MeasurementVectorType>::SetLength(m_Mean, measurementVectorLength);
  m_Covariance.SetSize(measurementVectorLength, measurementVectorLength);

  m_Mean.Fill(NumericTraits< double >::ZeroValue());

  m_Covariance.Fill(NumericTraits< double >::ZeroValue());

  typename NativeMembershipFunctionType::MeanVectorType mean;

  NumericTraits<typename NativeMembershipFunctionType::MeanVectorType>::SetLength(mean,
    measurementVectorLength);

  for ( unsigned int i = 0; i < measurementVectorLength; ++i )
    {
    mean[i] = m_Mean[i];
    }

  m_GaussianMembershipFunction->SetMean(mean);
}

template< typename TSample >
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
    if ( Math::NotExactlyEquals(m_Mean[i], parameters[paramIndex]) )
      {
      m_Mean[i] = parameters[paramIndex];
      changed = true;
      }

    ++paramIndex;
    }

  typename NativeMembershipFunctionType::MeanVectorType mean;

  NumericTraits<typename NativeMembershipFunctionType::MeanVectorType>::SetLength(mean,
    measurementVectorSize);

  for ( i = 0; i < measurementVectorSize; ++i )
    {
    mean[i] = m_Mean[i];
    }

  m_GaussianMembershipFunction->SetMean(mean);

  for ( i = 0; i < measurementVectorSize; i++ )
    {
    for ( j = 0; j < measurementVectorSize; j++ )
      {
      if ( Math::NotExactlyEquals( m_Covariance.GetVnlMatrix().get(i, j),
           parameters[paramIndex] ) )
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

template< typename TSample >
double
GaussianMixtureModelComponent< TSample >
::CalculateParametersChange()
{
  unsigned int i, j;

  typename MeanVectorType::MeasurementVectorType meanEstimate =
    m_MeanEstimator->GetMean();

  CovarianceMatrixType covEstimateDecoratedObject = m_CovarianceEstimator->GetOutput();
  typename CovarianceMatrixType::MeasurementVectorType covEstimate =  covEstimateDecoratedObject->et();

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

  changes = std::sqrt(changes);
  return changes;
}

template< typename TSample >
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

  // why these lines???
  while ( iter != end )
    {
    measurements = iter.GetMeasurementVector();
    ++iter;
    }

  m_MeanEstimator->SetWeights(weights);
  m_MeanEstimator->Update();

  MeasurementVectorSizeType   i, j;
  double         temp;
  double         changes;
  bool           changed = false;
  ParametersType parameters = this->GetFullParameters();
  MeasurementVectorSizeType            paramIndex  = 0;

  typename MeanEstimatorType::MeasurementVectorType meanEstimate = m_MeanEstimator->GetMean();
  for ( i = 0; i < measurementVectorSize; i++ )
    {
    changes = itk::Math::abs( m_Mean[i] - meanEstimate[i] );

    if ( changes > this->GetMinimalParametersChange() )
      {
      changed = true;
      break;
      }
    }

  if ( changed )
    {
    m_Mean = meanEstimate;
    for ( paramIndex = 0; paramIndex < measurementVectorSize; paramIndex++ )
      {
      parameters[paramIndex] = meanEstimate[paramIndex];
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
    if( !changed )
      {
      for ( j = 0; j < measurementVectorSize; j++ )
        {
        temp = m_Covariance.GetVnlMatrix().get(i, j)
               - covEstimate.GetVnlMatrix().get(i, j);
        changes = itk::Math::abs( temp );
        if ( changes > this->GetMinimalParametersChange() )
          {
          changed = true;
          break;
          }
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

  typename NativeMembershipFunctionType::MeanVectorType mean;
  NumericTraits<typename NativeMembershipFunctionType::MeanVectorType>::SetLength(mean,
    measurementVectorSize);

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

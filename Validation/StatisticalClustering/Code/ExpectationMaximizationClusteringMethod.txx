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
#ifndef __ExpectationMaximizationClusteringMethod_txx
#define __ExpectationMaximizationClusteringMethod_txx

#include "ExpectationMaximizationClusteringMethod.h"

template< class TSample, class THistogram >
ExpectationMaximizationClusteringMethod< TSample, THistogram >
::ExpectationMaximizationClusteringMethod()
{
  m_Estimator = EstimatorType::New() ;
}


template< class TSample, class THistogram >
void
ExpectationMaximizationClusteringMethod< TSample, THistogram >
::Initialize(unsigned int numberOfClusters)
{
  m_NumberOfComponents = numberOfClusters ;

  for (unsigned int i = 0 ; i < m_NumberOfComponents ; i++)
    {
      m_Components.push_back(ComponentType::New()) ;
      m_Components[i]->SetSample(m_Histogram) ;
      m_Estimator->AddComponent(m_Components[i]) ;
    }

  unsigned int numberOfParametersPerComponent =
    TSample::MeasurementVectorSize +
    TSample::MeasurementVectorSize * TSample::MeasurementVectorSize + 1 ;

  m_EstimatedParameters.resize(numberOfParametersPerComponent * m_NumberOfComponents) ;
}


template< class TSample, class THistogram >
void
ExpectationMaximizationClusteringMethod< TSample, THistogram >
::Run()
{
  m_Estimator->SetSample(m_Histogram) ;
  m_Estimator->SetMaximumIteration(m_MaximumIteration) ;

  unsigned int numberOfParametersPerComponent =
    TSample::MeasurementVectorSize +
    TSample::MeasurementVectorSize * TSample::MeasurementVectorSize + 1 ;

  ParametersType componentParams(numberOfParametersPerComponent) ;
  ParametersType proportions(m_NumberOfComponents) ;

  for ( unsigned int i = 0 ; i < m_NumberOfComponents ; i++ )
    {
      for ( unsigned int j = 0 ; j < numberOfParametersPerComponent - 1 ; j++ )
        {
          componentParams[j] =
            m_InitialParameters[numberOfParametersPerComponent * i + j] ;
        }
      proportions[i] =
        m_InitialParameters[numberOfParametersPerComponent * (i + 1) - 1 ] ;
      (m_Components[i])->SetParameters(componentParams) ;
    }

  m_Estimator->SetInitialProportions(proportions) ;
  m_ProcessBegin = clock() ;
  m_Estimator->Update() ;
  m_EstimationEnd = clock() ;

  unsigned int paramIndex = 0 ;
  for ( unsigned int i = 0 ; i < m_NumberOfComponents ; i++ )
    {
      for ( unsigned int j = 0 ; j < numberOfParametersPerComponent -  1 ; j++ )
        {
          m_EstimatedParameters[paramIndex] =
            m_Components[i]->GetFullParameters()[j] ;
          ++paramIndex ;
        }
      m_EstimatedParameters[paramIndex] = (*(m_Estimator->GetProportions()))[i] ;
      ++paramIndex ;
    }

  m_LastIteration = m_Estimator->GetCurrentIteration() ;

  // generates cluster labels
  typename THistogram::InstanceIdentifier histogramId ;
  double maxWeight ;
  unsigned int maxComponentIndex ;
  typename THistogram::MeasurementVectorType tempMeasurementVector ;
  typename TSample::Iterator d_iter = m_Sample->Begin() ;
  typename TSample::Iterator d_end = m_Sample->End() ;
  while( d_iter != d_end )
    {
      for ( unsigned int i = 0 ; i < TSample::MeasurementVectorSize ; i++ )
        {
          tempMeasurementVector[i] = d_iter.GetMeasurementVector()[i] ;
        }
      histogramId =
        m_Histogram->GetInstanceIdentifier(m_Histogram->GetIndex(tempMeasurementVector)) ;
      maxComponentIndex = 0 ;
      maxWeight = 0.0 ;
      for ( unsigned int i = 0 ; i < m_NumberOfComponents ; i++ )
        {
          if ( m_Components[i]->GetWeight(histogramId) > maxWeight )
            {
              maxWeight = m_Components[i]->GetWeight(histogramId) ;
              maxComponentIndex = i ;
            }
        }
      m_ClusterLabels[d_iter.GetInstanceIdentifier()] = maxComponentIndex ;
      ++d_iter ;
    }
  m_ProcessEnd = clock() ;
}

#endif

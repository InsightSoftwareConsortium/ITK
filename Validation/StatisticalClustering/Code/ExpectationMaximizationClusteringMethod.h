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
#ifndef __ExpectationMaximizationClusteringMethod_h
#define __ExpectationMaximizationClusteringMethod_h

#include <time.h>

#include "itkArray.h"
#include "itkVector.h"

#include "itkExpectationMaximizationMixtureModelEstimator.h"
#include "itkGaussianMixtureModelComponent.h"

template< class TSample, class THistogram >
class ExpectationMaximizationClusteringMethod
{
public:
  ExpectationMaximizationClusteringMethod() ;
  ~ExpectationMaximizationClusteringMethod() {}

  typedef itk::Statistics::ExpectationMaximizationMixtureModelEstimator< THistogram >
    EstimatorType ;

  typedef itk::Statistics::GaussianMixtureModelComponent< THistogram >
    ComponentType ;

  typedef itk::hash_map< typename TSample::InstanceIdentifier,
                         unsigned int > ClusterLabelsType ;

  typedef itk::Array< double > ParametersType ;

  void SetInitialParameters(ParametersType& parameters)
  { m_InitialParameters = parameters ; }

  void SetMaximumIteration(unsigned int numberOfIterations)
  { m_MaximumIteration = numberOfIterations ; }

  void SetHistogram(THistogram* histogram)
  { m_Histogram = histogram ; }

  void SetSample(TSample* sample)
  { m_Sample = sample ; }

  void Initialize(unsigned int numberOfClusters) ;

  void Run() ;

  unsigned int GetLastIteration()
  { return m_LastIteration ; }

  ParametersType& GetEstimatedParameters()
  { return m_EstimatedParameters ; }

  ClusterLabelsType* GetClusterLabels()
  { return &m_ClusterLabels ; }

  double GetTotalElapsedTime()
  { return double(m_ProcessEnd - m_ProcessBegin) / CLOCKS_PER_SEC ; }

  double GetEstimationElapsedTime()
  { return double(m_EstimationEnd - m_ProcessBegin) / CLOCKS_PER_SEC ; }

private:
  /** inputs */
  THistogram* m_Histogram ;
  TSample* m_Sample ;
  unsigned int m_NumberOfComponents ;
  ParametersType m_InitialParameters ;
  unsigned int m_MaximumIteration ;

  /** outputs */
  ParametersType m_EstimatedParameters ;
  unsigned int m_LastIteration ;
  time_t m_ProcessBegin ;
  time_t m_EstimationEnd ;
  time_t m_ProcessEnd ;
  itk::hash_map< typename TSample::InstanceIdentifier, unsigned int > m_ClusterLabels ;

  /** helper classes */
  EstimatorType::Pointer m_Estimator ;
  std::vector< typename ComponentType::Pointer > m_Components ;
} ; // end of class

#ifndef ITK_MANUAL_INSTANTIATION
#include "ExpectationMaximizationClusteringMethod.txx"
#endif

#endif // __ExpectationMaximizationClusteringMethod_h

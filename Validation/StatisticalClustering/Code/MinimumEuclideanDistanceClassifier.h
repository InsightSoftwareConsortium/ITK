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
#ifndef __MinimumEuclideanDistanceClassifier_h
#define __MinimumEuclideanDistanceClassifier_h

#include "itkObject.h"
#include "itkArray.h"
#include "itkEuclideanDistance.h"
#include "itkMinimumDecisionRule.h"
#include "itkSampleClassifier.h"

template< class TSample >
class MinimumEuclideanDistanceClassifier
{
public:
  MinimumEuclideanDistanceClassifier() ;
  ~MinimumEuclideanDistanceClassifier() ;

  typedef itk::Statistics::SampleClassifier< TSample > ClassifierType ;
  typedef itk::Statistics::EuclideanDistance< typename TSample::MeasurementVectorType >
    DistanceFunctionType ;
  typedef std::vector< DistanceFunctionType::Pointer > DistanceFunctionVectorType ;
  typedef itk::MinimumDecisionRule DecisionRuleType ;
  typedef itk::Array< double > ParametersType ;
  typedef ClassifierType::OutputType::ClassLabelHolderType ClassLabelsType ;

  void SetSample(TSample* sample) ;

  void SetParameters(ParametersType& parameters) ;

  void SetComponentClassLabels(std::vector< unsigned int >& classLabels) ;

  ClassLabelsType* GetClassLabels() ;

  void GenerateData() ;

private:
  TSample* m_Sample ;
  unsigned int m_NumberOfClasses ;
  ParametersType m_Parameters ;
  std::vector< unsigned int > m_ComponentClassLabels ;
  typename DecisionRuleType::Pointer m_DecisionRule ;
  DistanceFunctionVectorType m_FunctionVector ;
  typename ClassifierType::Pointer m_InternalClassifier ;
} ; // end of class

#ifndef ITK_MANUAL_INSTANTIATION
#include "MinimumEuclideanDistanceClassifier.txx"
#endif

#endif

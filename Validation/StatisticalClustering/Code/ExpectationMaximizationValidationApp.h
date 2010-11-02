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
#ifndef __ExpectationMaximizationValidationApp_h
#define __ExpectationMaximizationValidationApp_h

#include "StatisticalClusteringValidationAppBase.h"
#include "ExpectationMaximizationClusteringMethod.h"

template< class TPixel, unsigned int VMeasurementVectorSize >
class ExpectationMaximizationValidationApp :
  public StatisticalClusteringValidationAppBase< TPixel, VMeasurementVectorSize >
{
public:
  ExpectationMaximizationValidationApp() ;
  ~ExpectationMaximizationValidationApp() ;

  typedef StatisticalClusteringValidationAppBase< TPixel, VMeasurementVectorSize > Superclass ;
  typedef typename Superclass::ParametersType ParametersType ;
  typedef typename Superclass::SampleGeneratorType SampleGeneratorType ;

  typedef ExpectationMaximizationClusteringMethod< typename SampleGeneratorType::SubsampleType,
                                                   typename SampleGeneratorType::HistogramType >
  ClusteringMethodType ;

protected:
  void GenerateSample() ;
  void PrepareInputParameterFilter() ;
  void PrepareOutputParameterTableHeader() ;
  void StartClustering(ParametersType& params) ;
  void MapClusterToClass() ;
  void PutResult(unsigned int caseNo) ;

  ClusteringMethodType m_ClusteringMethod ;
} ; // end of class

#ifndef ITK_MANUAL_INSTANTIATION
#include "ExpectationMaximizationValidationApp.txx"
#endif

#endif

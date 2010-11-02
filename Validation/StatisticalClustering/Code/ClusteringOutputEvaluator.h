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
#ifndef __ClusteringOutputEvaluator_h
#define __ClusteringOutputEvaluator_h

#include "itk_hash_map.h"
#include <vector>
#include <map>
class ClusteringOutputEvaluator
{
public:
  ClusteringOutputEvaluator() ;
  ~ClusteringOutputEvaluator() ;

  typedef std::vector< unsigned int > TrueClassLabelsType ;
  typedef std::vector< unsigned int > ClassLabelsType ;
  typedef itk::hash_map< unsigned long, unsigned int > EstimatedClassLabelsType ;

  void SetTruth(EstimatedClassLabelsType* classLabels) ;

  void SetClusteringResult(EstimatedClassLabelsType* classLabels) ;

  void SetUniqueClassLabels(const ClassLabelsType& classLabels) ;

  void SetClusterMap(ClassLabelsType classLabels)
  { m_ClusterMap = classLabels ; }

  const std::vector< int >& GetComposition(const unsigned int classLabels) const
  { return m_ClassificationMatrix[this->GetClassIndex(classLabels)] ; }

  void GenerateData() ;

protected:
  unsigned int GetClassIndex(const unsigned int classLabel) const ;

  unsigned int GetMappedClassIndex(const unsigned int clusterLabel) const ;

private:
  EstimatedClassLabelsType* m_Truth ;
  EstimatedClassLabelsType* m_Estimates ;
  unsigned int m_NumberOfClasses ;
  std::vector< unsigned int > m_ClassLabels ;
  ClassLabelsType m_ClusterMap ;
  std::vector< std::vector< int > > m_ClassificationMatrix ;
} ; // end of class

#endif

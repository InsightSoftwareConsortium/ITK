/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ClusteringOutputEvaluator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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

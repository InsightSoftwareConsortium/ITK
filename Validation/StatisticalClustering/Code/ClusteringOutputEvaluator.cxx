/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ClusteringOutputEvaluator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "ClusteringOutputEvaluator.h"

ClusteringOutputEvaluator
::ClusteringOutputEvaluator()
{
  m_Truth = 0 ;
  m_NumberOfClasses = 0 ;
}

ClusteringOutputEvaluator 
::~ClusteringOutputEvaluator()
{
}

void
ClusteringOutputEvaluator 
::SetTruth(EstimatedClassLabelsType* classLabels)
{
  m_Truth = classLabels ;
}

void
ClusteringOutputEvaluator 
::SetClusteringResult(EstimatedClassLabelsType* classLabels)
{
  m_Estimates = classLabels ;
}

void
ClusteringOutputEvaluator 
::SetUniqueClassLabels(const std::vector< unsigned int >& classLabels)
{
  m_ClassLabels = classLabels ;
  m_NumberOfClasses = m_ClassLabels.size() ;
//   m_Sizes.resize(m_NumberOfClasses) ;
//   m_NumberOfMatches.resize(m_NumberOfClasses) ;
//   m_InclusionErrors.resize(m_NumberOfClasses) ;
//   m_ExclusionErrors.resize(m_NumberOfClasses) ;
  m_ClassificationMatrix.resize(m_NumberOfClasses) ;
  for ( int i = 0 ; i < m_NumberOfClasses ; i++ )
    {
      m_ClassificationMatrix[i].resize(m_NumberOfClasses) ;
    }
}

unsigned int
ClusteringOutputEvaluator 
::GetClassIndex(const unsigned int classLabel) const
{
  for ( unsigned int i = 0 ; i < m_NumberOfClasses ; i++ )
    {
      if ( classLabel == m_ClassLabels[i])
        {
          return i ;
        }
    }
 
 return 0 ;
}

unsigned int
ClusteringOutputEvaluator
::GetMappedClassIndex(const unsigned int clusterLabel) const
{
  return this->GetClassIndex(m_ClusterMap[clusterLabel]) ;
}

void
ClusteringOutputEvaluator 
::GenerateData()
{
  for ( int i = 0 ; i < m_NumberOfClasses ; i++ )
    {
      std::fill(m_ClassificationMatrix[i].begin(), 
                m_ClassificationMatrix[i].end(), 0) ;
    }

  EstimatedClassLabelsType::iterator t_iter = m_Truth->begin() ;
  unsigned int trueLabel ;
  unsigned int estimatedLabel ;
  while ( t_iter != m_Truth->end() )
    {
//       std::cout << "DEBUG: id = " << (*t_iter).first 
//                 << " true label = " << (*t_iter).second
//                 << " estimated cluster = " << (*(m_Estimates->find((*t_iter).first))).second 
//                 << std::endl ;
      trueLabel = this->GetClassIndex((*t_iter).second) ;
      estimatedLabel = 
        this->GetMappedClassIndex((*(m_Estimates->find((*t_iter).first))).second) ;
      m_ClassificationMatrix[estimatedLabel][trueLabel] += 1 ;
      ++t_iter ;
    }
}

